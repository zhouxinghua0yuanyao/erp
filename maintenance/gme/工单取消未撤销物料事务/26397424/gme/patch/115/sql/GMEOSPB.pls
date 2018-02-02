/* +======================================================================+ */
/* |    Copyright (c) 2005, 2017 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.0.12010000.23=120.0.12020000.23)(115.2=120.1):~PROD:~PATH:~FILE
create or replace package body gme_osp as
/* $Header: GMEOSPB.pls 120.0.12020000.23 2017/06/29 02:55:49 shalchen noship $ */

  g_debug               VARCHAR2 (5)  := NVL(fnd_profile.VALUE ('AFLOG_LEVEL'),-1);
  g_pkg_name   CONSTANT VARCHAR2 (30) := 'GME_OSP';


   /*================================================================================
    Procedure
      create_requisition
    Description
      This procedure is used to insert a row into PO_REQUISITIONS_INTERFACE_ALL
      table while batch creation/batch release/step release if the batch is OSP
      batch.

    Parameters
      P_run_ReqImport            Indicates whether requisition import concurrent program
                                 is invoked automatically.If parameter value is Yes,
                                 the concurrent program will be invokded after record is 
                                 inserted into requisition interface table
      p_addition_flag            Indicates the requisition is created by quantity change
                                 synchronization program.If parametere value is Yes,
                                 that means the requisition is created from quantity
                                 synchronization program.
      p_addition_qty             The quantity of quantity change synchronization.

    History
    =======
       BUG 22086884    06-NOV-2015 Shaliu Chen
                                 modify function Get_Actual_Resource_Rate to remove the logic
                                 of multiplying currency conversion rate of actual_resource_rate
  ================================================================================*/

PROCEDURE create_requisition(
                              p_batch_id              IN NUMBER,
                              p_organization_id       IN NUMBER,
                              p_batchstep_id          IN NUMBER,
                              p_batchstep_resource_id IN NUMBER DEFAULT NULL,
                              P_run_ReqImport         IN NUMBER DEFAULT WIP_CONSTANTS.NO,
                              p_addition_flag         IN VARCHAR2 DEFAULT 'N',
                              p_addition_qty          IN NUMBER   DEFAULT 0,
                              x_return_status         OUT NOCOPY  VARCHAR2,
                              x_message_list          OUT NOCOPY  VARCHAR2,
                              x_message_count         OUT NOCOPY  NUMBER) IS

  l_api_name        CONSTANT VARCHAR2 (30):= 'create_requisition';
  l_po_req_exists   BOOLEAN;

  x_emp_found BOOLEAN;
  x_no_loc_found BOOLEAN;
  x_dummy VARCHAR2(2);

  l_success number := 0 ;
  x_released_revs_type          NUMBER ;
  x_released_revs_meaning       Varchar2(30);
  l_org_acct_ctxt VARCHAR2(30):= 'Accounting Information';
  l_ou_id number;
  l_req_import VARCHAR2(25);
  l_location_id NUMBER;
  l_charge_account_id NUMBER;

  PO_REQ_EXIST       EXCEPTION;
  INVALID_EMPLOYEE   EXCEPTION;
  INVALID_LOCATION   EXCEPTION;
  EXP_ACCOUNT_NOT_DEFINED EXCEPTION;


    /* You cannot create a requisition unless
       you are an employee.  If this returns a row, you are OK */


 CURSOR Get_Location(v_organization_id NUMBER) IS
  SELECT location_id
    FROM po_ship_to_loc_org_v
   WHERE organization_id = v_organization_id
     AND rownum = 1;


 CURSOR Get_Org_Charge_Account(v_organization_id NUMBER) IS
  SELECT expense_account
    FROM mtl_parameters
   WHERE organization_id = v_organization_id;

 CURSOR Get_Item_Charge_Account(v_batchstep_resource_id  NUMBER,
                                v_organization_id        NUMBER ) IS
  SELECT expense_account
    FROM mtl_system_items_b msib,
         gme_batch_step_resources gbsr,
         cr_rsrc_dtl crd
   WHERE gbsr.batchstep_resource_id = v_batchstep_resource_id
     and gbsr.resources = crd.resources
     and crd.organization_id = v_organization_id
     and crd.purchase_item_id = msib.inventory_item_id
     and crd.organization_id  = msib.organization_id;





  BEGIN
    fnd_msg_pub.initialize;

    x_return_status := fnd_api.g_ret_sts_success;

    IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                             || l_api_name);
    END IF;
    /*
      Check whether there is corresponding PO/Req exist for the batch
      If requisition is created by Batch Creation,Batch Release,Step
      Release,there should be not any existing PO/Req linked to the 
      bathch.
      If requistion is created by quantity synchronization program,
      multiple PO/Req is allowed.
    */
    l_po_req_exists:= po_req_exists(p_batch_id          => P_Batch_Id,
                                     p_organization_id  => P_Organization_Id,
                                     p_batchstep_id     => P_Batchstep_Id);
    
    IF (l_po_req_exists AND p_addition_flag <> 'Y') THEN
      --BUG 21641714 If PO/Req exist,skip requisition creation without any error.
      /*gme_common_pvt.log_message('GME_OSP_PO_EXISTS');
      RAISE po_req_exist;*/
      RETURN;
    END IF;


   OPEN Get_Location(p_organization_id);
   FETCH Get_Location INTO l_location_id;
   CLOSE Get_Location;

   IF l_location_id is NULL THEN
     fnd_message.set_name ('PO', 'PO_PO_SHIP_LOCN_INVALID');
     fnd_msg_pub.add;
     RAISE invalid_location;

   END IF;
   
   --BUG 24905205 20-Oct-2016 Shaliu Chen
   --change source code to get charge account from item level first, then org level if it doesn't exist in the item level
   OPEN Get_Item_Charge_Account(P_Batchstep_Resource_id,p_organization_id);
   FETCH Get_Item_Charge_Account INTO l_charge_account_id;
   IF (Get_Item_Charge_Account%NOTFOUND OR l_charge_account_id IS NULL)  THEN
     CLOSE Get_Item_Charge_Account;
     OPEN Get_Org_Charge_Account(p_organization_id);
     FETCH Get_Org_Charge_Account INTO l_charge_account_id;
     IF Get_Org_Charge_Account%NOTFOUND THEN
       CLOSE Get_Org_Charge_Account;  
       fnd_message.set_name ('INV', 'INV_IOI_EXEPENSE_ACCOUNT');
       fnd_message.set_token('ORGANIZATION',p_organization_id);
       fnd_msg_pub.ADD;
       RAISE exp_account_not_defined;       
     ELSE
       CLOSE Get_Org_Charge_Account;  
     END IF;
   ELSE
     CLOSE Get_Item_Charge_Account;
   END IF;      

   /*OPEN Get_Org_Charge_Account(p_organization_id);
   FETCH Get_Org_Charge_Account INTO l_charge_account_id;
   IF Get_Org_Charge_Account%NOTFOUND THEN
     CLOSE Get_Org_Charge_Account;
     OPEN Get_Item_Charge_Account(P_Batchstep_Resource_id,p_organization_id);
     FETCH Get_Item_Charge_Account INTO l_charge_account_id;
     IF (Get_Item_Charge_Account%NOTFOUND OR l_charge_account_id IS NULL)  THEN
       CLOSE Get_Item_Charge_Account;
       fnd_message.set_name ('INV', 'INV_IOI_EXEPENSE_ACCOUNT');
       fnd_message.set_token('ORGANIZATION',p_organization_id);
       fnd_msg_pub.ADD;
       RAISE exp_account_not_defined;
     ELSE
       CLOSE Get_Item_Charge_Account;
     END IF;
   ELSE
     CLOSE Get_Org_Charge_Account;
   END IF;*/


    IF g_debug <= gme_debug.g_log_procedure THEN
         gme_debug.put_line ('Starting insert record into PO_REQUISITIONS_INTERFACE_ALL table');
    END IF;


    INSERT INTO PO_REQUISITIONS_INTERFACE_ALL
      ( last_update_date,
        last_updated_by,
        creation_date,
        created_by,
        last_update_login,
        request_id,
        program_application_id,
        program_id,
        program_update_date,
        org_id,
        preparer_id,
        interface_source_code,
        authorization_status,
        source_type_code,
        destination_organization_id,
        destination_type_code,
        item_id,
        item_revision,
        uom_code,
        quantity,
        line_type_id,
        charge_account_id,
        deliver_to_location_id,
        deliver_to_requestor_id,
        wip_entity_id,
        wip_operation_seq_num,
        wip_resource_seq_num,
        bom_resource_id,
        need_by_date,
        autosource_flag,
        group_code,
        suggested_buyer_id
      )
      SELECT SYSDATE,
             FND_GLOBAL.USER_ID,
             SYSDATE,
             FND_GLOBAL.USER_ID,
             FND_GLOBAL.LOGIN_ID,
             FND_GLOBAL.CONC_REQUEST_ID,
             FND_GLOBAL.PROG_APPL_ID,
             FND_GLOBAL.CONC_PROGRAM_ID,
             SYSDATE,
             TO_NUMBER(hoi.ORG_INFORMATION3) operating_unit,
             fu.employee_id,
             'OPM-OSP', --need to confirm interface_source_code
             'APPROVED',
             'VENDOR',  --need to confirm source_type_code
             gbsr.organization_id,
             'SHOP FLOOR',
             crd.purchase_item_id,
              DECODE (msi.revision_qty_control_code,
                     1, null ,
                     2, Get_Item_Revision(crd.purchase_item_id,p_organization_id,gbh.plan_start_date)),
             msi.primary_uom_code,
             DECODE(p_addition_flag,'N'
                                   ,DECODE(msi.outside_operation_uom_type,'RESOURCE'
                                                                         ,gbsr.PLAN_RSRC_USAGE
                                                                         ,'ASSEMBLY'
                                                                         ,DECODE(gbh.batch_status,GME_COMMON_PVT.g_batch_pending
                                                                                                 ,gmd.plan_qty
                                                                                                 ,GME_COMMON_PVT.g_batch_wip
                                                                                                 ,gmd.wip_plan_qty
                                                                                                 ,gmd.plan_qty))
                                  ,'Y'
                                  ,p_addition_qty),

             3,
             l_charge_account_id,
             l_location_id,
             fu.employee_id,
             gbh.batch_id,
             gbsr.batchstep_id,
             gbsr.batchstep_resource_id,
             crd.resource_id,
             gbs.plan_cmplt_date,
             'Y',
             NULL,
             msi.buyer_id
        FROM hr_organization_information hoi,
             fnd_user fu,
             mtl_system_items msi,
             cr_rsrc_mst_b crm,
             cr_rsrc_dtl crd,
             gme_batch_step_resources gbsr,
             gme_batch_steps gbs,
             gme_batch_header gbh,
             gme_material_details gmd,
             wip_entities we
       WHERE gbh.organization_id = P_Organization_Id
         AND gbh.batch_id = P_Batch_Id
         AND gmd.batch_id = gbh.batch_id
         AND we.wip_entity_id = gbh.batch_id
         AND gmd.line_type = gme_common_pvt.g_line_type_prod
         AND gmd.inventory_item_id = we.primary_item_id
         AND gbh.batch_id = gbs.batch_id
         AND gbs.batchstep_id = P_Batchstep_Id
         AND gbsr.batch_id = gbh.batch_id
         AND gbsr.batchstep_id = gbs.batchstep_id
         AND gbsr.batchstep_resource_id = P_Batchstep_Resource_id
         AND crd.resources = gbsr.resources
         AND crd.organization_id = gbsr.organization_id
         AND crd.purchase_item_id IS NOT NULL
         AND crd.resources = crm.resources
         AND NVL(crm.outside_process_ind,gme_osp.g_osp_ind_disabled) = gme_osp.g_osp_ind_enabled
         AND crd.purchase_item_id = msi.inventory_item_id
         AND msi.organization_id = P_Organization_Id
         AND hoi.organization_id = P_Organization_Id
         AND hoi.ORG_INFORMATION_CONTEXT = l_org_acct_ctxt
         AND FND_GLOBAL.User_Id = fu.user_id;


    IF (P_Run_ReqImport = GME_OSP.YES) THEN

      -- get the OU, set context for MOAC
      SELECT to_number(org_information3) INTO l_ou_id
        FROM hr_organization_information
       WHERE organization_id = p_organization_id
         AND org_information_context = l_org_acct_ctxt;


      FND_REQUEST.SET_ORG_ID (l_ou_id);



      BEGIN
        SELECT reqimport_group_by_code
        INTO l_req_import
        FROM po_system_parameters_all
        WHERE org_id = l_ou_id;
      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          RAISE fnd_api.g_exc_unexpected_error;
      END;

      l_success := fnd_request.submit_request(
        'PO', 'REQIMPORT', NULL, NULL, FALSE,'GME', NULL, l_req_import, --Fix for bug 8919025(Fp 8850950)
        NULL ,'N', 'Y' , chr(0), NULL, NULL, NULL,
        NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
        NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
        NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
        NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
        NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
        NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
        NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
        NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
        NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL
        ) ;
    END IF;

EXCEPTION
  WHEN po_req_exist THEN
     x_return_status := fnd_api.g_ret_sts_error;
     gme_common_pvt.count_and_get (x_count        => x_message_count
                                  ,p_encoded      => fnd_api.g_false
                                  ,x_data         => x_message_list);

  WHEN invalid_location THEN
     x_return_status := fnd_api.g_ret_sts_error;
     gme_common_pvt.count_and_get (x_count        => x_message_count
                                  ,p_encoded      => fnd_api.g_false
                                  ,x_data         => x_message_list);

  WHEN exp_account_not_defined THEN
     x_return_status := fnd_api.g_ret_sts_error;
     gme_common_pvt.count_and_get (x_count        => x_message_count
                                  ,p_encoded      => fnd_api.g_false
                                  ,x_data         => x_message_list);
  WHEN OTHERS THEN
    x_return_status := fnd_api.g_ret_sts_unexp_error;
    x_message_list := 'unexpected error: ' || SQLERRM || 'SQLCODE = ' || SQLCODE;
    IF (g_debug <= gme_debug.g_log_procedure) THEN
     gme_debug.put_line(x_message_list);
    END IF;

END CREATE_REQUISITION;



FUNCTION Get_Item_Revision(p_item_id         IN NUMBER,
                           p_organization_id IN NUMBER,
                           p_rev_date        IN DATE) 
RETURN VARCHAR2 IS

  l_revision VARCHAR2(3);
  x_message_list VARCHAR2(240);
BEGIN
  SELECT revision
    INTO l_revision
    FROM mtl_item_revisions_b mir
   WHERE inventory_item_id = p_item_id
     AND organization_id = p_organization_id
     AND mir.effectivity_date <= p_rev_date;

  RETURN l_revision;
EXCEPTION
  WHEN NO_DATA_FOUND THEN
    RETURN NULL;
  WHEN OTHERS THEN
    x_message_list := 'unexpected error: ' || SQLERRM || 'SQLCODE = ' || SQLCODE;
    IF (g_debug <= gme_debug.g_log_procedure) THEN
     gme_debug.put_line(x_message_list);
    END IF;
    RETURN NULL;


END Get_Item_Revision;


 /*================================================================================
  Procedure
    po_req_exists
  Description
    Check whether there is open PO/Req linked to OPM batch or batch step.

  Parameters
    p_batchstep_id             If this parameter value is NULL,that mean the program 
                               will check whether there is open PO/Req linked to the 
                               batch,If this parameter value is NOT NULL,that mean the
                               program is checking whether there is open PO/Req linked 
                               to the batch step.

  History
  =======

================================================================================*/
FUNCTION po_req_exists (p_batch_id         IN NUMBER,
                        p_organization_id  IN NUMBER,
                        p_batchstep_id     IN NUMBER DEFAULT NULL
                         ) RETURN BOOLEAN IS

  /*
    Query PO,Requisition,Requistion Interface table
    respectively
  */
  --26306441  29-JUN-2017  Shaliu Chen
  --Add 'CLOSED' as one of closed status
  CURSOR check_po_req_cur IS
    SELECT 'PO/REQ Linked'
      FROM po_releases_all pr,
           po_headers_all ph,
           po_distributions_all pd,
           po_line_locations_all pll
     WHERE pd.po_line_id IS NOT NULL
       AND pd.line_location_id IS NOT NULL
       AND pd.wip_entity_id = p_batch_id
       AND pd.destination_organization_id = p_organization_id
       AND (p_batchstep_id is NULL OR pd.wip_operation_seq_num = p_batchstep_id)
       AND ph.po_header_id = pd.po_header_id
       AND pll.line_location_id = pd.line_location_id
       AND pr.po_release_id (+) = pd.po_release_id
       AND (pll.cancel_flag IS NULL OR pll.cancel_flag = 'N')
       AND (nvl(pll.closed_code,'OPEN') NOT IN ('FINALLY CLOSED','CLOSED FOR RECEIVING','CLOSED') OR 
            (nvl(pll.closed_code,'OPEN') = 'CLOSED FOR RECEIVING'
             AND (NVL(pll.quantity_received,0) <>
                                 (SELECT NVL(SUM(NVL(pd2.quantity_delivered,0)),0)
                                    FROM po_distributions_all pd2
                                   WHERE pd2.line_location_id = pll.line_location_id
                                     AND pd2.line_location_id IS NOT NULL
                                     AND pd2.po_line_id IS NOT NULL))))
   UNION ALL
    SELECT 'PO/REQ Linked'
      FROM po_requisition_lines_all prl
     WHERE prl.wip_entity_id = p_batch_id
       AND prl.destination_organization_id = p_organization_id
       AND (p_batchstep_id is null or
            prl.wip_operation_seq_num = p_batchstep_id)
       AND nvl(prl.cancel_flag, 'N') = 'N'
       AND prl.line_location_id is NULL
   UNION ALL
    SELECT 'PO/REQ Linked'
      FROM po_requisitions_interface_all pri
     WHERE pri.wip_entity_id = p_batch_id
       AND pri.destination_organization_id = p_organization_id
       AND (p_batchstep_id is null or pri.wip_operation_seq_num = p_batchstep_id);


  po_req_exist VARCHAR2(20);

BEGIN
    OPEN check_po_req_cur;
    FETCH check_po_req_cur INTO po_req_exist;

    IF (check_po_req_cur%FOUND) THEN
      CLOSE check_po_req_cur;
      RETURN TRUE;
    ELSE
      CLOSE check_po_req_cur;
      RETURN FALSE;
    END IF;

END PO_REQ_EXISTS;



 /*================================================================================
  Procedure
    update_document
  Description
    Calls the PO Change API, which validates and applies the requested
    changes and any derived changes to the Purchase Order, Purchase
    Agreement, or Release.
  Parameters

  History
  =======

================================================================================*/

PROCEDURE update_document (
                            p_api_version            IN NUMBER,
                            p_init_msg_list          IN VARCHAR2,
                            x_return_status          OUT NOCOPY VARCHAR2,
                            p_changes                IN OUT NOCOPY PO_CHANGES_REC_TYPE,
                            p_run_submission_checks  IN VARCHAR2,
                            p_launch_approvals_flag  IN VARCHAR2,
                            p_buyer_id               IN NUMBER,
                            p_update_source          IN VARCHAR2,
                            p_override_date          IN DATE,
                            x_api_errors             OUT NOCOPY PO_API_ERRORS_REC_TYPE)
 IS
  l_api_name     CONSTANT VARCHAR(30) := 'UPDATE_DOCUMENT';
  l_api_version  CONSTANT NUMBER := 1.0;
BEGIN

  IF NOT FND_API.Compatible_API_Call ( l_api_version, p_api_version,
                                       l_api_name, g_pkg_name ) THEN
    RAISE FND_API.G_EXC_UNEXPECTED_ERROR;
  END IF;
  

  IF (FND_API.to_boolean(p_init_msg_list)) THEN
    FND_MSG_PUB.initialize;
  END IF;
  
  x_return_status := fnd_api.g_ret_sts_success;
  
  IF (g_debug <= gme_debug.g_log_procedure) THEN

    gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                       || l_api_name);
  END IF;
  -- Call the PO Change API.
  po_document_update_grp.update_document(
                                  p_api_version           => 1.0,
                                  p_init_msg_list         => p_init_msg_list,
                                  x_return_status         => x_return_status,
                                  p_changes               => p_changes,
                                  p_run_submission_checks => p_run_submission_checks,
                                  p_launch_approvals_flag => p_launch_approvals_flag,
                                  p_buyer_id              => p_buyer_id,
                                  p_update_source         => p_update_source,
                                  p_override_date         => p_override_date,
                                  x_api_errors            => x_api_errors
                                );                                

EXCEPTION
  WHEN FND_API.G_EXC_UNEXPECTED_ERROR THEN

    PO_DOCUMENT_UPDATE_PVT.add_message_list_errors (
                            p_api_errors => x_api_errors,
                            x_return_status => x_return_status
                            );
    x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
  WHEN OTHERS THEN

    PO_DEBUG.handle_unexp_error ( p_pkg_name => g_pkg_name,
                                  p_proc_name => l_api_name );

    PO_DOCUMENT_UPDATE_PVT.add_message_list_errors (
      p_api_errors => x_api_errors,
      x_return_status => x_return_status
    );
    x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;

END update_document;


 /*================================================================================
  Procedure
    cancel_document
  Description
    Wrapper to Update Requisition. Calls Requisition Update Group API

  Parameters

  History
  =======

================================================================================*/

PROCEDURE update_requisition (
    p_api_version            IN NUMBER,
    p_req_changes            IN OUT NOCOPY PO_REQ_CHANGES_REC_TYPE,
    p_update_source          IN VARCHAR2,
    x_return_status          OUT NOCOPY VARCHAR2,
    x_msg_count              OUT NOCOPY NUMBER,
    x_msg_data               OUT NOCOPY VARCHAR2
) IS

l_api_name     CONSTANT VARCHAR(30) := 'UPDATE_REQUISITION';
l_errMsg       VARCHAR2(240);
BEGIN
 
  FND_MSG_PUB.initialize;

  
  x_return_status := fnd_api.g_ret_sts_success;  

  IF (g_debug <= gme_debug.g_log_procedure) THEN

    gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                       || l_api_name);
  END IF;
  /*
   Call the group procedure to update requisition
  */
  PO_REQ_DOCUMENT_UPDATE_GRP.update_requisition(
                                        p_api_version    => p_api_version,
                                        p_req_changes    => p_req_changes,
                                        p_update_source  => p_update_source,
                                        x_return_status  => x_return_status,
                                        x_msg_count      => x_msg_count,
                                        x_msg_data       => x_msg_data);
EXCEPTION
  WHEN OTHERS THEN
    x_return_status := fnd_api.g_ret_sts_unexp_error;
    l_errMsg := 'unexpected error: ' || SQLERRM || 'SQLCODE = ' || SQLCODE;
    x_msg_data := l_errMsg;
    gme_debug.put_line('The procedure UpdtePOREqNBD failed:'||l_errMsg);
    fnd_message.set_name('FND', 'FND_GENERIC_MESSAGE');
    fnd_message.set_token('MESSAGE', l_errMsg);
    fnd_msg_pub.add;

 

END update_requisition;


 /*================================================================================
  Procedure
    cancelPOReq
  Description
    Wrapper to cancel the PO/Req which is linked to OPM batch

  Parameters
    p_batchstep_id             If this parameter value is NULL,that mean the program 
                               will cancel all of open PO/Req linked to the batch,
                               If this parameter value is NOT NULL,that mean the
                               program is cancel all of open PO/Req linked to the 
                               batch step.

  History
  =======

================================================================================*/

PROCEDURE cancelPOReq (p_batch_id      IN         NUMBER,
                       p_org_id        IN         NUMBER,
                       p_batchstep_id  IN         NUMBER :=NULL,
                       x_return_status OUT NOCOPY VARCHAR2,
                       x_message_list  OUT NOCOPY  VARCHAR2,
                       x_message_count OUT NOCOPY  NUMBER
                       ) IS

  CURSOR c_po_req IS
    SELECT pd.po_header_id po_header_id,
           to_number(null) po_release_id,
           pd.po_line_id po_line_id,
           pd.line_location_id po_line_location_id,
           to_number(null) req_header_id,
           to_number(null) req_line_id,
           ph.type_lookup_code po_req_type,
           ph.authorization_status approval_status,
           'PO' document_type,
           ph.type_lookup_code document_subtype,
           pd.org_id ou_id -- operating unit
      FROM po_distributions_all pd,
           po_headers_all ph,
           po_line_locations_all pll
     WHERE pd.po_line_id IS NOT NULL
       AND pd.line_location_id IS NOT NULL
       AND ph.type_lookup_code = 'STANDARD'
       AND ph.po_header_id = pd.po_header_id
       AND pd.line_location_id = pll.line_location_id
       AND pd.wip_entity_id = p_batch_id
       AND pd.destination_organization_id = p_org_id
       AND (p_batchstep_id IS NULL OR
            pd.wip_operation_seq_num = p_batchstep_id)
       AND (pll.cancel_flag IS NULL OR pll.cancel_flag = 'N')
   UNION ALL
    SELECT pd.po_header_id po_header_id,
           pr.po_release_id po_release_id,
           to_number(null) po_line_id,
           pd.line_location_id po_line_location_id,
           to_number(null) req_header_id,
           to_number(null) req_line_id,
           ph.type_lookup_code po_req_type,
           pr.authorization_status approval_status,
           'RELEASE' document_type,
           ph.type_lookup_code document_subtype,
           pd.org_id ou_id -- operating unit
      FROM po_distributions_all pd,
           po_headers_all ph,
           po_line_locations_all pll,
           po_releases_all pr
     WHERE pd.po_line_id IS NOT NULL
       AND pd.line_location_id IS NOT NULL
       AND ph.type_lookup_code = 'BLANKET'
       AND pr.po_header_id = ph.po_header_id
       AND pr.po_release_id = pd.po_release_id
       AND ph.po_header_id = pd.po_header_id
       AND pd.line_location_id = pll.line_location_id
       AND pd.wip_entity_id = p_batch_id
       AND pd.destination_organization_id = p_org_id
       AND (p_batchstep_id IS NULL OR
            pd.wip_operation_seq_num = p_batchstep_id)
       AND (pll.cancel_flag IS NULL OR pll.cancel_flag = 'N')
   UNION ALL
    SELECT to_number(null) po_header_id,
           to_number(null) po_release_id,
           to_number(null) po_line_id,
           to_number(null) po_line_location_id,
           prl.requisition_header_id req_header_id,
           prl.requisition_line_id req_line_id,
           'REQUISITION' po_req_type,
           prh.authorization_status approval_status,
           to_char(null) document_type,
           to_char(null) document_subtype,
           prl.org_id ou_id -- operating unit
      FROM po_requisition_headers_all prh,
           po_requisition_lines_all prl
     WHERE NOT EXISTS
          (SELECT 'x'
             FROM po_line_locations_all pll
            WHERE prl.line_location_id = pll.line_location_id)
       AND prh.requisition_header_id = prl.requisition_header_id
       AND prl.wip_entity_id = p_batch_id
       AND prl.destination_organization_id = p_org_id
       AND (p_batchstep_id IS NULL OR
            prl.wip_operation_seq_num = p_batchstep_id)
       AND (prl.cancel_flag IS NULL OR
            prl.cancel_flag = 'N');
            
  CURSOR get_pending_requisition IS
   SELECT count(1)
     FROM po_requisitions_interface_all
    WHERE wip_entity_id = p_batch_id
      AND (p_batchstep_id IS NULL OR
           wip_operation_seq_num  = p_batchstep_id)  
      AND interface_source_code = 'OPM-OSP';          

  l_api_name     CONSTANT VARCHAR2 (30):= 'cancelPOReq';
  l_err_count    NUMBER := 0;
  l_pending_recs NUMBER;
  l_params       wip_logger.param_tbl_t;
  l_logLevel     NUMBER := fnd_log.g_current_runtime_level;
  l_returnStatus VARCHAR2(1);
  l_errMsg       VARCHAR2(240);
  l_debugMsg     VARCHAR2(240);
  l_msgCount     NUMBER;
  l_msgData      VARCHAR2(2000);
  l_po_req c_po_req%ROWTYPE;
BEGIN

  fnd_msg_pub.initialize;

  IF (g_debug <= gme_debug.g_log_procedure) THEN
    gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                       || l_api_name);
    gme_debug.put_line('p_batch_id:'||p_batch_id);
    gme_debug.put_line('p_org_id:'||p_org_id);
    gme_debug.put_line('p_batch_id:'||p_batch_id);

  END IF;
  x_return_status  := fnd_api.g_ret_sts_success;
  /*
  Call PO API to cancel PO/release. If unable to cancel PO/release
  for any reason,skip the error one and try to cancel the next one.
  */
  FOR l_po_req IN c_po_req LOOP
    BEGIN
      mo_global.set_policy_context('S',l_po_req.ou_id);
      IF (l_po_req.po_req_type IN ('STANDARD', 'BLANKET'))THEN


        IF (g_debug <= gme_debug.g_log_procedure) THEN
          l_debugMsg := 'po_header_id = ' || l_po_req.po_header_id|| ' ; ' ||
                        'po_release_id = ' || l_po_req.po_release_id || ' ; '
                        ||
                        'po_line_id = ' || l_po_req.po_line_id || ' ; ' ||
                        'po_line_location_id = ' ||
                        l_po_req.po_line_location_id;
          gme_debug.put_line(l_debugMsg);

        END IF;

        -- Call PO API to cancel requisition
        cancel_document
         (p_api_version      => 1.0,
          p_doc_type         => PO_TBL_VARCHAR30(l_po_req.document_type),
          p_doc_subtype      => PO_TBL_VARCHAR30(l_po_req.document_subtype),
          p_doc_id           => PO_TBL_NUMBER(l_po_req.po_header_id),
          p_doc_num          => PO_TBL_VARCHAR30(NULL),
          p_release_id       => PO_TBL_NUMBER(l_po_req.po_release_id),
          p_release_num      => PO_TBL_NUMBER(NULL),
          p_doc_line_id      => PO_TBL_NUMBER(l_po_req.po_line_id),
          p_doc_line_num     => PO_TBL_NUMBER(NULL),
          p_doc_line_loc_id  => PO_TBL_NUMBER(l_po_req.po_line_location_id),
          p_doc_shipment_num => PO_TBL_NUMBER(NULL),
          p_source           => NULL,
          p_cancel_date      => SYSDATE,
          p_cancel_reason    => NULL,
          p_cancel_reqs_flag => 'Y',
          p_print_flag       => 'N',
          p_note_to_vendor   => NULL,
          x_return_status    => x_return_status,
          x_msg_count        => l_msgCount,
          x_msg_data         => l_msgData);

        IF(x_return_status <> fnd_api.g_ret_sts_success) THEN
          raise fnd_api.g_exc_unexpected_error;
        END IF;
      ELSE
        IF (g_debug <= gme_debug.g_log_procedure) THEN
          l_debugMsg := 'req_header_id = ' || l_po_req.req_header_id|| ' ; '
                        || 'req_line_id = ' || l_po_req.req_line_id;
          gme_debug.put_line(l_debugMsg);

        END IF;
        -- Call PO API to cancel requisition
        cancel_requisition
         (p_api_version   => 1.0,
          p_req_header_id => PO_TBL_NUMBER(l_po_req.req_header_id),
          p_req_line_id   => PO_TBL_NUMBER(l_po_req.req_line_id),
          p_cancel_date   => SYSDATE,
          p_cancel_reason => NULL,
          p_source        => NULL,
          x_return_status => x_return_status,
          x_msg_count     => l_msgCount,
          x_msg_data      => l_msgData);

        IF(x_return_status <> fnd_api.g_ret_sts_success) THEN
          raise fnd_api.g_exc_unexpected_error;
        END IF;
      END IF;
    EXCEPTION
      WHEN OTHERS THEN
        l_err_count := l_err_count + 1;
        l_errMsg := 'unexpected error: ' || SQLERRM || 'SQLCODE = ' || SQLCODE;
        IF (g_debug <= gme_debug.g_log_procedure) THEN
          gme_debug.put_line(l_errMsg);
        END IF;
    END;
  END LOOP;

  IF (l_err_count > 0) THEN
    x_return_status := fnd_api.g_ret_sts_unexp_error;
    fnd_message.set_name('GME','GME_CANCEL_PO_FAILED');
    fnd_msg_pub.add;
  END IF;

  IF (g_debug <= gme_debug.g_log_procedure) THEN
    gme_debug.put_line(g_pkg_name || '.'|| l_api_name||'complete successfully');
  END IF;
  
  /*
    check whether there is pending requisition record in the requisition open interface
    table,if exists,delete all of pending requisition records
  */
  OPEN get_pending_requisition;
  FETCH get_pending_requisition INTO l_pending_recs;
  CLOSE get_pending_requisition;
  
  IF l_pending_recs > 0 THEN
    DELETE FROM po_requisitions_interface_all
          WHERE interface_source_code = 'OPM-OSP'
            AND wip_entity_id = p_batch_id
            AND (p_batchstep_id IS NULL OR wip_operation_seq_num  = p_batchstep_id);   
  
  END IF; 
  
  
EXCEPTION
  WHEN fnd_api.g_exc_unexpected_error THEN
    x_return_status := fnd_api.g_ret_sts_unexp_error;
    gme_common_pvt.count_and_get (x_count        => x_message_count
                                 ,p_encoded      => fnd_api.g_false
                                 ,x_data         => x_message_list);
  WHEN OTHERS THEN
    x_return_status := fnd_api.g_ret_sts_unexp_error;
    l_errMsg := 'unexpected error: ' || SQLERRM || 'SQLCODE = ' || SQLCODE;
    IF (g_debug <= gme_debug.g_log_procedure) THEN
      gme_debug.put_line(l_errMsg);
    END IF;
    fnd_message.set_name('FND', 'FND_GENERIC_MESSAGE');
    fnd_message.set_token('MESSAGE', l_errMsg);
    fnd_msg_pub.add;
   gme_common_pvt.count_and_get (x_count        => x_message_count
                                ,p_encoded      => fnd_api.g_false
                                ,x_data         => x_message_list);
                                
                            
END cancelPOReq;


 /*================================================================================
  Procedure
    cancel_document
  Description
    Wrapper to Cancel Purchasing Document.
    Calls the PO Control API with Action=CANCEL to cancel the Purchase Order,
    Purchase Agreement, or Release.
  Parameters

  History
  =======

================================================================================*/

PROCEDURE cancel_document (
    p_api_version            IN NUMBER,
    p_doc_type               IN PO_TBL_VARCHAR30,
    p_doc_subtype            IN PO_TBL_VARCHAR30,
    p_doc_id                 IN PO_TBL_NUMBER,
    p_doc_num                IN PO_TBL_VARCHAR30,
    p_release_id             IN PO_TBL_NUMBER,
    p_release_num            IN PO_TBL_NUMBER,
    p_doc_line_id            IN PO_TBL_NUMBER,
    p_doc_line_num           IN PO_TBL_NUMBER,
    p_doc_line_loc_id        IN PO_TBL_NUMBER,
    p_doc_shipment_num       IN PO_TBL_NUMBER,
    p_source                 IN VARCHAR2,
    p_cancel_date            IN DATE,
    p_cancel_reason          IN VARCHAR2,
    p_cancel_reqs_flag       IN VARCHAR2,
    p_print_flag             IN VARCHAR2,
    p_note_to_vendor         IN VARCHAR2,
    x_return_status          OUT NOCOPY  VARCHAR2,
    x_msg_count              OUT NOCOPY  NUMBER,
    x_msg_data               OUT NOCOPY  VARCHAR2
) IS

l_api_name     CONSTANT VARCHAR(30) := 'CANCEL_DOCUMENT';
l_errMsg       VARCHAR2(240);

BEGIN

  fnd_msg_pub.initialize;
      
  IF (g_debug <= gme_debug.g_log_procedure) THEN
    gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                       || l_api_name);
  END IF;

  x_return_status  := fnd_api.g_ret_sts_success;
  
  /*
   Call Control Document API to Cancel PO Documents
  */
  FOR i IN 1..p_doc_id.count LOOP

    PO_Document_Control_GRP.control_document
                           (p_api_version      => p_api_version,
                            p_init_msg_list    => FND_API.G_FALSE,
                            p_commit           => FND_API.G_FALSE,
                            x_return_status    => x_return_status,
                            p_doc_type         => p_doc_type(i),
                            p_doc_subtype      => p_doc_subtype(i),
                            p_doc_id           => p_doc_id(i),
                            p_doc_num          => p_doc_num(i),
                            p_release_id       => p_release_id(i),
                            p_release_num      => p_release_num(i),
                            p_doc_line_id      => p_doc_line_id(i),
                            p_doc_line_num     => p_doc_line_num(i),
                            p_doc_line_loc_id  => p_doc_line_loc_id(i),
                            p_doc_shipment_num => p_doc_shipment_num(i),
                            p_source           => p_source,
                            p_action           => 'CANCEL',
                            p_action_date      => p_cancel_date,
                            p_cancel_reason    => p_cancel_reason,
                            p_cancel_reqs_flag => p_cancel_reqs_flag,
                            p_print_flag       => p_print_flag,
                            p_note_to_vendor   => p_note_to_vendor);

      IF (x_return_status = FND_API.g_ret_sts_error) THEN
        RAISE FND_API.g_exc_error;
      ELSIF (x_return_status = FND_API.g_ret_sts_unexp_error) THEN
        RAISE FND_API.g_exc_unexpected_error;
      END IF;
  END LOOP;

EXCEPTION
    WHEN FND_API.G_EXC_ERROR THEN
      x_return_status := FND_API.G_RET_STS_ERROR;
      gme_common_pvt.count_and_get (x_count        => x_msg_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_msg_data);
    WHEN FND_API.G_EXC_UNEXPECTED_ERROR THEN
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      gme_common_pvt.count_and_get (x_count        => x_msg_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_msg_data);
    WHEN OTHERS THEN
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      l_errMsg := 'unexpected error: ' || SQLERRM || 'SQLCODE = ' || SQLCODE;
      gme_debug.put_line('l_errMsg:'||l_errMsg);
      fnd_message.set_name('FND', 'FND_GENERIC_MESSAGE');
      fnd_message.set_token('MESSAGE', l_errMsg);
      fnd_msg_pub.add;
      gme_common_pvt.count_and_get (x_count        => x_msg_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_msg_data);
                       
END cancel_document;

 /*================================================================================
  Procedure
    cancel_document
  Description
    Wrapper to Cancel Requisition
  Parameters

  History
  =======

================================================================================*/

PROCEDURE cancel_requisition (
    p_api_version            IN NUMBER,
    p_req_header_id          IN PO_TBL_NUMBER,
    p_req_line_id            IN PO_TBL_NUMBER,
    p_cancel_date            IN DATE,
    p_cancel_reason          IN VARCHAR2,
    p_source                 IN VARCHAR2,
    x_return_status          OUT NOCOPY  VARCHAR2,
    x_msg_count              OUT NOCOPY  NUMBER,
    x_msg_data               OUT NOCOPY  VARCHAR2
) IS
l_api_name     CONSTANT VARCHAR(30) := 'CANCEL_REQUISITION';
l_errMsg       VARCHAR2(240);

BEGIN
  fnd_msg_pub.initialize;
    
  IF (g_debug <= gme_debug.g_log_procedure) THEN
    gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                       || l_api_name);
  END IF;

  x_return_status  := fnd_api.g_ret_sts_success;
  
  /*
   Call the group procedure to cancel requisition
  */
  PO_REQ_DOCUMENT_CANCEL_GRP.cancel_requisition(
                                  p_api_version     => 1.0,
                                  p_req_header_id   => p_req_header_id,
                                  p_req_line_id     => p_req_line_id,
                                  p_cancel_date     => p_cancel_date,
                                  p_cancel_reason   => p_cancel_reason,
                                  p_source          => p_source,
                                  x_return_status   => x_return_status,
                                  x_msg_count       => x_msg_count,
                                  x_msg_data        => x_msg_data);

    IF (x_return_status = fnd_api.g_ret_sts_error) THEN
      RAISE fnd_api.g_exc_error;
    ELSIF (x_return_status = fnd_api.g_ret_sts_unexp_error) THEN
      RAISE fnd_api.g_exc_unexpected_error;
    END IF;
EXCEPTION
    WHEN fnd_api.g_exc_error THEN
      x_return_status := fnd_api.g_ret_sts_error;
      gme_common_pvt.count_and_get (x_count        => x_msg_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_msg_data);
    WHEN fnd_api.g_exc_unexpected_error THEN
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      gme_common_pvt.count_and_get (x_count        => x_msg_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_msg_data);
    WHEN OTHERS THEN
      x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
      l_errMsg := 'unexpected error: ' || SQLERRM || 'SQLCODE = ' || SQLCODE;
      gme_debug.put_line('l_errMsg:'||l_errMsg);
      fnd_message.set_name('FND', 'FND_GENERIC_MESSAGE');
      fnd_message.set_token('MESSAGE', l_errMsg);
      fnd_msg_pub.add;
      gme_common_pvt.count_and_get (x_count        => x_msg_count
                                  ,p_encoded      => fnd_api.g_false
                                  ,x_data         => x_msg_data);
                       
END cancel_requisition;


 /*================================================================================
  Procedure
    reroute_batch_process
  Description
    The program is uesed to cancel existing PO or create new PO for batch reroute 
    action.
    If the existing OSP step doesn't exist after batch reroute,the existing PO/Req
    should be cancelled at the same time.
    If a new OSP step is present after batch reroute, a new requisition should be
    created at the same time.

  Parameters


  History
  =======

================================================================================*/
PROCEDURE reroute_batch_process(p_batch_id         IN NUMBER,
                                p_organization_id  IN NUMBER,
                                x_return_status    OUT NOCOPY VARCHAR2,
                                x_message_list     OUT NOCOPY  VARCHAR2,
                                x_message_count    OUT NOCOPY  NUMBER) IS
                                
  l_api_name     CONSTANT VARCHAR(30) := 'reroute_batch_process'; 
  

  l_recreate_po_flag           BOOLEAN;
  l_osp_release                BOOLEAN;
  l_propagate_change_to_po     NUMBER;
  l_batchstep_id               NUMBER;
  l_batchstep_resource_id      NUMBER;
  l_addition_qty               NUMBER;
  l_item_unit_type             VARCHAR2(25);
  l_errMsg                     VARCHAR2(2000);
  get_item_unit_type_failed    EXCEPTION;
  cancel_po_failed             EXCEPTION;
  recreate_po_failed           EXCEPTION; 
  
  CURSOR get_osp_batch_info(v_batch_id NUMBER) IS
  SELECT gbsr.batchstep_id,
         gbsr.batchstep_resource_id
    FROM gme_batch_step_resources gbsr,
         cr_rsrc_dtl crd,
         cr_rsrc_mst crm
   WHERE gbsr.batch_id = v_batch_id
     AND gbsr.resources = crd.resources
     AND gbsr.organization_id = crd.organization_id
     AND crd.resources = crm.resources
     AND crd.purchase_item_id IS NOT NULL
     AND NVL(crm.outside_process_ind,gme_osp.g_osp_ind_disabled) = gme_osp.g_osp_ind_enabled;
      
  
                                
                                
BEGIN
    
  fnd_msg_pub.initialize;
      
  IF (g_debug <= gme_debug.g_log_procedure) THEN

   gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                       || l_api_name);
     
   gme_debug.put_line('input parameter p_batch_id:'||p_batch_id);
        
  END IF;  
      
  x_return_status := fnd_api.g_ret_sts_success;
      
  /*
  The function is only supported if the release version is greater than 12.2
  */
  l_osp_release := Check_Release_Version;
      
  IF NOT l_osp_release THEN
    IF (g_debug <= gme_debug.g_log_procedure) THEN
      gme_debug.put_line ('The function is only supported if the release version is greater than 12.2');   
    END IF;
    x_return_status := fnd_api.g_ret_sts_success;
    RETURN;
      
  END IF;    
      
  SELECT propagate_change_to_po
    INTO l_propagate_change_to_po
    FROM gme_parameters
   WHERE organization_id = p_organization_id;
       
  /*   
  The program return if the parameter value is Munual
  */ 
  IF l_propagate_change_to_po IS NULL OR 
     l_propagate_change_to_po = gme_osp.g_propagate_change_manual THEN
    IF (g_debug <= gme_debug.g_log_procedure) THEN
      gme_debug.put_line ('The GME parameter value of propagate changes to PO is Munual,exiting the program');   
    END IF;    
    x_return_status := fnd_api.g_ret_sts_success;    
    RETURN; 
  END IF;   
  /*   
  Step1:Cancel existing PO/Req linked to the batch.
  */     
  cancelPOReq (p_batch_id        =>p_batch_id,
               p_org_id          =>p_organization_id,
               x_return_status   =>x_return_status,
               x_message_list    =>x_message_list,
               x_message_count   =>x_message_count); 
                   
  IF x_return_status <> fnd_api.g_ret_sts_success THEN
    RAISE cancel_po_failed;   
      
  END IF;    
  /*   
  Step2:Check whether the batch is still OSP batch after reroute
  */      
  l_recreate_po_flag := Is_OSP_Batch(p_batch_id => p_batch_id);
      
  IF l_recreate_po_flag THEN
    /*
    BEGIN ER 20809749
    Shaliu Chen 14-APR-2015
    change from fetch to loop to support multiple osp steps.
    */    
    FOR cur_gobi IN get_osp_batch_info(p_batch_id) LOOP
      l_batchstep_id          := cur_gobi.batchstep_id;
      l_batchstep_resource_id := cur_gobi.batchstep_resource_id;
        
      IF l_batchstep_resource_id IS NOT NULL THEN
        Get_Item_Unit_Type(p_batch_id              => p_batch_id,
                           p_batchstep_resource_id => l_batchstep_resource_id,
                           x_unit_type             => l_item_unit_type,
                           x_return_status         => x_return_status,
                           x_message_list          => x_message_list);
                           
        IF x_return_status <> fnd_api.g_ret_sts_success THEN
          RAISE get_item_unit_type_failed;  
        END IF;                  
                                      
      END IF;
          
      IF l_item_unit_type = gme_osp.g_unit_type_resource THEN
        SELECT NVL(plan_rsrc_usage,0)
          INTO l_addition_qty
          FROM gme_batch_step_resources
         WHERE batchstep_resource_id = l_batchstep_resource_id;
              
      ELSIF l_item_unit_type = gme_osp.g_unit_type_product THEN

        SELECT DECODE(gbh.batch_status,gme_common_pvt.g_batch_pending
                                     ,gmd.plan_qty
                                     ,gme_common_pvt.g_batch_wip
                                     ,gmd.wip_plan_qty
                                     ,gmd.plan_qty)
         INTO l_addition_qty
         FROM gme_material_details gmd,
              gme_batch_header gbh,
              wip_entities we
        WHERE gmd.batch_id = p_batch_id
          AND gmd.line_type = gme_common_pvt.g_line_type_prod
          AND gmd.batch_id = gbh.batch_id
          AND gmd.inventory_item_id = we.primary_item_id
          AND gmd.batch_id = we.wip_entity_id;           
          
      END IF;
          
      /*   
      Step3:recreate a new requisition for the batch if the 
            batch is still a OSP batch after reroute
      */        
      IF (l_batchstep_id IS NOT NULL AND 
          l_batchstep_resource_id IS NOT NULL AND
          l_addition_qty > 0) THEN 
        create_requisition(
                            p_batch_id               => p_batch_id,
                            p_organization_id        => p_organization_id,
                            p_batchstep_id           => l_batchstep_id,
                            p_batchstep_resource_id  => l_batchstep_resource_id,
                            p_addition_qty           => l_addition_qty,
                            x_return_status          => x_return_status,
                            x_message_list           => x_message_list,
                            x_message_count          => x_message_count );    
                                
      END IF;    
          
      IF x_return_status <> fnd_api.g_ret_sts_success THEN
        RAISE recreate_po_failed;  
      END IF;
    END LOOP;
  END IF;
    

EXCEPTION
  WHEN cancel_po_failed THEN
    x_return_status := fnd_api.g_ret_sts_error; 
    gme_common_pvt.count_and_get (x_count        => x_message_count
                                 ,p_encoded      => fnd_api.g_false
                                 ,x_data         => x_message_list);
    IF (g_debug <= gme_debug.g_log_procedure) THEN
      gme_debug.put_line ('Cancel existing PO/Req failed.'||x_message_list);   
    END IF;                                                                                                           
    
  WHEN recreate_po_failed THEN
    x_return_status := fnd_api.g_ret_sts_error;
    gme_common_pvt.count_and_get (x_count        => x_message_count
                                 ,p_encoded      => fnd_api.g_false
                                 ,x_data         => x_message_list);  
    IF (g_debug <= gme_debug.g_log_procedure) THEN
      gme_debug.put_line ('Recreate new Requisition failed'||x_message_list);   
    END IF;  
    
  WHEN get_item_unit_type_failed THEN                                         
    x_return_status := fnd_api.g_ret_sts_error;
    gme_common_pvt.count_and_get (x_count        => x_message_count
                                 ,p_encoded      => fnd_api.g_false
                                 ,x_data         => x_message_list);  
    IF (g_debug <= gme_debug.g_log_procedure) THEN
      gme_debug.put_line ('Get itme unit type failed'||x_message_list);   
    END IF;      
  WHEN OTHERS THEN
    x_return_status := fnd_api.g_ret_sts_unexp_error;
    l_errMsg := 'unexpected error: ' || SQLERRM || 'SQLCODE = ' || SQLCODE;
    gme_debug.put_line('l_errMsg:'||l_errMsg); 
    fnd_message.set_name('FND', 'FND_GENERIC_MESSAGE');
    fnd_message.set_token('MESSAGE', l_errMsg);
    fnd_msg_pub.add;
    gme_common_pvt.count_and_get (x_count        => x_message_count
                                 ,p_encoded      => fnd_api.g_false
                                 ,x_data         => x_message_list);                   
     
END reroute_batch_process;    


 /*================================================================================
  Procedure
    closePO
  Description
    Wrapper to close existing PO which is linked to OPM batch
  Parameters

  History
  =======

================================================================================*/
PROCEDURE closePO(p_batch_id      IN         NUMBER,
                  p_org_id        IN         NUMBER,
                  p_batchstep_id  IN         NUMBER :=NULL,
                  x_return_status OUT NOCOPY VARCHAR2,
                  x_message_list  OUT NOCOPY  VARCHAR2,
                  x_message_count OUT NOCOPY  NUMBER ) IS

 CURSOR c_po_req IS
    SELECT pd.po_header_id po_header_id,
           to_number(null) po_release_id,
           pd.po_line_id po_line_id,
           pd.line_location_id po_line_location_id,
           to_number(null) req_header_id,
           to_number(null) req_line_id,
           ph.type_lookup_code po_req_type,
           ph.authorization_status approval_status,
           'PO' document_type,
           ph.type_lookup_code document_subtype,
           pd.org_id ou_id -- operating unit
      FROM po_distributions_all pd,
           po_headers_all ph,
           po_line_locations_all pll
     WHERE pd.po_line_id IS NOT NULL
       AND pd.line_location_id IS NOT NULL
       AND ph.type_lookup_code = 'STANDARD'
       AND ph.po_header_id = pd.po_header_id
       AND pd.line_location_id = pll.line_location_id
       AND pd.wip_entity_id = p_batch_id
       AND pd.destination_organization_id = p_org_id
       AND (p_batchstep_id IS NULL OR
            pd.wip_operation_seq_num = p_batchstep_id)
       AND ph.authorization_status='APPROVED'
       AND NVL(pll.closed_code,'OPEN') IN ('CLOSED FOR INVOICE','CLOSED FOR RECEIVING','OPEN')
       AND (pll.cancel_flag IS NULL OR pll.cancel_flag = 'N')
   UNION ALL
    SELECT pd.po_header_id po_header_id,
           pr.po_release_id po_release_id,
           to_number(null) po_line_id,
           pd.line_location_id po_line_location_id,
           to_number(null) req_header_id,
           to_number(null) req_line_id,
           ph.type_lookup_code po_req_type,
           pr.authorization_status approval_status,
           'RELEASE' document_type,
           ph.type_lookup_code document_subtype,
           pd.org_id ou_id -- operating unit
      FROM po_distributions_all pd,
           po_headers_all ph,
           po_line_locations_all pll,
           po_releases_all pr
     WHERE pd.po_line_id IS NOT NULL
       AND pd.line_location_id IS NOT NULL
       AND ph.type_lookup_code = 'BLANKET'
       AND pr.po_header_id = ph.po_header_id
       AND pr.po_release_id = pd.po_release_id
       AND ph.po_header_id = pd.po_header_id
       AND pd.line_location_id = pll.line_location_id
       AND pd.wip_entity_id = p_batch_id
       AND pd.destination_organization_id = p_org_id
       AND (p_batchstep_id IS NULL OR
            pd.wip_operation_seq_num = p_batchstep_id)
       AND ph.authorization_status='APPROVED'
       AND NVL(ph.closed_code,'OPEN') NOT IN ('CLOSED','FINALLY CLOSED')
       AND (pll.cancel_flag IS NULL OR pll.cancel_flag = 'N');
/*   UNION ALL
    SELECT to_number(null) po_header_id,
           to_number(null) po_release_id,
           to_number(null) po_line_id,
           to_number(null) po_line_location_id,
           prl.requisition_header_id req_header_id,
           prl.requisition_line_id req_line_id,
           'REQUISITION' po_req_type,
           prh.authorization_status approval_status,
           to_char(null) document_type,
           to_char(null) document_subtype,
           prl.org_id ou_id -- operating unit
      FROM po_requisition_headers_all prh,
           po_requisition_lines_all prl
     WHERE NOT EXISTS
          (SELECT 'x'
             FROM po_line_locations_all pll
            WHERE prl.line_location_id = pll.line_location_id)
       AND prh.requisition_header_id = prl.requisition_header_id
       AND prl.wip_entity_id = p_batch_id
       AND prl.destination_organization_id = p_org_id
       AND (p_batchstep_id IS NULL OR
            prl.wip_operation_seq_num = p_batchstep_id)
       AND (prl.cancel_flag IS NULL OR
            prl.cancel_flag = 'N')*/


l_api_name                     CONSTANT VARCHAR(30) := 'CLOSE_PO';
l_err_count                    NUMBER;
l_return_code                  VARCHAR2(40);
l_errMsg                       VARCHAR2(240);
l_return_status                BOOLEAN;
BEGIN
  
  fnd_msg_pub.initialize;

  IF (g_debug <= gme_debug.g_log_procedure) THEN
    gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                       || l_api_name);
    gme_debug.put_line('p_batch_id:'||p_batch_id);
    gme_debug.put_line('p_org_id:'||p_org_id);
    gme_debug.put_line('p_batch_id:'||p_batch_id);

  END IF;

  x_return_status  := fnd_api.g_ret_sts_success;

  /*
    Get all of open PO which are linked to the batch,and close 
    PO one by one,If unable to cancel PO/release for any reason,
    skip the error one and try to cancel the next one.
  */
  FOR l_po_req IN c_po_req LOOP
    BEGIN

      l_return_status:= po_actions.close_po (p_docid         => l_po_req.po_header_id,
                                             p_doctyp        => l_po_req.document_type,
                                             p_docsubtyp     => l_po_req.document_subtype,
                                             p_lineid        => l_po_req.po_line_id,
                                             p_shipid        => l_po_req.po_line_location_id,
                                             p_action        => 'CLOSE',
                                             p_conc_flag     => 'Y',
                                             p_return_code   => l_return_code,
                                             p_auto_close    => 'N');

     IF NOT l_return_status THEN
       fnd_message.set_name('FND', 'FND_GENERIC_MESSAGE');
       fnd_message.set_token('MESSAGE', l_return_code);
       fnd_msg_pub.add;
       --need to confirm
       --RAISE fnd_api.g_exc_unexpected_error;
     END IF;
    EXCEPTION
      WHEN OTHERS THEN
        l_err_count := l_err_count + 1;
        l_errMsg := 'unexpected error: ' || SQLERRM || 'SQLCODE = ' || SQLCODE;
        IF (g_debug <= gme_debug.g_log_procedure) THEN
          gme_debug.put_line(l_errMsg);
        END IF;
       fnd_message.set_name('FND', 'FND_GENERIC_MESSAGE');
       fnd_message.set_token('MESSAGE', l_errMsg);
       fnd_msg_pub.add;
    END;
  END LOOP;

  IF (l_err_count > 0) THEN
    RAISE fnd_api.g_exc_unexpected_error;
  ELSE
    x_return_status  := fnd_api.g_ret_sts_success;
  END IF;

EXCEPTION
  WHEN fnd_api.g_exc_unexpected_error THEN
    x_return_status := fnd_api.g_ret_sts_unexp_error;
    gme_common_pvt.count_and_get (x_count        => x_message_count
                                 ,p_encoded      => fnd_api.g_false
                                 ,x_data         => x_message_list);
  WHEN OTHERS THEN
   x_return_status := fnd_api.g_ret_sts_unexp_error;
   l_errMsg := 'unexpected error: ' || SQLERRM || 'SQLCODE = ' || SQLCODE;
   IF (g_debug <= gme_debug.g_log_procedure) THEN
     gme_debug.put_line(l_errMsg);
   END IF;
   fnd_message.set_name('FND', 'FND_GENERIC_MESSAGE');
   fnd_message.set_token('MESSAGE', l_errMsg);
   fnd_msg_pub.add;
   gme_common_pvt.count_and_get (x_count        => x_message_count
                                ,p_encoded      => fnd_api.g_false
                                ,x_data         => x_message_list);                          
END;


 /*================================================================================
  Procedure
    Is_OSP_Batch
  Description
    The program is uesed to check whether the batch or batch step include OSP resource.

  Parameters
    p_batchstep_id             If this parameter value is NULL,that mean the program 
                               will check whether the batch include OSP resource,
                               If this parameter value is NOT NULL,that mean the
                               program will check whether the batch step include
                               OSP resource.

  History
  =======
  ER 20809749    Shaliu Chen     09-APR-2015  
                 Modify to support multiple osp steps.

================================================================================*/
FUNCTION Is_OSP_Batch(p_batch_id      IN NUMBER,
                      p_batchstep_id  IN NUMBER DEFAULT NULL)
  RETURN BOOLEAN IS
  l_count   NUMBER;
BEGIN     
   IF NOT Check_Release_Version THEN     
     RETURN FALSE;
   END IF;   
   
   IF p_batchstep_id IS NULL
   THEN
      SELECT COUNT (1)
        INTO l_count
        FROM gme_batch_step_resources gbsr,
             gme_batch_steps gbs,
             cr_rsrc_dtl crd,
             cr_rsrc_mst_b crm
       WHERE gbs.batch_id = p_batch_id
         AND gbs.batchstep_id = gbsr.batchstep_id
         AND gbsr.resources = crd.resources
         AND gbsr.organization_id = crd.organization_id
         AND crd.resources = crm.resources
         AND crd.purchase_item_id IS NOT NULL
         AND NVL (crm.outside_process_ind, gme_osp.g_osp_ind_disabled) = 
                gme_osp.g_osp_ind_enabled;
   ELSE
      SELECT COUNT (1)
        INTO l_count
        FROM gme_batch_step_resources gbsr, 
             cr_rsrc_dtl crd, 
             cr_rsrc_mst_b crm
       WHERE     gbsr.batch_id = p_batch_id
             AND gbsr.batchstep_id = p_batchstep_id
             AND gbsr.resources = crd.resources
             AND gbsr.organization_id = crd.organization_id
             AND crd.resources = crm.resources
             AND crd.purchase_item_id IS NOT NULL
             AND NVL (crm.outside_process_ind, gme_osp.g_osp_ind_disabled) =
                    gme_osp.g_osp_ind_enabled;
   END IF;
  /*                                    
  BEGIN ER 20809749                    
  Shaliu Chen 02-APR-2015               
  change l_count = 1 to l_count > 0 to support multiple osp steps. 
  */     
  IF l_count > 0 THEN
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END IF;

EXCEPTION
  WHEN OTHERS THEN
    RETURN FALSE;

END;


 /*================================================================================
  Procedure
    get_osp_parameters_value
  Description
    The program is uesed to get GME parameter regarding to outside processing.

  Parameters


  History
  =======

================================================================================*/
PROCEDURE get_osp_parameters_value(p_organization_id    IN NUMBER,
                                   x_gme_parameter_rec  OUT NOCOPY gme_parameters%ROWTYPE,
                                   x_return_status      OUT NOCOPY VARCHAR2) IS
  l_errMsg VARCHAR2(2000);
BEGIN
  x_return_status := fnd_api.g_ret_sts_success;
  
  SELECT *
    INTO x_gme_parameter_rec
    FROM gme_parameters
   WHERE organization_id = p_organization_id;


EXCEPTION
  WHEN OTHERS THEN
    x_return_status := fnd_api.g_ret_sts_unexp_error;
    l_errMsg := 'unexpected error: ' || SQLERRM || 'SQLCODE = ' || SQLCODE;
    IF (g_debug <= gme_debug.g_log_procedure) THEN
     gme_debug.put_line(l_errMsg);
    END IF;  
END;                                   

 /*================================================================================
  Procedure
    Check_Release_Version
  Description
    The program is uesed to check release version,if release version is equal
    to or greater than 12.2,return True,otherwise,return False.

  Parameters


  History
  =======

================================================================================*/
FUNCTION Check_Release_Version RETURN BOOLEAN IS
BEGIN
  IF ( fnd_release.major_version > 12 OR
      (fnd_release.major_version = 12 AND
       fnd_release.minor_version >= 2)) THEN
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END IF;
EXCEPTION
  WHEN OTHERS THEN
    RETURN FALSE;

END;


 /*================================================================================
  Procedure
    Get_Item_Unit_Type
  Description
    The program is uesed to get inventory item unit type.

  Parameters


  History
  =======

================================================================================*/

PROCEDURE Get_Item_Unit_Type(p_batch_id               IN          NUMBER,
                             p_batchstep_resource_id  IN          NUMBER,
                             x_unit_type              OUT NOCOPY  VARCHAR2,
                             x_return_status          OUT NOCOPY VARCHAR2,
                             x_message_list           OUT NOCOPY  VARCHAR2)
  IS
  l_errMsg                VARCHAR2(2000);
  item_unit_type_unset    EXCEPTION;
BEGIN
  IF p_batchstep_resource_id IS NOT NULL THEN
    SELECT outside_operation_uom_type
      INTO x_unit_type
      FROM gme_batch_step_resources gbsr,
           cr_rsrc_dtl crd,
           mtl_system_items_b msib
     WHERE gbsr.batchstep_resource_id = p_batchstep_resource_id
       AND gbsr.resources = crd.resources
       AND gbsr.organization_id = crd.organization_id
       AND crd.organization_id = msib.organization_id
       AND crd.purchase_item_id = msib.inventory_item_id
       AND crd.purchase_item_id IS NOT NULL;
  ELSE
    SELECT outside_operation_uom_type
      INTO x_unit_type
      FROM gme_batch_step_resources gbsr,
           cr_rsrc_mst crm,
           cr_rsrc_dtl crd,
           mtl_system_items_b msib
     WHERE gbsr.batch_id = p_batch_id
       AND gbsr.resources = crm.resources
       AND NVL(crm.outside_process_ind,gme_osp.g_osp_ind_disabled) = gme_osp.g_osp_ind_enabled
       AND crm.resources = crd.resources
       AND crd.purchase_item_id IS NOT NULL
       AND crd.organization_id  = gbsr.organization_id
       AND crd.purchase_item_id = msib.inventory_item_id
       AND crd.organization_id  = msib.organization_id;
  END IF;
    
EXCEPTION
  WHEN item_unit_type_unset THEN
    x_return_status := fnd_api.g_ret_sts_error;
    x_message_list  := fnd_msg_pub.get (p_msg_index      => fnd_msg_pub.g_last
                                       ,p_encoded        => fnd_api.g_false); 
  WHEN OTHERS THEN
    x_return_status := fnd_api.g_ret_sts_unexp_error;
    l_errMsg := 'unexpected error: ' || SQLERRM || 'SQLCODE = ' || SQLCODE;
    IF (g_debug <= gme_debug.g_log_procedure) THEN
     gme_debug.put_line(l_errMsg);
    END IF;  
    fnd_message.set_name('FND', 'FND_GENERIC_MESSAGE');
    fnd_message.set_token('MESSAGE', l_errMsg);
    fnd_msg_pub.add;
    x_message_list  := fnd_msg_pub.get (p_msg_index      => fnd_msg_pub.g_last
                                       ,p_encoded        => fnd_api.g_false);       

END;

 /*================================================================================
  Procedure
    check_req_intf_data_exist
  Description
    The program is uesed to check whether there is requisition in the open interface
    created for the opm osp batch.

  Parameters


  History
  =======
  ER 20809749    Shaliu Chen     09-APR-2015  
                 Modify to support multiple osp steps.
================================================================================*/
FUNCTION check_req_intf_data_exist(p_batch_id              IN NUMBER,
                                   p_batchstep_resource_id IN NUMBER)
  RETURN BOOLEAN IS
  l_count NUMBER;
BEGIN
  /*                                    
  BEGIN ER 20809749                    
  Shaliu Chen 09-APR-2015               
  Add an input paramter p_batchstep_resource_id to distinct different
  osp step.  
  */    
  SELECT count(1)
    INTO l_count
    FROM po_requisitions_interface_all
   WHERE wip_entity_id = p_batch_id
     AND wip_resource_seq_num = p_batchstep_resource_id;

  IF l_count >0 THEN
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END IF;

EXCEPTION
  WHEN OTHERS THEN
    RETURN FALSE;   
END;

 /*================================================================================
  Procedure
    get_po_req_count_qty
  Description
    The program is used to get the count of PO/Req which is linked to the OPM batch,
    and get the total PO/Req quantity also.

  Parameters

  History
  =======
  ER 20809749    Shaliu Chen     09-APR-2015  
                 Modify to support multiple osp steps.
================================================================================*/
PROCEDURE get_po_req_count_qty (p_batch_id                  IN NUMBER,
                                p_organization_id           IN NUMBER,
                                p_batchstep_resource_id     IN NUMBER default NULL,
                                x_total_count               OUT NOCOPY NUMBER,
                                x_total_quantity            OUT NOCOPY NUMBER)
  IS


  CURSOR get_po_count IS
    SELECT count(1)
      FROM po_headers_all ph,
           po_distributions_all pd,
           po_line_locations_all pll
     WHERE pd.po_line_id IS NOT NULL   --need to confirm
       AND pd.line_location_id IS NOT NULL
       AND pd.wip_entity_id = p_batch_id
       AND pd.destination_organization_id = p_organization_id
       AND (p_batchstep_resource_id is NULL OR pd.wip_resource_seq_num = p_batchstep_resource_id)
       AND ph.po_header_id = pd.po_header_id
       AND pll.line_location_id = pd.line_location_id
       AND (pll.cancel_flag IS NULL OR
            pll.cancel_flag = 'N')
       AND (pll.quantity_received < (pll.quantity-pll.quantity_cancelled))
       AND nvl(pll.closed_code,'OPEN') <> 'FINALLY CLOSED';

   CURSOR get_req_count IS
    SELECT count(1)
      FROM po_requisition_lines_all prl
     WHERE PRL.WIP_ENTITY_ID = p_batch_id
       AND PRL.DESTINATION_ORGANIZATION_ID = p_organization_id
       AND (p_batchstep_resource_id IS NULL OR
            prl.wip_resource_seq_num = p_batchstep_resource_id)
       AND nvl(prl.cancel_flag, 'N') = 'N'
       AND prl.line_location_id IS NULL;   --need to confirm.

  CURSOR get_po_qty IS
    SELECT NVL(SUM(NVL(pd.quantity_ordered,0)),0)
      FROM po_headers_all ph,
           po_distributions_all pd,
           po_line_locations_all pll
     WHERE pd.po_line_id IS NOT NULL   --need to confirm
       AND pd.line_location_id IS NOT NULL
       AND pd.wip_entity_id = p_batch_id
       AND pd.destination_organization_id = p_organization_id
       AND (p_batchstep_resource_id is NULL OR pd.wip_resource_seq_num = p_batchstep_resource_id)
       AND ph.po_header_id = pd.po_header_id
       AND pll.line_location_id = pd.line_location_id
       AND (pll.cancel_flag IS NULL OR
            pll.cancel_flag = 'N')
       AND (pll.quantity_received < (pll.quantity-pll.quantity_cancelled))
       AND nvl(pll.closed_code,'OPEN') <> 'FINALLY CLOSED';

   CURSOR get_req_qty IS
    SELECT NVL(SUM(NVL(prl.quantity,0)),0)
      FROM po_requisition_lines_all prl
     WHERE PRL.WIP_ENTITY_ID = p_batch_id
       AND PRL.DESTINATION_ORGANIZATION_ID = p_organization_id
       AND (p_batchstep_resource_id IS NULL OR
            prl.wip_resource_seq_num = p_batchstep_resource_id)
       AND nvl(prl.cancel_flag, 'N') = 'N'
       AND prl.line_location_id IS NULL;
       
    /*                                    
    BEGIN ER 20809749                    
    Shaliu Chen 09-APR-2015               
    Add an input paramter p_batchstep_resource_id to distinct different
    osp step.  
    */         
   CURSOR get_req_intf_count IS
    SELECT count(1)
      FROM po_requisitions_interface_all pri
     WHERE pri.WIP_ENTITY_ID = p_batch_id
       AND (p_batchstep_resource_id IS NULL OR
            pri.wip_resource_seq_num = p_batchstep_resource_id);     
 
    /*                                    
    BEGIN ER 20809749                    
    Shaliu Chen 09-APR-2015               
    Add an input paramter p_batchstep_resource_id to distinct different
    osp step.  
    */      
   CURSOR get_req_intf_qty IS  
    SELECT NVL(SUM(NVL(pri.quantity,0)),0)
      FROM po_requisitions_interface_all pri
     WHERE pri.WIP_ENTITY_ID = p_batch_id
       AND (p_batchstep_resource_id IS NULL OR
            pri.wip_resource_seq_num = p_batchstep_resource_id);      


  po_req_exist     VARCHAR2(20);
  l_po_distribution_id   NUMBER;
  l_req_line_id    NUMBER;
  l_batch_id       NUMBER;
  l_po_count       NUMBER;
  l_req_count      NUMBER;
  l_po_qty         NUMBER;
  l_req_qty        NUMBER;
  l_req_intf_count NUMBER;
  l_req_intf_qty   NUMBER;

  BEGIN     
    OPEN get_po_count;
    FETCH get_po_count INTO l_po_count;
    CLOSE get_po_count;

    OPEN get_req_count;
    FETCH get_req_count INTO l_req_count;
    CLOSE get_req_count;
    
    OPEN get_req_intf_count;
    FETCH get_req_intf_count INTO l_req_intf_count;
    CLOSE get_req_intf_count;    
    x_total_count := l_po_count+l_req_count+l_req_intf_count;

    IF x_total_count >= 1 THEN
      OPEN get_po_qty;
      FETCH get_po_qty INTO l_po_qty;
      CLOSE get_po_qty;

      OPEN get_req_qty;
      FETCH get_req_qty INTO l_req_qty;
      CLOSE get_req_qty;
      
      OPEN get_req_intf_qty;
      FETCH get_req_intf_qty INTO l_req_intf_qty;
      CLOSE get_req_intf_qty; 
      x_total_quantity := l_po_qty + l_req_qty+l_req_intf_qty;
    ELSE
      x_total_quantity :=0;

    END IF;

EXCEPTION
  WHEN OTHERS THEN
    x_total_count := 0;
    x_total_quantity := 0;
    
    IF get_po_count%ISOPEN THEN
      CLOSE get_po_count;  
    END IF;
    
    IF get_req_count%ISOPEN THEN
      CLOSE get_req_count;  
    END IF;
    
    IF get_po_qty%ISOPEN THEN
      CLOSE get_po_qty;  
    END IF;
    
    IF get_req_qty%ISOPEN THEN
      CLOSE get_req_qty;  
    END IF;     
           

 END get_po_req_count_qty;

 /*================================================================================
  Procedure
    updatePOReqQuantity
  Description
    The program is used to synchronize batch quantity change to existing PO/Req or
    create a new requistion.
    If user scale batch,update proudct quantity manually ,update plant resource
    usage manually or apply fixed process loss,the porgram will be invoked.
    Update Requisition and Purchase order's quantity  is Allowed only if
    1).No Records in Req Interface Table
    2).Only one Purchase order exists
    3).'Incomplete', 'Approved','Requires Reapproval' and the line is not cancelled
    If the Update of the requisition or the purchase order fails for any reasons,
    Cut a New requisition based on the User's response for the increase in Batch 
    Quantity and give a warning in the other cases 	

  Parameters

  History
  =======
  ER 20809749    Shaliu Chen     09-APR-2015  
                 Modify to support multiple osp steps.

================================================================================*/
 PROCEDURE updatePOReqQuantity(p_batch_id                    IN         NUMBER,
                               p_organization_id             IN         NUMBER,
                               p_batchstep_resource_id       IN         NUMBER DEFAULT NULL,
                               x_return_status               OUT NOCOPY VARCHAR2,
                               x_message_list                OUT NOCOPY VARCHAR2,
                               x_message_count               OUT NOCOPY NUMBER   ) IS


  CURSOR c_update_po_qty IS
    SELECT pd.po_header_id po_header_id,
           to_number(null) po_release_id,
           pd.po_distribution_id po_distribution_id,
           to_number(null) req_header_id,
           to_number(null) req_line_id,
           ph.type_lookup_code po_req_type,
           ph.authorization_status approval_status,
           msi.primary_uom_code uom_code,
           pd.org_id ou_id -- operating unit
      FROM mtl_system_items msi,
           po_distributions_all pd,
           po_headers_all ph,
           po_lines_all pl,
           po_line_locations_all pll
     WHERE ph.type_lookup_code = 'STANDARD'
       AND ph.po_header_id = pd.po_header_id
       AND pd.line_location_id = pll.line_location_id
       AND pd.po_line_id = pl.po_line_id
       AND pl.item_id = msi.inventory_item_id
       AND pd.destination_organization_id = msi.organization_id
       AND pd.wip_entity_id = p_batch_id
       --  ER 20809749    Shaliu Chen     09-APR-2015
       AND pd.wip_resource_seq_num = p_batchstep_resource_id
       AND pd.destination_organization_id = p_organization_id
       AND (pll.cancel_flag IS NULL OR pll.cancel_flag = 'N')
   UNION ALL
    SELECT pd.po_header_id po_header_id,
           pr.po_release_id po_release_id,
           pd.po_distribution_id po_distribution_id,
           to_number(null) req_header_id,
           to_number(null) req_line_id,
           ph.type_lookup_code po_req_type,
           pr.authorization_status approval_status,
           msi.primary_uom_code uom_code,
           pd.org_id ou_id -- operating unit
      FROM mtl_system_items msi,
           po_distributions_all pd,
           po_headers_all ph,
           po_lines_all pl,
           po_line_locations_all pll,
           po_releases_all pr
     WHERE ph.type_lookup_code = 'BLANKET'
       AND pr.po_release_id = pll.po_release_id
       AND pr.po_header_id = ph.po_header_id
       AND ph.po_header_id = pd.po_header_id
       AND pd.line_location_id = pll.line_location_id
       AND pd.po_line_id = pl.po_line_id
       AND pl.item_id = msi.inventory_item_id
       AND pd.destination_organization_id = msi.organization_id
       AND pd.wip_entity_id = p_batch_id
       --  ER 20809749    Shaliu Chen     09-APR-2015
       AND pd.wip_resource_seq_num = p_batchstep_resource_id       
       AND pd.destination_organization_id = p_organization_id
       AND (pll.cancel_flag IS NULL OR pll.cancel_flag = 'N')
    UNION ALL
    SELECT to_number(null) po_header_id,
           to_number(null) po_release_id,
           to_number(null) po_distribution_id,
           prl.requisition_header_id req_header_id,
           prl.requisition_line_id req_line_id,
           'REQUISITION' po_req_type,
           prh.authorization_status approval_status,
           msi.primary_uom_code uom_code,
           prl.org_id ou_id -- operating unit
      FROM mtl_system_items msi,
           po_requisition_headers_all prh,
           po_requisition_lines_all prl
     WHERE NOT EXISTS
          (SELECT 'x'
             FROM po_line_locations_all pll
            WHERE prl.line_location_id = pll.line_location_id)
       AND prh.requisition_header_id = prl.requisition_header_id
       AND prl.item_id = msi.inventory_item_id
       AND prl.destination_organization_id = msi.organization_id
       AND prl.wip_entity_id = p_batch_id
       --  ER 20809749    Shaliu Chen     09-APR-2015
       AND prl.wip_resource_seq_num = p_batchstep_resource_id       
       AND prl.destination_organization_id = p_organization_id
       AND (prl.cancel_flag IS NULL OR prl.cancel_flag = 'N');

  l_api_name     CONSTANT VARCHAR(30) := 'updatePOReqQuantity';
  l_update_po_qty    c_update_po_qty%ROWTYPE;
  l_po_changes       PO_CHANGES_REC_TYPE;
  l_errors_rec       PO_API_ERRORS_REC_TYPE;
  l_req_changes      PO_REQ_CHANGES_REC_TYPE;
  l_errMsg           VARCHAR2(240);
  l_msgData          VARCHAR2(2000);
  l_osp_release      BOOLEAN;
  l_osp_batch_flag   BOOLEAN;
  l_req_intf_exist   BOOLEAN;
  l_item_unit_type   VARCHAR2(25);
  l_update_flag      VARCHAR2(1);
  l_msgCount         NUMBER;
  l_po_count         NUMBER;
  l_new_qty          NUMBER;
  l_addition_qty     NUMBER;
  l_old_total_qty    NUMBER;
  l_propagate_change_to_po     NUMBER;
  l_batch_id                   NUMBER;
  l_batchstep_id               NUMBER;
  l_batchstep_resource_id      NUMBER;
  l_asqc_enabled               NUMBER;

  get_item_unit_type_failed    EXCEPTION;
  multiple_req_po_found        EXCEPTION;
  no_req_po_found              EXCEPTION;
  negative_addition_qty        EXCEPTION;
  create_requisition_failed    EXCEPTION;

  BEGIN

    fnd_msg_pub.initialize;

    IF (g_debug <= gme_debug.g_log_procedure) THEN

     gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                         || l_api_name);

      gme_debug.put_line('input parameter p_batch_id:'||p_batch_id);
      gme_debug.put_line('input parameter p_organization_id:'||p_organization_id);
      gme_debug.put_line('input parameter p_batchstep_resource_id:'||p_batchstep_resource_id);

    END IF;

    x_return_status := fnd_api.g_ret_sts_success;

    /*
    The function is only supported if the release version is greater than 12.2
    */

    l_osp_release := Check_Release_Version;

    IF NOT l_osp_release THEN
      IF (g_debug <= gme_debug.g_log_procedure) THEN
        gme_debug.put_line ('The function is only supported if the release version is greater than 12.2');
      END IF;
      x_return_status := fnd_api.g_ret_sts_success;
      RETURN;

    END IF;

    /*
      This flag is used to identify whehter updating exist PO or creating
      new requisition,updating existing po if flag value is Y,creating new
      requisition if flag value is N
    */
    l_update_flag := 'Y';

    SELECT propagate_change_to_po
      INTO l_propagate_change_to_po
      FROM gme_parameters
     WHERE organization_id = p_organization_id;

    /*
    The program return if the parameter value is Munual
    */
    IF l_propagate_change_to_po IS NULL OR
       l_propagate_change_to_po = gme_osp.g_propagate_change_manual THEN
      IF (g_debug <= gme_debug.g_log_procedure) THEN
        gme_debug.put_line ('The GME parameter value of propagate changes to PO is Munual,exiting the program');
      END IF;
      x_return_status := fnd_api.g_ret_sts_success;
      RETURN;
    END IF;

    /*
      Check whether batch is OSP batch,and the program will return
      if the batch is NOT OSP batch
    */
    l_osp_batch_flag := is_osp_batch(p_batch_id => p_batch_id);

    IF NOT l_osp_batch_flag THEN
      IF (g_debug <= gme_debug.g_log_procedure) THEN
        gme_debug.put_line ('The batch '||p_batch_id||' is not OSP batch,exiting the program');
      END IF;
      x_return_status := fnd_api.g_ret_sts_success;
      RETURN;
    END IF;
    
    /*
      Check whether batch is ASQC enabled,and the program will return
      if the batch is ASQC enabled batch
    */  
    SELECT automatic_step_calculation
      INTO l_asqc_enabled
      FROM gme_batch_header
     WHERE batch_id = p_batch_id;
     
    IF l_asqc_enabled = 1 THEN
      IF (g_debug <= gme_debug.g_log_procedure) THEN
        gme_debug.put_line ('The batch '||p_batch_id||' is ASQC enabled batch,quantity synchronization is not needed');
      END IF;      
      x_return_status := fnd_api.g_ret_sts_success;
      RETURN;          
    END IF;
      

    /*
      Check item unit type
      1.If item unit type is Procut,getting plan qty/wip plan qty of
        primary product item
      2.If item unit type is Resource,getting plan total usage of the
        OSP resource.
    */
    get_item_unit_type(p_batch_id              => p_batch_id,
                       p_batchstep_resource_id => p_batchstep_resource_id,
                       x_unit_type             => l_item_unit_type,
                       x_return_status         => x_return_status,
                       x_message_list          => x_message_list);

    IF x_return_status <> fnd_api.g_ret_sts_success THEN
      RAISE get_item_unit_type_failed;  
    END IF;                                                

    IF (l_item_unit_type = gme_osp.g_unit_type_product AND
        p_batch_id IS NOT NULL)  THEN
      SELECT DECODE(gbh.batch_status,gme_common_pvt.g_batch_pending
                                    ,gmd.plan_qty
                                    ,gme_common_pvt.g_batch_wip
                                    ,gmd.wip_plan_qty
                                    ,gmd.plan_qty)
       INTO l_new_qty
       FROM gme_material_details gmd,
            gme_batch_header gbh,
            wip_entities we
      WHERE gmd.batch_id = p_batch_id
        AND gmd.line_type = gme_common_pvt.g_line_type_prod
        AND gmd.batch_id = gbh.batch_id
        AND gmd.inventory_item_id = we.primary_item_id
        AND gmd.batch_id = we.wip_entity_id;

    /*
      BUG 19482173 28-AUG-2014 Shaliu Chen
      Add condition p_batchstep_resource_id IS NOT NULL to avoid the
      select below is executed during updating batch details 
      although unit type is Resource
    */
    ELSIF (l_item_unit_type = gme_osp.g_unit_type_resource AND
           p_batchstep_resource_id IS NOT NULL) THEN

      SELECT plan_rsrc_usage
       INTO l_new_qty
       FROM gme_batch_step_resources
      WHERE batchstep_resource_id = p_batchstep_resource_id;


    ELSE
      IF (g_debug <= gme_debug.g_log_procedure) THEN
        gme_debug.put_line ('The OSP item in the batch '||p_batch_id||
                            ' has not correct unit type,Pls check');
      END IF;
      x_return_status := fnd_api.g_ret_sts_success;
      RETURN;

    END IF;

    /*
      Check whether there is record in the requisition interface table which
      is linked to the batch,if exist,set l_update_flag = 'N'
    */
    l_req_intf_exist := check_req_intf_data_exist(p_batch_id              => p_batch_id,
                                                  p_batchstep_resource_id => p_batchstep_resource_id);

    IF l_req_intf_exist THEN

     l_update_flag := 'N';
     
     get_po_req_count_qty(p_batch_id              => p_batch_id,
                          p_organization_id       => p_organization_id,
                          p_batchstep_resource_id => p_batchstep_resource_id,
                          x_total_count           => l_po_count,
                          x_total_quantity        => l_old_total_qty);  
                                                      

    ELSE
    /*
      If there is not record in the requisition interface table,check how many
      POs have been created for the batch
      1.If 0,no PO need to sync,return
      2.If 1,there is only 1 PO created,can update PO qty directly,set
        l_update_flag = 'Y'
      3,If larger than 1, there are multiple PO created,need to create
        new requisition for qty synchronization, set l_update_flag = 'N'
    */

     get_po_req_count_qty(p_batch_id              => p_batch_id,
                          p_organization_id       => p_organization_id,
                          p_batchstep_resource_id => p_batchstep_resource_id,
                          x_total_count           => l_po_count,
                          x_total_quantity        => l_old_total_qty);
                          
     IF l_po_count = 0 THEN
       IF (g_debug <= gme_debug.g_log_procedure) THEN
         gme_debug.put_line ('There is no PO linked to the batch,quantity synchronization '||
                              'is not required');
       END IF;
       RETURN;
     ELSIF l_po_count = 1 THEN
       l_update_flag := 'Y';

     ELSIF l_po_count >1 THEN
       l_update_flag := 'N';
     END IF;
    END IF;
    /*
      If new qty is equal to old total qty,then return the program directly           
    */
    IF NVL(l_new_qty,0) = NVL(l_old_total_qty,0) THEN
      RETURN;      
    END IF;
     /*
     There are following scenarios:
     1.If l_update_flag = 'Y',and PO status in ('INCOMPLETE',
       'APPROVED','REQUIRES REAPPROVAL'), update existing Req/PO
     2.If l_update_flag = 'Y',and PO status NOT in ('INCOMPLETE',
       'APPROVED','REQUIRES REAPPROVAL'), create a new requisition if qty increase
     3.If l_update_flag = 'Y',and PO status NOT in ('INCOMPLETE',
       'APPROVED','REQUIRES REAPPROVAL'), popup a warning if qty decrease
     4.If l_update_flag = 'N',create a new requisition if qty increase
     5.If l_update_flag = 'N',popup a warning if qty decrease
     */
    IF l_update_flag = 'Y' THEN
     /*
     Get the PO linked to the batch
     */
     OPEN c_update_po_qty;
     FETCH c_update_po_qty INTO l_update_po_qty;
     IF c_update_po_qty%ROWCOUNT > 1  THEN
       CLOSE c_update_po_qty;
       RAISE multiple_req_po_found;
     ELSIF c_update_po_qty%ROWCOUNT = 0 THEN
       CLOSE c_update_po_qty;
       RAISE no_req_po_found;
     END IF;

     /*
     Set OU context before calling PO API.
     */
     mo_global.set_policy_context('S',l_update_po_qty.ou_id);

     /*
       Scenario 1
     */

     IF (l_update_po_qty.po_req_type IN ('STANDARD', 'BLANKET')) THEN
       IF (l_update_po_qty.approval_status IS NULL OR -- INCOMPLETE
           l_update_po_qty.approval_status IN ('INCOMPLETE',
                                               'APPROVED',
                                               'REQUIRES REAPPROVAL')) THEN

         l_po_changes := po_changes_rec_type.create_object(
                                             p_po_header_id  => l_update_po_qty.po_header_id,
                                             p_po_release_id => l_update_po_qty.po_release_id);

         l_po_changes.distribution_changes.add_change(
                                             p_po_distribution_id => l_update_po_qty.po_distribution_id,
                                             p_quantity_ordered   => l_new_qty);

         update_document(
                          p_api_version           => 1.0,
                          p_init_msg_list         => fnd_api.g_true,
                          p_changes               => l_po_changes,
                          p_run_submission_checks => fnd_api.g_true,
                          p_launch_approvals_flag => fnd_api.g_true,
                          p_buyer_id              => NULL,
                          p_update_source         => NULL,
                          p_override_date         => NULL,
                          x_return_status         => x_return_status,
                          x_api_errors            => l_errors_rec);

          IF(x_return_status <> fnd_api.g_ret_sts_success) THEN
            FOR i IN 1..l_errors_rec.message_name.count LOOP
              fnd_message.set_name('PO', l_errors_rec.message_name(i));
              fnd_msg_pub.add;
            END LOOP;
            RAISE fnd_api.g_exc_unexpected_error;
          END IF;
       ELSE
         l_addition_qty := l_new_qty - l_old_total_qty;
         /*
          Scenario 2
         */
         IF l_addition_qty > 0 THEN
           IF l_item_unit_type = gme_osp.g_unit_type_product THEN
             SELECT gbsr.batch_id
                   ,gbsr.batchstep_id
                   ,gbsr.batchstep_resource_id
               INTO l_batch_id
                   ,l_batchstep_id
                   ,l_batchstep_resource_id
               FROM gme_batch_step_resources gbsr,
                    cr_rsrc_dtl crd,
                    cr_rsrc_mst crm
              WHERE gbsr.batch_id = p_batch_id
                --ER 20809749    Shaliu Chen     09-APR-2015  
                AND gbsr.batchstep_resource_id = p_batchstep_resource_id 
                AND gbsr.resources = crd.resources
                AND gbsr.organization_id = crd.organization_id
                AND crd.resources = crm.resources
                AND NVL(crm.outside_process_ind,gme_osp.g_osp_ind_disabled) = gme_osp.g_osp_ind_enabled;


           ELSIF l_item_unit_type = gme_osp.g_unit_type_resource THEN
             SELECT gbsr.batch_id
                   ,gbsr.batchstep_id
                   ,gbsr.batchstep_resource_id
               INTO l_batch_id
                   ,l_batchstep_id
                   ,l_batchstep_resource_id
               FROM gme_batch_step_resources gbsr
              WHERE gbsr.batchstep_resource_id = p_batchstep_resource_id;

           END IF;

           create_requisition(
                              p_batch_id               => l_batch_id,
                              p_organization_id        => p_organization_id,
                              p_batchstep_id           => l_batchstep_id,
                              p_batchstep_resource_id  => l_batchstep_resource_id,
                              p_addition_flag          => 'Y',
                              p_addition_qty           => l_addition_qty,
                              x_return_status          => x_return_status,
                              x_message_list           => x_message_list,
                              x_message_count          => x_message_count );
           IF (x_return_status <> fnd_api.g_ret_sts_success) THEN
             RAISE create_requisition_failed; 
           END IF;
         ELSE
         /*
          Scenario 3
         */
           RAISE negative_addition_qty;

         END IF;
       END IF;
     ELSIF (l_update_po_qty.po_req_type = 'REQUISITION') THEN
       /*
        Call PO API to update QUANTITY in PO_REQUISITION_LINES_ALL.
       */
       l_req_changes := PO_REQ_CHANGES_REC_TYPE(
                                       req_header_id          => l_update_po_qty.req_header_id,
                                       line_changes           => NULL,
                                       distribution_changes   => NULL);

       l_req_changes.line_changes := PO_REQ_LINES_REC_TYPE(
                                       req_line_id            => PO_TBL_NUMBER(l_update_po_qty.req_line_id),
                                       unit_price             => PO_TBL_NUMBER(NULL),
                                       currency_unit_price    => PO_TBL_NUMBER(NULL),
                                       quantity               => PO_TBL_NUMBER(l_new_qty),
                                       secondary_quantity     => PO_TBL_NUMBER(NULL),
                                       need_by_date           => PO_TBL_DATE(NULL),
                                       deliver_to_location_id => PO_TBL_NUMBER(NULL),
                                       assignment_start_date  => PO_TBL_DATE(NULL),
                                       assignment_end_date    => PO_TBL_DATE(NULL),
                                       amount                 => PO_TBL_NUMBER(NULL));

        update_requisition(
        p_api_version           => 1.0,
        p_req_changes           => l_req_changes,
        p_update_source         => NULL,
        x_return_status         => x_return_status,
        x_msg_count             => l_msgCount,
        x_msg_data              => l_msgData);

        IF(x_return_status <> fnd_api.g_ret_sts_success) THEN
          IF g_debug <= gme_debug.g_log_procedure THEN
            gme_debug.put_line ('Qty synchronize to requisition failed,error message:'||l_msgData);
          END IF;
          x_message_list := l_msgData;
/*          fnd_message.set_name('FND', 'FND_GENERIC_MESSAGE');
          fnd_message.set_token('MESSAGE', l_msgData);
          fnd_msg_pub.add;*/
          RAISE fnd_api.g_exc_unexpected_error;
        END IF;
     END IF;
     
     IF c_update_po_qty%ISOPEN THEN
       CLOSE c_update_po_qty;
     END IF;

    /*
     Scenario 4
    */
    ELSIF l_update_flag = 'N' THEN
     IF l_item_unit_type = gme_osp.g_unit_type_product THEN
       SELECT gbsr.batch_id
             ,gbsr.batchstep_id
             ,gbsr.batchstep_resource_id
         INTO l_batch_id
             ,l_batchstep_id
             ,l_batchstep_resource_id
         FROM gme_batch_step_resources gbsr,
              cr_rsrc_dtl crd,
              cr_rsrc_mst crm
        WHERE gbsr.batch_id = p_batch_id
          --ER 20809749    Shaliu Chen     09-APR-2015  
          AND gbsr.batchstep_resource_id = p_batchstep_resource_id         
          AND gbsr.resources = crd.resources
          AND gbsr.organization_id = crd.organization_id
          AND crd.resources = crm.resources
          AND NVL(crm.outside_process_ind,gme_osp.g_osp_ind_disabled) = gme_osp.g_osp_ind_enabled;

     ELSIF l_item_unit_type = gme_osp.g_unit_type_resource THEN
       SELECT gbsr.batch_id
             ,gbsr.batchstep_id
             ,gbsr.batchstep_resource_id
         INTO l_batch_id
             ,l_batchstep_id
             ,l_batchstep_resource_id
         FROM gme_batch_step_resources gbsr
        WHERE gbsr.batchstep_resource_id = p_batchstep_resource_id;

     END IF;
     
     l_addition_qty := l_new_qty - l_old_total_qty;

     IF l_addition_qty > 0 THEN

       create_requisition(
                          p_batch_id               => l_batch_id,
                          p_organization_id        => p_organization_id,
                          p_batchstep_id           => l_batchstep_id,
                          p_batchstep_resource_id  => l_batchstep_resource_id,
                          p_addition_flag          => 'Y',
                          p_addition_qty           => l_addition_qty,
                          x_return_status          => x_return_status,
                          x_message_list           => x_message_list,
                          x_message_count          => x_message_count );
                          
       IF (x_return_status <> fnd_api.g_ret_sts_success) THEN
         RAISE create_requisition_failed; 
       END IF;                          
     /*
      Scenario 5
     */
     ELSE

       RAISE negative_addition_qty;

     END IF;

    END IF;


  EXCEPTION
    WHEN multiple_req_po_found THEN
      x_return_status := fnd_api.g_ret_sts_error;
      IF (g_debug <= gme_debug.g_log_procedure) THEN
        gme_debug.put_line ('There is multiple PO linked to the batch');
      END IF;
      gme_common_pvt.log_message('GME_MULTI_PO_EXISTS');
      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list);     
    WHEN no_req_po_found THEN
      x_return_status := fnd_api.g_ret_sts_error;
      IF (g_debug <= gme_debug.g_log_procedure) THEN
        gme_debug.put_line ('There is no PO linked to the batch');
      END IF;
      gme_common_pvt.log_message('GME_NO_PO_FOUND');
      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list);

    WHEN create_requisition_failed THEN                                         
      x_return_status := fnd_api.g_ret_sts_error;
      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list);
    WHEN fnd_api.g_exc_unexpected_error THEN
      x_return_status := fnd_api.g_ret_sts_error;
      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list);

    WHEN negative_addition_qty THEN
      x_return_status := fnd_api.g_ret_sts_success;
      IF (g_debug <= gme_debug.g_log_procedure) THEN
        gme_debug.put_line ('Do not be allowed to create a new requisition if qty decrease ');
      END IF;
      gme_common_pvt.log_message('GME_QTY_CHANGE_DECREASE');
      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list); 
                                         
    WHEN get_item_unit_type_failed THEN                                         
      x_return_status := fnd_api.g_ret_sts_error;
      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list);  
      IF (g_debug <= gme_debug.g_log_procedure) THEN
        gme_debug.put_line ('Get itme unit type failed'||x_message_list);   
      END IF;                                                

    WHEN OTHERS THEN
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      l_errMsg := 'unexpected error: ' || SQLERRM || 'SQLCODE = ' || SQLCODE;
      IF g_debug <= gme_debug.g_log_procedure THEN
        gme_debug.put_line('l_errMsg:'||l_errMsg);
      END IF;
      fnd_message.set_name('GME', 'GME_QTY_SYNC_FAILED');
      fnd_msg_pub.add;
      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list);   
  END updatePOReqQuantity;


 /*================================================================================
  Procedure
    updatePOReqNBD
  Description
    The program is used to synchronize batch step plan completion date change to
    existing PO/Req or create a new requistion.
    If user reschedule batch or reschedule step,the porgram will be invoked.
	  Update Requisition and Purchase order's need by date is Allowed only if 	
    1).PO is in status 'Incomplete', 'Approved', 'Requires Re approval' and the line is not cancelled
    2).The New date is less than old date-days early of need by date tolerance OR
    3).The New date is greater than the old date + days late of need by date tolerance
    If the Update of the requisition or the purchase order fails for any reasons, give a warning message.

  Parameters

  History
  =======
  ER 20809749    Shaliu Chen     10-APR-2015
                 Modify to support multiple osp steps.  

================================================================================*/
  PROCEDURE updatePOReqNBD(p_batch_id            IN         NUMBER,
                           p_batchstep_id        IN         NUMBER DEFAULT NULL,
                           p_organization_id     IN         NUMBER,
                           x_return_status       OUT NOCOPY VARCHAR2,
                           x_message_list        OUT NOCOPY VARCHAR2,
                           x_message_count       OUT NOCOPY NUMBER
                           ) IS
  l_api_name                CONSTANT VARCHAR2 (30):= 'updatePOReqNBD';

  l_logLevel                NUMBER := fnd_log.g_current_runtime_level;
  l_returnStatus            VARCHAR2(1);
  l_errMsg                  VARCHAR2(240);
  l_msgCount                NUMBER;
  l_msgData                 VARCHAR2(2000);
  l_po_changes              PO_CHANGES_REC_TYPE;
  l_errors_rec              PO_API_ERRORS_REC_TYPE;
  l_req_changes             PO_REQ_CHANGES_REC_TYPE;
  l_ou_id                   NUMBER;
  l_po_header_id            NUMBER;
  l_po_release_id           NUMBER;
  l_po_line_location_id     NUMBER;
  l_req_header_id           NUMBER;
  l_req_line_id             NUMBER;
  l_po_req_type             VARCHAR2(25);
  l_approval_status         VARCHAR2(25);
  l_old_need_by_date        DATE;
  l_item_id                 NUMBER;
  l_osp_release             BOOLEAN;
  l_propagate_change_to_po  NUMBER;
  l_osp_batch_flag          BOOLEAN;
  l_early_nbd_tolerance     NUMBER;
  l_late_nbd_tolerance      NUMBER;
  l_batchstep_id            NUMBER;
  l_error_count             NUMBER;
  l_new_nbd                 DATE;

  po_req_notfound           EXCEPTION;
  multi_po_req              EXCEPTION;
  l_new_nbd_required        EXCEPTION;


  CURSOR c_po_req (v_batch_id NUMBER,v_batchstep_id NUMBER,v_org_id NUMBER) IS
    SELECT pd.po_header_id po_header_id,
           to_number(null) po_release_id,
           pd.line_location_id po_line_location_id,
           to_number(null) req_header_id,
           to_number(null) req_line_id,
           ph.type_lookup_code po_req_type,
           ph.authorization_status approval_status,
           pll.need_by_date old_need_by_date,
           pl.item_id item_id,
           pd.org_id ou_id -- operating unit
      FROM po_distributions_all pd,
           po_headers_all ph,
           po_lines_all pl,
           po_line_locations_all pll,
           po_line_types plt
     WHERE ph.type_lookup_code = 'STANDARD'
       AND ph.po_header_id = pd.po_header_id
       AND pd.po_line_id = pl.po_line_id
       AND pd.line_location_id = pll.line_location_id
       AND pl.line_type_id = plt.line_type_id
       AND plt.outside_operation_flag = 'Y'
       AND pd.wip_entity_id = v_batch_id
       AND (v_batchstep_id IS NULL OR
            (v_batchstep_id IS NOT NULL AND pd.wip_operation_seq_num = v_batchstep_id))
       AND pd.destination_organization_id = v_org_id
       AND (ph.authorization_status IS NULL OR
            ph.authorization_status IN ('INCOMPLETE',
                                        'APPROVED',
                                        'REQUIRES REAPPROVAL'))
       AND (pll.cancel_flag IS NULL OR pll.cancel_flag = 'N')
  UNION ALL
    SELECT pd.po_header_id po_header_id,
           pr.po_release_id po_release_id,
           pd.line_location_id po_line_location_id,
           to_number(null) req_header_id,
           to_number(null) req_line_id,
           ph.type_lookup_code po_req_type,
           pr.authorization_status approval_status,
           pll.need_by_date old_need_by_date,
           pl.item_id item_id,
           pd.org_id ou_id -- operating unit
      FROM po_distributions_all pd,
           po_headers_all ph,
           po_lines_all pl,
           po_line_locations_all pll,
           po_releases_all pr,
           po_line_types plt
     WHERE ph.type_lookup_code = 'BLANKET'
       AND pr.po_release_id = pll.po_release_id
       AND pr.po_header_id = ph.po_header_id
       AND ph.po_header_id = pd.po_header_id
       AND pd.po_line_id = pl.po_line_id
       AND pd.line_location_id = pll.line_location_id
       AND pl.line_type_id = plt.line_type_id
       AND plt.outside_operation_flag = 'Y'
       AND pd.wip_entity_id = v_batch_id
       AND (v_batchstep_id IS NULL OR
            (v_batchstep_id IS NOT NULL AND pd.wip_operation_seq_num = v_batchstep_id))
       AND pd.destination_organization_id = v_org_id
       AND (pr.authorization_status IS NULL OR
            pr.authorization_status IN ('INCOMPLETE',
                                        'APPROVED',
                                        'REQUIRES REAPPROVAL'))
       AND (pll.cancel_flag IS NULL OR pll.cancel_flag = 'N')
  UNION ALL
    SELECT to_number(null) po_header_id,
           to_number(null) po_release_id,
           to_number(null) po_line_location_id,
           prl.requisition_header_id req_header_id,
           prl.requisition_line_id req_line_id,
           'REQUISITION' po_req_type,
           prh.authorization_status approval_status,
           prl.need_by_date old_need_by_date,
           prl.item_id item_id,
           prl.org_id ou_id -- operating unit
      FROM po_requisition_headers_all prh,
           po_requisition_lines_all prl,
           po_line_types plt
     WHERE NOT EXISTS
          (SELECT 'x'
             FROM po_line_locations_all pll
            WHERE prl.line_location_id = pll.line_location_id)
       AND prh.requisition_header_id = prl.requisition_header_id
       AND prl.line_type_id = plt.line_type_id
       AND plt.outside_operation_flag = 'Y'
       AND prl.wip_entity_id = v_batch_id
       AND (v_batchstep_id IS NULL OR
            (v_batchstep_id IS NOT NULL AND prl.wip_operation_seq_num = v_batchstep_id))
       AND prl.destination_organization_id = v_org_id
       AND (prl.cancel_flag IS NULL OR prl.cancel_flag = 'N')
   --BUG 22955827 Add source code to involve the requisition which is in requisition interface table    
   UNION ALL
     SELECT NULL po_header_id,
            NULL po_release_id,
            NULL po_line_location_id,
            NULL req_header_id,
            NULL req_line_id,
            'INTERFACE' po_req_type,
            NULL approval_status,
            need_by_date old_need_by_date,
            NULL item_id,
            NULL ou_id        
       FROM PO_REQUISITIONS_INTERFACE_ALL
      WHERE wip_entity_id = v_batch_id
        AND (v_batchstep_id IS NULL OR
            (v_batchstep_id IS NOT NULL AND wip_operation_seq_num = v_batchstep_id))
        AND interface_source_code = 'OPM-OSP';       

  CURSOR get_org_ou(v_organization_id NUMBER) IS
    SELECT to_number(ORG_INFORMATION3)
      FROM hr_organization_information hoi
     WHERE hoi.organization_id = v_organization_id
       AND ORG_INFORMATION_CONTEXT = 'Accounting Information';

  BEGIN

    fnd_msg_pub.initialize;

    IF (g_debug <= gme_debug.g_log_procedure) THEN

     gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                         || l_api_name);

      gme_debug.put_line('p_batch_id:'||p_batch_id);
      gme_debug.put_line('p_organization_id:'||p_organization_id);

    END IF;

    x_return_status := fnd_api.g_ret_sts_success;

    /*
    The function is only supported if the release version is greater than 12.2
    */
    l_osp_release := Check_Release_Version;

    IF NOT l_osp_release THEN
      IF (g_debug <= gme_debug.g_log_procedure) THEN
        gme_debug.put_line ('The function is only supported if the release version is greater than 12.2');
      END IF;
      x_return_status := fnd_api.g_ret_sts_success;
      RETURN;

    END IF;

    /*
    The program return if the parameter value is Munual
    */
    SELECT propagate_change_to_po
      INTO l_propagate_change_to_po
      FROM gme_parameters
     WHERE organization_id = p_organization_id;

    IF l_propagate_change_to_po IS NULL OR
       l_propagate_change_to_po = gme_osp.g_propagate_change_manual THEN
      IF (g_debug <= gme_debug.g_log_procedure) THEN
        gme_debug.put_line ('The GME parameter value of propagate changes to PO is Munual,exiting the program');
      END IF;
      x_return_status := fnd_api.g_ret_sts_success;
      RETURN;

    END IF;

    /*
      Check whether batch is OSP batch,and the program will return
      if the batch is NOT OSP batch
    */
    l_osp_batch_flag := is_osp_batch(p_batch_id => p_batch_id,
                                     p_batchstep_id => p_batchstep_id);

    IF NOT l_osp_batch_flag THEN
      IF (g_debug <= gme_debug.g_log_procedure) THEN
        gme_debug.put_line ('The batch '||p_batch_id||' is not OSP batch,exiting the program');
      END IF;
      x_return_status := fnd_api.g_ret_sts_success;
      RETURN;

    END IF;

    /*
      Get the value of GME parameters early/late nbd tolerance
    */
    SELECT NVL(early_need_by_date_tolerance,0)
          ,NVL(late_need_by_date_tolerance,0)
      INTO l_early_nbd_tolerance
          ,l_late_nbd_tolerance
      FROM gme_parameters
     WHERE organization_id = p_organization_id;

    /*
      Get New Step Plan Completion Date
      If reschedule batch,p_batch_is is not null,but p_batchstep_id
      is NULL.
      If reschedule step,p_batchstep_id is not NULL.
        
    */
    IF (p_batchstep_id IS NULL AND p_batch_id IS NOT NULL) THEN
      
      SELECT gbsr.batchstep_id
        INTO l_batchstep_id
        FROM gme_batch_step_resources gbsr,
             cr_rsrc_dtl crd,
             cr_rsrc_mst crm
       WHERE gbsr.batch_id = p_batch_id
         AND gbsr.resources = crd.resources
         AND gbsr.organization_id = crd.organization_id
         AND crd.purchase_item_id IS NOT NULL
         AND crd.resources = crm.resources
         AND NVL(crm.outside_process_ind,gme_osp.g_osp_ind_disabled) = gme_osp.g_osp_ind_enabled;

      IF l_batchstep_id IS NOT NULL THEN
        SELECT plan_cmplt_date
          INTO l_new_nbd
          FROM gme_batch_steps
         WHERE batchstep_id = l_batchstep_id;

      END IF;

    ELSIF p_batchstep_id IS NOT NULL THEN
      SELECT plan_cmplt_date
        INTO l_new_nbd
        FROM gme_batch_steps
       WHERE batchstep_id = p_batchstep_id;

    END IF;

    /*
     Set OU context before calling PO API.
    */
    OPEN get_org_ou(p_organization_id);
    FETCH get_org_ou INTO l_ou_id;
    CLOSE get_org_ou;

    mo_global.set_policy_context('S',l_ou_id);

    /*
     Get all of PO/Reqs linked to the batch,and update PO/Reqs
     One by One.
    */
    FOR cpr IN c_po_req(p_batch_id,p_batchstep_id,p_organization_id) LOOP
      l_old_need_by_date    := cpr.old_need_by_date;
      l_po_req_type         := cpr.po_req_type;
      l_approval_status     := cpr.approval_status;
      l_po_header_id        := cpr.po_header_id;
      l_po_release_id       := cpr.po_release_id;
      l_po_line_location_id := cpr.po_line_location_id;
      l_req_header_id       := cpr.req_header_id;
      l_req_line_id         := cpr.req_line_id;
      l_item_id             := cpr.item_id;
      l_ou_id               := cpr.ou_id;

      IF l_new_nbd IS NOT NULL AND l_old_need_by_date IS NOT NULL THEN

        IF l_new_nbd > l_old_need_by_date THEN
          /*
           new date is greater than old date,but within tolerance 
          */          
          IF l_new_nbd <= l_old_need_by_date + l_late_nbd_tolerance THEN
            IF (g_debug <= gme_debug.g_log_procedure) THEN
              gme_debug.put_line ('The new Need_By_Date is within tolerance,
                                  Need_By_Date change synchronization do not needed ');
            END IF;
            x_return_status := fnd_api.g_ret_sts_success;
            RETURN;
          END IF;

        ELSIF l_new_nbd < l_old_need_by_date THEN
          /*
           new date is less than old date,but within tolerance 
          */        
          IF l_new_nbd >=  l_old_need_by_date - l_early_nbd_tolerance THEN
            IF (g_debug <= gme_debug.g_log_procedure) THEN
              gme_debug.put_line ('The new Need_By_Date is within tolerance,
                                  Need_By_Date change synchronization do not needed ');
            END IF;
            x_return_status := fnd_api.g_ret_sts_success;
            RETURN;
          END IF;
        /*
          new date is equal to old date.
        */
        ELSE
          IF (g_debug <= gme_debug.g_log_procedure) THEN
            gme_debug.put_line ('The new Need_By_Date is within tolerance,
                                Need_By_Date change synchronization do not needed ');
          END IF;
          x_return_status := fnd_api.g_ret_sts_success;
          RETURN;

        END IF;
      ELSIF l_new_nbd IS NULL THEN
        RAISE l_new_nbd_required;
      END IF;

      /*
       Call PO API to update NBD.
      */
      IF(l_po_req_type IN('STANDARD', 'BLANKET')) THEN

        l_po_changes := po_changes_rec_type.create_object(
                          p_po_header_id  => l_po_header_id,
                          p_po_release_id => l_po_release_id);

        l_po_changes.shipment_changes.add_change(
                          p_po_line_location_id => l_po_line_location_id,
                          p_need_by_date        => l_new_nbd);

        update_document(
                          p_api_version           => 1.0,
                          p_init_msg_list         => fnd_api.g_true,
                          p_changes               => l_po_changes,
                          p_run_submission_checks => fnd_api.g_true,
                          p_launch_approvals_flag => fnd_api.g_true,
                          p_buyer_id              => NULL,
                          p_update_source         => NULL,
                          p_override_date         => NULL,
                          x_return_status         => x_return_status,
                          x_api_errors            => l_errors_rec);


        IF(x_return_status <> fnd_api.g_ret_sts_success) THEN
          
          FOR i IN 1..l_errors_rec.message_name.count LOOP
            fnd_message.set_name('PO', l_errors_rec.message_name(i));
            fnd_msg_pub.add;
          END LOOP;
          RAISE fnd_api.g_exc_unexpected_error;
        END IF;

      /*
       Call PO API to update requisition
      */
      ELSIF (l_po_req_type = 'REQUISITION') THEN

        l_req_changes := po_req_changes_rec_type(
                           req_header_id         => l_req_header_id,
                           line_changes          => NULL,
                           distribution_changes  => NULL);

        l_req_changes.line_changes := po_req_lines_rec_type(
                            req_line_id            => PO_TBL_NUMBER(l_req_line_id),
                            unit_price             => PO_TBL_NUMBER(NULL),
                            currency_unit_price    => PO_TBL_NUMBER(NULL),
                            quantity               => PO_TBL_NUMBER(NULL),
                            secondary_quantity     => PO_TBL_NUMBER(NULL),
                            need_by_date           => PO_TBL_DATE(l_new_nbd),
                            deliver_to_location_id => PO_TBL_NUMBER(NULL),
                            assignment_start_date  => PO_TBL_DATE(NULL),
                            assignment_end_date    => PO_TBL_DATE(NULL),
                            amount                 => PO_TBL_NUMBER(NULL));

         update_requisition(
                            p_api_version           => 1.0,
                            p_req_changes           => l_req_changes,
                            p_update_source         => NULL,
                            x_return_status         => x_return_status,
                            x_msg_count             => l_msgCount,
                            x_msg_data              => l_msgData);

        IF(x_return_status <> fnd_api.g_ret_sts_success) THEN
          fnd_message.set_name('FND', 'FND_GENERIC_MESSAGE');
          fnd_message.set_token('MESSAGE', l_msgData);
          fnd_msg_pub.add;
          RAISE fnd_api.g_exc_unexpected_error;
        END IF;
      --BUG 22955827 Add source code to involve the requisition which is in requisition interface table  
      ELSIF (l_po_req_type = 'INTERFACE') THEN
        UPDATE PO_REQUISITIONS_INTERFACE_ALL
           SET need_by_date = l_new_nbd
         WHERE wip_entity_id = p_batch_id
           AND wip_operation_seq_num = p_batchstep_id
           AND interface_source_code = 'OPM-OSP';         
        
      END IF; -- PO or Requisition
    END LOOP;

    IF (g_debug <= gme_debug.g_log_procedure) THEN
      gme_debug.put_line('The procedure UpdtePOREqNBD complete successfully');
    END IF;

  EXCEPTION
    WHEN l_new_nbd_required THEN
      x_return_status := fnd_api.g_ret_sts_error;
      gme_common_pvt.log_message('GME_NEW_NBD_DATE_REQUIRED');
      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list); 
      IF (g_debug <= gme_debug.g_log_procedure) THEN
        gme_debug.put_line(x_message_list);
      END IF;                                         

    WHEN fnd_api.g_exc_unexpected_error THEN
      x_return_status := fnd_api.g_ret_sts_error;
      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list); 
      IF g_debug <= gme_debug.g_log_procedure THEN
        gme_debug.put_line(x_message_list);
      END IF;                                   

    WHEN OTHERS THEN
      x_return_status := fnd_api.g_ret_sts_unexp_error;
      l_errMsg := 'unexpected error: ' || SQLERRM || 'SQLCODE = ' || SQLCODE;
      IF g_debug <= gme_debug.g_log_procedure THEN
        gme_debug.put_line('The procedure UpdtePOREqNBD failed:'||l_errMsg);
      END IF;
      fnd_message.set_name('FND', 'FND_GENERIC_MESSAGE');
      fnd_message.set_token('MESSAGE', l_errMsg);
      fnd_msg_pub.add;

      gme_common_pvt.count_and_get (x_count        => x_message_count
                                   ,p_encoded      => fnd_api.g_false
                                   ,x_data         => x_message_list);                   
  END updatePOReqNBD;


 /*================================================================================
  Procedure
    Attribute
  Description
    The program is used to populate values to gme resource txn reocrd.

  Parameters
      p_source_line_id           The Id of the RCV_TRANSACTIONS_INTERFACE table,
                                 it is passed by Receving Transaction Processor 
                                 Concurrent Program.
      p_rcv_txn_id               receiving transaction Id,It's passed by 
                                 Receving Transaction Processor Concurrent Program.
      x_Res_rec                  The record type of GME_RESOURCE_TXN_OPEN_INTF table.           
                                            
  History
  =======

================================================================================*/
PROCEDURE Attribute(p_source_line_id     IN  NUMBER,
                    p_rcv_txn_id         IN  NUMBER,
                    x_return_status      OUT NOCOPY VARCHAR2,
                    x_message_list       OUT NOCOPY VARCHAR2,
                    x_message_count      OUT NOCOPY NUMBER)
IS
l_api_name                    CONSTANT VARCHAR(30) := 'ATTRIBUTE';
l_wip_entity_id               NUMBER := FND_API.G_MISS_NUM;
l_wip_line_id                 NUMBER := FND_API.G_MISS_NUM;
l_wip_operation_seq_num       NUMBER := FND_API.G_MISS_NUM;
l_wip_resource_seq_num        NUMBER := FND_API.G_MISS_NUM;
l_resource_id                 NUMBER := FND_API.G_MISS_NUM;
l_transaction_date            DATE := FND_API.G_MISS_DATE;
l_transaction_type            VARCHAR2(25) := FND_API.G_MISS_CHAR;
l_parent_transaction_id       NUMBER := FND_API.G_MISS_NUM; 
l_creation_date               DATE := FND_API.G_MISS_DATE;
l_created_by                  NUMBER := FND_API.G_MISS_NUM;
l_item_id                     NUMBER := FND_API.G_MISS_NUM;
l_last_update_date            DATE := FND_API.G_MISS_DATE;
l_last_updated_by             NUMBER := FND_API.G_MISS_NUM;
l_last_update_login           NUMBER := FND_API.G_MISS_NUM;
l_organization_id             NUMBER := FND_API.G_MISS_NUM;
l_reason_id                   NUMBER := FND_API.G_MISS_NUM;
l_source_doc_unit_of_measure  VARCHAR2(25) := FND_API.G_MISS_CHAR;
l_comments                    VARCHAR2(240) := FND_API.G_MISS_CHAR;
l_po_header_id                NUMBER := FND_API.G_MISS_NUM;
l_po_line_id                  NUMBER := FND_API.G_MISS_NUM;
l_po_unit_price               NUMBER := FND_API.G_MISS_NUM;
l_quantity                    NUMBER := FND_API.G_MISS_NUM;
l_primary_quantity            NUMBER := FND_API.G_MISS_NUM;
l_unit_of_measure             VARCHAR2(25) := FND_API.G_MISS_CHAR;
l_primary_unit_of_measure     VARCHAR2(25) := FND_API.G_MISS_CHAR;
l_currency_code               VARCHAR2(15) := FND_API.G_MISS_CHAR;
l_currency_conversion_type    VARCHAR2(10) := FND_API.G_MISS_CHAR;
l_currency_conversion_rate    NUMBER := FND_API.G_MISS_NUM;
l_currency_conversion_date    DATE := FND_API.G_MISS_DATE;
l_end_date                    DATE := FND_API.G_MISS_DATE;
x_res_rec                     GME_RESOURCE_TXN_OPEN_INTF.Res_Rec_Type;


rcv_transaction_not_found     EXCEPTION;
BEGIN
  x_return_status := fnd_api.g_ret_sts_success;

  IF g_debug <= gme_debug.g_log_procedure THEN
       gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                           || l_api_name);
  END IF;      
  BEGIN
    SELECT wip_entity_id
          , wip_line_id
          , wip_operation_seq_num
          , wip_resource_seq_num
          , bom_resource_id
          , transaction_date
          , transaction_type
          , parent_transaction_id
          , creation_date
          , created_by
          , item_id
          , last_update_date
          , last_updated_by
          , last_update_login
          , to_organization_id
          , reason_id
          , source_doc_unit_of_measure
          , comments
          , po_header_id
          , po_line_id
          , po_unit_price
          , quantity
          , primary_quantity
          , unit_of_measure
          , primary_unit_of_measure
          , currency_code
          , currency_conversion_type
          , currency_conversion_rate
          , currency_conversion_date
          , sysdate
     INTO l_wip_entity_id
          , l_wip_line_id
          , l_wip_operation_seq_num
          , l_wip_resource_seq_num
          , l_resource_id
          , l_transaction_date
          , l_transaction_type
          , l_parent_transaction_id
          , l_creation_date
          , l_created_by
          , l_item_id
          , l_last_update_date
          , l_last_updated_by
          , l_last_update_login
          , l_organization_id
          , l_reason_id
          , l_source_doc_unit_of_measure
          , l_comments
          , l_po_header_id
          , l_po_line_id
          , l_po_unit_price
          , l_quantity
          , l_primary_quantity
          , l_unit_of_measure
          , l_primary_unit_of_measure
          , l_currency_code
          , l_currency_conversion_type
          , l_currency_conversion_rate
          , l_currency_conversion_date
          , l_end_date
      FROM rcv_transactions_interface
      WHERE interface_transaction_id = p_source_line_id;
    EXCEPTION
      WHEN OTHERS THEN
        RAISE rcv_transaction_not_found;
    END;

    IF x_res_rec.interface_id IS NULL THEN
      SELECT gme_resource_txns_interface_s.nextval
        INTO x_res_rec.interface_id
        FROM dual;
    END IF;

    IF x_res_rec.organization_code IS NULL AND l_organization_id IS NOT NULL THEN
      SELECT organization_code
        INTO x_res_rec.organization_code
        FROM mtl_parameters
       WHERE organization_id = l_organization_id;
    ELSE
      x_res_rec.organization_code := NULL;
    END IF;

    IF x_res_rec.batch_no IS NULL AND l_wip_entity_id IS NOT NULL THEN
      SELECT batch_no
        INTO x_res_rec.batch_no
        FROM gme_batch_header
       WHERE batch_id = l_wip_entity_id;
    ELSE
      x_res_rec.batch_no := NULL;
    END IF;

    IF x_res_rec.batch_step_no IS NULL AND l_wip_operation_seq_num IS NOT NULL THEN
      SELECT batchstep_no
        INTO x_res_rec.batch_step_no
        FROM gme_batch_steps
       WHERE batchstep_id = l_wip_operation_seq_num;
    ELSE
      x_res_rec.batch_step_no := NULL;
    END IF;

    IF x_res_rec.activity IS NULL AND l_wip_resource_seq_num IS NOT NULL THEN
      SELECT gbsa.activity
        INTO x_res_rec.activity
        FROM gme_batch_step_resources gbsr,
             gme_batch_step_activities gbsa
       WHERE gbsr.batchstep_activity_id = gbsa.batchstep_activity_id
         AND gbsr.batchstep_resource_id = l_wip_resource_seq_num;
    ELSE
      x_res_rec.activity := NULL;
    END IF;

    IF x_res_rec.resources IS NULL AND l_wip_resource_seq_num IS NOT NULL THEN
      SELECT resources
        INTO x_res_rec.resources
        FROM gme_batch_step_resources
       WHERE batchstep_resource_id = l_wip_resource_seq_num;
    ELSE
      x_res_rec.resources := NULL;
    END IF;

    IF x_res_rec.batch_id IS NULL AND l_wip_entity_id IS NOT NULL THEN
      x_res_rec.batch_id := l_wip_entity_id;
    END IF;

    IF x_res_rec.batchstep_resource_id IS NULL AND l_wip_resource_seq_num IS NOT NULL THEN
      x_res_rec.batchstep_resource_id := l_wip_resource_seq_num;
    END IF;


    IF x_res_rec.reason_name IS NULL AND l_reason_id IS NOT NULL THEN
      SELECT reason_name
        INTO x_res_rec.reason_name
        FROM mtl_transaction_reasons
       WHERE reason_id = l_reason_id;
    ELSE
      x_res_rec.reason_name := NULL;
    END IF;


    IF x_res_rec.trans_qty_um IS NULL AND l_wip_resource_seq_num IS NOT NULL THEN
      SELECT resource_qty_um
        INTO x_res_rec.trans_qty_um
        FROM gme_batch_step_resources
       WHERE batchstep_resource_id = l_wip_resource_seq_num;
    ELSE
      x_res_rec.trans_qty_um := NULL;
    END IF;

    IF x_res_rec.reason_id IS NULL AND l_reason_id IS NOT NULL THEN
      x_res_rec.reason_id := l_reason_id;
    ELSE
      x_res_rec.reason_id := NULL;
    END IF;

    IF x_res_rec.creation_date IS NULL AND l_creation_date IS NOT NULL THEN
      x_res_rec.creation_date := l_creation_date;
    ELSE
      x_res_rec.creation_date := SYSDATE;
    END IF;

    IF x_res_rec.last_update_date IS NULL AND l_last_update_date IS NOT NULL THEN
      x_res_rec.last_update_date := l_last_update_date;
    ELSE
      x_res_rec.last_update_date := SYSDATE;
    END IF;

    IF x_res_rec.created_by IS NULL AND l_created_by IS NOT NULL THEN
      x_res_rec.created_by := l_created_by;
    END IF;

    IF x_res_rec.last_updated_by IS NULL AND l_last_updated_by IS NOT NULL THEN
      x_res_rec.last_updated_by := l_last_updated_by;
    END IF;

    IF x_res_rec.cost_source IS NULL AND l_resource_id IS NOT NULL THEN
      SELECT cost_source
        INTO x_res_rec.cost_source
        FROM cr_rsrc_dtl
       WHERE resource_id = l_resource_id;
    ELSE
      x_res_rec.cost_source := NULL;   
    END IF;

    IF x_res_rec.po_header_id IS NULL AND l_po_header_id IS NOT NULL THEN
      x_res_rec.po_header_id := l_po_header_id;
    ELSE
      x_res_rec.po_header_id := NULL;
    END IF;

    IF x_res_rec.po_line_id IS NULL AND l_po_line_id IS NOT NULL THEN
      x_res_rec.po_line_id := l_po_line_id;
    ELSE
      x_res_rec.po_line_id := NULL;
    END IF;

    IF x_res_rec.currency_code IS NULL AND l_currency_code IS NOT NULL THEN
      x_res_rec.currency_code := l_currency_code;
    ELSE
      x_res_rec.currency_code := NULL;
    END IF;

    IF x_res_rec.currency_conversion_date IS NULL AND l_currency_conversion_date IS NOT NULL THEN
      x_res_rec.currency_conversion_date := l_currency_conversion_date;
    ELSE
      x_res_rec.currency_conversion_date := NULL;
    END IF;

    IF x_res_rec.currency_conversion_type IS NULL AND l_currency_conversion_type IS NOT NULL THEN
      x_res_rec.currency_conversion_type := l_currency_conversion_type;
    ELSE
      x_res_rec.currency_conversion_type := NULL;
    END IF;

    IF x_Res_rec.currency_conversion_rate IS NULL AND l_currency_conversion_rate IS NOT NULL THEN
      x_Res_rec.currency_conversion_rate := l_currency_conversion_rate;
    ELSE
      x_Res_rec.currency_conversion_rate := NULL; 
    END IF;

    IF x_res_rec.resource_usage IS NULL THEN      
      Convert_Delivery_Qty(p_inventory_item_id      =>l_item_id,
                           p_organization_id        =>l_organization_id,
                           p_primary_qty            =>l_primary_quantity,
                           p_batch_id               =>l_wip_entity_id,
                           p_batchstep_resource_id  =>l_wip_resource_seq_num,
                           x_return_status          =>x_return_status,
                           x_message_list           =>x_message_list,
                           x_message_count          =>x_message_count,
                           x_resource_usage         =>x_res_rec.resource_usage);
                           
       IF (x_return_status <> fnd_api.g_ret_sts_success) THEN
         RAISE fnd_api.g_exc_unexpected_error;  
       END IF;                           
                                                                                                             
      /*
       If receiving txn is Return,the resource usage should be negative.
      */                                                 
      IF l_transaction_type IN ( gme_osp.g_osp_ret_to_rcv
                                ,gme_osp.g_osp_ret_to_vend) THEN
                                
                                
        x_res_rec.resource_usage := -1*x_res_rec.resource_usage;                       
      END IF;                                                         

    END IF;
    --BUG 23195628 set receipt transaction date as end_date of resource txn 
    IF x_res_rec.end_date IS NULL AND l_transaction_date IS NOT NULL THEN
      x_res_rec.end_date := l_transaction_date;
    END IF;

    /*
      Calculate Transaction Date based on end data and resource usage.
    */ 
    --BUG 23595831 change trans_date of resource txn is equal to transaction date of receipt.
    IF x_res_rec.trans_date IS NULL THEN

      x_res_rec.trans_date := x_res_rec.end_date;                 
    END IF;

    --BUG 23595831 change start date of resource txn is equal to end date - resource usage.
    IF x_res_rec.start_date IS NULL THEN
      
      calc_trans_date(p_end_date               => x_res_rec.end_date,
                      p_resource_usage         => x_res_rec.resource_usage,
                      p_batchstep_resource_id  => x_res_rec.batchstep_resource_id,
                      x_trans_date             => x_res_rec.start_date,
                      x_return_status          => x_return_status,
                      x_message_list           => x_message_list,
                      x_message_count          => x_message_count);
                      
      IF x_return_status <> fnd_api.g_ret_sts_success THEN
        RAISE fnd_api.g_exc_unexpected_error;  
      END IF;     

      --x_res_rec.start_date := x_Res_rec.trans_date;
    END IF;     
    
    /*
     If receiving transaction is Correction or Return,
     Set parent transaction id as receiving transaction id
    */
    IF x_res_rec.rcv_transaction_id IS NULL THEN
      IF l_transaction_type IN ( gme_osp.g_osp_correct
                                ,gme_osp.g_osp_ret_to_rcv
                                ,gme_osp.g_osp_ret_to_vend) 
        AND l_parent_transaction_id IS NOT NULL THEN
        x_res_rec.rcv_transaction_id := l_parent_transaction_id;
      ELSE  
        x_res_rec.rcv_transaction_id := p_rcv_txn_id;
      END IF;
    END IF;

    /*
     Calculate Actual Resource Usage.
    */
    IF x_res_rec.actual_resource_rate IS NULL THEN
      x_res_rec.actual_resource_rate :=Get_Actual_Resource_Rate(p_organization_id          => l_organization_id,
                                                                p_resource_id              => l_resource_id,
                                                                p_po_line_id               => l_po_line_id,
                                                                p_po_unit_price            => l_po_unit_price,
                                                                p_primary_unit_of_measure  => l_primary_unit_of_measure, 
                                                                p_batchstep_resource_id    => l_wip_resource_seq_num, 
                                                                p_batch_id                 => l_wip_entity_id, 
                                                                p_currency_conversion_rate => l_currency_conversion_rate);
    
    END IF;    

    x_res_rec.PROCESS_STATUS            := 1;
    x_res_rec.RESOURCE_INSTANCE_NO      := NULL;
    x_res_rec.INT_BATCH_ID              := NULL;
    x_res_rec.INT_BATCHSTEP_RESOURCE_ID := NULL;
    x_res_rec.INSTANCE_ID               := NULL;
    x_res_rec.INT_RESOURCE_INSTANCE_ID  := NULL;
    x_res_rec.INT_ORGANIZATION_ID       := NULL;
    x_res_rec.LAST_UPDATE_LOGIN         := NULL;
    x_res_rec.PROCESS_PHASE             := NULL;
    x_res_rec.REQUEST_ID                := NULL;
    x_res_rec.GROUP_ID                  := NULL;
    
    
    INSERT INTO gme_resource_txns_interface
             (
              interface_id
              ,organization_code
              ,batch_no
              ,batch_step_no
              ,activity
              ,resources
              ,batch_id
              ,batchstep_resource_id
              ,resource_usage
              ,trans_date
              ,reason_name
              ,start_date
              ,end_date
              ,trans_qty_um
              ,reason_id
              ,cost_source
              ,po_header_id
              ,po_line_id
              ,actual_resource_rate
              ,currency_code
              ,currency_conversion_date
              ,currency_conversion_type
              ,currency_conversion_rate
              ,rcv_transaction_id
              ,process_status
              ,creation_date
              ,last_update_date
              ,created_by
              ,last_updated_by
              )
              VALUES
             (
               x_res_rec.interface_id
              ,x_res_rec.organization_code
              ,x_res_rec.batch_no
              ,x_res_rec.batch_step_no
              ,x_res_rec.activity
              ,x_res_rec.resources
              ,x_res_rec.batch_id
              ,x_res_rec.batchstep_resource_id
              ,x_res_rec.resource_usage
              ,x_res_rec.trans_date
              ,x_res_rec.reason_name
              ,x_res_rec.start_date
              ,x_res_rec.end_date
              ,x_res_rec.trans_qty_um
              ,x_res_rec.reason_id
              ,x_res_rec.cost_source
              ,x_res_rec.po_header_id
              ,x_res_rec.po_line_id
              ,x_res_rec.actual_resource_rate
              ,x_res_rec.currency_code
              ,x_res_rec.currency_conversion_date
              ,x_res_rec.currency_conversion_type
              ,x_res_rec.currency_conversion_rate
              ,x_res_rec.rcv_transaction_id
              ,x_res_rec.process_status
              ,x_res_rec.creation_date
              ,x_res_rec.last_update_date
              ,x_res_rec.created_by
              ,x_res_rec.last_updated_by);    
EXCEPTION
  WHEN rcv_transaction_not_found THEN
    x_return_status := fnd_api.g_ret_sts_error;
    gme_common_pvt.log_message('GME_RCV_TXN_NOT_FOUND');
    gme_common_pvt.count_and_get (x_count        => x_message_count
                                 ,p_encoded      => fnd_api.g_false
                                 ,x_data         => x_message_list); 
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line(x_message_list);
    END IF;                                 
                                 
  WHEN fnd_api.g_exc_unexpected_error THEN
    x_return_status := fnd_api.g_ret_sts_error;
    gme_common_pvt.count_and_get (x_count        => x_message_count
                                 ,p_encoded      => fnd_api.g_false
                                 ,x_data         => x_message_list); 
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line(x_message_list);
    END IF;                                                                          
  WHEN OTHERS THEN
    x_return_status := fnd_api.g_ret_sts_unexp_error;
    x_message_list := 'unexpected error: ' || SQLERRM || 'SQLCODE = ' || SQLCODE;
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line(x_message_list);
    END IF;
    fnd_message.set_name('FND', 'FND_GENERIC_MESSAGE');
    fnd_message.set_token('MESSAGE', x_message_list);
    fnd_msg_pub.add;
       
END;


 /*================================================================================
  Procedure
    Convert_Delivery_Qty
  Description
    calculate Actual Resource Usage

  Parameters
      p_primary_qty              The primary quantity of receving transaction.
  History
  =======

================================================================================*/
PROCEDURE Convert_Delivery_Qty(p_inventory_item_id     IN NUMBER,
                               p_organization_id       IN NUMBER,
                               p_primary_qty           IN NUMBER,
                               p_batch_id              IN NUMBER,
                               p_batchstep_resource_id IN NUMBER,
                               x_return_status         OUT NOCOPY VARCHAR2,
                               x_message_list          OUT NOCOPY VARCHAR2,
                               x_message_count         OUT NOCOPY NUMBER,
                               x_resource_usage        OUT NOCOPY NUMBER                                
                               )
 IS
l_api_name            CONSTANT VARCHAR(30) := 'CONVERT_DELIVERY_QTY'; 
l_primary_product_qty NUMBER;
l_plan_total_usage    NUMBER;
l_converted_qty       NUMBER;
l_unit_type           VARCHAR2(25);

item_unit_type_unset  EXCEPTION;
BEGIN
  x_return_status := fnd_api.g_ret_sts_success;

  IF g_debug <= gme_debug.g_log_procedure THEN
       gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                           || l_api_name);
  END IF;      
  SELECT outside_operation_uom_type
    INTO l_unit_type
    FROM mtl_system_items_b
   WHERE inventory_item_id = p_inventory_item_id
     AND organization_id   = p_organization_id;
  /*
   1.If the OSP item unit type is Resource:
     Resource usage = Primary quantity of the receiving transaction
   2.If the OSP item unit type is Product:
      Resource usage = (Converted receipt quantity *  (Planned resource usage/Planned primary product Quantity)
  */
  IF l_unit_type = gme_osp.g_unit_type_resource THEN
    x_resource_usage := p_primary_qty;
 
  ELSIF l_unit_type =  gme_osp.g_unit_type_product THEN
    SELECT DECODE(gbh.batch_status,gme_common_pvt.g_batch_pending
                                 ,gmd.plan_qty
                                 ,gme_common_pvt.g_batch_wip
                                 ,gmd.wip_plan_qty
                                 ,gmd.plan_qty)                                 
      INTO l_primary_product_qty
      FROM wip_entities we,
           gme_material_details gmd,
           gme_batch_header gbh
     WHERE we.wip_entity_id = p_batch_id
       AND we.wip_entity_id = gmd.batch_id
       AND we.primary_item_id = gmd.inventory_item_id
       AND gmd.line_type = 1  --need to replaced by constant variable
       AND we.wip_entity_id = gbh.batch_id;

    SELECT plan_rsrc_usage
      INTO l_plan_total_usage
      FROM gme_batch_step_resources
     WHERE batchstep_resource_id = p_batchstep_resource_id;

    l_converted_qty :=  p_primary_qty*l_plan_total_usage/l_primary_product_qty;
    x_resource_usage := l_converted_qty;
  END IF;
  
EXCEPTION     
  WHEN OTHERS THEN
    x_return_status := fnd_api.g_ret_sts_unexp_error;
    x_message_list := 'unexpected error: ' || SQLERRM || 'SQLCODE = ' || SQLCODE;
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line(x_message_list);
    END IF;
    fnd_message.set_name('FND', 'FND_GENERIC_MESSAGE');
    fnd_message.set_token('MESSAGE', x_message_list);
    fnd_msg_pub.add;  

END;

 /*================================================================================
  Procedure
    Insert_Resource_Txn_Intf
  Description
    The program is used to insert a record into gme_resource_txns_interface table.

  Parameters
   
  History
  =======

================================================================================*/
/*PROCEDURE Insert_Resource_Txn_Intf(p_Res_rec                IN GME_RESOURCE_TXN_OPEN_INTF.Res_Rec_Type,
                                   x_return_status          OUT NOCOPY  VARCHAR2,
                                   x_msg_count              OUT NOCOPY  NUMBER,
                                   x_msg_data               OUT NOCOPY  VARCHAR2)
IS
l_api_name                    CONSTANT VARCHAR(30) := 'INSERT_RESOURCE_TXN_INTF';
BEGIN
  x_return_status  := fnd_api.g_ret_sts_success;
  
  IF g_debug <= gme_debug.g_log_procedure THEN
       gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                           || l_api_name);
  END IF;    

  INSERT INTO gme_resource_txns_interface
           (
            interface_id
            ,organization_code
            ,batch_no
            ,batch_step_no
            ,activity
            ,resources
            ,batch_id
            ,batchstep_resource_id
            ,resource_usage
            ,trans_date
            ,reason_name
            ,start_date
            ,end_date
            ,trans_qty_um
            ,reason_id
            ,cost_source
            ,po_header_id
            ,po_line_id
            ,actual_resource_rate
            ,currency_code
            ,currency_conversion_date
            ,currency_conversion_type
            ,currency_conversion_rate
            ,rcv_transaction_id
            ,process_status
            ,creation_date
            ,last_update_date
            ,created_by
            ,last_updated_by
            )
            VALUES
           (
             p_res_rec.interface_id
            ,p_res_rec.organization_code
            ,p_res_rec.batch_no
            ,p_res_rec.batch_step_no
            ,p_res_rec.activity
            ,p_res_rec.resources
            ,p_res_rec.batch_id
            ,p_res_rec.batchstep_resource_id
            ,p_res_rec.resource_usage
            ,p_res_rec.trans_date
            ,p_res_rec.reason_name
            ,p_res_rec.start_date
            ,p_res_rec.end_date
            ,p_res_rec.trans_qty_um
            ,p_res_rec.reason_id
            ,p_res_rec.cost_source
            ,p_res_rec.po_header_id
            ,p_res_rec.po_line_id
            ,p_res_rec.actual_resource_rate
            ,p_res_rec.currency_code
            ,p_res_rec.currency_conversion_date
            ,p_res_rec.currency_conversion_type
            ,p_res_rec.currency_conversion_rate
            ,p_res_rec.rcv_transaction_id
            ,p_res_rec.process_status
            ,p_res_rec.creation_date
            ,p_res_rec.last_update_date
            ,p_res_rec.created_by
            ,p_res_rec.last_updated_by);
EXCEPTION
  WHEN OTHERS THEN
    x_return_status := FND_API.G_RET_STS_UNEXP_ERROR;
    x_msg_data := 'unexpected error: ' || SQLERRM || 'SQLCODE = ' || SQLCODE;
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line(x_msg_data);
    END IF;
    fnd_message.set_name('FND', 'FND_GENERIC_MESSAGE');
    fnd_message.set_token('MESSAGE', x_msg_data);
    fnd_msg_pub.add;     
    
END;*/

/*================================================================================
  Procedure
    Get_Actual_Resource_Rate
  Description
    The program is used to calculate Actual Resource Rate.

  Parameters
   
  History
  =======

================================================================================*/
FUNCTION Get_Actual_Resource_Rate(p_organization_id IN NUMBER,
                                  p_resource_id     IN NUMBER,
                                  p_po_line_id      IN NUMBER,
                                  p_po_unit_price   IN NUMBER,
                                  p_primary_unit_of_measure  IN VARCHAR2,
                                  p_batchstep_resource_id    IN NUMBER,
                                  p_batch_id                 IN NUMBER,
                                  p_currency_conversion_rate IN NUMBER)
  
RETURN NUMBER
IS
  l_api_name                    CONSTANT VARCHAR(30) := 'ATTRIBUTE';
  l_actual_resource_rate        NUMBER := FND_API.G_MISS_NUM;
  l_uom_basis                   VARCHAR2(25);
  l_po_uom                      VARCHAR2(25) ;
  l_conversion_factor           NUMBER;
  l_currency_conversion_rate    NUMBER;
  l_inventory_item_id           NUMBER;
  l_plan_rsrc_usage             NUMBER;
  l_plan_qty                    NUMBER;
BEGIN
  
  IF g_debug <= gme_debug.g_log_procedure THEN
       gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                           || l_api_name);
  END IF;     
  /*
  Actual resource rate calculation formula:
  Actual resource rate = po price/resource rate 
  resource rate = planned resource usage/batch qty  if osp uom is product  
  */
  SELECT msib.inventory_item_id,msib.outside_operation_uom_type
    INTO l_inventory_item_id,l_uom_basis
    FROM cr_rsrc_dtl crd,
         mtl_system_items_b msib
   WHERE crd.resource_id = p_resource_id
     AND crd.organization_id = p_organization_id
     AND crd.organization_id = msib.organization_id
     AND crd.purchase_item_id = msib.inventory_item_id;
    

   SELECT unit_meas_lookup_code
     INTO l_po_uom
     FROM po_lines_all
    WHERE po_line_id = p_po_line_id;

   IF l_po_uom  <> p_primary_unit_of_measure then
     l_conversion_factor  := inv_convert.inv_um_convert
                                 (l_inventory_item_id,
                                  NULL,
                                  1,
                                  NULL,
                                  NULL,
                                  p_primary_unit_of_measure,
                                  l_po_uom
                                 );
   ELSE
     l_conversion_factor := 1 ;
   END IF;


   IF l_conversion_factor = -99999 THEN
    RETURN FND_API.G_MISS_NUM;
   END IF ;


    IF l_uom_basis = gme_osp.g_unit_type_product AND p_po_unit_price IS NOT NULL THEN
              
        SELECT plan_rsrc_usage
          INTO l_plan_rsrc_usage
          FROM gme_batch_step_resources
         WHERE batchstep_resource_id = p_batchstep_resource_id;
               
        SELECT DECODE(gbh.batch_status,gme_common_pvt.g_batch_pending
                                      ,gmd.plan_qty
                                      ,gme_common_pvt.g_batch_wip
                                      ,gmd.wip_plan_qty
                                      ,gmd.plan_qty)
          INTO l_plan_qty
          FROM gme_material_details gmd,
               wip_entities we,
               gme_batch_header gbh
         WHERE we.wip_entity_id = p_batch_id
           AND we.wip_entity_id = gmd.batch_id
           AND we.primary_item_id = gmd.inventory_item_id
           AND gmd.line_type = gme_common_pvt.g_line_type_prod
           AND gbh.batch_id = we.wip_entity_id;
                 
         IF (l_plan_rsrc_usage <> 0 AND l_plan_qty <> 0) THEN
           l_actual_resource_rate := p_po_unit_price /(l_plan_rsrc_usage/l_plan_qty);
         ELSE
           l_actual_resource_rate := p_po_unit_price ;
         END IF;
    ELSIF p_po_unit_price IS NOT NULL then
      l_actual_resource_rate := p_po_unit_price ;
    END IF;


    /*
      BUG 22086884  06-NOV-2015 Shaliu Chen
      return the actual_resource_rate directly, removing the logic of
      multiplying currency_conversion_rate.
     */
    RETURN l_actual_resource_rate;
/*    l_currency_conversion_rate := nvl(p_currency_conversion_rate,-1);
    IF l_currency_conversion_rate = -1 THEN
      RETURN l_actual_resource_rate;
    ELSE
      RETURN l_actual_resource_rate * l_currency_conversion_rate;
    END IF;*/

EXCEPTION
   WHEN OTHERS THEN
      RETURN FND_API.G_MISS_NUM;

END Get_Actual_Resource_Rate;

/*================================================================================
  Procedure
    calc_trans_date
  Description
    The program is used to calculate trans_date and actual start date of resource
    transaction based on the actual end data and actual resource usage.
    

  Parameters
   
  History
  =======

================================================================================*/
PROCEDURE calc_trans_date(p_end_date              IN DATE,
                          p_resource_usage        IN NUMBER,
                          p_batchstep_resource_id IN NUMBER,
                          x_trans_date           OUT NOCOPY DATE,
                          x_return_status        OUT NOCOPY VARCHAR2,
                          x_message_list         OUT NOCOPY VARCHAR2,
                          x_message_count        OUT NOCOPY NUMBER)
IS
  l_api_name               CONSTANT VARCHAR(30) := 'CALC_TRANS_DATE';
  l_hour_um                sy_uoms_mst.um_code%TYPE;
  l_resources              VARCHAR2 (16);
  l_usage_uom              VARCHAR2 (4); 
  l_errMsg                 VARCHAR2 (2000);
  l_txn_usage              NUMBER; 
  l_trans_date             DATE;
  l_rsrc_act_start_date    DATE;

  missing_profile_option  EXCEPTION;
  uom_conversion_err      EXCEPTION;
  
  CURSOR cur_fetch_resource_dtl (v_batchstep_resource_id NUMBER)
  IS
     SELECT resources, usage_um,actual_start_date
       FROM gme_batch_step_resources
      WHERE batchstep_resource_id = v_batchstep_resource_id;  

BEGIN
  
  x_return_status := fnd_api.g_ret_sts_success;
  
  IF g_debug <= gme_debug.g_log_procedure THEN
       gme_debug.put_line ('Entering api ' || g_pkg_name || '.'
                           || l_api_name);
  END IF;  
  
   /*
    If resource usge is negative,that means the resource txn
    is return or correction,the start_date,trans_date will be
    calculate in the resource txn import concurrent program,
    just set end_date to trans_date in here
  */
  IF p_resource_usage < 0 THEN
    x_trans_date := p_end_date;
    RETURN;       
  END IF;  

  l_hour_um := fnd_profile.value_specific (name         => 'BOM:HOUR_UOM_CODE'
                                          ,user_id      => gme_common_pvt.g_user_ident);
                                          
                                          
                                          
  IF (l_hour_um IS NULL) THEN
     gme_common_pvt.log_message ('GME_API_UNABLE_TO_GET_CONSTANT'
                                ,'CONSTANT_NAME'
                                ,'BOM:HOUR_UOM_CODE');
     RAISE missing_profile_option;
  END IF;

  OPEN cur_fetch_resource_dtl (p_batchstep_resource_id);

  FETCH cur_fetch_resource_dtl
   INTO l_resources, l_usage_uom,l_rsrc_act_start_date; 

  IF l_hour_um <> l_usage_uom THEN
    l_txn_usage := inv_convert.inv_um_convert (item_id            => 0
                                              ,PRECISION          => 5
                                              ,from_quantity      => p_resource_usage
                                              ,from_unit          => l_usage_uom
                                              ,to_unit            => l_hour_um
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
    l_txn_usage := p_resource_usage;
  END IF;  

  x_trans_date := p_end_date - l_txn_usage/24;
  /*
   IF trans_date is less than actual_start_date of the source,
   assign actual_start_date to trans_date
  */
  IF (l_rsrc_act_start_date IS NOT NULL AND
      l_rsrc_act_start_date > x_trans_date)  THEN      
    x_trans_date := l_rsrc_act_start_date;  
  END IF;

  

EXCEPTION
  WHEN missing_profile_option THEN 
    x_return_status := fnd_api.g_ret_sts_error;
    gme_common_pvt.count_and_get (x_count        => x_message_count
                                 ,p_encoded      => fnd_api.g_false
                                 ,x_data         => x_message_list);     
  
  WHEN uom_conversion_err THEN
    /*
     BUG 19515491 Shaliu Chen 4-Sep-2014
     If uom conversion is failed,copy end_date value to trans_date.
    */
    x_return_status := fnd_api.g_ret_sts_success;
    x_trans_date := p_end_date;      

  WHEN OTHERS THEN
    x_return_status := fnd_api.g_ret_sts_unexp_error;
    x_message_list  := 'unexpected error: ' || SQLERRM || 'SQLCODE = ' || SQLCODE;
    IF g_debug <= gme_debug.g_log_procedure THEN
      gme_debug.put_line(x_message_list);
    END IF;
    fnd_message.set_name('FND', 'FND_GENERIC_MESSAGE');
    fnd_message.set_token('MESSAGE', x_message_list);
    fnd_msg_pub.add;   
END;     


 /*================================================================================
  Procedure
    IS_Resource_Txn_Exist
  Description
    The program is uesed to check whether the osp batch/step has resource txn exist
    which is generated by receiving po.

  Parameters
    p_batch_id
    p_batchstep_id             

  History
  =======
  ER 20809749    Shaliu Chen     14-APR-2015
                 modify source code to support multiple osp steps.
================================================================================*/
FUNCTION is_resource_txn_exist(p_batch_id      IN NUMBER,
                               p_batchstep_id  IN NUMBER DEFAULT NULL)
  RETURN BOOLEAN IS
  CURSOR Get_Resource_Txn(v_batch_id NUMBER,v_batchstep_id NUMBER) IS
    SELECT NVL(SUM(NVL(grt.resource_usage,0)),0)
      FROM gme_resource_txns grt,
           gme_batch_step_resources gbsr,
           cr_rsrc_mst crm
     WHERE grt.doc_id = v_batch_id
       AND grt.line_id = gbsr.batchstep_resource_id
       AND (v_batchstep_id IS NULL OR
            gbsr.batchstep_id = v_batchstep_id)       
       --AND gbsr.batchstep_id = v_batchstep_id
       AND gbsr.batch_id = v_batch_id
       AND grt.rcv_transaction_id IS NOT NULL
       AND gbsr.resources = crm.resources
       AND NVL(crm.outside_process_ind,gme_osp.g_osp_ind_disabled) = gme_osp.g_osp_ind_enabled;
       
/*   CURSOR Get_osp_step(v_batch_id NUMBER) IS
     SELECT gbsr.batchstep_id
       FROM gme_batch_step_resources gbsr,
            cr_rsrc_mst crm
      WHERE gbsr.batch_id = v_batch_id
        AND gbsr.resources = crm.resources
        AND NVL(crm.outside_process_ind,gme_osp.g_osp_ind_disabled) = gme_osp.g_osp_ind_enabled; */
        
   CURSOR Get_Pending_Resource_Txn(v_batch_id NUMBER,v_batchstep_id NUMBER) IS
     SELECT count(1)
       FROM gme_resource_txns_interface grti,
            gme_batch_step_resources gbsr,
            cr_rsrc_mst crm
      WHERE gbsr.batch_id = v_batch_id
        AND (v_batchstep_id IS NULL OR
            gbsr.batchstep_id = v_batchstep_id)        
        --AND gbsr.batchstep_id = v_batchstep_id
        AND gbsr.resources = crm.resources
        AND NVL(crm.outside_process_ind,gme_osp.g_osp_ind_disabled) = gme_osp.g_osp_ind_enabled
        AND gbsr.batch_id = grti.batch_id
        AND gbsr.batchstep_resource_id = grti.batchstep_resource_id
        AND grti.process_status <> GME_BATCH_OPEN_INTERFACE.PROCESS_STATUS_SUCCEED
        AND grti.rcv_transaction_id IS NOT NULL;
        
              

  l_total_usage   NUMBER; 
  l_batchstep_id  NUMBER;    
  l_count         NUMBER; 
       
BEGIN
/*  IF p_batchstep_id IS NULL THEN
    OPEN Get_osp_step(p_batch_id);
    FETCH Get_osp_step INTO l_batchstep_id;
    CLOSE Get_osp_step;
  ELSE
    l_batchstep_id := p_batchstep_id;        
  END IF;*/
  
  OPEN Get_Resource_Txn(p_batch_id,l_batchstep_id);
  FETCH Get_Resource_Txn INTO l_total_usage;
  CLOSE Get_Resource_Txn;
  
  IF l_total_usage <= 0 THEN
    RETURN FALSE; 
  END IF; 
  
  OPEN Get_Pending_Resource_Txn(p_batch_id,l_batchstep_id);
  FETCH Get_Pending_Resource_Txn INTO l_count;
  CLOSE Get_Pending_Resource_Txn;
  
  IF l_count > 0 THEN
   RETURN FALSE;
  END IF;
  RETURN TRUE; 
    
    
END; 



 /*================================================================================
  Procedure
    Check_Requisition_Imported
  Description
    Check there is pending requisition in the requistion open interface table.
    If exist,return FALSE,otherwise,return TRUE.

  Parameters
    p_batch_id            

  History
  =======
  ER 20809749    Shaliu Chen     14-APR-2015
                 Add input parameter - p_batchstep_id to support multiple osp steps.
================================================================================*/
FUNCTION Check_Requisition_Imported(p_batch_id     IN NUMBER,
                                    p_batchstep_id IN NUMBER DEFAULT NULL)
  RETURN BOOLEAN IS
  
  CURSOR get_pending_requisition(v_batch_id NUMBER,v_batchstep_id NUMBER) IS
    SELECT count(*)
      FROM po_requisitions_interface_all
     WHERE wip_entity_id = v_batch_id
       AND (v_batchstep_id IS NULL OR
            wip_operation_seq_num = v_batchstep_id)
       AND NVL(process_flag,'PENDING') <> 'ERROR';
       
  l_count NUMBER;       
BEGIN
  OPEN get_pending_requisition(p_batch_id,p_batchstep_id);
  FETCH get_pending_requisition INTO l_count;
  CLOSE get_pending_requisition;
  
  IF l_count > 0 THEN
    RETURN FALSE;
  ELSE
    RETURN TRUE;
  END IF;
  
EXCEPTION
  WHEN OTHERS THEN
    RETURN FALSE;  

END;  

 /*================================================================================
  Procedure
    Is_Propagate_Automatic
  Description
    Check gme parameter - propagate change to po value,If value is Automatic,
    return TRUE,otherwise,return FALSE.

  Parameters
    p_organization_id            

  History
  =======

================================================================================*/
FUNCTION Is_Propagate_Automatic(p_organization_id   IN NUMBER)
  RETURN BOOLEAN IS
  
  CURSOR get_propagate_value(v_organization_id NUMBER) IS
    SELECT NVL(propagate_change_to_po,gme_osp.g_propagate_change_manual)
      FROM gme_parameters
     WHERE organization_id = v_organization_id;   
     
  l_propagate_value  NUMBER;     
  
BEGIN
  OPEN get_propagate_value(p_organization_id);
  FETCH get_propagate_value INTO l_propagate_value;
  CLOSE get_propagate_value;
  
  IF l_propagate_value = gme_osp.g_propagate_change_automatic THEN
    RETURN TRUE;
  ELSE 
    RETURN FALSE;
  END IF; 


EXCEPTION
  WHEN OTHERS THEN
    RETURN FALSE;
  
END;  


end gme_osp;
  
                               
/

COMMIT;
EXIT;
