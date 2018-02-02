/* +======================================================================+ */
/* |    Copyright (c) 2005, 2015 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=pls \
REM dbdrv: checkfile(120.0.12010000.6=120.0.12020000.6)(115.2=120.1):~PROD:~PATH:~FILE
create or replace package gme_osp as
/* $Header: GMEOSPS.pls 120.0.12020000.6 2015/04/17 11:05:19 shalchen noship $ */


  -- Author  : Shaliu Chen
  -- Created : 2014-7-18
  -- Purpose : This package is used to process OPM Step level outside processing.

   /* PO Creation Time */
  g_batch_creation               CONSTANT NUMBER := 1;
  g_batch_rlease                 CONSTANT NUMBER := 2;
  g_step_release                 CONSTANT NUMBER := 3;   

  -- SYS_YES_NO
  YES                            CONSTANT NUMBER := 1;
  NO                             CONSTANT NUMBER := 2;

  g_unit_type_resource           CONSTANT  VARCHAR2(25) := 'RESOURCE';
  g_unit_type_product            CONSTANT  VARCHAR2(25) := 'ASSEMBLY';

  g_propagate_change_automatic   CONSTANT  NUMBER := 1;
  g_propagate_change_manual      CONSTANT  NUMBER := 2;
  
  g_osp_correct                  CONSTANT VARCHAR2(50) :=  'CORRECT';
  g_osp_ret_to_rcv               CONSTANT VARCHAR2(50) :=  'RETURN TO RECEIVING';
  g_osp_ret_to_vend              CONSTANT VARCHAR2(50) :=  'RETURN TO VENDOR';

  
  g_osp_ind_enabled              CONSTANT NUMBER(1)  :=  1;
  g_osp_ind_disabled             CONSTANT NUMBER(1)  :=  0;
  
  g_batch_entity_type            CONSTANT NUMBER := 10;
  



PROCEDURE create_requisition(
                              P_Batch_Id              IN NUMBER,
                              P_Organization_Id       IN NUMBER,
                              P_Batchstep_Id          IN NUMBER,
                              P_Batchstep_Resource_id IN NUMBER DEFAULT NULL,
                              P_Run_ReqImport         IN NUMBER DEFAULT WIP_CONSTANTS.NO,
                              p_addition_flag         IN VARCHAR2 DEFAULT 'N',
                              p_addition_qty          IN NUMBER   DEFAULT 0,
                              x_return_status         OUT NOCOPY  VARCHAR2,
                              x_message_list          OUT NOCOPY  VARCHAR2,
                              x_message_count         OUT NOCOPY  NUMBER);
                              

FUNCTION Get_Item_Revision(p_item_id         IN NUMBER,
                           p_organization_id IN NUMBER,
                           p_rev_date        IN DATE) 
RETURN VARCHAR2;                           

FUNCTION po_req_exists (p_batch_id         in NUMBER,
                        p_organization_id  in NUMBER,
                        p_batchstep_id     in NUMBER default NULL
                         ) RETURN BOOLEAN;
                         
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
  x_api_errors             OUT NOCOPY PO_API_ERRORS_REC_TYPE
);

PROCEDURE update_requisition (
    p_api_version            IN NUMBER,
    p_req_changes            IN OUT NOCOPY PO_REQ_CHANGES_REC_TYPE,
    p_update_source          IN VARCHAR2,
    x_return_status          OUT NOCOPY VARCHAR2,
    x_msg_count              OUT NOCOPY NUMBER,
    x_msg_data               OUT NOCOPY VARCHAR2
);                         



  PROCEDURE cancelPOReq (p_batch_id      IN         NUMBER,
                         p_org_id        IN         NUMBER,
                         p_batchstep_id  IN         NUMBER :=NULL,
                         x_return_status OUT NOCOPY VARCHAR2,
                         x_message_list  OUT NOCOPY  VARCHAR2,
                         x_message_count OUT NOCOPY  NUMBER
                         );


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
);


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
);


PROCEDURE reroute_batch_process(p_batch_id         IN NUMBER,
                                p_organization_id  IN NUMBER,
                                x_return_status    OUT NOCOPY VARCHAR2,
                                x_message_list     OUT NOCOPY  VARCHAR2,
                                x_message_count    OUT NOCOPY  NUMBER);

PROCEDURE closePO(p_batch_id      IN         NUMBER,
                  p_org_id        IN         NUMBER,
                  p_batchstep_id  IN         NUMBER :=NULL,
                  x_return_status OUT NOCOPY VARCHAR2,
                  x_message_list  OUT NOCOPY  VARCHAR2,
                  x_message_count OUT NOCOPY  NUMBER );



FUNCTION Is_OSP_Batch(p_batch_id      IN NUMBER,
                      p_batchstep_id  IN NUMBER DEFAULT NULL)
  RETURN BOOLEAN;
  
PROCEDURE get_osp_parameters_value(p_organization_id    IN NUMBER,
                                   x_gme_parameter_rec  OUT NOCOPY gme_parameters%ROWTYPE,
                                   x_return_status      OUT NOCOPY VARCHAR2);  

FUNCTION Check_Release_Version RETURN BOOLEAN;


PROCEDURE Get_Item_Unit_Type(p_batch_id               IN          NUMBER,
                             p_batchstep_resource_id  IN          NUMBER,
                             x_unit_type              OUT NOCOPY  VARCHAR2,
                             x_return_status          OUT NOCOPY VARCHAR2,
                             x_message_list           OUT NOCOPY  VARCHAR2);

  /*                                    
  BEGIN ER 20809749                    
  Shaliu Chen 09-APR-2015               
  Add an input paramter p_batchstep_resource_id to support multiple osp steps
  */  
FUNCTION check_req_intf_data_exist(p_batch_id IN NUMBER,
                                   p_batchstep_resource_id IN NUMBER)
  RETURN BOOLEAN;

PROCEDURE get_po_req_count_qty (p_batch_id                  IN NUMBER,
                                p_organization_id           IN NUMBER,
                                p_batchstep_resource_id     IN NUMBER default NULL,
                                x_total_count               OUT NOCOPY NUMBER,
                                x_total_quantity            OUT NOCOPY NUMBER);

 PROCEDURE updatePOReqQuantity(p_batch_id                    IN         NUMBER,
                               p_organization_id             IN         NUMBER,
                               p_batchstep_resource_id       IN         NUMBER DEFAULT NULL,
                               x_return_status               OUT NOCOPY VARCHAR2,
                               x_message_list                OUT NOCOPY VARCHAR2,
                               x_message_count               OUT NOCOPY NUMBER   );

  PROCEDURE updatePOReqNBD(p_batch_id            IN         NUMBER,
                           p_batchstep_id        IN         NUMBER DEFAULT NULL,
                           p_organization_id     IN         NUMBER,
                           x_return_status       OUT NOCOPY VARCHAR2,
                           x_message_list        OUT NOCOPY VARCHAR2,
                           x_message_count       OUT NOCOPY NUMBER
                           );
                           

PROCEDURE Attribute(p_source_line_id     IN  NUMBER,
                    p_rcv_txn_id         IN  NUMBER,
                    x_return_status      OUT NOCOPY VARCHAR2,
                    x_message_list       OUT NOCOPY VARCHAR2,
                    x_message_count      OUT NOCOPY NUMBER);


PROCEDURE Convert_Delivery_Qty(p_inventory_item_id     IN NUMBER,
                               p_organization_id       IN NUMBER,
                               p_primary_qty           IN NUMBER,
                               p_batch_id              IN NUMBER,
                               p_batchstep_resource_id IN NUMBER,
                               x_return_status         OUT NOCOPY VARCHAR2,
                               x_message_list          OUT NOCOPY VARCHAR2,
                               x_message_count         OUT NOCOPY NUMBER,
                               x_resource_usage        OUT NOCOPY NUMBER                                
                               );


                                
FUNCTION Get_Actual_Resource_Rate(p_organization_id IN NUMBER,
                                  p_resource_id     IN NUMBER,
                                  p_po_line_id      IN NUMBER,
                                  p_po_unit_price   IN NUMBER,
                                  p_primary_unit_of_measure  IN VARCHAR2,
                                  p_batchstep_resource_id    IN NUMBER,
                                  p_batch_id                 IN NUMBER,
                                  p_currency_conversion_rate IN NUMBER) RETURN NUMBER; 
                                  
                                  
                               
PROCEDURE calc_trans_date(p_end_date              IN DATE,
                          p_resource_usage        IN NUMBER,
                          p_batchstep_resource_id IN NUMBER,
                          x_trans_date           OUT NOCOPY DATE,
                          x_return_status        OUT NOCOPY VARCHAR2,
                          x_message_list         OUT NOCOPY VARCHAR2,
                          x_message_count        OUT NOCOPY NUMBER);   
                          
                          
FUNCTION is_resource_txn_exist(p_batch_id      IN NUMBER,
                               p_batchstep_id  IN NUMBER DEFAULT NULL)
  RETURN BOOLEAN;                                            


FUNCTION Check_Requisition_Imported(p_batch_id     IN NUMBER,
                                    p_batchstep_id IN NUMBER DEFAULT NULL)
  RETURN BOOLEAN;
  
FUNCTION Is_Propagate_Automatic(p_organization_id   IN NUMBER)
  RETURN BOOLEAN;  


end gme_osp;
/

COMMIT;
EXIT;
