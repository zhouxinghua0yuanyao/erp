REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=pls \
REM dbdrv: checkfile(120.8.12010000.5=120.10.12020000.3)(120.5.12000000.5=120.10):~PROD:~PATH:~FILE 

/*=======================================================================#
 #  Copyright (c) 2001, 2014 Oracle Corporation Redwood Shores, California, USA#
 #                           All rights reserved.                        #
 #=======================================================================+
 # FILENAME
 #   GMOGVBTS.pls - GMO Visual Batch 
 # DESCRIPTION
 #   PL/SQL body for package:  GMO_VBATCH_PVT
 # NOTES
 #   Private API 
 # HISTORY
 # 13-Jan-2005 rahugupt Modified for bug 4609365 to show dispense qty in owb 
 # 6-Jun-2006  rahugupt Modified for bug 5224634, to show step nodes.
 # 26-Feb-2007 kmotupal 5105424: Added the procedure INSTANTIATE_INSTR_FOR_LCF for 
 #                      GMO-LCF Batch Integration.
 #=======================================================================*/

SET VERIFY OFF
SET DEFINE OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK
WHENEVER OSERROR EXIT FAILURE ROLLBACK

CREATE OR REPLACE PACKAGE GMO_VBATCH_PVT AS
/* $Header: GMOVVBTS.pls 120.10.12020000.3 2014/04/30 03:00:21 biachen ship $ */

--This function would verify if the step is locked or not.

-- Start of comments
-- API name             : is_step_locked
-- Type                 : Private Utility.
-- Function             : verify if the step is locked or not 
-- Pre-reqs             : None.
-- Parameters           : 
-- IN                   : p_batchstep_id in number
--                        p_requestor in varchar2
-- OUT                  : Y or N
-- End of comments

function IS_STEP_LOCKED (P_BATCHSTEP_ID IN NUMBER,
			 P_REQUESTER IN NUMBER DEFAULT NULL) RETURN  VARCHAR2;


--This function would verify if the step is locked or not.

-- Start of comments
-- API name             : is_step_locked
-- Type                 : Private Utility.
-- Function             : verify if the step is locked or not 
-- Pre-reqs             : None.
-- Parameters           : 
-- IN                   : p_vbatch_mode in varchar2
--                        p_batchstep_id in number
--                        p_requestor in varchar2
-- OUT                  : Y or N
-- End of comments

function IS_STEP_LOCKED (P_VBATCH_MODE IN VARCHAR2,
			 P_BATCHSTEP_ID IN NUMBER,
			 P_REQUESTER IN NUMBER) RETURN VARCHAR2;

--This function would return the reserved quantity for the material.

-- Start of comments
-- API name             : get_material_reservation_qty 
-- Type                 : Private Utility.
-- Function             : gets the context information for the task
-- Pre-reqs             : None.
-- Parameters           : 
-- IN                   : p_organization_id in number 
--                        p_batch_id in number
--                        p_material_detail_id in number
-- OUT                  : material reserved quantity
-- End of comments

                                           

function GET_MATERIAL_RESERVATION_QTY  (P_ORGANIZATION_ID IN NUMBER,
					 P_BATCH_ID IN NUMBER,
					 P_MATERIAL_DETAIL_ID IN NUMBER) RETURN NUMBER;

--Bug 4609365: start
--This function would return the dispensed quantity for the material.

-- Start of comments
-- API name             : get_material_dispensed_qty
-- Type                 : Private Utility.
-- Function             : gets the  dispensed quantity
-- Pre-reqs             : None.
-- Parameters           : 
-- IN                   : p_material_detail_id in number
--                        P_UOM in varchar2 
-- OUT                  : material dispensed quantity
-- End of comments
function GET_MATERIAL_DISPENSED_QTY (P_MATERIAL_DETAIL_ID NUMBER, P_UOM VARCHAR2) RETURN NUMBER;

--Bug 4609365: end
--Bug 	13388249 : start
--This function would return the dispensed quantity for the material.

-- Start of comments
-- API name             : get_actual_quantity
-- Type                 : Private Utility.
-- Function             : gets the  actual quantity
-- Pre-reqs             : None.
-- Parameters           :
-- IN                   : p_material_detail_id in number
--                        P_INVENTORY_ITEM_ID in number
-- OUT                  : actual quantity
-- End of comments


function get_actual_quantity(p_material_detail_id NUMBER, p_inventory_item_id NUMBER) RETURN NUMBER;

--Bug 13388249: end

--This procedure would verify if batch step material is available.

-- Start of comments
-- API name             : get_step_material_availability 
-- Type                 : Private Utility.
-- Function             : verify if the batch step material is available or not.
-- Pre-reqs             : None.
-- Parameters           : 
-- IN                   : p_batchstep_id in number 
-- OUT                  : x_material_available out varchar2
--                        x_return_status out varchar2
--                        x_msg_count out number
--                        x_msg_data out varchar2
-- End of comments


PROCEDURE GET_STEP_MATERIAL_AVAILABILITY (P_BATCHSTEP_ID IN NUMBER,
				   X_MATERIAL_AVAILABLE	OUT NOCOPY VARCHAR2,
				   X_UNAVAL_MATERIAL_ID	OUT NOCOPY FND_TABLE_OF_VARCHAR2_255,
				   X_RETURN_STATUS OUT NOCOPY VARCHAR2,
				   X_MSG_COUNT OUT NOCOPY NUMBER,
				   X_MSG_DATA OUT NOCOPY VARCHAR2);                                           

--This procedure would verify if batch step material is available.

-- Start of comments
-- API name             : get_step_material_availability 
-- Type                 : Private Utility.
-- Function             : verify if the batch step material is available or not.
-- Pre-reqs             : None.
-- Parameters           : 
-- IN                   : p_batchstep_id in number 
-- OUT                  : x_material_available out varchar2
--                        x_return_status out varchar2
--                        x_msg_count out number
--                        x_msg_data out varchar2
-- End of comments


PROCEDURE GET_STEP_MATERIAL_AVAILABILITY (P_BATCHSTEP_ID IN NUMBER,
				   X_MATERIAL_AVAILABLE	OUT NOCOPY VARCHAR2,
				   X_RETURN_STATUS OUT NOCOPY VARCHAR2,
				   X_MSG_COUNT OUT NOCOPY NUMBER,
				   X_MSG_DATA OUT NOCOPY VARCHAR2);                                           

--This function would return the name for the user who has locked the step.

-- Start of comments
-- API name             : GET_STEP_LOCKED_BY  
-- Type                 : Private Utility.
-- Function             : return the name for the user who has locked the step.
-- Pre-reqs             : None.
-- Parameters           : 
-- IN                   : p_batchstep_id in number 
-- OUT                  : user display name 
-- End of comments

                                           

function GET_STEP_LOCKED_BY (P_BATCHSTEP_ID IN NUMBER) RETURN VARCHAR2;


--This procdeure would lock the step and do an autonomous commit.

-- Start of comments
-- API name             : lock_step
-- Type                 : Private Utility.
-- Function             : lock the step for the user
-- Pre-reqs             : None.
-- Parameters           : 
-- IN                   : p_batchstep_id in number
--                        p_requestor in number
-- OUT                  : x_return_status out varchar2
--                        x_msg_count out number
--                        x_msg_data out varchar2
-- End of comments


procedure LOCK_STEP	 (P_BATCHSTEP_ID IN NUMBER,
                          P_REQUESTER IN NUMBER,
                          X_RETURN_STATUS OUT NOCOPY VARCHAR2,
                          X_MSG_COUNT OUT NOCOPY NUMBER,
                          X_MSG_DATA OUT NOCOPY VARCHAR2);


--This procdeure would lock the (array) step and do an autonomous commit.

-- Start of comments
-- API name             : lock_step
-- Type                 : Private Utility.
-- Function             : lock the step for the user
-- Pre-reqs             : None.
-- Parameters           : 
-- IN                   : p_batchstep_id in fnd_table_of_varchar2_255
--                        p_requestor in number
-- OUT                  : x_return_status out varchar2
--                        x_msg_count out number
--                        x_msg_data out varchar2
-- End of comments


procedure LOCK_STEP	 (P_BATCHSTEP_ID IN FND_TABLE_OF_VARCHAR2_255,
                          P_REQUESTER IN NUMBER,
                          X_RETURN_STATUS OUT NOCOPY VARCHAR2,
                          X_MSG_COUNT OUT NOCOPY NUMBER,
                          X_MSG_DATA OUT NOCOPY VARCHAR2);



--This procdeure would unlock the step and do an autonomous commit.

-- Start of comments
-- API name             : unlock_step
-- Type                 : Private Utility.
-- Function             : unlock the step for the user
-- Pre-reqs             : None.
-- Parameters           : 
-- IN                   : p_batchstep_id in number
--                        p_requestor in number
-- OUT                  : x_return_status out varchar2
--                        x_msg_count out number
--                        x_msg_data out varchar2
-- End of comments

procedure UNLOCK_STEP	 (P_BATCHSTEP_ID IN NUMBER,
                          P_REQUESTER IN NUMBER,
                          X_RETURN_STATUS OUT NOCOPY VARCHAR2,
                          X_MSG_COUNT OUT NOCOPY NUMBER,
                          X_MSG_DATA OUT NOCOPY VARCHAR2);


--This procdeure would unlock the (array) step and do an autonomous commit..

-- Start of comments
-- API name             : unlock_step
-- Type                 : Private Utility.
-- Function             : unlock the step for the user
-- Pre-reqs             : None.
-- Parameters           : 
-- IN                   : p_batchstep_id in fnd_table_of_varchar2_255
--                        p_requestor in number
-- OUT                  : x_return_status out varchar2
--                        x_msg_count out number
--                        x_msg_data out varchar2
-- End of comments

procedure UNLOCK_STEP	 (P_BATCHSTEP_ID IN FND_TABLE_OF_VARCHAR2_255,
                          P_REQUESTER IN NUMBER,
                          X_RETURN_STATUS OUT NOCOPY VARCHAR2,
                          X_MSG_COUNT OUT NOCOPY NUMBER,
                          X_MSG_DATA OUT NOCOPY VARCHAR2);
                          

--This procdeure returns the batch id for the entity..

-- Start of comments
-- API name             : get_batch_id_for_entity
-- Type                 : Private Utility.
-- Function             : returns the batch id
-- Pre-reqs             : None.
-- Parameters           : 
-- IN                   : p_entity_name in varchar2
--                        p_entity_key in varchar2
-- OUT                  : x_batch_id out number
--                        x_return_status out varchar2
--                        x_msg_count out number
--                        x_msg_data out varchar2
-- End of comments

procedure GET_BATCH_ID_FOR_ENTITY (P_ENTITY_NAME IN VARCHAR2,
				   P_ENTITY_KEY IN VARCHAR2,
				   X_BATCH_ID	OUT NOCOPY NUMBER,
				   X_RETURN_STATUS OUT NOCOPY VARCHAR2,
				   X_MSG_COUNT OUT NOCOPY NUMBER,
				   X_MSG_DATA OUT NOCOPY VARCHAR2);


--This procdeure returns the batchstep id for the entity..

-- Start of comments
-- API name             : get_batchstep_id_for_entity
-- Type                 : Private Utility.
-- Function             : returns the batchstep id
-- Pre-reqs             : None.
-- Parameters           : 
-- IN                   : p_entity_name in varchar2
--                        p_entity_key in varchar2
-- OUT                  : x_batchstep_id out number
--                        x_return_status out varchar2
--                        x_msg_count out number
--                        x_msg_data out varchar2
-- End of comments

procedure GET_BATCHSTEP_ID_FOR_ENTITY (P_ENTITY_NAME IN VARCHAR2,
				   P_ENTITY_KEY IN VARCHAR2,
				   X_BATCHSTEP_ID OUT NOCOPY NUMBER,
				   X_RETURN_STATUS OUT NOCOPY VARCHAR2,
				   X_MSG_COUNT OUT NOCOPY NUMBER,
				   X_MSG_DATA OUT NOCOPY VARCHAR2);



--This procdeure would instantiate the process instructions for the batch.

-- Start of comments
-- API name             : instantiate_advanced_pi
-- Type                 : Group Utility.
-- Function             : Instantiates the process instructions 
-- Pre-reqs             : None.
-- Parameters           : 
-- IN                   : p_entity_name in varchar2
--                        p_entity_key in varchar2
-- OUT                  : x_return_status out varchar2
--                        x_msg_count out number
--                        x_msg_data out varchar2
-- End of comments

procedure INSTANTIATE_ADVANCED_PI (P_ENTITY_NAME IN VARCHAR2,
                                   P_ENTITY_KEY IN VARCHAR2,
 				   X_RETURN_STATUS OUT NOCOPY VARCHAR2,
				   X_MSG_COUNT OUT NOCOPY NUMBER,
				   X_MSG_DATA OUT NOCOPY VARCHAR2
);

-- enh 13724630, biachen. When adding an oprn to a batch, create batchstep PI instance from oprn PI defn
procedure INSTANTIATE_PI_FROM_OPRN (P_ENTITY_NAME IN VARCHAR2,
                                    P_ENTITY_KEY IN VARCHAR2,
 				   X_RETURN_STATUS OUT NOCOPY VARCHAR2,
				   X_MSG_COUNT OUT NOCOPY NUMBER,
				   X_MSG_DATA OUT NOCOPY VARCHAR2
);

--This procdeure would get the context information for the task.

-- Start of comments
-- API name             : on_task_load
-- Type                 : Group Utility.
-- Function             : gets the context information for the task
-- Pre-reqs             : None.
-- Parameters           : 
-- IN                   : p_from_module in varchar2 
--                        p_entity_name in varchar2
--                        p_entity_key in varchar2
--                        p_task in varchar2
--                        P_TASK_ATTRIBUTE in varchar2 default null
--                        p_instruction_id in number default null
-- OUT                  : x_entity_name out varchar2 
--                        x_entity_key out varchar2
--                        x_task out varchar2
--                        x_task_key out varchar2
--                        x_read_only out VARCHAR2 
--                        x_context_params_tbl out GMO_DATATYPES_GRP.CONTEXT_PARAMS_TBL_TYPE
--                        x_return_status out varchar2
--                        x_msg_count out number
--                        x_msg_data out varchar2
-- End of comments

procedure ON_TASK_LOAD (P_FROM_MODULE IN VARCHAR2,
                        P_ENTITY_NAME IN VARCHAR2,
                        P_ENTITY_KEY IN VARCHAR2,
                        P_TASK IN VARCHAR2,
                        P_TASK_ATTRIBUTE IN VARCHAR2,
                        P_INSTRUCTION_ID IN NUMBER,
                        P_INSTRUCTION_PROCESS_ID IN NUMBER,
                        P_REQUESTER IN NUMBER,
                        P_VBATCH_MODE IN VARCHAR2,
                        X_TASK_ENTITY_NAME OUT NOCOPY VARCHAR2,
                        X_TASK_ENTITY_KEY OUT NOCOPY VARCHAR2,
                        X_TASK_NAME OUT NOCOPY VARCHAR2,
                        X_TASK_KEY OUT NOCOPY VARCHAR2,
                        X_READ_ONLY OUT NOCOPY VARCHAR2,
                        X_CONTEXT_PARAMS_TBL OUT NOCOPY GMO_DATATYPES_GRP.CONTEXT_PARAMS_TBL_TYPE,
                        X_RETURN_STATUS OUT NOCOPY VARCHAR2,
                        X_MSG_COUNT OUT NOCOPY NUMBER,
                        X_MSG_DATA OUT NOCOPY VARCHAR2
);


--This procdeure would process the action performed by the task.

-- Start of comments
-- API name             : on_task_action
-- Type                 : Group Utility.
-- Function             : process the action performed by the task
-- Pre-reqs             : None.
-- Parameters           : 
-- IN                   : p_entity_name in varchar2
--                        p_entity_key in varchar2
--                        p_task in varchar2
--                        P_TASK_ATTRIBUTE in varchar2 default null
-- OUT                  : x_return_status out varchar2
--                        x_msg_count out number
--                        x_msg_data out varchar2
-- End of comments

procedure ON_TASK_ACTION (P_ENTITY_NAME IN VARCHAR2,
                          P_ENTITY_KEY IN VARCHAR2,
                          P_TASK IN VARCHAR2,
                          P_TASK_ATTRIBUTE IN VARCHAR2,
                          P_REQUESTER IN NUMBER,
                          X_RETURN_STATUS OUT NOCOPY VARCHAR2,
                          X_MSG_COUNT OUT NOCOPY NUMBER,
                          X_MSG_DATA OUT NOCOPY VARCHAR2
                          
);


--This procdeure would process the save event of the task.

-- Start of comments
-- API name             : on_task_save
-- Type                 : Group Utility.
-- Function             : process the save event of the task.
-- Pre-reqs             : None.
-- Parameters           : 
-- IN                   : p_from_module in varchar2 
--                        p_entity_name in varchar2
--                        p_entity_key in varchar2
--                        p_task in varchar2
--                        P_TASK_ATTRIBUTE in varchar2 default null
--                        p_instruction_id in number 
--                        p_task_identifier in fnd_table_of_varchar2 
--                        p_task_value in fnd_table_of_varchar2 
--                        p_task_erecord in fnd_table_of_varchar2
-- OUT                  : x_return_status out varchar2
--                        x_msg_count out number
--                        x_msg_data out varchar2
-- End of comments
                                              

procedure ON_TASK_SAVE (P_FROM_MODULE IN VARCHAR2,
                        P_ENTITY_NAME IN VARCHAR2,
                        P_ENTITY_KEY IN VARCHAR2,
                        P_TASK IN VARCHAR2,
                        P_TASK_ATTRIBUTE IN VARCHAR2,
                        P_INSTRUCTION_ID IN NUMBER ,
                        P_INSTRUCTION_PROCESS_ID IN NUMBER ,
                        P_TASK_IDENTIFIER IN FND_TABLE_OF_VARCHAR2_255,
                        P_TASK_VALUE IN FND_TABLE_OF_VARCHAR2_255,
                        P_TASK_ERECORD IN FND_TABLE_OF_VARCHAR2_255,
                        P_REQUESTER IN NUMBER,
                        X_RETURN_STATUS OUT NOCOPY VARCHAR2,
                        X_MSG_COUNT OUT NOCOPY NUMBER,
                        X_MSG_DATA OUT NOCOPY VARCHAR2
);


--This procdeure would check if the entity is locked or not

-- Start of comments
-- API name             : get_entity_lock_status
-- Type                 : Group Utility.
-- Function             : checks if the entity is locked or not
-- Pre-reqs             : None.
-- Parameters           : 
-- IN                   : p_entity_name in varchar2
--                        p_entity_key in varchar2
--                        p_requester in varchar2
-- OUT                  : x_lock_status out char
--			  x_locked_by_status out char
--			  x_lock_allowed out char
--                        x_return_status out varchar2
--                        x_msg_count out number
--                        x_msg_data out varchar2
-- End of comments

procedure GET_ENTITY_LOCK_STATUS (P_ENTITY_NAME IN VARCHAR2,
				  P_ENTITY_KEY IN VARCHAR2,
				  P_REQUESTER IN NUMBER,
				  X_LOCK_STATUS OUT NOCOPY  VARCHAR2, 
				  X_LOCKED_BY_STATUS OUT NOCOPY VARCHAR2,
				  X_LOCK_ALLOWED OUT NOCOPY  VARCHAR2,
				  X_RETURN_STATUS OUT NOCOPY VARCHAR2,
				  X_MSG_COUNT OUT NOCOPY NUMBER,
                        	  X_MSG_DATA OUT NOCOPY VARCHAR2
);
--Bug 5224634: start
--This procedure returns the batch step nodes
-- Start of comments
-- API name             : get_batchstep_nodes
-- Type                 : Group Utility
-- Function             : Get batch step nodes
-- Pre-reqs             : None
-- IN                   : P_BATCH_ID in number
--                        P_BATCHSTEP_ID in number
-- OUT                  : x_name out fnd_table_of_varchar2_255
--                        x_entity_name out fnd_table_of_varchar2_255
--                        x_entity_key out fnd_table_of_varchar2_255
--                        x_entity_level out fnd_table_of_varchar2_255
--                        x_return_status out varchar2
--                        x_msg_count out number
--                        x_msg_data out varchar2
-- End of comments

procedure GET_BATCHSTEP_NODES ( P_BATCH_ID IN NUMBER,
				P_BATCHSTEP_ID IN NUMBER,
				X_NAME OUT NOCOPY FND_TABLE_OF_VARCHAR2_255,
				X_ENTITY_NAME OUT NOCOPY FND_TABLE_OF_VARCHAR2_255,
				X_ENTITY_KEY OUT NOCOPY FND_TABLE_OF_VARCHAR2_255,
				X_ENTITY_LEVEL OUT NOCOPY FND_TABLE_OF_VARCHAR2_255,
				X_RETURN_STATUS OUT NOCOPY VARCHAR2,
				X_MSG_COUNT OUT NOCOPY NUMBER,
                        	X_MSG_DATA OUT NOCOPY VARCHAR2);
--Bug 5224634: end

-- Kapil ME GMO-LCF
procedure INSTANTIATE_INSTR_FOR_LCF (P_ENTITY_NAME IN VARCHAR2,
                                    P_ENTITY_KEY IN VARCHAR2,
 				    X_RETURN_STATUS OUT NOCOPY VARCHAR2,
				    X_MSG_COUNT OUT NOCOPY NUMBER,
				    X_MSG_DATA OUT NOCOPY VARCHAR2);

END GMO_VBATCH_PVT;
/
commit;
EXIT;
