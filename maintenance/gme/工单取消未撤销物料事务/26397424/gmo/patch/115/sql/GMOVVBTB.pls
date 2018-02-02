REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.24.12010000.6=120.27.12020000.3)(120.21.12000000.6=120.27):~PROD:~PATH:~FILE

/*=======================================================================#
 #  Copyright (c) 2001, 2014 Oracle Corporation Redwood Shores, California, USA#
 #                           All rights reserved.                        #
 #=======================================================================+
 # FILENAME
 #   GMOGVBTB.pls - GMO Visual Batch
 # DESCRIPTION
 #   PL/SQL body for package:  GMO_VBATCH_PVT
 # NOTES
 #   Private API 
 # HISTORY
 # 03-Jan-2006 rahugupt Modified for bug 4893056 to add handling for new 
 #                      task MATERIAL_TRANSCTIONS.
 # 13-Jan-2005 rahugupt Modified for bug 4609365 to show dispense qty in owb 
 # 19-Jan-2005 rahugupt Modified for bug 4893056 to include more parameters
 #                      INVOKE_MODE, LINE_NO, LINE_TYPE in out parameter
 #                      of on_task_load api.
 # 3-Feb-2006  rahugupt Modified for bug 5016043 to use RESERVATIONS instead of 
 #                      vbatch action RESERVATION. You will not see any bug
 #                      start or end, as its a very minor fix.
 # 8-Feb-2006  rahugupt Modified for bug 5023801.Setting the correct value for
 #                      X_READ_ONLY for VIEW_MATERIAL task.
 # 8-Mar-2006  rahugupt Modified for bug 5059386, to support all attribute.
 # 6-Jun-2006  rahugupt Modified for bug 5224634, to get all step nodes.
 # 8-Jun-2006  rahugupt Modified for bug 5293576, touse correct variable
 #                      for calling is_step_locked function. Minor fix,
 #                      all over in on_task_load api, so use file diff.
 #                      No bug tag comments provided.
 # 15-Jun-2006 rahugupt Modified for bug 520396. Added procedure update_task
 #                      _attribute, modified procedure instantiate_advanced_pi
 #                      to call update_Task_attribute. Modified on_task_load
 #                      to use direct GME id instead of NPD ids. Changes are all
 #                      over in these apis, so no bug tagging. Use file
 #                      difference.
 # 26-Feb-2007 kmotupal Bug# 5105424 : Added the procedure INSTANTIATE_INSTR_FOR_LCF for 
 #                      GMO-LCF Batch Integration. 
 #=======================================================================*/

SET VERIFY OFF
SET DEFINE OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK
WHENEVER OSERROR EXIT FAILURE ROLLBACK

CREATE OR REPLACE PACKAGE BODY GMO_VBATCH_PVT AS
/* $Header: GMOVVBTB.pls 120.27.12020000.3 2014/04/30 03:05:21 biachen ship $ */

GMO_INVALID_ENTITY_ERR exception;
GMO_DISABLED_ERR EXCEPTION;
GMO_NOT_ENHANCED_PI_ERR EXCEPTION;
GMO_INVALID_RECIPE_ERR EXCEPTION;

function is_batch_enhanced_pi (P_ENTITY_NAME IN VARCHAR2, P_ENTITY_KEY IN VARCHAR2) RETURN BOOLEAN
IS

l_batch_id number;
NO_BATCH_FOUND_ERR exception;
cursor is_enhanced_pi is select count(*) from gme_batch_header where enhanced_pi_ind = 'Y' and batch_id = l_batch_id;
l_count number;
l_return_status varchar2(100);
l_msg_count number;
l_msg_data varchar2(4000);

BEGIN
	
	get_batch_id_for_entity
	(
		p_entity_name => p_entity_name,
		p_entity_key => p_entity_key,
		x_batch_id => l_batch_id,
		x_return_status => l_return_status,
		x_msg_count => l_msg_count,
		x_msg_data => l_msg_data
	);
	if L_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS THEN
		RAISE NO_BATCH_FOUND_ERR;
	end if;

        open is_enhanced_pi;
	fetch is_enhanced_pi into l_count;
	close is_enhanced_pi;

	IF (l_count = 0) THEN
		return false;
	ELSE
		return true;
	END IF;
END is_batch_enhanced_pi;

--This function would verify if the step is locked or not.

function IS_STEP_LOCKED (P_BATCHSTEP_ID IN NUMBER,
			 P_REQUESTER IN NUMBER DEFAULT NULL) RETURN VARCHAR2 
IS

l_count number;
l_locked varchar2(1);
cursor is_step_locked_by_user is select count(*) from gmo_batch_step_lock_details where batchstep_id = p_batchstep_id and lock_requester = p_requester;
cursor is_step_locked is select count(*) from gmo_batch_step_lock_details where batchstep_id = p_batchstep_id;

BEGIN
	
	IF (P_REQUESTER is null) THEN
		open is_step_locked;
		fetch is_step_locked into l_count;
		close is_step_locked;
	ELSE
		open is_step_locked_by_user;
		fetch is_step_locked_by_user into l_count;
		close is_step_locked_by_user;
	END IF;		
	
	IF (l_count = 0) THEN
		l_locked := GMO_CONSTANTS_GRP.NO;
	ELSE
		l_locked := GMO_CONSTANTS_GRP.YES;
	END IF;
	
	return l_locked;

END IS_STEP_LOCKED;

function IS_STEP_LOCKED (P_VBATCH_MODE IN VARCHAR2,
			 P_BATCHSTEP_ID IN NUMBER,
			 P_REQUESTER IN NUMBER) RETURN VARCHAR2 
IS
l_locked varchar2(1);

BEGIN
	IF (P_VBATCH_MODE = GMO_CONSTANTS_GRP.VBATCH_ADMIN_MODE) then
		l_locked := IS_STEP_LOCKED (P_BATCHSTEP_ID => P_BATCHSTEP_ID);
	else
		l_locked := IS_STEP_LOCKED (P_BATCHSTEP_ID => P_BATCHSTEP_ID, P_REQUESTER => P_REQUESTER);
	end if;
	return l_locked;
			 
END IS_STEP_LOCKED;

--This function would return the reserved quantity for the material.

function GET_MATERIAL_RESERVATION_QTY  (P_ORGANIZATION_ID IN NUMBER,
					 P_BATCH_ID IN NUMBER,
					 P_MATERIAL_DETAIL_ID IN NUMBER) RETURN NUMBER
IS

MATERIAL_RESERVATION_ERR EXCEPTION;

l_return_status varchar2(10);
l_reserved_quantity number;
l_reservation_tbl GME_COMMON_PVT.reservations_tab;
l_msg_count number;
l_msg_data varchar2(4000);
l_detail_uom varchar2(3);
l_reserved_quantity_t number;

BEGIN
	l_reserved_quantity := 0;
	l_reserved_quantity_t := 0;

	GME_API_GRP.GET_MATERIAL_RESERVATIONS (
		X_MSG_COUNT => l_msg_count,
                X_MSG_DATA => l_msg_data,
		P_ORGANIZATION_ID => P_ORGANIZATION_ID,
		P_BATCH_ID => P_BATCH_ID,
		P_MATERIAL_DETAIL_ID => P_MATERIAL_DETAIL_ID,
		X_RETURN_STATUS => l_return_status,
		X_RESERVATIONS_TBL => l_reservation_tbl
	);
	
	if L_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS THEN
		RAISE MATERIAL_RESERVATION_ERR;
	end if;

	--Bug 4609365: start
	select dtl_um into l_detail_uom from gme_material_details where material_detail_id = P_MATERIAL_DETAIL_ID;
	--Bug 4609365: end
	
	for i in 1 .. l_reservation_tbl.count loop
		--Bug 4609365: start
		l_reserved_quantity_t := 0;
		if ( l_detail_uom is not null and l_detail_uom <> l_reservation_tbl(i).primary_uom_code) then
			/* 	It is safe to assume that conversion will not fail because this is an internal 
 				procedure and will only be called where conversion do exist and l_detail_uom is not null.
 				Therefore no need to check the failure (-99999 return value) 
			*/
			l_reserved_quantity_t := inv_convert.inv_um_convert
						(
						ITEM_ID => l_reservation_tbl(i).inventory_item_id,
                                         	LOT_NUMBER => l_reservation_tbl(i).lot_number,
                                         	ORGANIZATION_ID => l_reservation_tbl(i).organization_id,
                                         	PRECISION => 5,
                                         	FROM_QUANTITY => l_reservation_tbl(i).PRIMARY_RESERVATION_QUANTITY,
                                         	FROM_UNIT => l_reservation_tbl(i).primary_uom_code,
                                         	TO_UNIT => l_detail_uom,
                                         	FROM_NAME => null,
                                         	TO_NAME => null
						);
		else
			l_reserved_quantity_t := l_reservation_tbl(i).PRIMARY_RESERVATION_QUANTITY;
		end if;
		--l_reserved_quantity := l_reserved_quantity + l_reservation_tbl(i).PRIMARY_RESERVATION_QUANTITY;
		l_reserved_quantity := l_reserved_quantity + l_reserved_quantity_t;
		--Bug 4609365: end
	end loop;

	if (FND_LOG.LEVEL_EVENT >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then

		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_MAT_RSRV_QTY_MSG');
		FND_MESSAGE.SET_TOKEN('ORGANIZATION_ID',P_ORGANIZATION_ID);
		FND_MESSAGE.SET_TOKEN('BATCH_ID',P_BATCH_ID);
		FND_MESSAGE.SET_TOKEN('MATERIAL_DETAIL_ID',P_MATERIAL_DETAIL_ID);
		FND_MESSAGE.SET_TOKEN('RESERVED_QUANTITY',l_reserved_quantity);
		
		FND_LOG.MESSAGE(FND_LOG.LEVEL_EVENT,'gmo.plsql.gmo_vbatch_pvt.get_material_reservation_qty', FALSE);
	end if;
	
	return l_reserved_quantity;

EXCEPTION
	WHEN MATERIAL_RESERVATION_ERR THEN

			FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_MAT_RSRV_QTY_ERR');
			FND_MESSAGE.SET_TOKEN('ORGANIZATION_ID',P_ORGANIZATION_ID);
			FND_MESSAGE.SET_TOKEN('BATCH_ID',P_BATCH_ID);
			FND_MESSAGE.SET_TOKEN('MATERIAL_DETAIL_ID',P_MATERIAL_DETAIL_ID);
			FND_MESSAGE.SET_TOKEN('RESERVED_QUANTITY',l_reserved_quantity);
			FND_MESSAGE.SET_TOKEN('GME_RETURN_STATUS', L_RETURN_STATUS);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.get_material_reservation_qty', FALSE);
		end if;
                APP_EXCEPTION.RAISE_EXCEPTION;
	WHEN OTHERS THEN
			FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_UNEXPECTED_DB_ERR');
			FND_MESSAGE.SET_TOKEN('ERROR_TEXT',SQLERRM);
			FND_MESSAGE.SET_TOKEN('ERROR_CODE',SQLCODE);
		if (FND_LOG.LEVEL_UNEXPECTED >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then      
			FND_LOG.MESSAGE(FND_LOG.LEVEL_UNEXPECTED,'gmo.plsql.gmo_vbatch_pvt.get_material_reservation_qty', FALSE);
		end if;
		APP_EXCEPTION.RAISE_EXCEPTION; 
END GET_MATERIAL_RESERVATION_QTY;

--Bug 4609365: start
--This function is a wrapper function over GMO_DISPENSE_PVT.GET_NET_MTL_DISPENSED_QTY
FUNCTION GET_MATERIAL_DISPENSED_QTY (P_MATERIAL_DETAIL_ID NUMBER, P_UOM VARCHAR2) RETURN NUMBER
IS

BEGIN

	return GMO_DISPENSE_PVT.GET_NET_MTL_DISPENSED_QTY (P_MATERIAL_DETAIL_ID => P_MATERIAL_DETAIL_ID, P_UOM => P_UOM);

END GET_MATERIAL_DISPENSED_QTY;
--Bug 4609365: end
--Bug : start
--This function is return actual quantity amidst of transactions
--function get_actual_quantity(P_MATERIAL_DETAIL_ID NUMBER, P_INVENTORY_ITEM_ID NUMBER) RETURN NUMBER;
--

function get_actual_quantity(p_material_detail_id NUMBER, p_inventory_item_id NUMBER) RETURN NUMBER
IS

    l_actual_quantity NUMBER;
    l_transaction_quantity NUMBER :=0;
    cv_transaction_qty NUMBER :=0;
    l_line_type NUMBER;
    l_total_actual_quantity NUMBER;

    CURSOR c_transaction_qty(cp_inventory_item_id NUMBER,cp_material_detail_id NUMBER) IS
    SELECT transaction_quantity
    FROM mtl_material_transactions_temp
    WHERE inventory_item_id=cp_inventory_item_id
    AND trx_source_line_id= cp_material_detail_id
    AND transaction_source_type_id=5 ;


BEGIN

      SELECT actual_qty ,line_type
      INTO l_actual_quantity,l_line_type
      FROM gme_material_details
      WHERE material_detail_id=P_MATERIAL_DETAIL_ID
      AND inventory_item_id= P_INVENTORY_ITEM_ID;

          OPEN c_transaction_qty(P_INVENTORY_ITEM_ID,P_MATERIAL_DETAIL_ID);
          LOOP
            FETCH c_transaction_qty INTO cv_transaction_qty;
            EXIT WHEN c_transaction_qty%NOTFOUND;
                IF(l_line_type=-1) THEN
                cv_transaction_qty := cv_transaction_qty*(-1); -- Ingredients
                END IF;
            l_transaction_quantity := l_transaction_quantity+cv_transaction_qty;
          END LOOP;
          CLOSE c_transaction_qty;
          l_total_actual_quantity := l_actual_quantity+l_transaction_quantity;

    RETURN  l_total_actual_quantity;

END get_actual_quantity;

--This procedure would verify if batch step material is available.

PROCEDURE GET_STEP_MATERIAL_AVAILABILITY (P_BATCHSTEP_ID IN NUMBER,
				   X_MATERIAL_AVAILABLE	OUT NOCOPY VARCHAR2,
				   X_RETURN_STATUS OUT NOCOPY VARCHAR2,
				   X_MSG_COUNT OUT NOCOPY NUMBER,
				   X_MSG_DATA OUT NOCOPY VARCHAR2)
IS
l_unavail_mat fnd_table_of_varchar2_255;
BEGIN
	GET_STEP_MATERIAL_AVAILABILITY (
		P_BATCHSTEP_ID => P_BATCHSTEP_ID,
		X_MATERIAL_AVAILABLE => X_MATERIAL_AVAILABLE,
		X_UNAVAL_MATERIAL_ID => l_unavail_mat,
		X_RETURN_STATUS => X_RETURN_STATUS,
		X_MSG_COUNT => X_MSG_COUNT,
		X_MSG_DATA => X_MSG_DATA
	); 

END;

PROCEDURE GET_STEP_MATERIAL_AVAILABILITY (P_BATCHSTEP_ID IN NUMBER,
				   X_MATERIAL_AVAILABLE	OUT NOCOPY VARCHAR2,
				   X_UNAVAL_MATERIAL_ID OUT NOCOPY FND_TABLE_OF_VARCHAR2_255,
				   X_RETURN_STATUS OUT NOCOPY VARCHAR2,
				   X_MSG_COUNT OUT NOCOPY NUMBER,
				   X_MSG_DATA OUT NOCOPY VARCHAR2)

IS

l_organization_id number;
l_batch_id number;
l_return_status varchar2(10);
l_exception_tbl gme_common_pvt.exceptions_tab;
l_material_detail_id number;
l_count number;
l_material_unavailable number;
l_material_available varchar2(1);
BATCH_SHORTAGE_ERR exception;
l_msg_count number;
l_msg_data varchar2(4000);
k number;

cursor c_get_batch_details is select a.batch_id, a.organization_id from gme_batch_header a, gme_batch_steps b where a.batch_id = b.batch_id and b.batchstep_id = p_batchstep_id;
cursor c_is_material_reqd_for_step is select count(*) from gme_batch_step_items where material_detail_id = l_material_detail_id and batchstep_id = p_batchstep_id;
BEGIN

	open c_get_batch_details;
	fetch c_get_batch_details into l_batch_id, l_organization_id;
	close c_get_batch_details;
	
	l_count  := 0;
	l_material_unavailable := 0;


	GME_API_GRP.GET_BATCH_SHORTAGES(
		X_MSG_COUNT => l_msg_count,
                X_MSG_DATA => l_msg_data,
		P_ORGANIZATION_ID => l_organization_id,
		P_BATCH_ID => l_batch_id,
		P_INVOKE_MODE => 'O',
		P_TREE_MODE => 2,
		X_RETURN_STATUS => l_return_status,
		X_EXCEPTION_TBL => l_exception_tbl
	);
	
	if (l_return_status <> FND_API.G_RET_STS_SUCCESS) THEN
		RAISE BATCH_SHORTAGE_ERR;
	end if;
	
 	X_UNAVAL_MATERIAL_ID := new FND_TABLE_OF_VARCHAR2_255();	
	k := 0;

	for i in 1 .. l_exception_tbl.count loop
		l_material_detail_id := l_exception_tbl(i).MATERIAL_DETAIL_ID;
		open c_is_material_reqd_for_step;
		fetch c_is_material_reqd_for_step into l_count;
		close c_is_material_reqd_for_step;		
		
		if (l_count > 0) then
			X_UNAVAL_MATERIAL_ID.extend;
			k := k+1;
			X_UNAVAL_MATERIAL_ID(k) := l_material_detail_id;
			l_material_unavailable := l_material_unavailable + 1;
		end if;
	end loop;
	
	if (l_material_unavailable > 0) then
		l_material_available := GMO_CONSTANTS_GRP.NO;
	else
		l_material_available := GMO_CONSTANTS_GRP.YES;
	end if;

	if (FND_LOG.LEVEL_EVENT >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then

		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_STEP_MAT_AVBL_MSG');
		FND_MESSAGE.SET_TOKEN('BATCHSTEP_ID',P_BATCHSTEP_ID);
		FND_MESSAGE.SET_TOKEN('MATERIAL_AVAILABLE',l_material_available);
		FND_LOG.MESSAGE(FND_LOG.LEVEL_EVENT,'gmo.plsql.gmo_vbatch_pvt.batchstep_material_available', FALSE);
	end if;
	
	X_MATERIAL_AVAILABLE := l_material_available;

	X_RETURN_STATUS := FND_API.G_RET_STS_SUCCESS;
EXCEPTION
	WHEN BATCH_SHORTAGE_ERR THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_ERROR;

		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_STEP_MAT_AVBL_ERR');
		FND_MESSAGE.SET_TOKEN('BATCHSTEP_ID',P_BATCHSTEP_ID);
		FND_MESSAGE.SET_TOKEN('MATERIAL_AVAILABLE',l_material_available);
		FND_MESSAGE.SET_TOKEN('GME_RETURN_STATUS', L_RETURN_STATUS);
		FND_MSG_PUB.ADD;
		FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);

		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.batchstep_material_available', FALSE);
		end if;

	WHEN OTHERS THEN

		X_RETURN_STATUS := FND_API.G_RET_STS_UNEXP_ERROR;
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_UNEXPECTED_DB_ERR');
		FND_MESSAGE.SET_TOKEN('ERROR_TEXT',SQLERRM);
		FND_MESSAGE.SET_TOKEN('ERROR_CODE',SQLCODE);
		FND_MSG_PUB.ADD;
		if (FND_LOG.LEVEL_UNEXPECTED >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then      
			FND_LOG.MESSAGE(FND_LOG.LEVEL_UNEXPECTED,'gmo.plsql.gmo_vbatch_pvt.batchstep_material_available', FALSE);
		end if;
END GET_STEP_MATERIAL_AVAILABILITY;



--This function would return the name for the user who has locked the step.

function GET_STEP_LOCKED_BY (P_BATCHSTEP_ID IN NUMBER) RETURN VARCHAR2

IS

cursor c_get_step_lock_by is select lock_requester from gmo_batch_step_lock_details where batchstep_id = p_batchstep_id;
l_user_id number;
l_user_display_name varchar2(1000);
begin

	open c_get_step_lock_by;
	fetch c_get_step_lock_by into l_user_id;
	close c_get_step_lock_by;
	
	return gmo_utilities.get_user_display_name (p_user_id => l_user_id);

END GET_STEP_LOCKED_BY;


--This procdeure would lock the step.
procedure LOCK_STEP	 (P_BATCHSTEP_ID IN NUMBER,
                          P_REQUESTER IN NUMBER,
                          X_RETURN_STATUS OUT NOCOPY VARCHAR2,
                          X_MSG_COUNT OUT NOCOPY NUMBER,
                          X_MSG_DATA OUT NOCOPY VARCHAR2)

IS PRAGMA AUTONOMOUS_TRANSACTION;

l_batch_id number;
cursor c_get_batch_id is select batch_id from gme_batch_steps where batchstep_id = P_BATCHSTEP_ID;
STEP_ALREADY_LOCKED_ERR exception;
NO_BATCH_FOUND_ERR exception;

BEGIN
	IF (IS_STEP_LOCKED (P_BATCHSTEP_ID => P_BATCHSTEP_ID, 	P_REQUESTER => P_REQUESTER) = 'Y') THEN
		RAISE STEP_ALREADY_LOCKED_ERR;
	END IF;
	
	open c_get_batch_id;
	fetch c_get_batch_id into l_batch_id;
	close c_get_batch_id;

	IF (l_batch_id is null) THEN
		RAISE NO_BATCH_FOUND_ERR;
	END IF;

	
	INSERT into GMO_BATCH_STEP_LOCK_DETAILS (BATCH_ID, BATCHSTEP_ID, LOCK_REQUESTER, LOCK_DATE, CREATED_BY, CREATION_DATE, LAST_UPDATED_BY, LAST_UPDATE_DATE, LAST_UPDATE_LOGIN)
	VALUES (l_batch_id, P_BATCHSTEP_ID, P_REQUESTER, sysdate, FND_GLOBAL.USER_ID, sysdate, FND_GLOBAL.USER_ID, sysdate, FND_GLOBAL.LOGIN_ID);
	
	commit;
  
	if (FND_LOG.LEVEL_EVENT >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_LOCKSTEP_REQ_MSG');
		FND_MESSAGE.SET_TOKEN('BATCHSTEP_ID',P_BATCHSTEP_ID);
		FND_MESSAGE.SET_TOKEN('REQUESTER',P_REQUESTER);
		FND_MESSAGE.SET_TOKEN('REQUEST','LOCK');
		FND_LOG.MESSAGE(FND_LOG.LEVEL_EVENT,'gmo.plsql.gmo_vbatch_pvt.lock_step', FALSE);
	end if;
	
	X_RETURN_STATUS := FND_API.G_RET_STS_SUCCESS;
	
EXCEPTION	
	WHEN STEP_ALREADY_LOCKED_ERR THEN
		rollback;
		X_RETURN_STATUS := FND_API.G_RET_STS_ERROR;
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_LOCK_ERR');
		FND_MSG_PUB.ADD;
		FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.lock_step', FALSE);
		end if;
	WHEN NO_BATCH_FOUND_ERR THEN
		rollback;
		X_RETURN_STATUS := FND_API.G_RET_STS_ERROR;
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_INVALID_ENTITY');
		FND_MESSAGE.SET_TOKEN('ENTITY_NAME','OPERATION');
		FND_MESSAGE.SET_TOKEN('ENTITY_KEY',P_BATCHSTEP_ID);
		FND_MSG_PUB.ADD;
		FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_UNEXPECTED >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_UNEXPECTED,'gmo.plsql.gmo_vbatch_pvt.lock_step', FALSE);
		end if;
	WHEN OTHERS THEN
		rollback;
		X_RETURN_STATUS := FND_API.G_RET_STS_UNEXP_ERROR;
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_UNEXPECTED_DB_ERR');
		FND_MESSAGE.SET_TOKEN('ERROR_TEXT',SQLERRM);
		FND_MESSAGE.SET_TOKEN('ERROR_CODE', SQLCODE);
		FND_MSG_PUB.ADD;
		FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_UNEXPECTED >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_UNEXPECTED,'gmo.plsql.gmo_vbatch_pvt.lock_step', FALSE);
		end if;
END LOCK_STEP;

--This procdeure would lock the (array) step and do an autonomous commit..

procedure LOCK_STEP	 (P_BATCHSTEP_ID IN FND_TABLE_OF_VARCHAR2_255,
                          P_REQUESTER IN NUMBER,
                          X_RETURN_STATUS OUT NOCOPY VARCHAR2,
                          X_MSG_COUNT OUT NOCOPY NUMBER,
                          X_MSG_DATA OUT NOCOPY VARCHAR2)

IS PRAGMA AUTONOMOUS_TRANSACTION;

l_batch_id number;
l_batchstep_id number;
cursor c_get_batch_id is select batch_id from gme_batch_steps where batchstep_id = l_batchstep_id;
STEP_ALREADY_LOCKED_ERR exception;
NO_BATCH_FOUND_ERR exception;

BEGIN
	
	for i in 1 .. P_BATCHSTEP_ID.count loop
		
		l_batchstep_id := to_number (P_BATCHSTEP_ID (i));

		IF (IS_STEP_LOCKED (P_BATCHSTEP_ID => l_batchstep_id, 	P_REQUESTER => P_REQUESTER) = 'Y') THEN
			RAISE STEP_ALREADY_LOCKED_ERR;
		END IF;

		open c_get_batch_id;
		fetch c_get_batch_id into l_batch_id;
		close c_get_batch_id;

		IF (l_batch_id is null) THEN
			RAISE NO_BATCH_FOUND_ERR;
		END IF;


		INSERT into GMO_BATCH_STEP_LOCK_DETAILS (BATCH_ID, BATCHSTEP_ID, LOCK_REQUESTER, LOCK_DATE, CREATED_BY, CREATION_DATE, LAST_UPDATED_BY, LAST_UPDATE_DATE, LAST_UPDATE_LOGIN)
		VALUES (l_batch_id, l_batchstep_id, P_REQUESTER, sysdate, FND_GLOBAL.USER_ID, sysdate, FND_GLOBAL.USER_ID, sysdate, FND_GLOBAL.LOGIN_ID);

		if (FND_LOG.LEVEL_EVENT >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
			FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_LOCKSTEP_REQ_MSG');
			FND_MESSAGE.SET_TOKEN('BATCHSTEP_ID',l_batchstep_id);
			FND_MESSAGE.SET_TOKEN('REQUESTER',P_REQUESTER);
			FND_MESSAGE.SET_TOKEN('REQUEST','LOCK');
			FND_LOG.MESSAGE(FND_LOG.LEVEL_EVENT,'gmo.plsql.gmo_vbatch_pvt.lock_step', FALSE);
		end if;


	end loop;

	commit;
	
	X_RETURN_STATUS := FND_API.G_RET_STS_SUCCESS;
	
EXCEPTION	
	WHEN STEP_ALREADY_LOCKED_ERR THEN
		rollback;
		X_RETURN_STATUS := FND_API.G_RET_STS_ERROR;
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_LOCK_ERR');
		FND_MSG_PUB.ADD;
		FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.lock_step', FALSE);
		end if;
	WHEN NO_BATCH_FOUND_ERR THEN
		rollback;
		X_RETURN_STATUS := FND_API.G_RET_STS_ERROR;
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_INVALID_ENTITY');
		FND_MESSAGE.SET_TOKEN('ENTITY_NAME','OPERATION');
		FND_MESSAGE.SET_TOKEN('ENTITY_KEY', l_batchstep_id);
		FND_MSG_PUB.ADD;
		FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_UNEXPECTED >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_UNEXPECTED,'gmo.plsql.gmo_vbatch_pvt.lock_step', FALSE);
		end if;
	WHEN OTHERS THEN
		rollback;
		X_RETURN_STATUS := FND_API.G_RET_STS_UNEXP_ERROR;
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_UNEXPECTED_DB_ERR');
		FND_MESSAGE.SET_TOKEN('ERROR_TEXT',SQLERRM);
		FND_MESSAGE.SET_TOKEN('ERROR_CODE', SQLCODE);
		FND_MSG_PUB.ADD;
		FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_UNEXPECTED >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_UNEXPECTED,'gmo.plsql.gmo_vbatch_pvt.lock_step', FALSE);
		end if;
END LOCK_STEP;

--This procdeure would unlock the step.

procedure UNLOCK_STEP	 (P_BATCHSTEP_ID IN NUMBER,
                          P_REQUESTER IN NUMBER,
                          X_RETURN_STATUS OUT NOCOPY VARCHAR2,
                          X_MSG_COUNT OUT NOCOPY NUMBER,
                          X_MSG_DATA OUT NOCOPY VARCHAR2)

IS PRAGMA AUTONOMOUS_TRANSACTION;

l_batch_id number;
l_lock_requester number;
l_lock_date date;
l_locked varchar2(1);
cursor c_get_lock_details is select batch_id, lock_requester,lock_date from gmo_batch_step_lock_details where batchstep_id = P_BATCHSTEP_ID;


STEP_NOT_LOCKED_ERR exception;

BEGIN
	l_locked := IS_STEP_LOCKED (P_BATCHSTEP_ID => P_BATCHSTEP_ID);

	IF (l_locked = 'N') THEN
		RAISE STEP_NOT_LOCKED_ERR;
	END IF;

	open c_get_lock_details;
	fetch c_get_lock_details into l_batch_id, l_lock_requester, l_lock_date;
	close c_get_lock_details;

	
	INSERT into GMO_BATCH_STEP_LOCK_HIST (LOCK_HIST_SEQ, BATCH_ID, BATCHSTEP_ID, LOCK_REQUESTER, LOCK_DATE, UNLOCK_REQUESTER, UNLOCK_DATE, CREATED_BY, CREATION_DATE, LAST_UPDATED_BY, LAST_UPDATE_DATE, LAST_UPDATE_LOGIN)
	VALUES (GMO_BATCH_STEP_LOCK_HIST_S.NEXTVAL, l_batch_id, P_BATCHSTEP_ID, l_lock_requester, l_lock_date, P_REQUESTER, sysdate, FND_GLOBAL.USER_ID, sysdate, FND_GLOBAL.USER_ID, sysdate, FND_GLOBAL.LOGIN_ID);
	
	DELETE FROM GMO_BATCH_STEP_LOCK_DETAILS WHERE BATCHSTEP_ID = P_BATCHSTEP_ID;
	
	commit;

	if (FND_LOG.LEVEL_EVENT >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_UNLOCKSTEP_REQ_MSG');
		FND_MESSAGE.SET_TOKEN('BATCHSTEP_ID',P_BATCHSTEP_ID);
		FND_MESSAGE.SET_TOKEN('REQUESTER',P_REQUESTER);
		FND_MESSAGE.SET_TOKEN('REQUEST','UNLOCK');
		FND_LOG.MESSAGE(FND_LOG.LEVEL_EVENT,'gmo.plsql.gmo_vbatch_pvt.unlock_step', FALSE);
	end if;

	
	X_RETURN_STATUS := FND_API.G_RET_STS_SUCCESS;
	
EXCEPTION	
	WHEN STEP_NOT_LOCKED_ERR THEN
		rollback;
		X_RETURN_STATUS := FND_API.G_RET_STS_ERROR;
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_UNLOCK_ERR');
		FND_MSG_PUB.ADD;
		FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.unlock_step', FALSE);
		end if;
	WHEN OTHERS THEN
		rollback;
		X_RETURN_STATUS := FND_API.G_RET_STS_UNEXP_ERROR;
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_UNEXPECTED_DB_ERR');
		FND_MESSAGE.SET_TOKEN('ERROR_TEXT',SQLERRM);
		FND_MESSAGE.SET_TOKEN('ERROR_CODE', SQLCODE);
		FND_MSG_PUB.ADD;
		FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_UNEXPECTED >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_UNEXPECTED,'gmo.plsql.gmo_vbatch_pvt.unlock_step', FALSE);
		end if;
END UNLOCK_STEP;

--This procdeure would unlock the step.

procedure UNLOCK_STEP	 (P_BATCHSTEP_ID IN FND_TABLE_OF_VARCHAR2_255,
                          P_REQUESTER IN NUMBER,
                          X_RETURN_STATUS OUT NOCOPY VARCHAR2,
                          X_MSG_COUNT OUT NOCOPY NUMBER,
                          X_MSG_DATA OUT NOCOPY VARCHAR2)

IS PRAGMA AUTONOMOUS_TRANSACTION;

l_batch_id number;
l_batchstep_id number;
l_lock_requester number;
l_lock_date date;
l_locked varchar2(1);
cursor c_get_lock_details is select batch_id, lock_requester,lock_date from gmo_batch_step_lock_details where batchstep_id = l_batchstep_id;


STEP_NOT_LOCKED_ERR exception;

BEGIN
	
	for i in 1 .. P_BATCHSTEP_ID.count loop
	
		l_batchstep_id := to_number (p_batchstep_id (i));
		
		l_locked := IS_STEP_LOCKED (P_BATCHSTEP_ID => l_batchstep_id);
	
		IF (l_locked = 'N') THEN
			RAISE STEP_NOT_LOCKED_ERR;
		END IF;

		open c_get_lock_details;
		fetch c_get_lock_details into l_batch_id, l_lock_requester, l_lock_date;
		close c_get_lock_details;


		INSERT into GMO_BATCH_STEP_LOCK_HIST (LOCK_HIST_SEQ, BATCH_ID, BATCHSTEP_ID, LOCK_REQUESTER, LOCK_DATE, UNLOCK_REQUESTER, UNLOCK_DATE, CREATED_BY, CREATION_DATE, LAST_UPDATED_BY, LAST_UPDATE_DATE, LAST_UPDATE_LOGIN)
		VALUES (GMO_BATCH_STEP_LOCK_HIST_S.NEXTVAL, l_batch_id, l_batchstep_id, l_lock_requester, l_lock_date, P_REQUESTER, sysdate, FND_GLOBAL.USER_ID, sysdate, FND_GLOBAL.USER_ID, sysdate, FND_GLOBAL.LOGIN_ID);

		DELETE FROM GMO_BATCH_STEP_LOCK_DETAILS WHERE BATCHSTEP_ID = l_batchstep_id;

		if (FND_LOG.LEVEL_EVENT >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
			FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_STEP_REQ_MSG');
			FND_MESSAGE.SET_TOKEN('BATCHSTEP_ID',l_batchstep_id);
			FND_MESSAGE.SET_TOKEN('REQUESTER',P_REQUESTER);
			FND_MESSAGE.SET_TOKEN('REQUEST','UNLOCK');
			FND_LOG.MESSAGE(FND_LOG.LEVEL_EVENT,'gmo.plsql.gmo_vbatch_pvt.unlock_step', FALSE);
		end if;

		
	end loop;
	
	commit;
	
	X_RETURN_STATUS := FND_API.G_RET_STS_SUCCESS;
	
EXCEPTION	
	WHEN STEP_NOT_LOCKED_ERR THEN
		rollback;
		X_RETURN_STATUS := FND_API.G_RET_STS_ERROR;
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_UNLOCK_ERR');
		FND_MSG_PUB.ADD;
		FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.unlock_step', FALSE);
		end if;
	WHEN OTHERS THEN
		rollback;
		X_RETURN_STATUS := FND_API.G_RET_STS_UNEXP_ERROR;
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_UNEXPECTED_DB_ERR');
		FND_MESSAGE.SET_TOKEN('ERROR_TEXT',SQLERRM);
		FND_MESSAGE.SET_TOKEN('ERROR_CODE', SQLCODE);
		FND_MSG_PUB.ADD;
		FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_UNEXPECTED >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_UNEXPECTED,'gmo.plsql.gmo_vbatch_pvt.unlock_step', FALSE);
		end if;
END UNLOCK_STEP;

-- this procedure returns the batch id for the entity

procedure GET_BATCH_ID_FOR_ENTITY (P_ENTITY_NAME IN VARCHAR2,
				   P_ENTITY_KEY IN VARCHAR2,
				   X_BATCH_ID	OUT NOCOPY NUMBER,
				   X_RETURN_STATUS OUT NOCOPY VARCHAR2,
				   X_MSG_COUNT OUT NOCOPY NUMBER,
				   X_MSG_DATA OUT NOCOPY VARCHAR2)

IS

l_batch_id number;

cursor get_batch_id_for_step is select batch_id from gme_batch_steps where batchstep_id = p_entity_key;
cursor get_batch_id_for_resource is select batch_id from gme_batch_step_resources where batchstep_resource_id = p_entity_key;
cursor get_batch_id_for_activity is select batch_id from gme_batch_step_activities where batchstep_activity_id = p_entity_key;
cursor get_batch_id_for_material is select batch_id from gme_material_details where material_detail_id = p_entity_key;

NO_BATCH_FOUND_ERR exception;

BEGIN

	IF (P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_BATCH) THEN
		l_batch_id := to_number (P_ENTITY_KEY);
	ELSIF (P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_OPERATION) THEN
		open get_batch_id_for_step;
		fetch get_batch_id_for_step into l_batch_id;
		close get_batch_id_for_step;
	ELSIF (P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_ACTIVITY) THEN
		open get_batch_id_for_activity;
		fetch get_batch_id_for_activity into l_batch_id;
		close get_batch_id_for_activity;
	ELSIF (P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_RESOURCE) THEN
		open get_batch_id_for_resource;
		fetch get_batch_id_for_resource into l_batch_id;
		close get_batch_id_for_resource;
	ELSIF (P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_MATERIAL) THEN
		open get_batch_id_for_material;
		fetch get_batch_id_for_material into l_batch_id;
		close get_batch_id_for_material;
	ELSE
		RAISE gmo_invalid_entity_err;
	END IF;
	
	IF (l_batch_id is null) THEN
		RAISE NO_BATCH_FOUND_ERR;
	END IF;

	if (FND_LOG.LEVEL_EVENT >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_ENTITY_BATCH_MSG');
		FND_MESSAGE.SET_TOKEN('ENTITY_NAME',P_ENTITY_NAME);
		FND_MESSAGE.SET_TOKEN('ENTITY_KEY',P_ENTITY_KEY);
		FND_MESSAGE.SET_TOKEN('BATCH_ID',l_batch_id);
		FND_LOG.MESSAGE(FND_LOG.LEVEL_EVENT,'gmo.plsql.gmo_vbatch_pvt.get_batch_id_for_entity', FALSE);
	end if;

	X_RETURN_STATUS := FND_API.G_RET_STS_SUCCESS;
	X_BATCH_ID := l_batch_id;

EXCEPTION
	WHEN NO_BATCH_FOUND_ERR THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_ERROR;
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_INVALID_ENTITY_OPRN');
		FND_MESSAGE.SET_TOKEN('ENTITY_NAME',P_ENTITY_NAME);
		FND_MESSAGE.SET_TOKEN('ENTITY_KEY',P_ENTITY_KEY);
		FND_MESSAGE.SET_TOKEN('OPRN','GET_BATCH_ID');
		FND_MSG_PUB.ADD;
		FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.get_batch_id_for_entity', FALSE);
		end if;
	WHEN GMO_INVALID_ENTITY_ERR THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_ERROR;
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_INVALID_ENTITY');
                FND_MESSAGE.SET_TOKEN('ENTITY_NAME',P_ENTITY_NAME);
                FND_MESSAGE.SET_TOKEN('ENTITY_KEY',P_ENTITY_KEY);
                FND_MSG_PUB.ADD;
                FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.get_batch_id_for_entity', FALSE);
		end if;
	WHEN OTHERS THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_UNEXP_ERROR;
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_UNEXPECTED_DB_ERR');
		FND_MESSAGE.SET_TOKEN('ERROR_TEXT',SQLERRM);
		FND_MESSAGE.SET_TOKEN('ERROR_CODE', SQLCODE);
		FND_MSG_PUB.ADD;
		FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_UNEXPECTED >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_UNEXPECTED,'gmo.plsql.gmo_vbatch_pvt.get_batch_id_for_entity', FALSE);
		end if;
END GET_BATCH_ID_FOR_ENTITY;

procedure GET_BATCHSTEP_ID_FOR_ENTITY (P_ENTITY_NAME IN VARCHAR2,
				   P_ENTITY_KEY IN VARCHAR2,
				   X_BATCHSTEP_ID OUT NOCOPY NUMBER,
				   X_RETURN_STATUS OUT NOCOPY VARCHAR2,
				   X_MSG_COUNT OUT NOCOPY NUMBER,
				   X_MSG_DATA OUT NOCOPY VARCHAR2)

IS

l_batchstep_id number;
l_batch_operation varchar2(1000);

cursor get_batchstep_id_for_resource is select batchstep_id from gme_batch_step_resources where batchstep_resource_id = p_entity_key;
cursor get_batchstep_id_for_activity is select batchstep_id from gme_batch_step_activities where batchstep_activity_id = p_entity_key;
cursor get_batchstep_id_for_material is select batchstep_id from gme_batch_step_items where material_detail_id = p_entity_key;

NO_BATCHSTEP_FOUND_ERR exception;

BEGIN

	IF (P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_OPERATION) THEN
		l_batchstep_id := to_number (P_ENTITY_KEY);
	ELSIF (P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_ACTIVITY) THEN
		open get_batchstep_id_for_activity;
		fetch get_batchstep_id_for_activity into l_batchstep_id;
		close get_batchstep_id_for_activity;
	ELSIF (P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_RESOURCE) THEN
		open get_batchstep_id_for_resource;
		fetch get_batchstep_id_for_resource into l_batchstep_id;
		close get_batchstep_id_for_resource;
	ELSIF (P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_MATERIAL) THEN
		open get_batchstep_id_for_material;
		fetch get_batchstep_id_for_material into l_batchstep_id;
		close get_batchstep_id_for_material;
	ELSE
		RAISE gmo_invalid_entity_err;
	END IF;
	
	IF (l_batchstep_id is null) THEN
		RAISE NO_BATCHSTEP_FOUND_ERR;
	END IF;

	if (FND_LOG.LEVEL_EVENT >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_ENTITY_BATCH_MSG');
		FND_MESSAGE.SET_TOKEN('ENTITY_NAME',P_ENTITY_NAME);
		FND_MESSAGE.SET_TOKEN('ENTITY_KEY',P_ENTITY_KEY);
		FND_MESSAGE.SET_TOKEN('BATCHSTEP_ID',l_batchstep_id);
		FND_LOG.MESSAGE(FND_LOG.LEVEL_EVENT,'gmo.plsql.gmo_vbatch_pvt.get_batchstep_id_for_entity', FALSE);
	end if;
	

	X_RETURN_STATUS := FND_API.G_RET_STS_SUCCESS;
	X_BATCHSTEP_ID := l_batchstep_id;

EXCEPTION
	WHEN NO_BATCHSTEP_FOUND_ERR THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_ERROR;
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_INVALID_ENTITY_OPRN');
		FND_MESSAGE.SET_TOKEN('ENTITY_NAME',P_ENTITY_NAME);
		FND_MESSAGE.SET_TOKEN('ENTITY_KEY',P_ENTITY_KEY);
		FND_MESSAGE.SET_TOKEN('OPRN','GET_BATCHSTEP_ID');
		FND_MSG_PUB.ADD;
		FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.get_batchstep_id_for_entity', FALSE);
		end if;
	WHEN GMO_INVALID_ENTITY_ERR THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_ERROR;
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_INVALID_ENTITY');
                FND_MESSAGE.SET_TOKEN('ENTITY_NAME',P_ENTITY_NAME);
                FND_MESSAGE.SET_TOKEN('ENTITY_KEY',P_ENTITY_KEY);
                FND_MSG_PUB.ADD;
                FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.get_batchstep_id_for_entity', FALSE);
		end if;
	WHEN OTHERS THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_UNEXP_ERROR;
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_UNEXPECTED_DB_ERR');
		FND_MESSAGE.SET_TOKEN('ERROR_TEXT',SQLERRM);
		FND_MESSAGE.SET_TOKEN('ERROR_CODE', SQLCODE);
		FND_MSG_PUB.ADD;
		FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_UNEXPECTED >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_UNEXPECTED,'gmo.plsql.gmo_vbatch_pvt.get_batchstep_id_for_entity', FALSE);
		end if;
END GET_BATCHSTEP_ID_FOR_ENTITY;

-- this function is internally used by the apis to 
-- check if gmo is enabled or not.
function is_gmo_enabled return boolean is
begin
	if (GMO_SETUP_GRP.IS_GMO_ENABLED = GMO_CONSTANTS_GRP.YES) then
		return true;
	else
		return false;
	end if;
end;

procedure update_task_attribute 
(
P_ENTITY_NAME IN VARCHAR2,
P_ENTITY_KEY IN VARCHAR2,
P_INSTRUCTION_SET_ID IN NUMBER,
X_ATTRIBUTE_STATUS OUT NOCOPY VARCHAR2,
X_INVALID_INSTR OUT NOCOPY FND_TABLE_OF_VARCHAR2_255 
)

IS 

l_batch_id number;
l_batchstep_id number;
l_batchstep_activity_id number;
l_batchstep_resource_id number;
l_material_detail_id number;
l_formula_line_id number;
l_oprn_line_id number;
l_oprn_id number;
l_resources varchar2(100);
l_process_param_id number;
l_param_id number;
l_param_id_vchar varchar2(4000);
l_inventory_item_id number;

l_instruction_id number;
l_task_attribute varchar2(4000);
l_task_attribute_id_inst varchar2(4000);
l_instr_number varchar2(200);

l_min_assoc varchar2(1);

l_invalid_instr fnd_table_of_varchar2_255;
j binary_integer;

l_return_status varchar2(1);
l_msg_count number;
l_msg_data varchar2(4000);
l_task varchar2(500);
l_task_id number;

cursor get_activity is select batchstep_activity_id from gme_batch_step_activities where batchstep_id = l_batchstep_id and oprn_line_id = l_oprn_line_id;
cursor get_min_activity is select min(batchstep_activity_id) from gme_batch_step_activities  where batchstep_id = l_batchstep_id;

cursor get_resource is select batchstep_resource_id from gme_batch_step_resources where batchstep_activity_id = l_batchstep_activity_id and resources = l_resources;
cursor get_min_resource is select min(batchstep_resource_id) from gme_batch_step_resources where batchstep_activity_id = l_batchstep_activity_id;

cursor get_param is select process_param_id from gme_process_parameters where batchstep_resource_id = l_batchstep_resource_id and parameter_id = l_param_id;
cursor get_min_param is select (process_param_id) from gme_process_parameters where batchstep_resource_id = l_batchstep_resource_id;

cursor get_material is select material_detail_id from gme_material_details where formulaline_id = l_formula_line_id and batch_id = l_batch_id;
cursor get_min_material_for_step is select min(material_detail_id) from gme_batch_step_items where batchstep_id = l_batchstep_id;

cursor get_instr_details is select instr_number, instruction_id, task_id, task_attribute_id from gmo_instr_instance_b where instruction_set_id = P_INSTRUCTION_SET_ID;

cursor get_task is select task_name from gmo_instr_task_defn_b where task_id = l_task_id;

BEGIN
	
	l_invalid_instr := fnd_table_of_varchar2_255();
	j := 0;

	open get_instr_details;
	loop
	fetch get_instr_details into l_instr_number, l_instruction_id, l_task_id, l_task_attribute;
	exit when get_instr_details%NOTFOUND;

		l_min_assoc := 'N';
		l_task_attribute_id_inst := '';
		l_process_param_id := null;
		l_material_detail_id := null;
		l_batchstep_activity_id := null;
		l_batchstep_id := null;
		l_batchstep_resource_id := null;
		l_inventory_item_id := null;
		l_batch_id := null;

		l_task := '';
		open get_task;
		fetch get_task into l_task;
		close get_task;


		IF (L_TASK = 'UPDATE_ACTIVITY' or L_TASK='VIEW_ACTIVITY') then
			IF (P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_OPERATION) then
				l_batchstep_id := to_number(p_entity_key);
				if (l_task_attribute is not null and l_task_attribute <> gmo_constants_grp.all_attribute) then
					l_oprn_line_id := to_number (l_task_attribute);
					open get_activity;
					fetch get_activity into l_batchstep_activity_id;
					close get_activity;
				else
					open get_min_activity;
					fetch get_min_activity into l_batchstep_activity_id;
					close get_min_activity;

					l_min_assoc := 'Y';
				end if;
				l_task_attribute_id_inst := l_batchstep_activity_id;
			end if;

		ELSIF (L_TASK = 'UPDATE_RESOURCE' or L_TASK='VIEW_RESOURCE' or L_TASK =  GMO_CONSTANTS_GRP.TASK_RESOURCE_TRANSACTION) then

			IF (P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_OPERATION or P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_ACTIVITY ) THEN
		                -- pattern = OprnLineId$Resources
				l_oprn_line_id := to_number (substr(l_task_attribute, 1, instr (l_task_attribute, '$') - 1));
				l_resources := substr(l_task_attribute, instr (l_task_attribute, '$') + 1);

				IF (P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_OPERATION) then
					l_batchstep_id := to_number(p_entity_key);
					open get_activity;
					fetch get_activity into l_batchstep_activity_id;
         	                        close get_activity;
				ELSE
					l_batchstep_activity_id := to_number(p_entity_key);
				END IF;

		                if (l_resources <> gmo_constants_grp.all_attribute) then
					open get_resource;
					fetch get_resource into l_batchstep_resource_id;
					close get_resource;

				else
					open get_min_resource;
					fetch get_min_resource into l_batchstep_resource_id;
					close get_min_resource;

					l_min_assoc := 'Y';
				end if;
				l_task_attribute_id_inst := l_batchstep_resource_id;

			END IF;

		ELSIF (L_TASK = GMO_CONSTANTS_GRP.TASK_PROCESS_PARAMETER) then

			IF (P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_OPERATION or P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_ACTIVITY 
				or P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_RESOURCE) THEN
				-- pattern = OprnLineId$Resources$ParameterId
				l_oprn_line_id := to_number (substr(l_task_attribute, 1, instr (l_task_attribute, '$') - 1));
				l_resources := substr(l_task_attribute, instr (l_task_attribute, '$') + 1, (instr (l_task_attribute, '$', 1,2 )-1) - instr (l_task_attribute, '$'));
				l_param_id_vchar := substr(l_task_attribute, instr (l_task_attribute, '$', 1,2)	+ 1);

			 	IF (P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_OPERATION) then
                                        l_batchstep_id := to_number(p_entity_key);
                                        open get_activity;
                                        fetch get_activity into l_batchstep_activity_id;
                                        close get_activity;

					open get_resource;
					fetch get_resource into l_batchstep_resource_id;
					close get_resource;
                                ELSIF (P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_ACTIVITY) THEN
					l_batchstep_activity_id := to_number(p_entity_key);	

					open get_resource;
					fetch get_resource into l_batchstep_resource_id;
					close get_resource;
				ELSE
					l_batchstep_resource_id := to_number(p_entity_key);
				END IF;

				if (l_param_id_vchar <> gmo_constants_grp.all_attribute) then
					l_param_id := to_number(l_param_id_vchar);
					open get_param;
					fetch get_param into l_process_param_id;
					close get_param;
				else
					open get_min_param;
					fetch get_min_param into l_process_param_id;
					close get_min_param;

					l_min_assoc := 'Y';
				end if;
				l_task_attribute_id_inst := l_process_param_id;

			END IF;

		ELSIF (L_TASK = 'MATERIAL' OR L_TASK = 'RESERVATIONS' OR L_TASK = 'VIEW_MATERIAL' OR L_TASK = 'UPDATE_MATERIAL' OR L_TASK='MATERIAL_TRANSACTIONS' OR L_TASK = 'QUALITY') THEN

			IF (P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_OPERATION or P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_ACTIVITY 
				or P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_RESOURCE) THEN

				GET_BATCH_ID_FOR_ENTITY(P_ENTITY_NAME => P_ENTITY_NAME,
				   			P_ENTITY_KEY => P_ENTITY_KEY,
				   			X_BATCH_ID => l_batch_id,
				   			X_RETURN_STATUS => l_return_status,
							X_MSG_COUNT => l_msg_count,
							X_MSG_DATA => l_msg_data);

				GET_BATCHSTEP_ID_FOR_ENTITY(P_ENTITY_NAME => P_ENTITY_NAME,
				   			P_ENTITY_KEY => P_ENTITY_KEY,
				   			X_BATCHSTEP_ID => l_batchstep_id,
				   			X_RETURN_STATUS => l_return_status,
							X_MSG_COUNT => l_msg_count,
							X_MSG_DATA => l_msg_data);
				
				if (l_task_attribute is not null and l_task_attribute <> gmo_constants_grp.all_attribute) then
					l_formula_line_id := to_number(l_task_attribute);
					open get_material;
					fetch get_material into l_material_detail_id;
					close get_material;

				else
					open get_min_material_for_step;
					fetch get_min_material_for_step into l_material_detail_id;
					close get_min_material_for_step;

					l_min_assoc := 'Y';
				end if;

				l_task_attribute_id_inst := l_material_detail_id;
			END IF;
		end if;

		if (l_task_attribute is not null) then
			if (l_task_attribute_id_inst is not null) then
				update gmo_instr_instance_b set task_attribute_id = l_task_attribute_id_inst
				where instruction_set_id = P_INSTRUCTION_SET_ID
				and instruction_id = l_instruction_id;
			elsif (l_min_assoc = 'N') then
				j := j + 1;
				l_invalid_instr.extend;
				l_invalid_instr(j) := l_instr_number;
			end if;
		end if;
	end loop;
	close get_instr_details;
	
	if (j = 0) then 
		X_ATTRIBUTE_STATUS := 'S';
	else
		X_ATTRIBUTE_STATUS := 'E';
	end if;
	X_INVALID_INSTR := l_invalid_instr;

END update_task_attribute;

--This procdeure would instantiate the process instructions for the batch.
procedure INSTANTIATE_ADVANCED_PI (P_ENTITY_NAME IN VARCHAR2,
                                   P_ENTITY_KEY IN VARCHAR2,
 				   X_RETURN_STATUS OUT NOCOPY VARCHAR2,
				   X_MSG_COUNT OUT NOCOPY NUMBER,
				   X_MSG_DATA OUT NOCOPY VARCHAR2)

IS

l_recipe_id number;
l_batchstep_id number;
l_routingstep_id number;
l_oprn_id number;
l_oprn_line_id number;
l_resources varchar2(16);
l_material_detail_id number;
l_formulaline_id number;
l_inventory_item_id number;
l_batchstep_activity_id number;
l_batchstep_resource_id number;
l_instruction_set_id number;
l_return_status varchar2(100);
l_msg_count number;
l_msg_data varchar2(4000);
instruction_create_err exception;

l_entity_display_name varchar2(255);
l_invalid_instr fnd_table_of_varchar2_255;
l_invalid_instr_master fnd_table_of_varchar2_255;
l_invalid_entity_master fnd_table_of_varchar2_255;

j binary_integer;
k binary_integer;
l_attribute_Status varchar2(1); 
INVALID_ATTRIBUTE_ASSOC_ERR exception;

cursor c_get_recipe is 
  select grv.recipe_id 
  from   gme_batch_header gbh, 
         gmd_recipe_validity_rules grv 
  where gbh.recipe_validity_rule_id = grv.recipe_validity_rule_id and gbh.batch_id = p_entity_key;

cursor c_get_steps is 
  select 
    batchstep_id, 
    routingstep_id, 
    a.oprn_id,
    batchstep_no || '-' || b.oprn_no 
  from gme_batch_steps a, gmd_operations_b b where  a.oprn_id = b.oprn_id and batch_id = p_entity_key;

cursor c_get_activities is 
  select batchstep_activity_id, 
        routingstep_id, 
        oprn_line_id ,
	activity
  from  gme_batch_step_activities gbsa, 
        gme_batch_steps gbs 
  where gbsa.batchstep_id = gbs.batchstep_id and gbsa.batch_id = p_entity_key;

cursor c_get_resources is 
  select batchstep_resource_id, 
         routingstep_id, 
         oprn_line_id, 
         resources ,
         gbsa.activity || '-' || resources 
  from   gme_batch_step_resources gbsr, 
         gme_batch_step_Activities gbsa, 
         gme_batch_steps gbs 
  where  gbsa.batchstep_activity_id = gbsr.batchstep_activity_id 
  and    gbsa.batchstep_id = gbs.batchstep_id and gbsr.batch_id = p_entity_key;

cursor c_get_materials is 
  select a.material_detail_id, a.formulaline_id, a.inventory_item_id, b.concatenated_segments from gme_material_details a, mtl_system_items_kfv b
  where a.organization_id = b.organization_id
  and a.inventory_item_id = b.inventory_item_id 
  and a.batch_id = p_entity_key;

BEGIN

	IF (NOT IS_GMO_ENABLED) THEN
		RAISE GMO_DISABLED_ERR;
	END IF;

	IF (NOT IS_BATCH_ENHANCED_PI (P_ENTITY_NAME => P_ENTITY_NAME, P_ENTITY_KEY => P_ENTITY_KEY) ) THEN
		RAISE GMO_NOT_ENHANCED_PI_ERR;
	END IF;
	
	IF (P_ENTITY_NAME <> GMO_CONSTANTS_GRP.ENTITY_BATCH OR p_entity_key is null) THEN
		RAISE GMO_INVALID_ENTITY_ERR;
        END IF;

  open c_get_recipe;
	fetch c_get_recipe into l_recipe_id;
	close c_get_recipe;
	if l_recipe_id is null then
        -- Kapil ME LCF-GMO
        -- For LCF Batches, instantiate PI through the follwoing Procedure.
	   INSTANTIATE_INSTR_FOR_LCF(P_ENTITY_NAME => P_ENTITY_NAME,
                                 P_ENTITY_KEY  => P_ENTITY_KEY,
                                 X_RETURN_STATUS => X_RETURN_STATUS,
                                 X_MSG_COUNT => X_MSG_COUNT,
                                 X_MSG_DATA => X_MSG_DATA); 
	else


	j := 0;
	k := 0;
	l_invalid_entity_master := fnd_table_of_varchar2_255();
	l_invalid_instr_master := fnd_table_of_varchar2_255();

	open c_get_steps;
	loop
	fetch c_get_steps into l_batchstep_id, l_routingstep_id, l_oprn_id, l_entity_display_name;
	exit when c_get_steps%NOTFOUND;

		gmo_instruction_grp.CREATE_INSTANCE_FROM_DEFN (
			P_API_VERSION => 1.0,
			X_RETURN_STATUS => l_return_status,
			X_MSG_COUNT => l_msg_count,
			X_MSG_DATA => l_msg_data,
			P_DEFINITION_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_OPERATION,
			P_DEFINITION_ENTITY_KEY => l_recipe_id || GMO_CONSTANTS_GRP.ENTITY_KEY_SEPARATOR || l_routingstep_id || GMO_CONSTANTS_GRP.ENTITY_KEY_SEPARATOR || l_oprn_id,
			P_INSTANCE_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_OPERATION,
			P_INSTANCE_ENTITY_KEY => l_batchstep_id,
			P_INSTRUCTION_TYPE => GMO_CONSTANTS_GRP.VBATCH_INSTRUCTION_TYPE, 
			X_INSTRUCTION_SET_ID => l_instruction_set_id
		);

	 	if l_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS THEN
			RAISE instruction_create_err;
		end if;
		update_task_attribute
		(
			P_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_OPERATION,
			P_ENTITY_KEY =>  l_batchstep_id,
			P_INSTRUCTION_SET_ID => l_instruction_set_id,
			X_ATTRIBUTE_STATUS => l_attribute_status,
			X_INVALID_INSTR => l_invalid_instr
		);
		if (l_attribute_status <> 'S' and l_invalid_instr.count > 0) then
			for i in 1 .. l_invalid_instr.count loop
				j := j + 1;
				l_invalid_instr_master.extend;
				l_invalid_instr_master (j) := l_invalid_instr(i);
			end loop;
			k := k+1;
			l_invalid_entity_master.extend;
			l_invalid_entity_master (k) := l_entity_display_name;
		end if;

		if (FND_LOG.LEVEL_EVENT >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
			FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_INSTR_CREATE_MSG');
			FND_MESSAGE.SET_TOKEN('ENTITY_NAME',P_ENTITY_NAME);
			FND_MESSAGE.SET_TOKEN('ENTITY_KEY',P_ENTITY_KEY);
			FND_MESSAGE.SET_TOKEN('INSTR_ENTITY_NAME','STEP');
			FND_MESSAGE.SET_TOKEN('INSTR_ENTITY_KEY',l_batchstep_id);
			FND_LOG.MESSAGE(FND_LOG.LEVEL_EVENT,'gmo.plsql.gmo_vbatch_pvt.instantiate_advanced_pi', FALSE);
		end if;
		
	end loop;
	close c_get_steps;

	open c_get_activities;
        loop
        fetch c_get_activities into l_batchstep_activity_id, l_routingstep_id, l_oprn_line_id, l_entity_display_name;
        exit when c_get_activities%NOTFOUND;

                gmo_instruction_grp.CREATE_INSTANCE_FROM_DEFN (
			P_API_VERSION => 1.0,
			X_RETURN_STATUS => l_return_status,
			X_MSG_COUNT => l_msg_count,
			X_MSG_DATA => l_msg_data,
                        P_DEFINITION_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_ACTIVITY,
                        P_DEFINITION_ENTITY_KEY => l_recipe_id || GMO_CONSTANTS_GRP.ENTITY_KEY_SEPARATOR || l_routingstep_id || GMO_CONSTANTS_GRP.ENTITY_KEY_SEPARATOR || l_oprn_line_id,
                        P_INSTANCE_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_ACTIVITY,
                        P_INSTANCE_ENTITY_KEY => l_batchstep_activity_id,
                        P_INSTRUCTION_TYPE => GMO_CONSTANTS_GRP.VBATCH_INSTRUCTION_TYPE,
                        X_INSTRUCTION_SET_ID => l_instruction_set_id
                );

                if l_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS THEN
                        RAISE instruction_create_err;
                end if;

		update_task_attribute
		(
			P_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_ACTIVITY,
			P_ENTITY_KEY =>  l_batchstep_activity_id,
			P_INSTRUCTION_SET_ID => l_instruction_set_id,
			X_ATTRIBUTE_STATUS => l_attribute_status,
			X_INVALID_INSTR => l_invalid_instr
		);
		if (l_attribute_status <> 'S' and l_invalid_instr.count > 0) then
			for i in 1 .. l_invalid_instr.count loop
				j := j + 1;
				l_invalid_instr_master.extend;
				l_invalid_instr_master (j) := l_invalid_instr(i);
			end loop;
			k := k+1;
			l_invalid_entity_master.extend;
			l_invalid_entity_master (k) := l_entity_display_name;
		end if;

		if (FND_LOG.LEVEL_EVENT >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
			FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_INSTR_CREATE_MSG');
			FND_MESSAGE.SET_TOKEN('ENTITY_NAME',P_ENTITY_NAME);
			FND_MESSAGE.SET_TOKEN('ENTITY_KEY',P_ENTITY_KEY);
			FND_MESSAGE.SET_TOKEN('INSTR_ENTITY_NAME','ACTIVITY');
			FND_MESSAGE.SET_TOKEN('INSTR_ENTITY_KEY',l_batchstep_activity_id);
			FND_LOG.MESSAGE(FND_LOG.LEVEL_EVENT,'gmo.plsql.gmo_vbatch_pvt.instantiate_advanced_pi', FALSE);
		end if;

        end loop;
        close c_get_activities;

	open c_get_resources;
        loop
        fetch c_get_resources into l_batchstep_resource_id, l_routingstep_id, l_oprn_line_id, l_resources, l_entity_display_name;
        exit when c_get_resources%NOTFOUND;

                gmo_instruction_grp.CREATE_INSTANCE_FROM_DEFN (
			P_API_VERSION => 1.0,
			X_RETURN_STATUS => l_return_status,
			X_MSG_COUNT => l_msg_count,
			X_MSG_DATA => l_msg_data,
                        P_DEFINITION_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_RESOURCE,
                        P_DEFINITION_ENTITY_KEY => l_recipe_id || GMO_CONSTANTS_GRP.ENTITY_KEY_SEPARATOR || l_routingstep_id || GMO_CONSTANTS_GRP.ENTITY_KEY_SEPARATOR || l_oprn_line_id || GMO_CONSTANTS_GRP.ENTITY_KEY_SEPARATOR || l_resources,
                        P_INSTANCE_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_RESOURCE,
                        P_INSTANCE_ENTITY_KEY => l_batchstep_resource_id,
                        P_INSTRUCTION_TYPE => GMO_CONSTANTS_GRP.VBATCH_INSTRUCTION_TYPE,
                        X_INSTRUCTION_SET_ID => l_instruction_set_id
                );

                if l_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS THEN
                        RAISE instruction_create_err;
                end if;

		update_task_attribute
		(
			P_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_RESOURCE,
			P_ENTITY_KEY =>  l_batchstep_resource_id,
			P_INSTRUCTION_SET_ID => l_instruction_set_id,
			X_ATTRIBUTE_STATUS => l_attribute_status,
			X_INVALID_INSTR => l_invalid_instr
		);
		if (l_attribute_status <> 'S' and l_invalid_instr.count > 0) then
			for i in 1 .. l_invalid_instr.count loop
				j := j + 1;
				l_invalid_instr_master.extend;
				l_invalid_instr_master (j) := l_invalid_instr(i);
			end loop;
			k := k+1;
			l_invalid_entity_master.extend;
			l_invalid_entity_master (k) := l_entity_display_name;
		end if;

		if (FND_LOG.LEVEL_EVENT >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
			FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_INSTR_CREATE_MSG');
			FND_MESSAGE.SET_TOKEN('ENTITY_NAME',P_ENTITY_NAME);
			FND_MESSAGE.SET_TOKEN('ENTITY_KEY',P_ENTITY_KEY);
			FND_MESSAGE.SET_TOKEN('INSTR_ENTITY_NAME','RESOURCE');
			FND_MESSAGE.SET_TOKEN('INSTR_ENTITY_KEY',l_batchstep_resource_id);
			FND_LOG.MESSAGE(FND_LOG.LEVEL_EVENT,'gmo.plsql.gmo_vbatch_pvt.instantiate_advanced_pi', FALSE);
		end if;

        end loop;
        close c_get_resources;

	open c_get_materials;
        loop
        fetch c_get_materials into l_material_detail_id, l_formulaline_id, l_inventory_item_id, l_entity_display_name;
        exit when c_get_materials%NOTFOUND;

                gmo_instruction_grp.CREATE_INSTANCE_FROM_DEFN (
			P_API_VERSION => 1.0,
			X_RETURN_STATUS => l_return_status,
			X_MSG_COUNT => l_msg_count,
			X_MSG_DATA => l_msg_data,
                        P_DEFINITION_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_MATERIAL,
                        P_DEFINITION_ENTITY_KEY => l_recipe_id || GMO_CONSTANTS_GRP.ENTITY_KEY_SEPARATOR || l_formulaline_id || GMO_CONSTANTS_GRP.ENTITY_KEY_SEPARATOR || l_inventory_item_id,
                        P_INSTANCE_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_MATERIAL,
                        P_INSTANCE_ENTITY_KEY => l_material_detail_id,
                        P_INSTRUCTION_TYPE => GMO_CONSTANTS_GRP.VBATCH_INSTRUCTION_TYPE,
                        X_INSTRUCTION_SET_ID => l_instruction_set_id
                );

                if l_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS THEN
                        RAISE instruction_create_err;
                end if;

		update_task_attribute
		(
			P_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_MATERIAL,
			P_ENTITY_KEY =>  l_material_detail_id,
			P_INSTRUCTION_SET_ID => l_instruction_set_id,
			X_ATTRIBUTE_STATUS => l_attribute_status,
			X_INVALID_INSTR => l_invalid_instr
		);
		if (l_attribute_status <> 'S' and l_invalid_instr.count > 0) then
			for i in 1 .. l_invalid_instr.count loop
				j := j + 1;
				l_invalid_instr_master.extend;
				l_invalid_instr_master (j) := l_invalid_instr(i);
			end loop;
			k := k+1;
			l_invalid_entity_master.extend;
			l_invalid_entity_master (k) := l_entity_display_name;
		end if;

		if (FND_LOG.LEVEL_EVENT >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
			FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_INSTR_CREATE_MSG');
			FND_MESSAGE.SET_TOKEN('ENTITY_NAME',P_ENTITY_NAME);
			FND_MESSAGE.SET_TOKEN('ENTITY_KEY',P_ENTITY_KEY);
			FND_MESSAGE.SET_TOKEN('INSTR_ENTITY_NAME','MATERIAL');
			FND_MESSAGE.SET_TOKEN('INSTR_ENTITY_KEY',l_material_detail_id);
			FND_LOG.MESSAGE(FND_LOG.LEVEL_EVENT,'gmo.plsql.gmo_vbatch_pvt.instantiate_advanced_pi', FALSE);
		end if;

        end loop;
        close c_get_materials;

	if (l_invalid_entity_master.count > 0)  then
		RAISE INVALID_ATTRIBUTE_ASSOC_ERR;
	end if;
	
	end if;

	x_return_status :=  FND_API.G_RET_STS_SUCCESS;

EXCEPTION
	WHEN instruction_create_err then
		x_return_status :=  FND_API.G_RET_STS_ERROR;
		FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.instantiate_advanced_pi', FALSE);
		end if;
	WHEN GMO_NOT_ENHANCED_PI_ERR THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_SUCCESS;
                FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_NOT_ENHANCED_PI_ERR');
                FND_MSG_PUB.ADD;
                FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.instantiate_advanced_pi', FALSE);
		end if;
	WHEN GMO_INVALID_RECIPE_ERR THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_SUCCESS;
                FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_INVALID_RECIPE');
                FND_MSG_PUB.ADD;
                FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.instantiate_advanced_pi', FALSE);
		end if;
	WHEN GMO_DISABLED_ERR THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_SUCCESS;
                FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_DISABLED_ERR');
                FND_MSG_PUB.ADD;
                FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.instantiate_advanced_pi', FALSE);
		end if;
	WHEN GMO_INVALID_ENTITY_ERR THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_ERROR;
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_INVALID_ENTITY');
                FND_MESSAGE.SET_TOKEN('ENTITY_NAME',P_ENTITY_NAME);
                FND_MESSAGE.SET_TOKEN('ENTITY_KEY',P_ENTITY_KEY);
                FND_MSG_PUB.ADD;
                FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.instantiate_advanced_pi', FALSE);
		end if;
	WHEN INVALID_ATTRIBUTE_ASSOC_ERR THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_ERROR;
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_INVALID_ATTR_ASSOC');
                FND_MSG_PUB.ADD;
                FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.instantiate_advanced_pi', FALSE);
		end if;
	WHEN OTHERS THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_UNEXP_ERROR;
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_UNEXPECTED_DB_ERR');
		FND_MESSAGE.SET_TOKEN('ERROR_TEXT',SQLERRM);
		FND_MESSAGE.SET_TOKEN('ERROR_CODE', SQLCODE);
		FND_MSG_PUB.ADD;
		FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_UNEXPECTED >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_UNEXPECTED,'gmo.plsql.gmo_vbatch_pvt.instantiate_advanced_pi', FALSE);
		end if;

END INSTANTIATE_ADVANCED_PI;


-- enh 13724630, biachen. When adding an oprn to a batch, create batchstep PI instance from oprn PI defn
procedure INSTANTIATE_PI_FROM_OPRN (P_ENTITY_NAME IN VARCHAR2,
                                    P_ENTITY_KEY IN VARCHAR2,
 				   X_RETURN_STATUS OUT NOCOPY VARCHAR2,
				   X_MSG_COUNT OUT NOCOPY NUMBER,
				   X_MSG_DATA OUT NOCOPY VARCHAR2)
IS

l_batchstep_id number;
l_routingstep_id number;
l_oprn_id number;
l_oprn_line_id number;
l_resources varchar2(16);
l_batchstep_activity_id number;
l_batchstep_resource_id number;
l_instruction_set_id number;
l_return_status varchar2(100);
l_msg_count number;
l_msg_data varchar2(4000);
instruction_create_err exception;

l_entity_display_name varchar2(255);
l_invalid_instr fnd_table_of_varchar2_255;
l_invalid_instr_master fnd_table_of_varchar2_255;
l_invalid_entity_master fnd_table_of_varchar2_255;

j binary_integer;
k binary_integer;
l_attribute_Status varchar2(1);
INVALID_ATTRIBUTE_ASSOC_ERR exception;

cursor c_get_step is
  select
    batchstep_id,
    a.oprn_id,
    batchstep_no || '-' || b.oprn_no
  from gme_batch_steps a, gmd_operations_b b 
  where a.oprn_id = b.oprn_id and batchstep_id = p_entity_key;

cursor c_get_activities is
  select batchstep_activity_id,
         oprn_line_id ,
	     activity
  from  gme_batch_step_activities gbsa,
        gme_batch_steps gbs
  where gbsa.batchstep_id = gbs.batchstep_id and gbsa.batchstep_id = p_entity_key;

cursor c_get_resources is
  select batchstep_resource_id,
         oprn_line_id,
         resources,
         gbsa.activity || '-' || resources
  from   gme_batch_step_resources gbsr,
         gme_batch_step_activities gbsa,
         gme_batch_steps gbs
  where  gbsa.batchstep_activity_id = gbsr.batchstep_activity_id
  and    gbsa.batchstep_id = gbs.batchstep_id and gbsr.batchstep_id = p_entity_key;

BEGIN
	IF (NOT IS_GMO_ENABLED) THEN
		RAISE GMO_DISABLED_ERR;
	END IF;

	IF (NOT IS_BATCH_ENHANCED_PI (P_ENTITY_NAME => P_ENTITY_NAME, P_ENTITY_KEY => P_ENTITY_KEY) ) THEN
		RAISE GMO_NOT_ENHANCED_PI_ERR;
	END IF;

	IF (P_ENTITY_NAME <> GMO_CONSTANTS_GRP.ENTITY_OPERATION OR p_entity_key is null) THEN
		RAISE GMO_INVALID_ENTITY_ERR;
    END IF;

	j := 0;
	k := 0;
	l_invalid_entity_master := fnd_table_of_varchar2_255();
	l_invalid_instr_master := fnd_table_of_varchar2_255();

	open c_get_step;
	loop
	fetch c_get_step into l_batchstep_id, l_oprn_id, l_entity_display_name;
	exit when c_get_step%NOTFOUND;
  
		gmo_instruction_grp.CREATE_INSTANCE_FROM_DEFN (
			P_API_VERSION => 1.0,
			X_RETURN_STATUS => l_return_status,
			X_MSG_COUNT => l_msg_count,
			X_MSG_DATA => l_msg_data,
			P_DEFINITION_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_OPERATION,
			P_DEFINITION_ENTITY_KEY => l_oprn_id,
            P_INSTANCE_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_OPERATION,
			P_INSTANCE_ENTITY_KEY => l_batchstep_id,
			P_INSTRUCTION_TYPE => GMO_CONSTANTS_GRP.VBATCH_INSTRUCTION_TYPE,
			X_INSTRUCTION_SET_ID => l_instruction_set_id
		);

	 	if l_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS THEN
			RAISE instruction_create_err;
		end if;
  
		update_task_attribute
		(
			P_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_OPERATION,
			P_ENTITY_KEY =>  l_batchstep_id,
			P_INSTRUCTION_SET_ID => l_instruction_set_id,
			X_ATTRIBUTE_STATUS => l_attribute_status,
			X_INVALID_INSTR => l_invalid_instr
		);
		if (l_attribute_status <> 'S' and l_invalid_instr.count > 0) then
			for i in 1 .. l_invalid_instr.count loop
				j := j + 1;
				l_invalid_instr_master.extend;
				l_invalid_instr_master (j) := l_invalid_instr(i);
			end loop;
			k := k+1;
			l_invalid_entity_master.extend;
			l_invalid_entity_master (k) := l_entity_display_name;
		end if;

		if (FND_LOG.LEVEL_EVENT >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
			FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_INSTR_CREATE_MSG');
			FND_MESSAGE.SET_TOKEN('ENTITY_NAME',P_ENTITY_NAME);
			FND_MESSAGE.SET_TOKEN('ENTITY_KEY',P_ENTITY_KEY);
			FND_MESSAGE.SET_TOKEN('INSTR_ENTITY_NAME','STEP');
			FND_MESSAGE.SET_TOKEN('INSTR_ENTITY_KEY',l_batchstep_id);
			FND_LOG.MESSAGE(FND_LOG.LEVEL_EVENT,'gmo.plsql.gmo_vbatch_pvt.INSTANTIATE_PI_FROM_OPRN', FALSE);
		end if;

	end loop;
	close c_get_step;

	open c_get_activities;
        loop
        fetch c_get_activities into l_batchstep_activity_id, l_oprn_line_id, l_entity_display_name;
        exit when c_get_activities%NOTFOUND;
        
     gmo_instruction_grp.CREATE_INSTANCE_FROM_DEFN (
			P_API_VERSION => 1.0,
			X_RETURN_STATUS => l_return_status,
			X_MSG_COUNT => l_msg_count,
			X_MSG_DATA => l_msg_data,
                        P_DEFINITION_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_ACTIVITY,
                        P_DEFINITION_ENTITY_KEY => l_oprn_line_id,
                        P_INSTANCE_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_ACTIVITY,
                        P_INSTANCE_ENTITY_KEY => l_batchstep_activity_id,
                        P_INSTRUCTION_TYPE => GMO_CONSTANTS_GRP.VBATCH_INSTRUCTION_TYPE,
                        X_INSTRUCTION_SET_ID => l_instruction_set_id
                );

                if l_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS THEN
                        RAISE instruction_create_err;
                end if;

		update_task_attribute
		(
			P_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_ACTIVITY,
			P_ENTITY_KEY =>  l_batchstep_activity_id,
			P_INSTRUCTION_SET_ID => l_instruction_set_id,
			X_ATTRIBUTE_STATUS => l_attribute_status,
			X_INVALID_INSTR => l_invalid_instr
		);
		if (l_attribute_status <> 'S' and l_invalid_instr.count > 0) then
			for i in 1 .. l_invalid_instr.count loop
				j := j + 1;
				l_invalid_instr_master.extend;
				l_invalid_instr_master (j) := l_invalid_instr(i);
			end loop;
			k := k+1;
			l_invalid_entity_master.extend;
			l_invalid_entity_master (k) := l_entity_display_name;
		end if;

		if (FND_LOG.LEVEL_EVENT >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
			FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_INSTR_CREATE_MSG');
			FND_MESSAGE.SET_TOKEN('ENTITY_NAME',P_ENTITY_NAME);
			FND_MESSAGE.SET_TOKEN('ENTITY_KEY',P_ENTITY_KEY);
			FND_MESSAGE.SET_TOKEN('INSTR_ENTITY_NAME','ACTIVITY');
			FND_MESSAGE.SET_TOKEN('INSTR_ENTITY_KEY',l_batchstep_activity_id);
			FND_LOG.MESSAGE(FND_LOG.LEVEL_EVENT,'gmo.plsql.gmo_vbatch_pvt.INSTANTIATE_PI_FROM_OPRN', FALSE);
		end if;

        end loop;
        close c_get_activities;

	open c_get_resources;
        loop
        fetch c_get_resources into l_batchstep_resource_id, l_oprn_line_id, l_resources, l_entity_display_name;
        exit when c_get_resources%NOTFOUND;

    gmo_instruction_grp.CREATE_INSTANCE_FROM_DEFN (
			P_API_VERSION => 1.0,
			X_RETURN_STATUS => l_return_status,
			X_MSG_COUNT => l_msg_count,
			X_MSG_DATA => l_msg_data,
                        P_DEFINITION_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_RESOURCE,
                        P_DEFINITION_ENTITY_KEY => l_oprn_line_id || GMO_CONSTANTS_GRP.ENTITY_KEY_SEPARATOR || l_resources,
                        P_INSTANCE_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_RESOURCE,
                        P_INSTANCE_ENTITY_KEY => l_batchstep_resource_id,
                        P_INSTRUCTION_TYPE => GMO_CONSTANTS_GRP.VBATCH_INSTRUCTION_TYPE,
                        X_INSTRUCTION_SET_ID => l_instruction_set_id
                );

                if l_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS THEN
                        RAISE instruction_create_err;
                end if;

		update_task_attribute
		(
			P_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_RESOURCE,
			P_ENTITY_KEY =>  l_batchstep_resource_id,
			P_INSTRUCTION_SET_ID => l_instruction_set_id,
			X_ATTRIBUTE_STATUS => l_attribute_status,
			X_INVALID_INSTR => l_invalid_instr
		);
		if (l_attribute_status <> 'S' and l_invalid_instr.count > 0) then
			for i in 1 .. l_invalid_instr.count loop
				j := j + 1;
				l_invalid_instr_master.extend;
				l_invalid_instr_master (j) := l_invalid_instr(i);
			end loop;
			k := k+1;
			l_invalid_entity_master.extend;
			l_invalid_entity_master (k) := l_entity_display_name;
		end if;

		if (FND_LOG.LEVEL_EVENT >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
			FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_INSTR_CREATE_MSG');
			FND_MESSAGE.SET_TOKEN('ENTITY_NAME',P_ENTITY_NAME);
			FND_MESSAGE.SET_TOKEN('ENTITY_KEY',P_ENTITY_KEY);
			FND_MESSAGE.SET_TOKEN('INSTR_ENTITY_NAME','RESOURCE');
			FND_MESSAGE.SET_TOKEN('INSTR_ENTITY_KEY',l_batchstep_resource_id);
			FND_LOG.MESSAGE(FND_LOG.LEVEL_EVENT,'gmo.plsql.gmo_vbatch_pvt.INSTANTIATE_PI_FROM_OPRN', FALSE);
		end if;

        end loop;
        close c_get_resources;

	if (l_invalid_entity_master.count > 0)  then
		RAISE INVALID_ATTRIBUTE_ASSOC_ERR;
	end if;

	x_return_status :=  FND_API.G_RET_STS_SUCCESS;

EXCEPTION
	WHEN instruction_create_err then
		x_return_status :=  FND_API.G_RET_STS_ERROR;
		FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.INSTANTIATE_PI_FROM_OPRN', FALSE);
		end if;
	WHEN GMO_NOT_ENHANCED_PI_ERR THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_SUCCESS;
                FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_NOT_ENHANCED_PI_ERR');
                FND_MSG_PUB.ADD;
                FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.INSTANTIATE_PI_FROM_OPRN', FALSE);
		end if;
	WHEN GMO_DISABLED_ERR THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_SUCCESS;
                FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_DISABLED_ERR');
                FND_MSG_PUB.ADD;
                FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.INSTANTIATE_PI_FROM_OPRN', FALSE);
		end if;
	WHEN GMO_INVALID_ENTITY_ERR THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_ERROR;
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_INVALID_ENTITY');
                FND_MESSAGE.SET_TOKEN('ENTITY_NAME',P_ENTITY_NAME);
                FND_MESSAGE.SET_TOKEN('ENTITY_KEY',P_ENTITY_KEY);
                FND_MSG_PUB.ADD;
                FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.INSTANTIATE_PI_FROM_OPRN', FALSE);
		end if;
	WHEN INVALID_ATTRIBUTE_ASSOC_ERR THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_ERROR;
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_INVALID_ATTR_ASSOC');
                FND_MSG_PUB.ADD;
                FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.INSTANTIATE_PI_FROM_OPRN', FALSE);
		end if;
	WHEN OTHERS THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_UNEXP_ERROR;
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_UNEXPECTED_DB_ERR');
		FND_MESSAGE.SET_TOKEN('ERROR_TEXT',SQLERRM);
		FND_MESSAGE.SET_TOKEN('ERROR_CODE', SQLCODE);
		FND_MSG_PUB.ADD;
		FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_UNEXPECTED >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_UNEXPECTED,'gmo.plsql.gmo_vbatch_pvt.INSTANTIATE_PI_FROM_OPRN', FALSE);
		end if;

END INSTANTIATE_PI_FROM_OPRN;




--This procdeure would get the context information for the task.

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
                        X_READ_ONLY OUT NOCOPY  VARCHAR2,
                        X_CONTEXT_PARAMS_TBL OUT NOCOPY GMO_DATATYPES_GRP.CONTEXT_PARAMS_TBL_TYPE,
                        X_RETURN_STATUS OUT NOCOPY VARCHAR2,
                        X_MSG_COUNT OUT NOCOPY NUMBER,
                        X_MSG_DATA OUT NOCOPY VARCHAR2
)
IS

l_batch_id number;
l_batchstep_id number;
l_batchstep_activity_id number;
l_batchstep_resource_id number;
l_material_detail_id number;
l_formula_line_id number;
l_oprn_line_id number;
l_oprn_id number;
l_resources varchar2(100);
l_process_param_id number;
l_param_id number;
l_param_id_vchar varchar2(4000);
l_inventory_item_id number;

l_org_id number;
l_org_code varchar2(10);

l_vbatch_mode varchar2(1);
l_step_locked varchar2(1);
l_requester number;
l_return_status varchar2(100);
l_msg_count number;
l_msg_data varchar2(4000);

l_line_type number;
l_line_no number;
l_invoke_mode varchar2(1);

l_context_params_rec_type GMO_DATATYPES_GRP.CONTEXT_PARAMS_REC_TYPE;

cursor get_step_detail is select batch_id, batchstep_id from gme_batch_steps where batchstep_id = l_batchstep_id;

cursor get_activity_detail is select batch_id, batchstep_id, batchstep_activity_id from gme_batch_step_activities where batchstep_id = l_batchstep_id and oprn_line_id = l_oprn_line_id;
cursor get_activity_detail_from_id is select batch_id, batchstep_id, batchstep_activity_id from gme_batch_step_activities where batchstep_activity_id = l_batchstep_activity_id;

cursor get_resource_detail is select batch_id, batchstep_id, batchstep_activity_id, batchstep_resource_id from gme_batch_step_resources where batchstep_activity_id = l_batchstep_activity_id and resources = l_resources;
cursor get_resource_detail_from_id is select batch_id, batchstep_id, batchstep_activity_id, batchstep_resource_id from gme_batch_step_resources where batchstep_resource_id = l_batchstep_resource_id;

cursor get_param_detail is select batch_id, batchstep_id, batchstep_activity_id, batchstep_resource_id, process_param_id from gme_process_parameters where batchstep_resource_id = l_batchstep_resource_id and parameter_id = l_param_id;
cursor get_param_detail_from_id is select batch_id, batchstep_id, batchstep_activity_id, batchstep_resource_id, process_param_id from gme_process_parameters where process_param_id = l_process_param_id;


cursor get_material_detail is select batch_id, material_detail_id, inventory_item_id, line_no, line_type from gme_material_details where formulaline_id = l_formula_line_id and batch_id = l_batch_id;
cursor get_material_detail_from_id is select batch_id, material_detail_id, inventory_item_id, line_no, line_type from gme_material_details where material_detail_id = l_material_detail_id;
cursor get_step_id_for_material is select batchstep_id from gme_batch_step_items where material_detail_id = l_material_detail_id;

cursor get_org_detail is select a.organization_id, a.organization_code from mtl_parameters a, gme_batch_header b where a.organization_id = b.organization_id and b.batch_id = l_batch_id;

GMO_INVALID_TASK_ERR exception;
GMO_INSTR_TASK_PARAM_ERR exception;
NO_BATCH_FOUND_ERR exception;
GMO_INVALID_MODULE_ERR exception;
l_advanced_pi boolean;

BEGIN
	IF (NOT IS_GMO_ENABLED) THEN
		RAISE GMO_DISABLED_ERR;
	END IF;
	

	--validte if the module is in vbatch or pi
	IF (P_FROM_MODULE <> GMO_CONSTANTS_GRP.FROM_MODULE_PI AND P_FROM_MODULE <> GMO_CONSTANTS_GRP.FROM_MODULE_VBATCH) THEN
		RAISE GMO_INVALID_MODULE_ERR;
	END IF;

	
	l_advanced_pi :=  IS_BATCH_ENHANCED_PI (P_ENTITY_NAME => P_ENTITY_NAME, P_ENTITY_KEY => P_ENTITY_KEY); 
	
	--we should check the admin privleges from the function access instead
	--of banking on the parameter.
	/*

	if (P_VBATCH_MODE = GMO_CONSTANTS_GRP.VBATCH_ADMIN_MODE) then
		l_vbatch_mode := GMO_CONSTANTS_GRP.VBATCH_ADMIN_MODE;
	else
		l_vbatch_mode := GMO_CONSTANTS_GRP.VBATCH_NORMAL_MODE;
	end if;

	if (P_FROM_MODULE = GMO_CONSTANTS_GRP.FROM_MODULE_PI) then
		
		GMO_INSTRUCTION_GRP.GET_TASK_PARAMETER
		(
			P_API_VERSION => 1.0,
			X_RETURN_STATUS => l_return_status,
			X_MSG_COUNT => l_msg_count,
			X_MSG_DATA => l_msg_data,
			P_INSTRUCTION_PROCESS_ID => P_INSTRUCTION_PROCESS_ID,
			P_ATTRIBUTE_NAME => GMO_CONSTANTS_GRP.VBATCH_MODE_PARAMETER,
			X_ATTRIBUTE_VALUE => l_vbatch_mode
		);


		if l_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS THEN
			RAISE gmo_instr_task_param_err;
		end if;
	
	end if;
	--if (l_vbatch_mode is null) then
	*/
	if (FND_FUNCTION.TEST('GMO_VBATCH_DETAIL_ADMIN')) then
		l_vbatch_mode := GMO_CONSTANTS_GRP.VBATCH_ADMIN_MODE;
	else
		l_vbatch_mode := GMO_CONSTANTS_GRP.VBATCH_NORMAL_MODE;
	end if;
	
	l_requester := P_REQUESTER;
	
	IF (l_requester is null) THEN
		l_requester := FND_GLOBAL.USER_ID;
	END IF;
		
	IF (P_TASK = 'VIEW_BATCH' or P_TASK = 'UPDATE_BATCH') THEN

		IF (P_ENTITY_NAME <> GMO_CONSTANTS_GRP.ENTITY_BATCH) THEN
			RAISE GMO_INVALID_TASK_ERR;
		END IF;

		X_TASK_ENTITY_NAME := GMO_CONSTANTS_GRP.ENTITY_BATCH;
		X_TASK_ENTITY_KEY := P_ENTITY_KEY;
		X_TASK_NAME := null;
		X_TASK_KEY := null;

		IF (P_TASK = 'UPDATE_BATCH') THEN
			X_READ_ONLY := GMO_CONSTANTS_GRP.READ_ONLY_NO;
		ELSE
			X_READ_ONLY := GMO_CONSTANTS_GRP.READ_ONLY_YES;	

		END IF;

	ELSIF (P_TASK = 'VIEW_OPERATION' or P_TASK = 'UPDATE_OPERATION' or P_TASK = 'RELEASE_STEP' or P_TASK = 'CLOSE_STEP') THEN

		IF (P_ENTITY_NAME <> GMO_CONSTANTS_GRP.ENTITY_OPERATION) THEN
			RAISE GMO_INVALID_TASK_ERR;
		END IF;
			
	        l_batchstep_id := to_number (p_entity_key);
		X_TASK_ENTITY_NAME := GMO_CONSTANTS_GRP.ENTITY_OPERATION;
		X_TASK_ENTITY_KEY := P_ENTITY_KEY;
		X_TASK_NAME := null;
		X_TASK_KEY := null;

		IF (P_TASK = 'VIEW_OPERATION') THEN
			X_READ_ONLY := GMO_CONSTANTS_GRP.READ_ONLY_YES;
		ELSE
			l_step_locked := GMO_VBATCH_PVT.IS_STEP_LOCKED (P_VBATCH_MODE => l_vbatch_mode, P_BATCHSTEP_ID => l_batchstep_id, P_REQUESTER => l_requester);

			IF (not l_advanced_pi or l_step_locked = GMO_CONSTANTS_GRP.YES) THEN
				X_READ_ONLY := GMO_CONSTANTS_GRP.READ_ONLY_NO;	
			ELSE
				X_READ_ONLY := GMO_CONSTANTS_GRP.READ_ONLY_YES;	
			END IF;

		END IF;

	ELSIF (P_TASK = 'UPDATE_ACTIVITY' OR P_TASK = 'VIEW_ACTIVITY') THEN
	
		IF (P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_OPERATION) THEN
			l_batchstep_id := to_number (p_entity_key);
			l_batchstep_activity_id := to_number(p_task_attribute);
			
		ELSIF (P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_ACTIVITY) THEN
			l_batchstep_activity_id := to_number (p_entity_key);

		ELSE
			RAISE GMO_INVALID_TASK_ERR;
		END IF;
	 	
		open get_activity_detail_from_id;
		fetch get_activity_detail_from_id into l_batch_id, l_batchstep_id, l_batchstep_activity_id;
		close get_activity_detail_from_id;

		X_TASK_ENTITY_NAME := GMO_CONSTANTS_GRP.ENTITY_OPERATION;
		X_TASK_ENTITY_KEY := l_batchstep_id;
		X_TASK_NAME := GMO_CONSTANTS_GRP.TASK_ACTIVITY;
		X_TASK_KEY := l_batchstep_activity_id;
		
		IF (P_TASK = 'VIEW_ACTIVITY') THEN
			X_READ_ONLY := GMO_CONSTANTS_GRP.READ_ONLY_YES;
		ELSE
			 
			l_step_locked := GMO_VBATCH_PVT.IS_STEP_LOCKED (P_VBATCH_MODE => l_vbatch_mode, P_BATCHSTEP_ID => l_batchstep_id, P_REQUESTER => l_requester);

			IF (not l_advanced_pi or l_step_locked = GMO_CONSTANTS_GRP.YES) THEN
				X_READ_ONLY := GMO_CONSTANTS_GRP.READ_ONLY_NO;	
			ELSE
				X_READ_ONLY := GMO_CONSTANTS_GRP.READ_ONLY_YES;	
			END IF;

		END IF;


	ELSIF (P_TASK = 'UPDATE_RESOURCE' OR P_TASK = 'VIEW_RESOURCE' OR P_TASK = GMO_CONSTANTS_GRP.TASK_RESOURCE_TRANSACTION) THEN
		
		IF (P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_OPERATION) THEN
			l_batchstep_id := to_number (p_entity_key);
			l_batchstep_resource_id := to_number(p_task_attribute);

		ELSIF (P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_ACTIVITY) THEN

			l_batchstep_activity_id := to_number (p_entity_key);
			l_batchstep_resource_id := to_number(p_task_attribute);

		ELSIF (P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_RESOURCE) THEN
		
			l_batchstep_resource_id := to_number(p_entity_key);
			
		ELSE
			RAISE GMO_INVALID_TASK_ERR;
		END IF;
		
		open get_resource_detail_from_id;
		fetch get_resource_detail_from_id into l_batch_id, l_batchstep_id, l_batchstep_activity_id, l_batchstep_resource_id;
		close get_resource_detail_from_id;
			
		X_TASK_ENTITY_NAME := GMO_CONSTANTS_GRP.ENTITY_ACTIVITY;
		X_TASK_ENTITY_KEY := l_batchstep_activity_id;
		IF (P_TASK = GMO_CONSTANTS_GRP.TASK_RESOURCE_TRANSACTION) THEN
			X_TASK_NAME := GMO_CONSTANTS_GRP.TASK_RESOURCE_TRANSACTION;
		ELSE
			X_TASK_NAME := GMO_CONSTANTS_GRP.TASK_RESOURCE;
		END IF;
		X_TASK_KEY := l_batchstep_resource_id;
		
		IF (P_TASK = 'VIEW_RESOURCE') THEN
			X_READ_ONLY := GMO_CONSTANTS_GRP.READ_ONLY_YES;
		ELSE

			l_step_locked := GMO_VBATCH_PVT.IS_STEP_LOCKED (P_VBATCH_MODE => l_vbatch_mode, P_BATCHSTEP_ID => l_batchstep_id, P_REQUESTER => l_requester);

			IF (not l_advanced_pi or l_step_locked = GMO_CONSTANTS_GRP.YES) THEN
				X_READ_ONLY := GMO_CONSTANTS_GRP.READ_ONLY_NO;	
			ELSE
				X_READ_ONLY := GMO_CONSTANTS_GRP.READ_ONLY_YES;	
			END IF;

		END IF;

	ELSIF (P_TASK = GMO_CONSTANTS_GRP.TASK_PROCESS_PARAMETER) THEN
		
		IF (P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_OPERATION) THEN
			l_batchstep_id := to_number (p_entity_key);

		ELSIF (P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_ACTIVITY) THEN
			l_batchstep_activity_id := to_number (p_entity_key);

		ELSIF (P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_RESOURCE) THEN
			l_batchstep_resource_id := to_number(p_entity_key);
			
		ELSE
			RAISE GMO_INVALID_TASK_ERR;
		END IF;
		l_process_param_id := to_number(p_task_attribute);

		open get_param_detail_from_id;
		fetch get_param_detail_from_id into l_batch_id, l_batchstep_id, l_batchstep_activity_id, l_batchstep_resource_id, l_process_param_id;
		close get_param_detail_from_id;

		X_TASK_ENTITY_NAME := GMO_CONSTANTS_GRP.ENTITY_RESOURCE;
		X_TASK_ENTITY_KEY := l_batchstep_resource_id;
		X_TASK_NAME := GMO_CONSTANTS_GRP.TASK_PROCESS_PARAMETER;
		X_TASK_KEY := l_process_param_id;
		
		l_step_locked := GMO_VBATCH_PVT.IS_STEP_LOCKED (P_VBATCH_MODE => l_vbatch_mode, P_BATCHSTEP_ID => l_batchstep_id, P_REQUESTER => l_requester);

		IF (not l_advanced_pi or l_step_locked = GMO_CONSTANTS_GRP.YES) THEN
			X_READ_ONLY := GMO_CONSTANTS_GRP.READ_ONLY_NO;	
		ELSE
			X_READ_ONLY := GMO_CONSTANTS_GRP.READ_ONLY_YES;	
		END IF;

	ELSIF (P_TASK = 'MATERIAL' OR P_TASK = 'RESERVATIONS' OR P_TASK = 'VIEW_MATERIAL' OR P_TASK = 'UPDATE_MATERIAL' OR P_TASK='MATERIAL_TRANSACTIONS' OR P_TASK = 'QUALITY') THEN
		

		IF (P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_BATCH) THEN

			l_batch_id := to_number (p_entity_key);

		ELSIF (P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_OPERATION) THEN
			l_batchstep_id := to_number (p_entity_key);
			l_material_detail_id := to_number(p_task_attribute);

			open get_step_detail;
			fetch get_step_detail into l_batch_id, l_batchstep_id;
			close get_step_detail;
			
		ELSIF (P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_ACTIVITY) THEN
		
			l_batchstep_activity_id := to_number(p_entity_key);
			l_material_detail_id := to_number(p_task_attribute);

			open get_activity_detail_from_id;
			fetch get_activity_detail_from_id into l_batch_id, l_batchstep_id, l_batchstep_activity_id;
			close get_activity_detail_from_id;
			
		ELSIF (P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_RESOURCE) THEN
		
			l_batchstep_resource_id := to_number(p_entity_key);
			l_material_detail_id := to_number(p_task_attribute);

			open get_resource_detail_from_id;
			fetch get_resource_detail_from_id into l_batch_id, l_batchstep_id, l_batchstep_activity_id, l_batchstep_resource_id;
			close get_resource_detail_from_id;

		ELSIF (P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_MATERIAL) THEN
		
			l_material_detail_id := to_number(p_entity_key);
		
			open get_step_id_for_material;
			fetch get_step_id_for_material into l_batchstep_id;
			close get_step_id_for_material;

		ELSE
			RAISE GMO_INVALID_TASK_ERR;
		END IF;
		
		open get_material_detail_from_id;
		fetch get_material_detail_from_id into l_batch_id, l_material_detail_id, l_inventory_item_id, l_line_no, l_line_type;
		close get_material_detail_from_id;

		IF (P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_BATCH OR P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_MATERIAL OR P_TASK = 'QUALITY') THEN
			X_TASK_ENTITY_NAME := GMO_CONSTANTS_GRP.ENTITY_BATCH;
			X_TASK_ENTITY_KEY := l_batch_id;
		ELSE
			X_TASK_ENTITY_NAME := GMO_CONSTANTS_GRP.ENTITY_OPERATION;
			X_TASK_ENTITY_KEY := l_batchstep_id;
		END IF;
		
		IF (P_TASK= GMO_CONSTANTS_GRP.TASK_MATERIAL_TRANSACTIONS) THEN	
			X_TASK_NAME := GMO_CONSTANTS_GRP.TASK_MATERIAL_TRANSACTIONS;
			l_invoke_mode := 'T';
			X_TASK_KEY := l_material_detail_id;

		ELSIF (P_TASK = 'QUALITY') THEN
			X_TASK_NAME := GMO_CONSTANTS_GRP.TASK_QUALITY;
			X_TASK_KEY := l_inventory_item_id;

		ELSE
			X_TASK_NAME := GMO_CONSTANTS_GRP.TASK_MATERIAL;
			X_TASK_KEY := l_material_detail_id;
		END IF;
		
		--Bug 5023801: start
		IF (P_TASK = 'VIEW_MATERIAL') THEN
			X_READ_ONLY := GMO_CONSTANTS_GRP.READ_ONLY_YES;
		--Bug 5023801: end
		ELSIF (X_TASK_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_BATCH or l_batchstep_id is null )  THEN
			X_READ_ONLY := GMO_CONSTANTS_GRP.READ_ONLY_NO;
		ELSE

			l_step_locked := GMO_VBATCH_PVT.IS_STEP_LOCKED (P_VBATCH_MODE => l_vbatch_mode, P_BATCHSTEP_ID => l_batchstep_id, P_REQUESTER => l_requester);

			IF (not l_advanced_pi or l_step_locked = GMO_CONSTANTS_GRP.YES) THEN
				X_READ_ONLY := GMO_CONSTANTS_GRP.READ_ONLY_NO;	
			ELSE
				X_READ_ONLY := GMO_CONSTANTS_GRP.READ_ONLY_YES;	
			END IF;

		END IF;

	ELSE
		RAISE GMO_INVALID_TASK_ERR;
	END IF;

	if (FND_LOG.LEVEL_EVENT >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_ON_TASK_LOAD_MSG');
		FND_MESSAGE.SET_TOKEN('FROM_MODULE',P_FROM_MODULE);
		FND_MESSAGE.SET_TOKEN('ENTITY_NAME',P_ENTITY_NAME);
		FND_MESSAGE.SET_TOKEN('ENTITY_KEY',P_ENTITY_KEY);
		FND_MESSAGE.SET_TOKEN('TASK',P_TASK);
		FND_MESSAGE.SET_TOKEN('TASK_ATTRIBUTE',P_TASK_ATTRIBUTE);
		FND_MESSAGE.SET_TOKEN('TASK_ENTITY_NAME',X_TASK_ENTITY_NAME);
		FND_MESSAGE.SET_TOKEN('TASK_ENTITY_KEY',X_TASK_ENTITY_KEY);
		FND_MESSAGE.SET_TOKEN('TASK_NAME',X_TASK_NAME);
		FND_MESSAGE.SET_TOKEN('TASK_KEY',X_TASK_KEY);
		FND_MESSAGE.SET_TOKEN('READ_ONLY',X_READ_ONLY);
		FND_LOG.MESSAGE(FND_LOG.LEVEL_EVENT,'gmo.plsql.gmo_vbatch_pvt.on_task_load', FALSE);
	end if;
	
	if (l_batch_id is null) then
		get_batch_id_for_entity
		(
			p_entity_name => p_entity_name,
			p_entity_key => p_entity_key,
			x_batch_id => l_batch_id,
			x_return_status => l_return_status,
			x_msg_count => l_msg_count,
			x_msg_data => l_msg_data
		);
		if L_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS THEN
			RAISE NO_BATCH_FOUND_ERR;
		end if;
	end if;		

	open get_org_detail;
	fetch get_org_detail into l_org_id, l_org_code;
	close get_org_detail;
	
	l_context_params_rec_type.NAME := 'ORGN_CODE';
	l_context_params_rec_type.VALUE := l_org_code;
	X_CONTEXT_PARAMS_TBL(1) := l_context_params_rec_type;
	
	l_context_params_rec_type.NAME := 'ORGN_ID';
	l_context_params_rec_type.VALUE := l_org_id;
	X_CONTEXT_PARAMS_TBL(2) := l_context_params_rec_type;


	l_context_params_rec_type.NAME := 'BATCH_ID';
	l_context_params_rec_type.VALUE := l_batch_id;
	X_CONTEXT_PARAMS_TBL(3) := l_context_params_rec_type;
	
	
	l_context_params_rec_type.NAME := 'BATCHSTEP_ID';
	l_context_params_rec_type.VALUE := l_batchstep_id;
	X_CONTEXT_PARAMS_TBL(4) := l_context_params_rec_type;	
	
	l_context_params_rec_type.NAME := 'BATCHSTEP_ACTIVITY_ID';
	l_context_params_rec_type.VALUE := l_batchstep_activity_id;
	X_CONTEXT_PARAMS_TBL(5) := l_context_params_rec_type;
	
	l_context_params_rec_type.NAME := 'BATCHSTEP_RESOURCE_ID';
	l_context_params_rec_type.VALUE := l_batchstep_resource_id;
	X_CONTEXT_PARAMS_TBL(6) := l_context_params_rec_type;
	
	l_context_params_rec_type.NAME := 'MATERIAL_DETAIL_ID';
	l_context_params_rec_type.VALUE := l_material_detail_id;
	X_CONTEXT_PARAMS_TBL(7) := l_context_params_rec_type;
	
	l_context_params_rec_type.NAME := 'PROCESS_PARAM_ID';
	l_context_params_rec_type.VALUE := l_process_param_id;
	X_CONTEXT_PARAMS_TBL(8) := l_context_params_rec_type;

	--Bug 4893056: start
	l_context_params_rec_type.NAME := 'INVOKE_MODE';
        l_context_params_rec_type.VALUE := l_invoke_mode;
        X_CONTEXT_PARAMS_TBL(9) := l_context_params_rec_type;

	l_context_params_rec_type.NAME := 'LINE_TYPE';
        l_context_params_rec_type.VALUE := l_line_type;
        X_CONTEXT_PARAMS_TBL(10) := l_context_params_rec_type;

	l_context_params_rec_type.NAME := 'LINE_NO';
        l_context_params_rec_type.VALUE := l_line_no;
        X_CONTEXT_PARAMS_TBL(11) := l_context_params_rec_type;
	--Bug 4893056: end

	X_RETURN_STATUS := FND_API.G_RET_STS_SUCCESS;

EXCEPTION	
	WHEN GMO_DISABLED_ERR THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_SUCCESS;
                FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_DISABLED_ERR');
                FND_MSG_PUB.ADD;
                FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.on_task_load', FALSE);
		end if;
	WHEN GMO_NOT_ENHANCED_PI_ERR THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_SUCCESS;
		X_READ_ONLY := GMO_CONSTANTS_GRP.READ_ONLY_NO;
                FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_NOT_ENHANCED_PI_ERR');
                FND_MSG_PUB.ADD;
                FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.on_task_load', FALSE);
		end if;
	
        WHEN GMO_INVALID_MODULE_ERR THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_ERROR;
                FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_INVALID_MODULE');
                FND_MSG_PUB.ADD;
                FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.on_task_load', FALSE);
		end if;

	WHEN GMO_INSTR_TASK_PARAM_ERR THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_ERROR;
                FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_INSTR_PARAM_ERR');
                FND_MSG_PUB.ADD;
                FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.on_task_load', FALSE);
		end if;
		
	WHEN GMO_INVALID_TASK_ERR THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_ERROR;
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_INVALID_TASK');
                FND_MESSAGE.SET_TOKEN('ENTITY_NAME',P_ENTITY_NAME);
                FND_MESSAGE.SET_TOKEN('ENTITY_KEY',P_ENTITY_KEY);
                FND_MESSAGE.SET_TOKEN('TASK',P_TASK);
                FND_MSG_PUB.ADD;
                FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.on_task_load', FALSE);
		end if;
	WHEN GMO_INVALID_ENTITY_ERR THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_ERROR;
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_INVALID_ENTITY');
                FND_MESSAGE.SET_TOKEN('ENTITY_NAME',P_ENTITY_NAME);
                FND_MESSAGE.SET_TOKEN('ENTITY_KEY',P_ENTITY_KEY);
                FND_MSG_PUB.ADD;
                FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.on_task_load', FALSE);
		end if;
	WHEN OTHERS THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_UNEXP_ERROR;
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_UNEXPECTED_DB_ERR');
		FND_MESSAGE.SET_TOKEN('ERROR_TEXT',SQLERRM);
		FND_MESSAGE.SET_TOKEN('ERROR_CODE', SQLCODE);
		FND_MSG_PUB.ADD;
		FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_UNEXPECTED >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_UNEXPECTED,'gmo.plsql.gmo_vbatch_pvt.on_task_load', FALSE);
		end if;
END ON_TASK_LOAD;
                        



--This procdeure would process the action performed by the task.

procedure ON_TASK_ACTION (P_ENTITY_NAME IN VARCHAR2,
                          P_ENTITY_KEY IN VARCHAR2,
                          P_TASK IN VARCHAR2,
                          P_TASK_ATTRIBUTE IN VARCHAR2,
                          P_REQUESTER IN NUMBER,
                          X_RETURN_STATUS OUT NOCOPY VARCHAR2,
                          X_MSG_COUNT OUT NOCOPY NUMBER,
                          X_MSG_DATA OUT NOCOPY VARCHAR2)

IS

l_batchstep_id number;
l_material_detail_id number;
l_batchstep_activity_id number;
l_batchstep_resource_id number;
l_instruction_set_id number;
l_return_status varchar2(100);
l_msg_count number;
l_msg_data varchar2(4000);
instruction_create_err exception;
instruction_nullify_err exception;

cursor c_get_steps is select batchstep_id from gme_batch_steps where batch_id = p_entity_key;

cursor c_get_activities is select batchstep_activity_id from gme_batch_step_activities where batch_id = p_entity_key;

cursor c_get_resources is select batchstep_resource_id from gme_batch_step_resources where batch_id = p_entity_key;

cursor c_get_materials is select material_detail_id from gme_material_details where batch_id = p_entity_key;


cursor c_get_step_activities is select batchstep_activity_id from gme_batch_step_activities where batchstep_id = p_entity_key;
cursor c_get_step_resources is select batchstep_resource_id from gme_batch_step_resources where batchstep_id = p_entity_key;

cursor c_get_step_material is select material_detail_id from gme_batch_step_items where batchstep_id = p_entity_key;

cursor c_get_activity_resources is select batchstep_resource_id from gme_batch_step_resources where batchstep_activity_id = l_batchstep_activity_id;


BEGIN
	
	IF (NOT IS_GMO_ENABLED) THEN
		RAISE GMO_DISABLED_ERR;
	END IF;

	IF (NOT IS_BATCH_ENHANCED_PI (P_ENTITY_NAME => P_ENTITY_NAME, P_ENTITY_KEY => P_ENTITY_KEY) ) THEN
		RAISE GMO_NOT_ENHANCED_PI_ERR;
	END IF;

	IF (P_ENTITY_NAME <> GMO_CONSTANTS_GRP.ENTITY_BATCH AND P_ENTITY_NAME <> GMO_CONSTANTS_GRP.ENTITY_OPERATION OR p_entity_key is null) THEN
		RAISE GMO_INVALID_ENTITY_ERR;
        END IF;

	if (FND_LOG.LEVEL_EVENT >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_ON_TASK_ACTION_MSG');
		FND_MESSAGE.SET_TOKEN('ENTITY_NAME',P_ENTITY_NAME);
		FND_MESSAGE.SET_TOKEN('ENTITY_KEY',P_ENTITY_KEY);
		FND_MESSAGE.SET_TOKEN('TASK',P_TASK);
		FND_MESSAGE.SET_TOKEN('TASK_ATTRIBUTE',P_TASK_ATTRIBUTE);
		FND_LOG.MESSAGE(FND_LOG.LEVEL_EVENT,'gmo.plsql.gmo_vbatch_pvt.on_task_action', FALSE);
	end if;


	IF (P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_BATCH) THEN
	    /* enh 13724630, biachen. add case Reroute Batch. */
		IF (P_TASK = GMO_CONSTANTS_GRP.ACTION_TERMINATE OR P_TASK = GMO_CONSTANTS_GRP.ACTION_REROUTE OR P_TASK = GMO_CONSTANTS_GRP.ACTION_UNRELEASE) THEN
			open c_get_steps;
			loop
			fetch c_get_steps into l_batchstep_id;
			exit when c_get_steps%NOTFOUND;

				gmo_instruction_grp.NULLIFY_INSTR_FOR_ENTITY (
					P_API_VERSION => 1.0,
					X_RETURN_STATUS => l_return_status,
					X_MSG_COUNT => l_msg_count,
					X_MSG_DATA => l_msg_data,
					P_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_OPERATION,
					P_ENTITY_KEY => l_batchstep_id,
					P_INSTRUCTION_TYPE => GMO_CONSTANTS_GRP.VBATCH_INSTRUCTION_TYPE
				);	

				if l_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS THEN
					RAISE instruction_nullify_err;
				end if;

				IF (P_TASK = GMO_CONSTANTS_GRP.ACTION_UNRELEASE) THEN


					gmo_instruction_grp.create_instance_from_instance (
						P_API_VERSION => 1.0,
						X_RETURN_STATUS => l_return_status,
						X_MSG_COUNT => l_msg_count,
						X_MSG_DATA => l_msg_data,
						P_SOURCE_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_OPERATION,
						P_SOURCE_ENTITY_KEY => l_batchstep_id,
						P_TARGET_ENTITY_KEY => l_batchstep_id,
						P_INSTRUCTION_TYPE => GMO_CONSTANTS_GRP.VBATCH_INSTRUCTION_TYPE,
						X_INSTRUCTION_SET_ID => l_instruction_set_id
					);
					if l_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS THEN
						RAISE instruction_create_err;
					end if;
				end if;
			end loop;
			close c_get_steps;		
			
			open c_get_activities;
			loop
			fetch c_get_activities into l_batchstep_activity_id;
			exit when c_get_activities%NOTFOUND;
				
				gmo_instruction_grp.NULLIFY_INSTR_FOR_ENTITY (
					P_API_VERSION => 1.0,
					X_RETURN_STATUS => l_return_status,
					X_MSG_COUNT => l_msg_count,
					X_MSG_DATA => l_msg_data,
					P_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_ACTIVITY,
					P_ENTITY_KEY => l_batchstep_activity_id,
					P_INSTRUCTION_TYPE => GMO_CONSTANTS_GRP.VBATCH_INSTRUCTION_TYPE
				);
			
				if l_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS THEN
					RAISE instruction_nullify_err;
				end if;
				
				IF (P_TASK = GMO_CONSTANTS_GRP.ACTION_UNRELEASE) THEN
					gmo_instruction_grp.create_instance_from_instance (
						P_API_VERSION => 1.0,
						X_RETURN_STATUS => l_return_status,
						X_MSG_COUNT => l_msg_count,
						X_MSG_DATA => l_msg_data,
						P_SOURCE_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_ACTIVITY,
						P_SOURCE_ENTITY_KEY => l_batchstep_activity_id,
						P_TARGET_ENTITY_KEY => l_batchstep_activity_id,
						P_INSTRUCTION_TYPE => GMO_CONSTANTS_GRP.VBATCH_INSTRUCTION_TYPE,
						X_INSTRUCTION_SET_ID => l_instruction_set_id
					);
					if l_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS THEN
						RAISE instruction_create_err;
					end if;
				end if;

			end loop;
		        close c_get_activities;
		        
		        open c_get_resources;
			loop
			fetch c_get_resources into l_batchstep_resource_id;
			exit when c_get_resources%NOTFOUND;
				
				gmo_instruction_grp.NULLIFY_INSTR_FOR_ENTITY (
					P_API_VERSION => 1.0,
					X_RETURN_STATUS => l_return_status,
					X_MSG_COUNT => l_msg_count,
					X_MSG_DATA => l_msg_data,
					P_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_RESOURCE,
					P_ENTITY_KEY => l_batchstep_resource_id,
					P_INSTRUCTION_TYPE => GMO_CONSTANTS_GRP.VBATCH_INSTRUCTION_TYPE
				);

				if X_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS THEN
					RAISE instruction_nullify_err;
				end if;

				IF (P_TASK = GMO_CONSTANTS_GRP.ACTION_UNRELEASE) THEN
					gmo_instruction_grp.create_instance_from_instance (
						P_API_VERSION => 1.0,
						X_RETURN_STATUS => l_return_status,
						X_MSG_COUNT => l_msg_count,
						X_MSG_DATA => l_msg_data,
						P_SOURCE_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_RESOURCE,
						P_SOURCE_ENTITY_KEY => l_batchstep_resource_id,
						P_TARGET_ENTITY_KEY => l_batchstep_resource_id,
						P_INSTRUCTION_TYPE => GMO_CONSTANTS_GRP.VBATCH_INSTRUCTION_TYPE,
						X_INSTRUCTION_SET_ID => l_instruction_set_id
					);
					if l_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS THEN
						RAISE instruction_create_err;
					end if;
				end if;

			end loop;
		        close c_get_resources;
		        
		        open c_get_materials;
			loop
			fetch c_get_materials into l_material_detail_id;
			exit when c_get_materials%NOTFOUND;
				
				gmo_instruction_grp.NULLIFY_INSTR_FOR_ENTITY (
					P_API_VERSION => 1.0,
					X_RETURN_STATUS => l_return_status,
					X_MSG_COUNT => l_msg_count,
					X_MSG_DATA => l_msg_data,
					P_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_MATERIAL,
					P_ENTITY_KEY => l_material_detail_id,
					P_INSTRUCTION_TYPE => GMO_CONSTANTS_GRP.VBATCH_INSTRUCTION_TYPE
				);

				if X_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS THEN
					RAISE instruction_nullify_err;
				end if;

				IF (P_TASK = GMO_CONSTANTS_GRP.ACTION_UNRELEASE) THEN
					gmo_instruction_grp.create_instance_from_instance (
						P_API_VERSION => 1.0,
						X_RETURN_STATUS => l_return_status,
						X_MSG_COUNT => l_msg_count,
						X_MSG_DATA => l_msg_data,
						P_SOURCE_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_MATERIAL,
						P_SOURCE_ENTITY_KEY => l_material_detail_id,
						P_TARGET_ENTITY_KEY => l_material_detail_id,
						P_INSTRUCTION_TYPE => GMO_CONSTANTS_GRP.VBATCH_INSTRUCTION_TYPE,
						X_INSTRUCTION_SET_ID => l_instruction_set_id
					);
					if l_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS THEN
						RAISE instruction_create_err;
					end if;
				end if;
			end loop;
        		close c_get_materials; 
		
		ELSIF (P_TASK = GMO_CONSTANTS_GRP.ACTION_DELETE_MATERIAL) THEN

			l_material_detail_id := P_TASK_ATTRIBUTE;
			gmo_instruction_grp.NULLIFY_INSTR_FOR_ENTITY (
				P_API_VERSION => 1.0,
				X_RETURN_STATUS => l_return_status,
				X_MSG_COUNT => l_msg_count,
				X_MSG_DATA => l_msg_data,
				P_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_MATERIAL,
				P_ENTITY_KEY => l_material_detail_id,
				P_INSTRUCTION_TYPE => GMO_CONSTANTS_GRP.VBATCH_INSTRUCTION_TYPE
			);

			if l_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS THEN
				RAISE instruction_nullify_err;
			end if;

		END IF; -- end IF (P_TASK = GMO_CONSTANTS_GRP.ACTION_TERMINATE OR P_TASK = GMO_CONSTANTS_GRP.ACTION_UNRELEASE) THEN
		
	ELSIF (P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_OPERATION) THEN

		IF (P_TASK = GMO_CONSTANTS_GRP.ACTION_DELETE_OPERATION OR P_TASK = GMO_CONSTANTS_GRP.ACTION_UNRELEASE) THEN

			l_batchstep_id := p_entity_key;
			gmo_instruction_grp.NULLIFY_INSTR_FOR_ENTITY (
				P_API_VERSION => 1.0,
				X_RETURN_STATUS => l_return_status,
				X_MSG_COUNT => l_msg_count,
				X_MSG_DATA => l_msg_data,
				P_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_OPERATION,
				P_ENTITY_KEY => l_batchstep_id,
				P_INSTRUCTION_TYPE => GMO_CONSTANTS_GRP.VBATCH_INSTRUCTION_TYPE
			);	

			if l_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS THEN
				RAISE instruction_nullify_err;
			end if;

			IF (P_TASK = GMO_CONSTANTS_GRP.ACTION_UNRELEASE) THEN


				gmo_instruction_grp.create_instance_from_instance (
					P_API_VERSION => 1.0,
					X_RETURN_STATUS => l_return_status,
					X_MSG_COUNT => l_msg_count,
					X_MSG_DATA => l_msg_data,
					P_SOURCE_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_OPERATION,
					P_SOURCE_ENTITY_KEY => l_batchstep_id,
					P_TARGET_ENTITY_KEY => l_batchstep_id,
					P_INSTRUCTION_TYPE => GMO_CONSTANTS_GRP.VBATCH_INSTRUCTION_TYPE,
					X_INSTRUCTION_SET_ID => l_instruction_set_id
				);
				if l_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS THEN
					RAISE instruction_create_err;
				end if;
			end if;


			open c_get_step_material;
			loop
			fetch c_get_step_material into l_material_detail_id;
			exit when c_get_step_material%NOTFOUND;
				
				gmo_instruction_grp.NULLIFY_INSTR_FOR_ENTITY (
					P_API_VERSION => 1.0,
					X_RETURN_STATUS => l_return_status,
					X_MSG_COUNT => l_msg_count,
					X_MSG_DATA => l_msg_data,
					P_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_MATERIAL,
					P_ENTITY_KEY => l_material_detail_id,
					P_INSTRUCTION_TYPE => GMO_CONSTANTS_GRP.VBATCH_INSTRUCTION_TYPE
				);
			
				if l_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS THEN
					RAISE instruction_nullify_err;
				end if;
				
				IF (P_TASK = GMO_CONSTANTS_GRP.ACTION_UNRELEASE) THEN
					gmo_instruction_grp.create_instance_from_instance (
						P_API_VERSION => 1.0,
						X_RETURN_STATUS => l_return_status,
						X_MSG_COUNT => l_msg_count,
						X_MSG_DATA => l_msg_data,
						P_SOURCE_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_MATERIAL,
						P_SOURCE_ENTITY_KEY => l_material_detail_id,
						P_TARGET_ENTITY_KEY => l_material_detail_id,
						P_INSTRUCTION_TYPE => GMO_CONSTANTS_GRP.VBATCH_INSTRUCTION_TYPE,
						X_INSTRUCTION_SET_ID => l_instruction_set_id
					);
					if l_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS THEN
						RAISE instruction_create_err;
					end if;
				end if;

			end loop;
		        close c_get_step_material;

			
			open c_get_step_activities;
			loop
			fetch c_get_step_activities into l_batchstep_activity_id;
			exit when c_get_step_activities%NOTFOUND;
				
				gmo_instruction_grp.NULLIFY_INSTR_FOR_ENTITY (
					P_API_VERSION => 1.0,
					X_RETURN_STATUS => l_return_status,
					X_MSG_COUNT => l_msg_count,
					X_MSG_DATA => l_msg_data,
					P_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_ACTIVITY,
					P_ENTITY_KEY => l_batchstep_activity_id,
					P_INSTRUCTION_TYPE => GMO_CONSTANTS_GRP.VBATCH_INSTRUCTION_TYPE
				);
			
				if l_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS THEN
					RAISE instruction_nullify_err;
				end if;
				
				IF (P_TASK = GMO_CONSTANTS_GRP.ACTION_UNRELEASE) THEN
					gmo_instruction_grp.create_instance_from_instance (
						P_API_VERSION => 1.0,
						X_RETURN_STATUS => l_return_status,
						X_MSG_COUNT => l_msg_count,
						X_MSG_DATA => l_msg_data,
						P_SOURCE_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_ACTIVITY,
						P_SOURCE_ENTITY_KEY => l_batchstep_activity_id,
						P_TARGET_ENTITY_KEY => l_batchstep_activity_id,
						P_INSTRUCTION_TYPE => GMO_CONSTANTS_GRP.VBATCH_INSTRUCTION_TYPE,
						X_INSTRUCTION_SET_ID => l_instruction_set_id
					);
					if l_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS THEN
						RAISE instruction_create_err;
					end if;
				end if;

			end loop;
		        close c_get_step_activities;
		        
		        open c_get_step_resources;
			loop
			fetch c_get_step_resources into l_batchstep_resource_id;
			exit when c_get_step_resources%NOTFOUND;
				
				gmo_instruction_grp.NULLIFY_INSTR_FOR_ENTITY (
					P_API_VERSION => 1.0,
					X_RETURN_STATUS => l_return_status,
					X_MSG_COUNT => l_msg_count,
					X_MSG_DATA => l_msg_data,
					P_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_RESOURCE,
					P_ENTITY_KEY => l_batchstep_resource_id,
					P_INSTRUCTION_TYPE => GMO_CONSTANTS_GRP.VBATCH_INSTRUCTION_TYPE
				);

				if l_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS THEN
					RAISE instruction_nullify_err;
				end if;

				IF (P_TASK = GMO_CONSTANTS_GRP.ACTION_UNRELEASE) THEN
					gmo_instruction_grp.create_instance_from_instance (
						P_API_VERSION => 1.0,
						X_RETURN_STATUS => l_return_status,
						X_MSG_COUNT => l_msg_count,
						X_MSG_DATA => l_msg_data,
						P_SOURCE_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_RESOURCE,
						P_SOURCE_ENTITY_KEY => l_batchstep_resource_id,
						P_TARGET_ENTITY_KEY => l_batchstep_resource_id,
						P_INSTRUCTION_TYPE => GMO_CONSTANTS_GRP.VBATCH_INSTRUCTION_TYPE,
						X_INSTRUCTION_SET_ID => l_instruction_set_id
					);
					if l_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS THEN
						RAISE instruction_create_err;
					end if;
				end if;

			end loop;
		        close c_get_step_resources;
		
		ELSIF (P_TASK = GMO_CONSTANTS_GRP.ACTION_DELETE_ACTIVITY) THEN
			
			l_batchstep_activity_id := P_TASK_ATTRIBUTE;
			gmo_instruction_grp.NULLIFY_INSTR_FOR_ENTITY (
				P_API_VERSION => 1.0,
				X_RETURN_STATUS => l_return_status,
				X_MSG_COUNT => l_msg_count,
				X_MSG_DATA => l_msg_data,
				P_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_ACTIVITY,
				P_ENTITY_KEY => l_batchstep_activity_id,
				P_INSTRUCTION_TYPE => GMO_CONSTANTS_GRP.VBATCH_INSTRUCTION_TYPE
			);

			if l_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS THEN
				RAISE instruction_nullify_err;
			end if;

			open c_get_activity_resources;
			loop
			fetch c_get_activity_resources into l_batchstep_resource_id;
			exit when c_get_activity_resources%NOTFOUND;

				gmo_instruction_grp.NULLIFY_INSTR_FOR_ENTITY (
					P_API_VERSION => 1.0,
					X_RETURN_STATUS => l_return_status,
					X_MSG_COUNT => l_msg_count,
					X_MSG_DATA => l_msg_data,
					P_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_RESOURCE,
					P_ENTITY_KEY => l_batchstep_resource_id,
					P_INSTRUCTION_TYPE => GMO_CONSTANTS_GRP.VBATCH_INSTRUCTION_TYPE
				);

				if l_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS THEN
					RAISE instruction_nullify_err;
				end if;

			end loop;
		        close c_get_activity_resources;
		
		ELSIF (P_TASK = GMO_CONSTANTS_GRP.ACTION_DELETE_RESOURCE) THEN

			l_batchstep_resource_id := P_TASK_ATTRIBUTE;
			gmo_instruction_grp.NULLIFY_INSTR_FOR_ENTITY (
				P_API_VERSION => 1.0,
				X_RETURN_STATUS => l_return_status,
				X_MSG_COUNT => l_msg_count,
				X_MSG_DATA => l_msg_data,
				P_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_RESOURCE,
				P_ENTITY_KEY => l_batchstep_resource_id,
				P_INSTRUCTION_TYPE => GMO_CONSTANTS_GRP.VBATCH_INSTRUCTION_TYPE
			);

			if l_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS THEN
				RAISE instruction_nullify_err;
			end if;

		ELSIF (P_TASK = GMO_CONSTANTS_GRP.ACTION_DELETE_MATERIAL) THEN

			l_material_detail_id := P_TASK_ATTRIBUTE;
			gmo_instruction_grp.NULLIFY_INSTR_FOR_ENTITY (
				P_API_VERSION => 1.0,
				X_RETURN_STATUS => l_return_status,
				X_MSG_COUNT => l_msg_count,
				X_MSG_DATA => l_msg_data,
				P_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_MATERIAL,
				P_ENTITY_KEY => l_material_detail_id,
				P_INSTRUCTION_TYPE => GMO_CONSTANTS_GRP.VBATCH_INSTRUCTION_TYPE
			);

			if l_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS THEN
				RAISE instruction_nullify_err;
			end if;
		END IF;		
		
		
	END IF;

	x_return_status :=  FND_API.G_RET_STS_SUCCESS;

EXCEPTION
	WHEN GMO_DISABLED_ERR THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_SUCCESS;
                FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_DISABLED_ERR');
                FND_MSG_PUB.ADD;
                FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.on_task_action', FALSE);
		end if;
	WHEN GMO_NOT_ENHANCED_PI_ERR THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_SUCCESS;
                FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_NOT_ENHANCED_PI_ERR');
                FND_MSG_PUB.ADD;
                FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.on_task_action', FALSE);
		end if;
	WHEN instruction_nullify_err then
		x_return_status :=  FND_API.G_RET_STS_ERROR;
		FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.on_task_action', FALSE);
		end if;
	WHEN instruction_create_err then
		x_return_status :=  FND_API.G_RET_STS_ERROR;
		FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.on_task_action', FALSE);
		end if;
	WHEN GMO_INVALID_ENTITY_ERR THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_ERROR;
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_INVALID_ENTITY');
                FND_MESSAGE.SET_TOKEN('ENTITY_NAME',P_ENTITY_NAME);
                FND_MESSAGE.SET_TOKEN('ENTITY_KEY',P_ENTITY_KEY);
                FND_MSG_PUB.ADD;
                FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.on_task_action', FALSE);
		end if;
	WHEN OTHERS THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_UNEXP_ERROR;
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_UNEXPECTED_DB_ERR');
		FND_MESSAGE.SET_TOKEN('ERROR_TEXT',SQLERRM);
		FND_MESSAGE.SET_TOKEN('ERROR_CODE', SQLCODE);
		FND_MSG_PUB.ADD;
		FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_UNEXPECTED >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_UNEXPECTED,'gmo.plsql.gmo_vbatch_pvt.on_task_action', FALSE);
		end if;
END ON_TASK_ACTION;


--This procdeure would process the save event of the task.

procedure ON_TASK_SAVE (P_FROM_MODULE IN VARCHAR2,
                        P_ENTITY_NAME IN VARCHAR2,
                        P_ENTITY_KEY IN VARCHAR2,
                        P_TASK IN VARCHAR2,
                        P_TASK_ATTRIBUTE IN VARCHAR2 ,
                        P_INSTRUCTION_ID IN NUMBER ,
                        P_INSTRUCTION_PROCESS_ID IN NUMBER ,
                        P_TASK_IDENTIFIER IN FND_TABLE_OF_VARCHAR2_255,
                        P_TASK_VALUE IN FND_TABLE_OF_VARCHAR2_255,
                        P_TASK_ERECORD IN FND_TABLE_OF_VARCHAR2_255,
                        P_REQUESTER IN NUMBER,
                        X_RETURN_STATUS OUT NOCOPY VARCHAR2,
                        X_MSG_COUNT OUT NOCOPY NUMBER,
                        X_MSG_DATA OUT NOCOPY VARCHAR2)

IS

l_return_status varchar2(100);
l_msg_count number;
l_msg_data varchar2(4000);
task_Acknowledgement_err exception;

BEGIN
	
	IF (NOT IS_GMO_ENABLED) THEN
		RAISE GMO_DISABLED_ERR;
	END IF;

	IF (NOT IS_BATCH_ENHANCED_PI (P_ENTITY_NAME => P_ENTITY_NAME, P_ENTITY_KEY => P_ENTITY_KEY) ) THEN
		RAISE GMO_NOT_ENHANCED_PI_ERR;
	END IF;

	if (FND_LOG.LEVEL_EVENT >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_ON_TASK_SAVE_MSG');
		FND_MESSAGE.SET_TOKEN('FROM_MODULE',P_FROM_MODULE);
		FND_MESSAGE.SET_TOKEN('ENTITY_NAME',P_ENTITY_NAME);
		FND_MESSAGE.SET_TOKEN('ENTITY_KEY',P_ENTITY_KEY);
		FND_MESSAGE.SET_TOKEN('TASK',P_TASK);
		FND_MESSAGE.SET_TOKEN('TASK_ATTRIBUTE',P_TASK_ATTRIBUTE);
		FND_LOG.MESSAGE(FND_LOG.LEVEL_EVENT,'gmo.plsql.gmo_vbatch_pvt.on_task_save', FALSE);
	end if;

	
	IF (P_FROM_MODULE = GMO_CONSTANTS_GRP.FROM_MODULE_PI) THEN
		
		gmo_instruction_grp.SEND_TASK_ACKN (
			P_API_VERSION => 1.0,
			X_RETURN_STATUS => l_return_status,
			X_MSG_COUNT => l_msg_count,
			X_MSG_DATA => l_msg_data,
			P_INSTRUCTION_ID => P_INSTRUCTION_ID,
			P_INSTRUCTION_PROCESS_ID => P_INSTRUCTION_PROCESS_ID,
			P_ENTITY_KEY => P_ENTITY_KEY,
			P_TASK_ERECORD_ID => P_TASK_ERECORD,
			P_TASK_IDENTIFIER => P_TASK_IDENTIFIER,
			P_TASK_VALUE => P_TASK_VALUE,
			P_DISABLE_TASK => GMO_CONSTANTS_GRP.DISABLE_TASK_NO
		);
		

		if l_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS THEN
			RAISE task_Acknowledgement_err;
		end if;

	END IF;

	X_RETURN_STATUS := FND_API.G_RET_STS_SUCCESS;

EXCEPTION
	WHEN GMO_NOT_ENHANCED_PI_ERR THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_SUCCESS;
                FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_NOT_ENHANCED_PI_ERR');
                FND_MSG_PUB.ADD;
                FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.on_task_save', FALSE);
		end if;
	WHEN task_Acknowledgement_err then
		x_return_status :=  FND_API.G_RET_STS_ERROR;
		FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.on_task_save', FALSE);
		end if;
	WHEN GMO_DISABLED_ERR THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_SUCCESS;
                FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_DISABLED_ERR');
                FND_MSG_PUB.ADD;
                FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.on_task_save', FALSE);
		end if;
	WHEN OTHERS THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_UNEXP_ERROR;
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_UNEXPECTED_DB_ERR');
		FND_MESSAGE.SET_TOKEN('ERROR_TEXT',SQLERRM);
		FND_MESSAGE.SET_TOKEN('ERROR_CODE', SQLCODE);
		FND_MSG_PUB.ADD;
		FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_UNEXPECTED >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_UNEXPECTED,'gmo.plsql.gmo_vbatch_pvt.on_task_save', FALSE);
		end if;

END ON_TASK_SAVE;


--This procdeure would check if the step is locked or not

procedure GET_ENTITY_LOCK_STATUS (P_ENTITY_NAME IN VARCHAR2,
				  P_ENTITY_KEY IN VARCHAR2,
				  P_REQUESTER IN NUMBER,
				  X_LOCK_STATUS OUT NOCOPY VARCHAR2, 
				  X_LOCKED_BY_STATUS OUT NOCOPY VARCHAR2,
				  X_LOCK_ALLOWED OUT NOCOPY VARCHAR2,
				  X_RETURN_STATUS OUT NOCOPY VARCHAR2,
				  X_MSG_COUNT OUT NOCOPY NUMBER,
                        	  X_MSG_DATA OUT NOCOPY VARCHAR2)

IS

l_batchstep_id number;
l_count number;
l_step_status number;
cursor is_step_locked is select count(*) from gmo_batch_step_lock_details where batchstep_id = l_batchstep_id;
cursor is_step_locked_by is select count(*) from gmo_batch_step_lock_details where batchstep_id = l_batchstep_id and lock_requester = P_REQUESTER;

cursor get_step_id_for_resource is select batchstep_id from gme_batch_step_resources where batchstep_resource_id = p_entity_key;
cursor get_step_id_for_activity is select batchstep_id from gme_batch_step_activities where batchstep_activity_id = p_entity_key;
cursor get_step_id_for_material is select batchstep_id from gme_batch_step_items where material_detail_id = p_entity_key;

cursor get_step_detail is select step_status from gme_batch_steps where batchstep_id = l_batchstep_id;

no_step_found_err exception;

BEGIN
	
	IF (NOT IS_GMO_ENABLED) THEN
		RAISE GMO_DISABLED_ERR;
	END IF;

	IF (NOT IS_BATCH_ENHANCED_PI (P_ENTITY_NAME => P_ENTITY_NAME, P_ENTITY_KEY => P_ENTITY_KEY) ) THEN
		RAISE GMO_NOT_ENHANCED_PI_ERR;
	END IF;
	
	IF (P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_OPERATION) THEN
		l_batchstep_id := P_ENTITY_KEY;
	ELSIF (P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_ACTIVITY) THEN
		open get_step_id_for_activity;
		fetch get_step_id_for_activity into l_batchstep_id;
		close get_step_id_for_activity;
	ELSIF (P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_RESOURCE) THEN
		open get_step_id_for_resource;
		fetch get_step_id_for_resource into l_batchstep_id;
		close get_step_id_for_resource;
	ELSIF (P_ENTITY_NAME = GMO_CONSTANTS_GRP.ENTITY_MATERIAL) THEN
		open get_step_id_for_material;
		fetch get_step_id_for_material into l_batchstep_id;
		close get_step_id_for_material;
	ELSE
		RAISE gmo_invalid_entity_err;
	END IF;
	
	IF (l_batchstep_id is null) THEN
		RAISE no_step_found_err;
	END IF;
	
	open is_step_locked;
	fetch is_step_locked into l_count;
	close is_step_locked;
	
	IF (l_count = 0) THEN
		X_LOCK_STATUS := GMO_CONSTANTS_GRP.NO;
	ELSE
		X_LOCK_STATUS := GMO_CONSTANTS_GRP.YES;
	END IF;

	open is_step_locked_by;
	fetch is_step_locked_by into l_count;
	close is_step_locked_by;
	
	IF (l_count = 0) THEN
		X_LOCKED_BY_STATUS := GMO_CONSTANTS_GRP.NO;
	ELSE
		X_LOCKED_BY_STATUS := GMO_CONSTANTS_GRP.YES;
	END IF;
	
	open get_step_detail;
	fetch get_step_detail into l_step_status;
	close get_step_detail;

	IF (l_step_status = GMO_CONSTANTS_GRP.STEP_WIP_STATUS and X_LOCKED_BY_STATUS <> GMO_CONSTANTS_GRP.YES and X_LOCK_STATUS <> GMO_CONSTANTS_GRP.YES) THEN
		X_LOCK_ALLOWED := GMO_CONSTANTS_GRP.YES;
	ELSE
		X_LOCK_ALLOWED := GMO_CONSTANTS_GRP.NO;
	END IF;

	if (FND_LOG.LEVEL_EVENT >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_ENTITY_LOCK_MSG');
		FND_MESSAGE.SET_TOKEN('ENTITY_NAME',P_ENTITY_NAME);
		FND_MESSAGE.SET_TOKEN('ENTITY_KEY',P_ENTITY_KEY);
		FND_MESSAGE.SET_TOKEN('REQUESTER',P_REQUESTER);
		FND_MESSAGE.SET_TOKEN('LOCK_STATUS',X_LOCK_STATUS);
		FND_MESSAGE.SET_TOKEN('LOCKED_BY_STATUS',X_LOCKED_BY_STATUS);
		FND_MESSAGE.SET_TOKEN('LOCK_ALLOWED',X_LOCK_ALLOWED);
		FND_LOG.MESSAGE(FND_LOG.LEVEL_EVENT,'gmo.plsql.gmo_vbatch_pvt.get_entity_lock_status', FALSE);
	end if;
	
	X_RETURN_STATUS := FND_API.G_RET_STS_SUCCESS;

EXCEPTION
	WHEN GMO_NOT_ENHANCED_PI_ERR THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_SUCCESS;
                FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_NOT_ENHANCED_PI_ERR');
                FND_MSG_PUB.ADD;
                FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.get_entity_lock_status', FALSE);
		end if;
	WHEN GMO_DISABLED_ERR THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_SUCCESS;
                FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_DISABLED_ERR');
                FND_MSG_PUB.ADD;
                FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.get_entity_lock_status', FALSE);
		end if;
	WHEN NO_STEP_FOUND_ERR THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_ERROR;
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_INVALID_ENTITY');
		FND_MESSAGE.SET_TOKEN('ENTITY_NAME',P_ENTITY_NAME);
		FND_MESSAGE.SET_TOKEN('ENTITY_KEY',P_ENTITY_KEY);
		FND_MSG_PUB.ADD;
		FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.get_entity_lock_status', FALSE);
		end if;
	WHEN GMO_INVALID_ENTITY_ERR THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_ERROR;
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_INVALID_ENTITY');
                FND_MESSAGE.SET_TOKEN('ENTITY_NAME',P_ENTITY_NAME);
                FND_MESSAGE.SET_TOKEN('ENTITY_KEY',P_ENTITY_KEY);
                FND_MSG_PUB.ADD;
                FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_EXCEPTION >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_EXCEPTION,'gmo.plsql.gmo_vbatch_pvt.get_entity_lock_status', FALSE);
		end if;
	WHEN OTHERS THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_UNEXP_ERROR;
		FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_UNEXPECTED_DB_ERR');
		FND_MESSAGE.SET_TOKEN('ERROR_TEXT',SQLERRM);
		FND_MESSAGE.SET_TOKEN('ERROR_CODE', SQLCODE);
		FND_MSG_PUB.ADD;
		FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_UNEXPECTED >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_UNEXPECTED,'gmo.plsql.gmo_vbatch_pvt.get_entity_lock_status', FALSE);
		end if;
END GET_ENTITY_LOCK_STATUS;

--Bug 5224634: start
procedure GET_BATCHSTEP_NODES ( P_BATCH_ID IN NUMBER,
				P_BATCHSTEP_ID IN NUMBER,
				X_NAME OUT NOCOPY FND_TABLE_OF_VARCHAR2_255,
				X_ENTITY_NAME OUT NOCOPY FND_TABLE_OF_VARCHAR2_255,
				X_ENTITY_KEY OUT NOCOPY FND_TABLE_OF_VARCHAR2_255,
				X_ENTITY_LEVEL OUT NOCOPY FND_TABLE_OF_VARCHAR2_255,
				X_RETURN_STATUS OUT NOCOPY VARCHAR2,
				X_MSG_COUNT OUT NOCOPY NUMBER,
                        	X_MSG_DATA OUT NOCOPY VARCHAR2)
IS

t_name		fnd_table_of_varchar2_255;
t_entity_name	fnd_table_of_varchar2_255;
t_entity_key	fnd_table_of_varchar2_255;
t_entity_level	fnd_table_of_varchar2_255;

l_entity_name varchar2(255);
l_entity_key varchar2(255);

l_batchstep_activity_id number;
l_oprn_no varchar2(255);
l_batchstep_id number;
l_activity varchar2(255);
l_batchstep_resource_id number;
l_resources varchar2(255);
l_material_detail_id number;
l_material_desc varchar2(255);

l_counter number;
l_instr_count number;

cursor c_get_oprn is select a.batchstep_no || '-' || oprn_no, batchstep_id from
gme_batch_steps a, gmd_operations_b b where a.oprn_id = b.oprn_id and
batchstep_id= P_BATCHSTEP_ID and batch_id = P_BATCH_ID;

cursor c_get_activity is select activity, batchstep_activity_id from
gme_batch_step_activities where batchstep_id=P_BATCHSTEP_ID  and batch_id =
P_BATCH_ID;

cursor c_get_resource is select resources, batchstep_resource_id from
gme_batch_step_resources where batchstep_id=P_BATCHSTEP_ID  and batch_id =
P_BATCH_ID and batchstep_activity_id = l_batchstep_activity_id;

cursor c_get_material is select e.Concatenated_segments, a.material_detail_id
from gme_material_details a, gme_batch_step_items b,  MTL_SYSTEM_ITEMS_KFV e
where a.material_detail_id = b.material_detail_id
and (a.inventory_item_id = e.inventory_item_id and a.organization_id =
e.organization_id)
and b.batchstep_id = P_BATCHSTEP_ID and a.batch_id = P_BATCH_ID;


begin

	t_name		:= fnd_table_of_varchar2_255();
	t_entity_name	:= fnd_table_of_varchar2_255();
	t_entity_key	:= fnd_table_of_varchar2_255();
	t_entity_level	:= fnd_table_of_varchar2_255();

	x_name		:= fnd_table_of_varchar2_255();
	x_entity_name	:= fnd_table_of_varchar2_255();
	x_entity_key	:= fnd_table_of_varchar2_255();
	x_entity_level	:= fnd_table_of_varchar2_255();

	l_counter := 0;

	open c_get_oprn;
	fetch c_get_oprn into l_oprn_no, l_batchstep_id;
	close c_get_oprn;

	t_name.extend;
	t_entity_name.extend;
	t_entity_key.extend;
	t_entity_level.extend;
	l_counter := l_counter + 1;

	t_name(l_counter) := l_oprn_no;
	t_entity_name(l_counter) := GMO_CONSTANTS_GRP.ENTITY_OPERATION;
	t_entity_key(l_counter) := l_batchstep_id;
	t_entity_level(l_counter) := '1';



	open c_get_material;
	loop
	fetch c_get_material into l_material_desc, l_material_detail_id;
	exit when c_get_material%NOTFOUND;

		t_name.extend;
		t_entity_name.extend;
		t_entity_key.extend;
		t_entity_level.extend;
		l_counter := l_counter + 1;

		t_name(l_counter) := l_material_desc;
		t_entity_name(l_counter) := GMO_CONSTANTS_GRP.ENTITY_MATERIAL;
		t_entity_key(l_counter) := l_material_detail_id;
		t_entity_level(l_counter) := '2';


	end loop;
	close c_get_material;

	open c_get_activity;
	loop
	fetch c_get_activity into l_activity, l_batchstep_activity_id;
	exit when c_get_activity%NOTFOUND;

		t_name.extend;
		t_entity_name.extend;
		t_entity_key.extend;
		t_entity_level.extend;
		l_counter := l_counter + 1;

		t_name(l_counter) := l_activity;
		t_entity_name(l_counter) := GMO_CONSTANTS_GRP.ENTITY_ACTIVITY;
		t_entity_key(l_counter) := l_batchstep_activity_id;
		t_entity_level(l_counter) := '2';


		open c_get_resource;
		loop
		fetch c_get_resource into l_resources, l_batchstep_resource_id;
		exit when c_get_resource%NOTFOUND;

			t_name.extend;
			t_entity_name.extend;
			t_entity_key.extend;
			t_entity_level.extend;
			l_counter := l_counter + 1;

			t_name(l_counter) := l_resources;
			t_entity_name(l_counter) := GMO_CONSTANTS_GRP.ENTITY_RESOURCE;
			t_entity_key(l_counter) := l_batchstep_resource_id;
			t_entity_level(l_counter) := '3';


		end loop;
		close c_get_resource;



	end loop;
	close c_get_activity;

	l_counter := 0;
	
	for i in 1 .. t_name.count loop
	
		l_entity_name := t_entity_name(i);
		l_entity_key := t_entity_key(i);
	

		x_name.extend;
		x_entity_name.extend;
		x_entity_key.extend;
		x_entity_level.extend;
		l_counter := l_counter + 1;

		x_name(l_counter) := t_name(i);
		x_entity_name(l_counter) := t_entity_name(i);
		x_entity_key(l_counter) := t_entity_key(i);
		x_entity_level(l_counter) := t_entity_level(i);
	
	end loop;
	
	X_RETURN_STATUS := FND_API.G_RET_STS_SUCCESS;

EXCEPTION
	WHEN OTHERS THEN
		X_RETURN_STATUS := FND_API.G_RET_STS_UNEXP_ERROR;
		FND_MESSAGE.SET_NAME('GMO','GET_BATCHSTEP_NODES');
		FND_MESSAGE.SET_TOKEN('ERROR_TEXT',SQLERRM);
		FND_MESSAGE.SET_TOKEN('ERROR_CODE', SQLCODE);
		FND_MSG_PUB.ADD;
		FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_data => x_msg_data);
		if (FND_LOG.LEVEL_UNEXPECTED >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
      			FND_LOG.MESSAGE(FND_LOG.LEVEL_UNEXPECTED,'gmo.plsql.gmo_vbatch_pvt.get_batchstep_nodes', FALSE);
		end if;
END GET_BATCHSTEP_NODES;

--Bug 5224634: end


-- Kapil ME LCF-GMO
-- This procedure instantiates PI for LCF Batches
procedure INSTANTIATE_INSTR_FOR_LCF (P_ENTITY_NAME IN VARCHAR2,
                                   P_ENTITY_KEY IN VARCHAR2,
 				   X_RETURN_STATUS OUT NOCOPY VARCHAR2,
				   X_MSG_COUNT OUT NOCOPY NUMBER,
				   X_MSG_DATA OUT NOCOPY VARCHAR2)
				   IS
				   
l_recipe_id number;
l_batchstep_id number;
l_routingstep_id number;
l_oprn_id number;
l_oprn_line_id number;
l_resources varchar2(16);
l_material_detail_id number;
l_formulaline_id number;
l_inventory_item_id number;
l_batchstep_activity_id number;
l_batchstep_resource_id number;
l_instruction_set_id number;
l_return_status varchar2(100);
l_msg_count number;
l_msg_data varchar2(4000);
instruction_create_err exception;

l_entity_display_name varchar2(255);
l_invalid_instr fnd_table_of_varchar2_255;
l_invalid_instr_master fnd_table_of_varchar2_255;
l_invalid_entity_master fnd_table_of_varchar2_255;

j binary_integer;
k binary_integer;
l_attribute_Status varchar2(1);
INVALID_ATTRIBUTE_ASSOC_ERR exception;

cursor c_get_steps is
  select
    batchstep_id,
    routingstep_id,
    a.oprn_id,
    batchstep_no || '-' || b.oprn_no
  from gme_batch_steps a, gmd_operations_b b where  a.oprn_id = b.oprn_id and batch_id = p_entity_key;

cursor c_get_activities is
  select batchstep_activity_id,
        routingstep_id,
        oprn_line_id ,
	activity
  from  gme_batch_step_activities gbsa,
        gme_batch_steps gbs
  where gbsa.batchstep_id = gbs.batchstep_id and gbsa.batch_id = p_entity_key;

cursor c_get_resources is
  select batchstep_resource_id,
         routingstep_id,
         oprn_line_id,
         resources ,
         gbsa.activity || '-' || resources
  from   gme_batch_step_resources gbsr,
         gme_batch_step_Activities gbsa,
         gme_batch_steps gbs
  where  gbsa.batchstep_activity_id = gbsr.batchstep_activity_id
  and    gbsa.batchstep_id = gbs.batchstep_id and gbsr.batch_id = p_entity_key;

	
    BEGIN

	j := 0;
	k := 0;
	l_invalid_entity_master := fnd_table_of_varchar2_255();
	l_invalid_instr_master := fnd_table_of_varchar2_255();

	open c_get_steps;
	loop
	fetch c_get_steps into l_batchstep_id, l_routingstep_id, l_oprn_id, l_entity_display_name;
	exit when c_get_steps%NOTFOUND;

		gmo_instruction_grp.CREATE_INSTANCE_FROM_DEFN (
			P_API_VERSION => 1.0,
			X_RETURN_STATUS => l_return_status,
			X_MSG_COUNT => l_msg_count,
			X_MSG_DATA => l_msg_data,
			P_DEFINITION_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_OPERATION,
			P_DEFINITION_ENTITY_KEY =>  l_routingstep_id || GMO_CONSTANTS_GRP.ENTITY_KEY_SEPARATOR || l_oprn_id,
			P_INSTANCE_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_OPERATION,
			P_INSTANCE_ENTITY_KEY => l_batchstep_id,
			P_INSTRUCTION_TYPE => GMO_CONSTANTS_GRP.VBATCH_INSTRUCTION_TYPE,
			X_INSTRUCTION_SET_ID => l_instruction_set_id
		);

	 	if l_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS THEN
			RAISE instruction_create_err;
		end if;
		update_task_attribute
		(
			P_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_OPERATION,
			P_ENTITY_KEY =>  l_batchstep_id,
			P_INSTRUCTION_SET_ID => l_instruction_set_id,
			X_ATTRIBUTE_STATUS => l_attribute_status,
			X_INVALID_INSTR => l_invalid_instr
		);
		if (l_attribute_status <> 'S' and l_invalid_instr.count > 0) then
			for i in 1 .. l_invalid_instr.count loop
				j := j + 1;
				l_invalid_instr_master.extend;
				l_invalid_instr_master (j) := l_invalid_instr(i);
			end loop;
			k := k+1;
			l_invalid_entity_master.extend;
			l_invalid_entity_master (k) := l_entity_display_name;
		end if;

		if (FND_LOG.LEVEL_EVENT >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
			FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_INSTR_CREATE_MSG');
			FND_MESSAGE.SET_TOKEN('ENTITY_NAME',P_ENTITY_NAME);
			FND_MESSAGE.SET_TOKEN('ENTITY_KEY',P_ENTITY_KEY);
			FND_MESSAGE.SET_TOKEN('INSTR_ENTITY_NAME','STEP');
			FND_MESSAGE.SET_TOKEN('INSTR_ENTITY_KEY',l_batchstep_id);
			FND_LOG.MESSAGE(FND_LOG.LEVEL_EVENT,'gmo.plsql.gmo_vbatch_pvt.instantiate_advanced_pi', FALSE);
		end if;

	end loop;
	close c_get_steps;

	open c_get_activities;
        loop
        fetch c_get_activities into l_batchstep_activity_id, l_routingstep_id, l_oprn_line_id, l_entity_display_name;
        exit when c_get_activities%NOTFOUND;

                gmo_instruction_grp.CREATE_INSTANCE_FROM_DEFN (
			P_API_VERSION => 1.0,
			X_RETURN_STATUS => l_return_status,
			X_MSG_COUNT => l_msg_count,
			X_MSG_DATA => l_msg_data,
                        P_DEFINITION_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_ACTIVITY,
                        P_DEFINITION_ENTITY_KEY =>  l_routingstep_id || GMO_CONSTANTS_GRP.ENTITY_KEY_SEPARATOR || l_oprn_line_id,
                        P_INSTANCE_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_ACTIVITY,
                        P_INSTANCE_ENTITY_KEY => l_batchstep_activity_id,
                        P_INSTRUCTION_TYPE => GMO_CONSTANTS_GRP.VBATCH_INSTRUCTION_TYPE,
                        X_INSTRUCTION_SET_ID => l_instruction_set_id
                );

                if l_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS THEN
                        RAISE instruction_create_err;
                end if;

		update_task_attribute
		(
			P_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_ACTIVITY,
			P_ENTITY_KEY =>  l_batchstep_activity_id,
			P_INSTRUCTION_SET_ID => l_instruction_set_id,
			X_ATTRIBUTE_STATUS => l_attribute_status,
			X_INVALID_INSTR => l_invalid_instr
		);
		if (l_attribute_status <> 'S' and l_invalid_instr.count > 0) then
			for i in 1 .. l_invalid_instr.count loop
				j := j + 1;
				l_invalid_instr_master.extend;
				l_invalid_instr_master (j) := l_invalid_instr(i);
			end loop;
			k := k+1;
			l_invalid_entity_master.extend;
			l_invalid_entity_master (k) := l_entity_display_name;
		end if;

		if (FND_LOG.LEVEL_EVENT >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
			FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_INSTR_CREATE_MSG');
			FND_MESSAGE.SET_TOKEN('ENTITY_NAME',P_ENTITY_NAME);
			FND_MESSAGE.SET_TOKEN('ENTITY_KEY',P_ENTITY_KEY);
			FND_MESSAGE.SET_TOKEN('INSTR_ENTITY_NAME','ACTIVITY');
			FND_MESSAGE.SET_TOKEN('INSTR_ENTITY_KEY',l_batchstep_activity_id);
			FND_LOG.MESSAGE(FND_LOG.LEVEL_EVENT,'gmo.plsql.gmo_vbatch_pvt.instantiate_advanced_pi', FALSE);
		end if;

        end loop;
        close c_get_activities;

	open c_get_resources;
        loop
        fetch c_get_resources into l_batchstep_resource_id, l_routingstep_id, l_oprn_line_id, l_resources, l_entity_display_name;
        exit when c_get_resources%NOTFOUND;

                gmo_instruction_grp.CREATE_INSTANCE_FROM_DEFN (
			P_API_VERSION => 1.0,
			X_RETURN_STATUS => l_return_status,
			X_MSG_COUNT => l_msg_count,
			X_MSG_DATA => l_msg_data,
                        P_DEFINITION_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_RESOURCE,
                        P_DEFINITION_ENTITY_KEY => l_routingstep_id || GMO_CONSTANTS_GRP.ENTITY_KEY_SEPARATOR || l_oprn_line_id || GMO_CONSTANTS_GRP.ENTITY_KEY_SEPARATOR || l_resources,
                        P_INSTANCE_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_RESOURCE,
                        P_INSTANCE_ENTITY_KEY => l_batchstep_resource_id,
                        P_INSTRUCTION_TYPE => GMO_CONSTANTS_GRP.VBATCH_INSTRUCTION_TYPE,
                        X_INSTRUCTION_SET_ID => l_instruction_set_id
                );

                if l_RETURN_STATUS <> FND_API.G_RET_STS_SUCCESS THEN
                        RAISE instruction_create_err;
                end if;

		update_task_attribute
		(
			P_ENTITY_NAME => GMO_CONSTANTS_GRP.ENTITY_RESOURCE,
			P_ENTITY_KEY =>  l_batchstep_resource_id,
			P_INSTRUCTION_SET_ID => l_instruction_set_id,
			X_ATTRIBUTE_STATUS => l_attribute_status,
			X_INVALID_INSTR => l_invalid_instr
		);
		if (l_attribute_status <> 'S' and l_invalid_instr.count > 0) then
			for i in 1 .. l_invalid_instr.count loop
				j := j + 1;
				l_invalid_instr_master.extend;
				l_invalid_instr_master (j) := l_invalid_instr(i);
			end loop;
			k := k+1;
			l_invalid_entity_master.extend;
			l_invalid_entity_master (k) := l_entity_display_name;
		end if;

		if (FND_LOG.LEVEL_EVENT >= FND_LOG.G_CURRENT_RUNTIME_LEVEL) then
			FND_MESSAGE.SET_NAME('GMO','GMO_VBATCH_INSTR_CREATE_MSG');
			FND_MESSAGE.SET_TOKEN('ENTITY_NAME',P_ENTITY_NAME);
			FND_MESSAGE.SET_TOKEN('ENTITY_KEY',P_ENTITY_KEY);
			FND_MESSAGE.SET_TOKEN('INSTR_ENTITY_NAME','RESOURCE');
			FND_MESSAGE.SET_TOKEN('INSTR_ENTITY_KEY',l_batchstep_resource_id);
			FND_LOG.MESSAGE(FND_LOG.LEVEL_EVENT,'gmo.plsql.gmo_vbatch_pvt.instantiate_advanced_pi', FALSE);
		end if;

        end loop;
        close c_get_resources;

	if (l_invalid_entity_master.count > 0)  then
		RAISE INVALID_ATTRIBUTE_ASSOC_ERR;
	end if;

	x_return_status :=  FND_API.G_RET_STS_SUCCESS;

    END INSTANTIATE_INSTR_FOR_LCF;	


END GMO_VBATCH_PVT;
/
commit;
EXIT;
