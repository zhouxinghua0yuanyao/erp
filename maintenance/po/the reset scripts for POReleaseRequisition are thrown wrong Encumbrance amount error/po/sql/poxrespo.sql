REM dbdrv: none
        /*=======================================================================+
 	 |  Copyright (c) 2009, 2016 Oracle Corporation Redwood Shores, California, USA|
 	 |                            All rights reserved.                       |
 	 +=======================================================================*/
 
 /* $Header: poxrespo.sql 120.3.12020000.7 2016/02/27 01:14:45 pla ship $ */
SET SERVEROUTPUT ON
SET VERIFY OFF;


/* PLEASE READ NOTE 390023.1 CAREFULLY BEFORE EXECUTING THIS SCRIPT.

 This script will:
* reset the document to incomplete/requires reapproval status.
* delete/update action history as desired (refere note 390023.1 for more details).
* abort all the related workflows 

* If there is a distribution with wrong encumbrance amount related to this PO, 
* it will: skip the reset action on the document.
*/

set serveroutput on size 100000
prompt
prompt
accept sql_po_number prompt 'Please enter the PO number to reset : ';
accept sql_org_id default NULL prompt 'Please enter the organization id to which the PO belongs (Default NULL) : ';
accept delete_act_hist prompt 'Do you want to delete the action history since the last approval ? (Y/N) ';
prompt


DECLARE

/* select only the POs which are in preapproved, in process state and are not finally closed 
   cancelled */
   
CURSOR potoreset(po_number varchar2, x_org_id number) is
SELECT wf_item_type, wf_item_key, po_header_id, segment1,
       revision_num, type_lookup_code,approved_date,ame_transaction_type
FROM po_headers_all
WHERE segment1 = po_number
and NVL(org_id,-99) = NVL(x_org_id,-99)
-- bug 5015493: Need to allow reset of blankets and PPOs also.
-- and type_lookup_code = 'STANDARD'
and authorization_status IN ('IN PROCESS', 'PRE-APPROVED')
and NVL(cancel_flag, 'N') = 'N'
and NVL(closed_code, 'OPEN') <> 'FINALLY_CLOSED';

/* select the max sequence number with NULL action code */

CURSOR maxseq(id number, subtype po_action_history.object_sub_type_code%type) is
SELECT nvl(max(sequence_num), 0)
FROM   po_action_history
WHERE  object_type_code IN ('PO', 'PA')
AND    object_sub_type_code = subtype
AND    object_id = id
AND    action_code is NULL;

/* select the max sequence number with submit action */

CURSOR poaction(id number, subtype po_action_history.object_sub_type_code%type) is
SELECT nvl(max(sequence_num), 0)
FROM   po_action_history
WHERE  object_type_code IN ('PO', 'PA')
AND    object_sub_type_code = subtype
AND    object_id = id
AND    action_code = 'SUBMIT';

cursor wfstoabort(st_item_type varchar2,st_item_key varchar2) is
select level,item_type,item_key,end_date
from wf_items
start with
    item_type = st_item_type and
    item_key =  st_item_key
connect by
    prior item_type = parent_item_type and
    prior item_key = parent_item_key
order by level desc;

wf_rec wfstoabort%ROWTYPE;

submitseq po_action_history.sequence_num%type;
nullseq po_action_history.sequence_num%type;

 x_organization_id number ;
 x_po_number varchar2(20);
 po_enc_flag varchar2(1);
 x_open_notif_exist varchar2(1);
 po_reset potoreset%ROWTYPE;
 
 x_progress varchar2(500);
 x_cont varchar2(10);
 x_active_wf_exists varchar2(1);
 l_delete_act_hist varchar2(1);
 l_change_req_exists varchar2(1);
 l_res_seq po_action_history.sequence_num%TYPE;
 l_sub_res_seq po_action_history.sequence_num%TYPE;
 l_res_act po_action_history.action_code%TYPE;
 l_del_res_hist varchar2(1);
 

 /* For encumbrance actions */
 
 NAME_ALREADY_USED EXCEPTION;
 PRAGMA Exception_Init(NAME_ALREADY_USED,-955);
 X_STMT VARCHAR2(2000);
 disallow_script VARCHAR2(1);
 
 l_purch_encumbrance_flag varchar2(1);

/* Bug#21531821 start */

 l_return_status              VARCHAR2(1);
 l_po_dist_id_tbl             PO_TBL_NUMBER;
 l_sequence_num_tbl           PO_TBL_NUMBER;
 l_amount_to_round_tbl        PO_TBL_NUMBER;
 l_exchange_rate_tbl          PO_TBL_NUMBER;
 l_cur_precision_foreign_tbl  PO_TBL_NUMBER;
 l_min_acct_unit_foreign_tbl  PO_TBL_NUMBER;
 l_cur_precision_func_tbl     PO_TBL_NUMBER;
 l_min_acct_unit_func_tbl     PO_TBL_NUMBER;
 l_round_only_flag_tbl        PO_TBL_VARCHAR1;  --bug 3568671
 l_amount_result_tbl          PO_TBL_NUMBER;
 l_encumbered_amount_tbl      PO_TBL_NUMBER;

 l_planned_po_count           NUMBER;
 g_DEF_BATCH_SIZE             NUMBER := 5000;

CURSOR c_check_enc_amt( c_po_header_id  NUMBER)
IS 
SELECT
  d.po_distribution_id
,  0
,  DECODE(  s.matching_basis
         ,  'AMOUNT', Nvl(d.amount_ordered,0)+NVL(d.NONRECOVERABLE_TAX,0)
         , s.PRICE_OVERRIDE * d.QUANTITY_ORDERED + NVL(d.NONRECOVERABLE_TAX,0)
)
,  nvl(d.encumbered_amount, 0)
,  nvl(d.rate,1)
,  fc1.precision
,  fc1.MINIMUM_ACCOUNTABLE_UNIT
,  fc.precision
,  fc.MINIMUM_ACCOUNTABLE_UNIT
,  'N'
FROM   po_headers_all h,
       po_lines_all l,
       po_line_locations_all s,
       po_distributions_all d,
       fnd_currencies fc1,
       gl_ledgers g, fnd_currencies fc         --BUG 19254890
WHERE  s.line_location_id = d.line_location_id
  AND l.po_line_id = s.po_line_id
  AND h.po_header_id = d.po_header_id
  AND d.po_header_id = c_po_header_id
  AND d.set_of_books_id = g.ledger_id
  AND g.currency_code = fc.currency_code  --BUG 19254890
  AND fc1.currency_code = h.currency_code
  AND Nvl(d.encumbered_flag, 'N') = 'Y'
  AND Nvl(s.cancel_flag, 'N') = 'N'
  AND Nvl(s.closed_code, 'OPEN') <> 'FINALLY CLOSED'
  AND Nvl(d.prevent_encumbrance_flag, 'N') = 'N'
  AND d.budget_account_id IS NOT NULL
  AND Nvl(d.amount_changed_flag, 'N') = 'N'    -- BUG 21451256
  AND Nvl(s.shipment_type,'BLANKET') = 'STANDARD';

/*Bug#21531821 end */

BEGIN

select '&delete_act_hist'
	into l_delete_act_hist
	from dual;

select &sql_org_id
  into x_organization_id
  from dual;

select '&sql_po_number'
  into x_po_number
  from dual;


x_progress := '010: start';

   begin
   select 'Y'
    into x_open_notif_exist
    from dual
    where exists (select 'open notifications'
		    from wf_item_activity_statuses wias,
			 wf_notifications wfn,
			 po_headers_all poh
		    where wias.notification_id is not null
		      and wias.notification_id = wfn.group_id
		      and wfn.status = 'OPEN'
		      and wias.item_type = poh.wf_item_type  --'POAPPRV' Bug 22004029: to leverage the po reset script to accommodate POAPPAME item type
		      and wias.item_key = poh.wf_item_key
		      and NVL(poh.org_id,-99) = NVL(x_organization_id,-99)
		      and poh.segment1=x_po_number
		      and poh.authorization_status IN ('IN PROCESS', 'PRE-APPROVED'));
   exception 
   when NO_DATA_FOUND then
     null;
   end;
		      
x_progress := '020: selected open notif';

if (x_open_notif_exist = 'Y') then
   dbms_output.put_line('  ');
   dbms_output.put_line('An Open notification exists for this document, you may want to use the notification to process this document. Do not commit if you wish to use the notification');
end if;   

begin
select 'Y'
  into l_change_req_exists
  from dual
  where exists (select 'po with change request'
  		  from po_headers_all h
		  where h.segment1 = x_po_number
		    and nvl(h.org_id, -99) = NVL(x_organization_id, -99)
		    and h.change_requested_by in ('REQUESTER', 'SUPPLIER'));
exception 
   when NO_DATA_FOUND then
     null;
end;

if (l_change_req_exists = 'Y') then
   dbms_output.put_line('  ');
   dbms_output.put_line('ATTENTION !!! There is an open change request against this PO. You should respond to the notification for the same.');
   return;
--   dbms_output.put_line('If you are running this script unaware of the change request, Please ROLLBACK');
end if;
		      
open potoreset(x_po_number, x_organization_id);

fetch potoreset into po_reset;
if potoreset%NOTFOUND then 
   dbms_output.put_line('No PO with PO Number '||x_po_number ||
   			' exists in org '||to_char(x_organization_id) 
			|| ' which requires to be reset');
   return;
end if;
close potoreset;

 x_progress := '030 checking enc action ';

 -- If there exists any open shipment with one of its distributions reserved, then
 -- 1. For a Standard PO, check whether the present Encumbrance amount on the distribution
 --    is correct or not. If its not correct do not reset the document.
 -- 2. For a Blanket PO (irrespective of Encumbrance enabled or not), reset the document.
 -- 3. For a Planned PO, always do not reset the document.
 disallow_script := 'N'; 

 l_planned_po_count := 0;

 -- check if it is a encumbrance Planned PO
 SELECT count(*)
   INTO  l_planned_po_count 
 FROM po_headers_all h,
      po_lines_all l,
      po_line_locations_all s,
      po_distributions_all d
 WHERE  s.line_location_id = d.line_location_id
   AND l.po_line_id = s.po_line_id
   AND h.po_header_id = d.po_header_id
   AND d.po_header_id = po_reset.po_header_id
   AND Nvl(d.encumbered_flag, 'N') = 'Y'
   AND Nvl(d.prevent_encumbrance_flag, 'N') = 'N'
   AND d.budget_account_id IS NOT NULL
   AND Nvl(d.amount_changed_flag, 'N') = 'N'    -- BUG 21451256
   AND Nvl(s.shipment_type,'BLANKET') = 'PLANNED';

 IF  l_planned_po_count > 0 THEN
    dbms_output.put_line('This PO has encumbrance PLANNED shipments.');
    dbms_output.put_line('Hence this PO can not be reset.');
    return;
 END IF;

 OPEN c_check_enc_amt (po_reset.po_header_id);
 LOOP
   FETCH c_check_enc_amt BULK COLLECT INTO
     l_po_dist_id_tbl
  ,  l_sequence_num_tbl
  ,  l_amount_to_round_tbl
  ,  l_encumbered_amount_tbl
  ,  l_exchange_rate_tbl
  ,  l_cur_precision_foreign_tbl
  ,  l_min_acct_unit_foreign_tbl
  ,  l_cur_precision_func_tbl
  ,  l_min_acct_unit_func_tbl
  ,  l_round_only_flag_tbl
   LIMIT g_DEF_BATCH_SIZE;

   -- 
   -- reassign value to l_sequence_num_tbl to make sure
   -- it is unique and in same order of data fetched.
   -- The sequence_num is used in round_and_convert_currency
   -- to order the result output after rounding and conversion.
   -- see bug 4878973 for more info
   -- 
   FOR i in 1..l_po_dist_id_tbl.COUNT 
     LOOP
       l_sequence_num_tbl(i) := i;
     END LOOP;

    l_return_status := FND_API.G_RET_STS_SUCCESS;

    PO_CORE_S2.round_and_convert_currency(
      x_return_status               => l_return_status
,     p_unique_id_tbl               => l_sequence_num_tbl
,     p_amount_in_tbl               => l_amount_to_round_tbl
,     p_exchange_rate_tbl           => l_exchange_rate_tbl
,     p_from_currency_precision_tbl => l_cur_precision_foreign_tbl
,     p_from_currency_mau_tbl       => l_min_acct_unit_foreign_tbl
,     p_to_currency_precision_tbl   => l_cur_precision_func_tbl
,     p_to_currency_mau_tbl         => l_min_acct_unit_func_tbl
,     p_round_only_flag_tbl         => l_round_only_flag_tbl
,     x_amount_out_tbl              => l_amount_result_tbl
);

    IF l_return_status <> FND_API.G_RET_STS_SUCCESS THEN
      dbms_output.put_line('some error occured '||'PO_CORE_S2.round_and_convert_currency' ||x_progress);
      disallow_script := 'Y';
      EXIT;
    END IF;

    FOR i in 1..l_po_dist_id_tbl.COUNT LOOP
      IF l_amount_result_tbl(i) <> l_encumbered_amount_tbl(i) THEN
        dbms_output.put_line('This PO has at least one distribution with wrong Encumbrance amount.');
        dbms_output.put_line('Hence this PO can not be reset.');
        disallow_script := 'Y';
        EXIT;
      END IF;
    END LOOP;

    IF disallow_script = 'Y' THEN
      EXIT;
    END IF;

    EXIT WHEN c_check_enc_amt%NOTFOUND;

  END LOOP;

  CLOSE c_check_enc_amt;
  
  IF disallow_script = 'Y' THEN
    return;
  END IF;

      dbms_output.put_line('Processing '||po_reset.type_lookup_code
                            ||' PO Number: '
                            ||po_reset.segment1);
      dbms_output.put_line('......................................');
            
      begin 
       select 'Y' 
         into x_active_wf_exists
         from wf_items wfi
        where wfi.item_type = po_reset.wf_item_type
   	  and wfi.item_key = po_reset.wf_item_key
	  and wfi.end_date is null;

      exception
      when NO_DATA_FOUND then
      x_active_wf_exists := 'N';
      end;

      if (x_active_wf_exists = 'Y') then
         dbms_output.put_line('Aborting Workflow...');
         open wfstoabort(po_reset.wf_item_type,po_reset.wf_item_key);
         loop
         fetch wfstoabort into wf_rec;
	 if wfstoabort%NOTFOUND then
	    close wfstoabort;
	    exit;
	 end if;

	 if (wf_rec.end_date is null) then
	 BEGIN
	   WF_Engine.AbortProcess(wf_rec.item_type, wf_rec.item_key);
         EXCEPTION
           WHEN OTHERS THEN
                dbms_output.put_line(' workflow not aborted :'
		||wf_rec.item_type ||'-'||wf_rec.item_key);
                
         END;

	 end if;
	 end loop;
      end if;
 
      dbms_output.put_line('Updating PO Status..'); 
      
    if nvl(po_reset.ame_transaction_type,'N') = 'N' then
    
      UPDATE po_headers_all
         SET authorization_status = decode(po_reset.approved_date, NULL, 'INCOMPLETE',
                                        'REQUIRES REAPPROVAL'),
          wf_item_type = NULL,
          wf_item_key = NULL,
	  approved_flag = decode(po_reset.approved_date, NULL, 'N', 'R') 
       WHERE po_header_id = po_reset.po_header_id;
       
    else
    
       UPDATE po_headers_all
         SET authorization_status = decode(po_reset.approved_date, NULL, 'INCOMPLETE',
                                        'REQUIRES REAPPROVAL'),
          wf_item_type = NULL,
          wf_item_key = NULL,
          ame_approval_id = po_ame_approvals_s.nextval,
	  approved_flag = decode(po_reset.approved_date, NULL, 'N', 'R') 
       WHERE po_header_id = po_reset.po_header_id;
       
    end if;       

      OPEN  maxseq(po_reset.po_header_id, po_reset.type_lookup_code); 
      FETCH maxseq into nullseq;
      CLOSE maxseq;

      OPEN  poaction(po_reset.po_header_id, po_reset.type_lookup_code);
      FETCH poaction into submitseq;
      CLOSE poaction;
      IF nullseq > submitseq THEN
        
	if nvl(l_delete_act_hist,'N') = 'N' then
   	   Update po_action_history
   	      set action_code = 'NO ACTION',
   	          action_date = trunc(sysdate),
   	          note = 'updated by reset script on '||to_char(trunc(sysdate))
           WHERE object_id = po_reset.po_header_id
             AND  object_type_code = decode(po_reset.type_lookup_code,
	     				    'STANDARD','PO',
					    'PLANNED', 'PO', --future plan to enhance for planned PO
					    'PA')
             AND object_sub_type_code = po_reset.type_lookup_code
             AND sequence_num = nullseq
             AND action_code is NULL;
        else

	   Delete po_action_history
	    where object_id = po_reset.po_header_id
	      and object_type_code = decode(po_reset.type_lookup_code,
	     				    'STANDARD','PO',
					    'PLANNED', 'PO', --future plan to enhance for planned PO
					    'PA')
	      and object_sub_type_code = po_reset.type_lookup_code				    
	      and sequence_num >= submitseq
	      and sequence_num <= nullseq;

	end if;
	
      END IF;

      dbms_output.put_line('Done Approval Processing.');
      
 select nvl(purch_encumbrance_flag,'N')
   into l_purch_encumbrance_flag
   from financials_system_params_all fspa
   where NVL(fspa.org_id,-99) = NVL(x_organization_id,-99);
   
   if (l_purch_encumbrance_flag='N') 
      -- bug 5015493 : Need to allow reset for blankets also
      OR (po_reset.type_lookup_code = 'BLANKET') then
   
      if (po_reset.type_lookup_code = 'BLANKET') then
      	dbms_output.put_line('document reset successfully');
      	dbms_output.put_line('If you are using Blanket encumbrance, Please ROLLBACK, else COMMIT');
      else
      	dbms_output.put_line('document reset successfully');
      	dbms_output.put_line('please COMMIT data');
      end if;
      return;
   end if;
 
-- reserve action history stuff
-- check the action history and delete any reserve to submit actions if all the distributions
-- are now unencumbered, this should happen only if we are deleting the action history

if l_delete_act_hist = 'Y' then

   -- first get the last sequence and action code from action history
   begin 
      select sequence_num, action_code
        into l_res_seq, l_res_act
	from po_action_history pah
	WHERE pah.object_id = po_reset.po_header_id
          AND  pah.object_type_code = decode(po_reset.type_lookup_code,
    				    'STANDARD','PO',
				    'PLANNED', 'PO', --future plan to enhance for planned PO
				    'PA')
          AND pah.object_sub_type_code = po_reset.type_lookup_code
	  AND sequence_num in (select max(sequence_num)
	  			from po_action_history pah1
				where pah1.object_id = pah.object_id
          			  AND  pah1.object_type_code =pah.object_type_code
          		          AND  pah1.object_sub_type_code =pah.object_sub_type_code);
   exception
   when TOO_MANY_ROWS then
     dbms_output.put_line('action history needs to be corrected separately ');
   when NO_DATA_FOUND then
     null;
   end;

   -- now if the last action is reserve get the last submit action sequence

   if (l_res_act = 'RESERVE') then
   begin
      select max(sequence_num)
        into l_sub_res_seq
       from  po_action_history pah
      where action_code = 'SUBMIT'
        and  pah.object_id = po_reset.po_header_id
        and  pah.object_type_code = decode(po_reset.type_lookup_code,
    				       'STANDARD','PO',
				       'PLANNED', 'PO', --future plan to enhance for planned PO
				       'PA')
        and pah.object_sub_type_code = po_reset.type_lookup_code;
   exception
   when NO_DATA_FOUND then
     null;
   end;

      -- check if we need to delete the action history, ie. if all the distbributions
      -- are unreserved

      if ((l_sub_res_seq is not null ) and (l_res_seq > l_sub_res_seq)) then

	 begin
         select 'Y'
            into l_del_res_hist
           from dual
           where not exists (select 'encumbered dist'
     			from po_distributions_all pod
			where pod.po_header_id = po_reset.po_header_id
			  and nvl(pod.encumbered_flag,'N') = 'Y'
			  and nvl(pod.prevent_encumbrance_flag,'N')='N');
         exception
	 when NO_DATA_FOUND then
	    l_del_res_hist := 'N';
         end;

         if l_del_res_hist = 'Y' THEN

	    dbms_output.put_line('deleting reservation action history ... ');

            delete po_action_history pah
              where pah.object_id = po_reset.po_header_id
                and  pah.object_type_code = decode(po_reset.type_lookup_code,
       				       'STANDARD','PO',
   				       'PLANNED', 'PO', --future plan to enhance for planned PO
   				       'PA')
                and pah.object_sub_type_code = po_reset.type_lookup_code
                and sequence_num >= l_sub_res_seq
                and sequence_num <= l_res_seq;
         end if;
       
      end if; -- l_res_seq > l_sub_res_seq

   end if;

end if;

EXCEPTION
WHEN OTHERS THEN
  dbms_output.put_line('some exception occured '||sqlerrm||' rolling back'||x_progress);
  rollback;
END;
/
