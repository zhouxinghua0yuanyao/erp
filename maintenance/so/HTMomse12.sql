/* # $Header: HTMomse12.sql 12.0.4                                  20121015    Support $

#+======================================================================================+
#|  Copyright (c) 2002 Oracle Corporation Redwood Shores, California, USA               |
#|   Oracle Support Services.  All rights reserved                                      |
#+======================================================================================+

#========================================================================================
#  PURPOSE:		This script will collect information related to a sales order 
#			transaction and it's Workflow processes. This script should be 
#			run whenever there are problems related to a sales order 
#			transaction, including processes like Pick Release, Shipping, 
#			Invoice interface.
#
#  FILE NAME:		HTMomse12.sql
#  FILE VERSION:	12.0.4
#  PRODUCT: 		Oracle Order Management
#  PRODUCT VERSIONS: 	12.0 and above
#  PLATFORM: 		Generic
#  PARAMETERS:		Order_Number, Header_Id (opt), Line_Id (opt)
#========================================================================================

#  USAGE: 	sqlplus apps/apps @HTMomse12.sql
#
#  This script requires a header_id to be passed in as a parameter ONLY if there are 
#  multiple header_id's for the order requested.  If there is only one header_id returned 
#  after inputting the order number, simply hit enter and this header_id will be assumed.  
#
#  Entering a line_id is only required when you wish to have output for one order line.  
#  Otherwise, hit enter without entering a line_id and all lines will be output in the 
#  script.
#
#  The script writes to an output file named HTMomse12_<ordernumber>.html on DB Server
#  under 'utl_file_dir', this file may be viewed in any HTML browser.
#  
*/

/* Verify that the instance is Release 12, on failure script is aborted. */

variable        apps_version         varchar2(100);


set serveroutput on

BEGIN
 SELECT max(release_name)
   INTO :apps_version
   FROM fnd_product_groups;
 dbms_output.put_line('Applications Version '||:apps_version);
 dbms_output.put_line('Verifying the script is for this version...');
    
 if substr(:apps_version,1,2) < '12' then 
    dbms_output.put_line('***************************************************************');
    dbms_output.put_line('*** This instance is eBusiness Suite version '|| :apps_version);
    dbms_output.put_line('*** This script must run in version 12 or above');
    dbms_output.put_line('***************************************************************');
    dbms_output.put_line('+++');
    dbms_output.put_line('The script for this version can be downloaded from');
    dbms_output.put_line('.Note 133464.1 - HTMOMSE Sales Order Diagnostic Script');
    dbms_output.put_line('+++');
    dbms_output.put_line('ERROR: The script requires eBusiness Version 12 or higher');
    dbms_output.put_line('+++');
    dbms_output.put_line('+++ Script excution will be aborted.');
    dbms_output.put_line('+++');
   else
    dbms_output.put_line('Version is correct, script will continue.');
 end if;

END;
/

WHENEVER SQLERROR EXIT FAILURE ROLLBACK;

BEGIN
 if substr(:apps_version,1,2) < '12' then 
    DBMS_LOCK.Sleep(5);
    raise_application_error(-20001, 'ERROR: The script requires eBusiness Version 12 or higher');
 end if;

END;
/

WHENEVER SQLERROR CONTINUE;

set term off;
/*
#=========================================================================================
# CHANGE HISTORY: 
  This script is based on HTMomse11i version 7.5.1, for the complete
  change history please see HTMomse11i (Note 133464.1)

 From HTMomse11i:
 31-MAR-00 Created                                                   rnmercer 
 12-DEC-00 Major modifications to the look of the report including   rnmercer
           abreviated column names for flags, etc.  A legend cross
           referencing the abreviations is included in the output.
           Got rid of the need to input the org_id. It is set via
           derived data from the header_id.  
 17-MAY-01 Fix the line number to display all 5 numbers              rnmercer
           line.shipment.option.component.service
 05-DEC-01 Changed format of output filename as part of the iTar
           automated scripting initiative.                           rodavid
 13-JUN-02 Modified to change version numbering to match Repository  rodavid
 02-APR-04 Modified to create HTML output file and included basic
 14-APR-04 verifications to help the Analysis                        rijames
 14-APR-04 include table ONT_WF_SKIP_LOG on header and lines         rijames
 01-NOV-07 Changed output function from DBMS_OUTOUT for UTL_FILE,    rijames  Ver. 7.5
           due to size limitations on DBMS_OUTPUT function.
           Output file will now be located on DB Server under 'utl_file_dir'.
           Script will printout the filename and location.
           Added some columns as requested by Support.

 For HTMomse12:
 09-JAN-08 Created as copy of HTMomse11i 7.5.1                       rijames
 11-JAN-08 Modified set environment using rodavid script             rijames
           Exclude IC tables (for OPM) as now OPM information is
           contained on OM/WSH base tables.
 14-JAN-08 Corrected conditions for sections to use UPPER() when     rijames
           answer is provided on lower case.
 04-FEB-08 Move INVentory and OM Interface Flags after the Release   rijames  Ver 12.0.2
           status field.
           Correction on decode of the Hold Entity Code, label for 
           code 'W' is changed to 'Warehouse' (from 'Workflow').
 06-FEB-08 Add outer join to MTL_SALES_ORDERS to print when Order    rijames
           Type definition has been deleted.
           Move Parameters summary to top of output.
           Modify to handle Orders with no lines, only Header 
           information will be printed.
 07-FEB-08 Add Hostname on printout for ouput location.              rijames
 05-MAR-08 Add OPM fields on ORDER_LINES and most MTL queries,       rijames
           MTL_TRANSACTION_LOT_NUMBERS table included on MTL queries.
 06-SEP-11 Correct Header on RA_INTERFACE_TABLE field                rijames  Ver 12.0.3
           RAI.INTERFACE_LINE_ATTRIBUTE6  Line_ID_6 (was Line_ID_5)
 06-SEP-11 Add transaction date and transaction interface id to      rijames
           MTL_TRANSACTIONS_INTERFACE.
 06-SEP-11 Add transaction date to MTL_MATERIAL_TRANSACTIONS_TEMP.   rijames
 06-SEP-11 Add transaction date to MTL_MATERIAL_TRANSACTIONS.        rijames
 06-SEP-11 Add transaction date to MTL_UNIT_TRANSACTIONS.            rijames
 16-SEP-11 Add table MTL_SERIAL_NUMBERS.                             rijames
 22-SEP-11 Add option to All or only first 10 records from tables    rijames
           MTL_UNIT_TRANSACTIONS (MUT), WSH_SERIAL_NUMBERS (WSN)
           and MTL_SERIAL_NUMBERS 
 26-SEP-11 Add multiple Close and Reopen of Output file, so in case  rijames
           of SQL failure only a few tables are not printed.
 29-SEP-11 Add additional fields to ORGANIZATIONS listing.           rijames
 17-OCT-11 Add 'SERIAL NUMBER Details' on Parameters Listing         rijames
 17-OCT-11 Modify title for column Transaction_Interface_Id  for     rijames
           table MTL_TRANSACTIONS_INTERFACE 
 17-OCT-11 Modify title color for column INV_INTERFACED_FLAG  for    rijames
           table WSH_DELIVERY_DETAILS, also the title bar will be
           printed every 35 rows instead of every 50.
 16-NOV-11 Split printout for ORGANIZATIONS and ACCOUNTING PERIODS   rijames
 10-FEB-12 Correct print sequence on Order Lines (Ordered Item)      rijames
 10-FEB-12 Print Column Headers for Order Lines every 35 lines       rijames
 10-FEB-12 Modify query for ACCOUNTING PERIODS to include Period     rijames
           for lines not Shipped yet.
 14-MAR-12 Modify 'Serial Num. Details' title on Parameters Listing  rijames
 14-MAR-12 Add new close/open after ACCOUNTING PERIODS table         rijames
 04-MAY-12 Correct printout for 'UNPICKED LINES' table.              rijames
 11-OCT-12 Include WMS Rules tables.                                 rijames
 11-OCT-12 Include link to table WMS Rules on Tables Index.          rijames
 11-OCT-12 Change version to 12.0.4                                  rijames Ver. 12.0.4
 15-OCT-12 Include line number and Internal Result code on lines     rijames
           Workflow table
 15-OCT-12 Include Internal Result code on header Workflow table     rijames
 15-OCT-12 Correct OE_LINES FULFIL_QTY field printed                 rijames
 15-MAR-13 Set WMS Information to Default to 'N' as the file         rijames
           created is too extense.
 14-OCT-13 Sorting for Workflow lines corrected                      rijames
 14-OCT-13 Change Version numbering to include date .yymmdd          rijames Ver. 12.0.4.131014
 24-MAR-14 Change SQL to obtain utl_file_dir value                   rijames
 24-MAR-14 Add field Source_line_set_id to Delivery Details query    rijames
 10-JUN-14 Correct WSH_DELIVERY_DETAILS (DET) table Headers          rijames Ver. 12.0.4.140610
 10-JUN-14 Correct WSH_DELIVERY_DETAILS (CONTAINERS) columns order   rijames
 10-JUN-14 Add field Source Line Set Id to WSH_DELIVERY_DETAILS      rijames
 28-FEB-15 Remove duplicate field Ship_From_org_id from OE_LINES     rijames 
 23-MAR-15 Add To_Char to SQL's referencing ITEM_KEY field on WF     rijames 
 24-MAR-15 Add section to RA_INTERFACE_LINES for Freight (req. AMAY) rijames Ver. 12.0.4.150324
 24-MAR-15 Replace reference LINE_ID_5 with LINE_ID_6 on RAI table   rijames
 24-MAR-15 Add option to skip printout of Lines Summary              rijames
 11-SEP-15 Load Error Messages table even if Validation is 'N'       rijames Ver. 12.0.4.150911
 07-OCT-15 Add INTERFACE_LINE_ATTRIBUTE9–>Customer Item on RA tables rijames Ver. 12.0.4.151007
 07-OCT-15 Add Language selection to Freight Costs SQL               rijames
 07-OCT-15 Add Released_Flag to the Accounting Periods query         rijames
 14-OCT-15 Add field ORIG_SYS_DOCUMENT_REF on OE_Headers and         rijames Ver. 12.0.4.151014
           OE_Lines, plus field ORIG_SYS_LINE_REF on OE_Lines
 28-OCT-15 Correct Header 'Period Name' on MTI Query                 rijames
 28-OCT-15 Add Creation Date on WSH_SERIAL_NUMBERS query             rijames
 06-NOV-15 Add Release verification                                  rijames Ver. 12.0.4.151106
 08-DEC-15 OE_LINES Move Other Sets out of verification section      rijames 
 12-JAN-16 Correct option to skip printout of Lines Summary (case)   rijames
 15-JAN-16 OE_LINES add fields to match PO_REQUISITIONS for Internal rijames
           Orders: Source_Doc_Id, Source_Doc_Line_Id, Order_Source_Id
 15-JAN-16 Move OE_LINES titles to the cicle to print every 50 lines rijames Ver. 12.0.4.160115
 22-JAN-16 Add validation for options case and size-1 capital char   rijames Ver. 12.0.4.160122
 08-FEB-16 Allow Header information (WF, Price, Holds) to print      rijames
           when  'Header Only' is enabled (no lines on Order). 
 08-FEB-16 Add Internal Activity Status on Work Flow outputs.        rijames
 23-FEB-16 Add field Batch_id on DELIVERY DETAILS.                   rijames
 23-FEB-16 Add field Reservable on ORDER LINES.                      rijames
 26-FEB-16 Add queries for Back to Back Orders on PO Requisitions    rijames
 09-MAR-16 Add rownum = 1 on OE_LINES WF status to manage ORA-1422   rijames Ver. 12.0.4.160309
 24-MAR-16 Correct WDD titles (batch num. printed twice)             rijames Ver. 12.0.4.160324

#
#========================================================================================

References
----------
For documentation and white papers on the Order Management product suite go to Metalink,
http://metalink.us.oracle.com, click on Top Tech Docs button -> E-Business Suite: ERP -> 
Distribution/Supply Chain -> OM Suite: Order Management. 
*/

variable        v_order_num         varchar2(100);
variable        v_error             number;
variable        v_line_tot          number;
variable        v_line_cnt          number;
variable        sales_ord_id        number;
variable        v_op_unit           number;
variable        v_head_cnt          number;
variable        v_header_id         number;
variable        v_head_only         varchar2(1);

variable        r_line_t            varchar2(100);
variable        r_res_q             number;
variable        r_wdd               number;
variable        r_flag              varchar2(100);
variable        r_pro_na            varchar2(100);
variable        r_act_na            varchar2(100);
variable        r_result            varchar2(100);
variable        r_act_s             varchar2(100);
variable        r_error             number;
variable        is_opm              varchar2(2);

variable        all_or_top10        varchar2(100);


set arraysize 4;
set pagesize 58; 
set term on;
set linesize 145;
set underline =; 
set verify off; 
set serveroutput on size 1000000;
set feedback off;
     
-- OE_ORDER_HEADERS 
column HEADER_ID               format 99999999;
column ORGANIZATION_NAME       format A30;
column TYPE_ID                 format 99999999;
column TYPE_NAME               format A15;
column ORD_DATE_TYPE           format a15;
column ORD_NUM                 format 999999999;
column VERS                    format 9999;
column ORD_TYPE_ID             format 999999999; 
column FLOW_CODE               format a22;
column CUST_NAME               format A20;
column CUST_ID                 format 999999999; 
column SHIP_TO_ID              format 999999999;
column SHIP_TO                 format A12;
column SOLD_TO_ID              format 999999999;
column PO_NUMBER               format A09;
column DATE_ORD                format A15;
column PL_ID                   format 99999;
column PRICE_LIST_NAME         format A15;
column PL_NAME                 format A15;
column SHIP_PART               format A09; 
column ORGANIZATION            format 9999999;
column WH_ID                   format 99999;
column DEMAND_CLASS            format A12;
column OP                      format A05;
column CN                      format A05;
column BK                      format A05;
column DRP_SHP                 format A07; 
column CYCLE_ID                format 99999999;
column ORD_SRC_ID              format 99999999;
column TRANS_TYPE              format A12;
column CATEGORY                format A08;
column SP                      format A05;
column CAT                     format A06;

-- WF_NOTIFICATIONS
column TO_USER        format a22;
column ORIG_RECIP     format a10;
column RECIP_ROLE     format a10;
column MAIL_STAT      format a9;
column MESSAGE_NAME   format a25;
column SUBJECT        format a45;

-- OE_TRANSACTION_TYPES_ALL
column ORDER_TYPE_NAME         format A25;
column TYPE_NAME               format A15;
column CONSTANTOE              format A15;
column SHIP_PRIORITY           format A13;
column SYS_REQD                format A08;

--  OE_ORDER_LINES_ALL

column LINE_ID                 format 99999999;
column LINE                    format A08;
column LINE_NUM                format A08;
column LINE_CTG                format A09;
column CONFIG_HDR_ID           format B99999999;                 
column SSCHED                  format B9999;
column PRT_LINE_ID             format B99999999;
column ATO_LINE_ID             format B99999999;
column LNK_LINE_ID             format B99999999;
column SHP_LINE_ID             format B99999999;
column SRC_LINE_ID             format B99999999;
column SRV_LINE_ID             format B99999999;
column ITEM_ID                 format 99999999;
column ITEM                    format A17;
column ORD_Q                   format 999999.99;
column ORG_Q                   format 999999.99;
column REQ_Q                   format 999999.99;
column PRICE                   format $999999999.99;
column SHP_Q                   format 999999.99;
column Q_INC                   format 999999.99;
column SHN_Q                   format 999999.99;
column SHP_Q                   format 999999.99;
column FUL_Q                   format 999999.99
column RES_Q                   format 999999.99;
column INC_Q                   format 999999.99;
column CAN_Q                   format 999999.99;
column LN_SET_TY             format A11;
column SH_SET_TY             format A11;
column AR_SET_TY             format A11;
column REQUEST_D               format A15;
column SCHEDUL_D               format A15;
column SOURCE_TYPE             format A11;
column DEM_CLASS               format A10;
column LINE_DETAIL_ID          format 9999999999; 
--column OPEN_FL                 format A07;
--column CANC_FL                 format A07;
--column BOOKED                  format A07;
column SH                      format A05;
column VD                      format A05;
column FF                      format A05;
column SUBINV                  format A10;
column SSC                     format A05;
column SI                      format A05;
column II                      format A05;
column INVC_INT_STAT           format A14;
column SHIPPABLE               format A08;
column TRANSACTABLE            format A08;
column RESERVABLE              format A08;
column RELEASED                format A08;
column FOR_REVENUE             format A08;
column DELIVERY                format 9999999999;

column SET_TYPE                format A15;
column SET_NAME                format A10;
column STATUS                  format A06;
column SCH_SHP_DT              format A10;
column SCH_ARV_DT              format A10;
column CARRIER                 format A15;
column SHIP_METHOD             format A15;

column wf_act_code             format a11;
column wf_result               format a9;
column hist_comments           format a45;
column hist_type               format a12;
-- column PRICE                   format $9999999999.99;


-- WORKFLOW TABLES
column ITEM_KEY                format A08;
column FLOW_PROCESS            format A22;
column RESULT_CODE             format A15;
column RESULT                  format A15;
column ASSIGNED_USER           format A10;
column ERROR_NAME              format A19;
column PROCESS_NAME            format A25;
column ACTIVITY_NAME           format A25;
column ERROR_ACTIVITY_NAME     format A31;
column ACT_STATUS              format A10;
column HEAD_ID                 format A08;
column LIN_ID                  format A08; 
column NOTIF_ID                format 99999999; 
column ERROR_NAME              format A14;
column ERR_RETRY_ROLE          format A14;
column ERR_RETRY_USER          format A14;
column BEGIN_DATE              format A18;
column END_DATE                format A18;

column ORD_TYPE_NAME           format A20;
column CONSTANTOE              format A12;
column DEMAND_ID               format 9999999999;
column PARDEM_ID               format 9999999999;
column DS_HEADER_ID            format 99999999;
column DS_LINE                 format A08;
column RSV_QTY                 format 9999999.99;
column SAT_QTY                 format 9999999.99;
column PND_QTY                 format 9999999.99;
column REQUIRD_D               format A15;
column TY                      format 99999;
column UP                      format 99999.99;
column MRP                     format 99999.99;
column ATP                     format 99999.99;
column PICKING_HEADER_ID       format 9999999999;
column PICK_SLIP               format 9999999999;
column BATCH_ID                format 99999999;
column STATUS_CODE             format A11;
column BATCH_NAME              format A30;
column PICKING_LINE_ID         format 9999999999;

column CONFIRMED               format A09;
column RA_INTERFACE            format A12;
column IN_INTERFACE            format A15;
column PICK_LN_DTL_ID          format 9999999999;
column SUBINVENTORY            format A12;
column REV                     format A05;
column LOT_NUMBER              format A15;
column SERIAL_NUMBER           format A20;
column LOCATION_ID             format 9999999999;
column ORG                     format A05;
column ATO                     format A05;
column OPT                     format A05;
column CFG                     format A05;
column SHIP_MC                 format A07;
column SHIP_SET                format 99999999;
column ITEM_TYPE               format A09;
column LINE_TYPE               format A09;
column ENTER                   format A10;
column CANCEL_ORD              format A10;
column CANCEL_LIN              format A10;
column CTG                     format A05;
column INCL                    format A05;
column CONF                    format A05;
column WIP_RSV                 format 9999999;
column WIP_COM                 format 9999999;
column COMP_CODE               format A10;
column SRC_TYP                 format 9999999;
column DEM_TYP                 format 9999999;
column WIP_ID                  format B99999999;
column JOB_NAME                format A15;
column JOB_STAT                format A12;
column UOM                     format A5;
column REL_FL                  format A06;
column SHIP_FL                 format A07;
column FOR_REV                 format A07;
column AUTOSCH                 format A06;
column DEP_NAME                format a10;
column ORG_ID                  format 99999;
column WSH_ID                  format 9999999;
column SOURCE_CD               format a12;
column STAT_CODE               format A11;
column CLOSED_DT               format A09;
column FREIGHT                 format A09;
column PLN_DEP_DT              format A10;
column ACT_DEP_DT              format A10;
column PLN_DEP_ID              format 9999999999;
column ACT_DEP_ID              format 9999999999;
column LOAD_ORD_FLG            format A12;
column DEL_SEL                 format A07;
column DEP_SEL                 format A07;
column REL_STAT                format A08;
column LIN_STAT                format A08;
column WIP_ENT_ID              format 9999999999;
column CONFIRM_D               format A09;
column INVC_STAT               format A10;
column INVC_INTF               format A10;
column SHIP_STAT               format A10;
column SHIP_INTF               format A10;
column LOT                     format A10;
column REV                     format A05;
column SERIAL_NUM              format A10;
column LOC_ID                  format 99999999;
column RESV_FL                 format A07;
column TRAN_FL                 format A07;
column SCHED_STAT                  format A10;
column INVOICE_TRIGGER             format A15;
column SOURCING_STATUS             format A15;
column INVOICING_STATUS            format A16; 
column CANCELLED_FLAG              format A14;
column OPEN_FLAG                   format A09;
column CONSOLIDATION_STATUS_CODE   format A25;
column CONSOLIDATION_PREF_FLAG     format A23; 
column PURCHASE_ORDER_NUM          format A18;

column DEL_ID                      format 9999999;
column DEL_NAME                    format A10;

-- PO_REQUISITION_INTERFACE_ALL
column AUTH_STATUS                 format A11;   
column DEST_TYPE                   format A10;   
column SRC_CODE                    format A11;   
column SRC_TYPE_CODE               format A13;   
  
-- PO_REQUISITION_HEADERS_ALL
column REQ_NUMBER                  format A10;
column SUMMARY                     format A07;
column XFR_OE_FLAG                 format A11;
column REQ_TYPE                    format A11;
column ENABLED                     format A07;

column ITEM_DESC                   format A40;
column CANC                        format A05;        
column ENC_FL                      format A06;               
column RFQ                         format A05;
column SRC_TYPE                    format a09;
column SRC_ORG                     format 9999999;

-- PO_HEADERS
column PO_NUM                      format A06;

-- MTL_SUPPLY         
column SUP_TYPE                    format a08;
-- MTL_RESERVATIONS
column SHIP_READY                  format A12;
column SS_TYPE_ID                  format A11;
-- WSH_TRIPS
column TRIP_ID                     format 9999999;
column TRIP_NAME                   format A10;
column PLND                        format A05;
column VEH_NUM                     format A10;
column CARR_ID                     format 9999999;
column DET_Q                       format 99999;
column DS_TYPE                     format 9999999;
column ROUTE_ID                    format 99999999;
column VEH_ORG_ID                  format 9999999999;

-- WSH_TRIP_STOPS
column STOP_ID                     format 99999999;
column SEQ_NUM                     format 9999999;
column STOP_LOC_ID                 format 99999999999;
column PLN_DEP_DATE                format A15;
column PLN_ARV_DATE                format A15;
column ACT_DEP_DATE                format A15;
column ACT_ARV_DATE                format A15;
column PEND_INTERF                 format A11;


-- WSH_DELIVERY_LEGS
column LEG_ID                      format 9999999;
column LOAD_TENDER_STAT            format A15;

-- WSH_NEW_DELIVERIES  DEL
column DEL_NAME                    format A12;
column BOOKING_NUM                 format A12;
column WAYBILL                     format A12;
column ACCEPTED                    format A15;
column PICKUP_DT                   format A15;
column DROPOFF_DT                  format A15;

-- WSH_DELIVERY_ASSIGNMENTS  ASG
column ACTIVE                      format A06;

-- WSH_DELIVERY_DETAILS
column REL_STATUS                  format A17;
column STA                         format 99999;
column STB                         format 99999;
column HOLD_CODE                   format A09;
column SMC                         format A05;
column SUB                         format A09;
column CUR_SUB                     format A09;
column ORG_SUB                     format A09;
column REV                         format A05;
column LOT                         format A05;
column SERIAL                      format A10;
column LOC_ID                      format 99999999;
column SHIP_METH                   format A10;
column OMI                         format A05;
column INI                         format A05;
column MVT_STATUS                  format A10;
column INV_INT                     format A07;
column CONT_NAME                   format A15;
column CONT_TYPE                   format A10;
column UNIT_NUM                    format A08; 
column CONTAINER                format A09;
column REQ_DATE                 format A15;
column MVT_STAT                 format A08;
column DEL_DET_ID               format 9999999999;
column RELEASE_STAT             format A15;
column REQ_Q                    format 999999.99;
column DLV_Q                    format 999999.99;
column SRQ_Q                    format 999999.99;
column BO_Q                     format 999999.99;
--column CAN_QTY                  format 9999999;
column SHIP_TO_ID               format 999999999;
column LINE_REQ_QTY             format 999999999999;
column TYPE                     format A05;
column MO_LINE_ID               format 9999999999;
column DELIV_ID                 format 999999999;
column SH_FROM_ID               format 999999999;
column SH_TO_ID                 format 99999999;

--WSH_PICK_SLIP_V
column LINE_STAT                format A11;
column FROM_SUB                 format A09;
column TO_SUB                   format A09;
column DETL_DATE                format A15;

column LINE_STATUS              format A11;

--MTL_TXN_REQUEST_LINES_V
column REQ_NUM                  format A10;
column MV_LINE_STAT             format A14;
column MO_NUMBER                format A09;
column MOVE_TYPE_NAME           format A14;
column TRNS_SRC_TYPE            format A13;
column TRNS_TYPE_NAME           format A30;
column TRNS_ACTION              format A15;
column LOT_NUM                  format A10;

--MTL_MATERIAL_TRANSACTIONS_TEMP
column ERROR_EXPL               format A10;
-- RA_CUSTOMER_TRX 
-- RA_CUSTOMER_TRX_LINES
column ORD_LINE_NUM             format 999999999999;
column TRX_NUMBER               format A10;
column CONTEXT                  format A12;
column Order_Num_1              format A11;
column Order_Type_2             format A12;
column Delivery_3               format A10;
column WayBill_4                format A09;
column Line_ID_6                format A09;
column Pick_Line_Id_7           format A14;
column Bill_Lading_8            format A13;
column Warehouse_10             format A12;
column SOURCE                   format A30;
column TAX_FL                   format A06;    

column QTY_ORD                  format 9999999.99;
column QTY                      format 9999999.99;
column ORD_LINE_NUM             format A12;
column HEAD_CAT                 format A8;
column CURR                     format A4;
column TAX_EX_FL                format A09;
column TERR_SEG1                format A09;
column TERR_SEG2                format A09;
column TERR_SEG3                format A09;

column MESSAGE_TEXT             format A145;
column INVALID_VALUE            format A30;

column STATUS                   format a8;

column   ERR_TYPE_KEY      format a14;
column   ASGND_USER        format a10;
column   ERR_PROCESS_NAME  format a18;
column   ERR_ACTIVITY_NAME format a22;

column hold_name format a22;
column hold_type format a12;
column WF_ACTIVITY format a15;
column ENTITY format a8;
column ENTITY2 format a8;
column HOLD_UNTIL format a18;
column RELEASE_REASON format a14;
column H_REL format a5;
column S_REL format a5;

column AF             format A5;
column LIST_TYPE_CODE format a14;
column UA             format A5;
column UF             format A5;
column AP             format A5;
column ARITH_OP       format a8;
column TAX_CODE       format a12;
column INF            format A5;
column EF             format A5;
column CHG_TY_CD      format a9;
column AC             format A5;
column PI             format A5;
column CD             format A5;
column LIST_LN_NO     format a10;
column LK             format 99999;
column PP             format 99999;
column MOD_LVL        format a7;
column PERC           format 9999;

column STATUS         format a6;

column msg_Source     format a14;  

column error          format a30;
column ITEM_DESC      format a25;
column PRICE          format $999999999.99

column REQ_NUM_IK     format A9;

column ACCEPT_REQD    format A10;
column CLS_STAT       format A08;
column CONF_ORD       format A08;
column CURR_CODE      format A05;
column ENABLED        format A07;
column FROZEN         format A06;
column SUMM           format A05;
column TYPE           format A10;
column ITEM_KEY       format A09;
column SHIP_TO        format 9999999;

column PO_LINE        format 9999;
column FIRM           format A05;
column ITEM_DESC      format A40;

column PO_NUM_IK      format A9;
column ERROR_N        format A7;

column SRC_DOC format a9;
column SHP_LINE_STATUS format a15;
column RCV_Q   format 99999;
column TRANS_TYPE              format A10;
column INSPECT_STAT            format A13;
column INTF_SRC                format A11;
column SRC_DOC_CODE            format A12;
column QTY                     format 9999;

column PROC_MODE format a10;
column DEST_TYPE format a9;
column INSP_STAT format a13;
column INSP_SRC  format a8;
column PROC_STAT format a9;
column RCPT_SRC  format a8;
column TRNS_STAT format a9;
column TRNS_TYPE format a9;

column error_wie format a100;

column status format a9;
column firm format 9999;
column WIP_SUP_TYPE format a13;
column JOB_NAME     format a8;
column TRANS_TYPE   format a19;

column SRC_CODE format a8;
column PHS format 99999;
column STAT_TY format 9999999;
column STAT format 9999;
column LOAD format 9999;

column status format a9;
column firm format 9999;
column WIP_SUP_TYPE format a13;
column JOB_NAME     format a8;
column TRANS_TYPE   format a19;

column SRC_CODE format a8;
column PHS format 99999;
column STAT_TY format 9999999;
column STAT format 9999;
column LOAD format 9999;

column STOP_DESCRIPTION   format A58;

column GRS_WT  format 999999;
column FL_PER  format 999.99;
column VOL     format 9999;
column VOL_UOM format a07;
column CF      format A05;
column WT_UOM  format a06;
column FOB_CODE format a11;
column FRT_TERMS format a9;

column LCK       format 99999;
column PROCESS   format 9999999;
column ERROR_CODE format A30;
column ERROR_EXPL format A60;

column FRT_NAME      format a30;
column FRT_TYPE      format a15;
column FRT_LEVEL     format a15;
column QTY           format 9999;

column PRM_Q    format 99999;          
column DLV_Q    format 99999;          
column DTL_Q    format 99999;          

column LCK       format A05;
column PROCESS   format A07;
column ERROR_CODE format A11;
column ERROR_EXPL format A30;

column TRANS_TYPE format A11;

column QTY          format 9999;
column LINE_NUM     format 99999999;
column BATCH_SOURCE format a20;
column SO_LIN       format a06;
column AR_ID        format 99999;
column IR_ID        format 99999;
column WH_ID_10     format a08;
column PA_ID_11     format a08;
column C_RATE       format 999999;
column TR           format 9999;
column EF           format A05;
column INTF_LINE_ID format 99999999999;
column CRD_Q        format 99999;
column PRICE        format $9999999.99;

column PO_NUMBER   format A09;
column COMP_FL     format a07;
column EXTD_AMT    format $999,999.99;
column REV_AMT     format $999,999.99;
column TF          format A05;
column SOURCE      format A11;
column INV_Q       format 99999;
column SHIP_VIA    format A12;

Set heading on

WHENEVER SQLERROR EXIT FAILURE ROLLBACK;


prompt 
accept order_number_selected prompt 'Please enter Sales Order number: ' 
prompt 

begin
  
  :v_error     := 0;
  :v_order_num := '&order_number_selected';
  
  select count(*) 
  into :v_head_cnt
  from oe_order_headers_all
  where 
  order_number = :v_order_num;
  
  if :v_head_cnt = 0 then
      RAISE no_data_found;
  end if;

  if :v_head_cnt = 1 then
     select header_id 
     into :v_header_id
     from oe_order_headers_all
     where order_number = :v_order_num;
  end if;


exception
   when no_data_found then
      dbms_output.put_line('ERROR - Invalid order number entered');
      dbms_output.put_line('ACTION - Script execution must be aborted');
      dbms_output.put_line('ACTION - Please hit CTL-C to exit');
      :v_error := 1;
      raise;
   when others then
      dbms_output.put_line('ERROR - Unable to retrieve order due to error: '||SQLERRM);
      dbms_output.put_line('ACTION - Script execution must be aborted');
      dbms_output.put_line('ACTION - Please hit CTL-C to exit');
      :v_error := 1;
      raise;

end;
/


-- spool &out_file;


/* Display list of header_ids matching the order number entered */
  select
       ORD.HEADER_ID                 HEADER_ID,
       TYP.NAME                      ORDER_TYPE_NAME,
       ORD.ORDER_CATEGORY_CODE       CATEGORY,
       ORD.ORG_ID                    ORG_ID,
       (select name
        from hr_operating_units
        where organization_id = nvl(ORD.ORG_ID,-99)) ORGANIZATION_NAME
  from
       OE_ORDER_HEADERS_ALL          ORD,
       OE_TRANSACTION_TYPES_TL       TYP,
       FND_LANGUAGES                 FLA
  where
       ORD.ORDER_NUMBER              = :v_order_num
  and  TYP.LANGUAGE                  = FLA.LANGUAGE_CODE 
  and  FLA.INSTALLED_FLAG            = 'B'
  and  ORD.ORDER_TYPE_ID             = TYP.TRANSACTION_TYPE_ID
  and :v_error                       = 0;


prompt 
accept header_id_selected prompt 'Please enter HEADER_ID from list above (optional): '
prompt 

/* Set client info context based on the org_id from sales order */
begin
  select org_id
    into :v_op_unit
  from   oe_order_headers_all ord
  where 
     ORD.HEADER_ID = nvl('&header_id_selected',:v_header_id);

  mo_global.set_policy_context(p_access_mode => 'S',  p_org_id  => :v_op_unit); -- by rodavid

  fnd_client_info.set_org_context(to_char(:v_op_unit));

exception
   when no_data_found then
      dbms_output.put_line('ERROR - Invalid order number entered');
      dbms_output.put_line('ACTION - Script execution must be aborted');
      dbms_output.put_line('ACTION - Please hit CTL-C to exit');
      :v_error := 1;
      raise;
   when others then
      dbms_output.put_line('ERROR - Unable to retrieve order due to error: '||SQLERRM);
      dbms_output.put_line('ACTION - Script execution must be aborted');
      dbms_output.put_line('ACTION - Please hit CTL-C to exit');
      :v_error := 1;
      raise;
end;
/

begin
  select count(*) 
    into :v_line_tot
    from oe_order_lines_all
   where header_id = nvl('&header_id_selected',:v_header_id);
  
  if :v_line_tot = 0 then
      dbms_output.put_line('WARNING - Order selected does not have any lines');
      dbms_output.put_line('WARNING - Please hit CTL-C to abort');
      dbms_output.put_line('WARNING - If you chose to continue only Header information will be printed,');
      dbms_output.put_line('WARNING - all additional entries will be ignored.');
      :v_head_only := 'Y';
    else
      :v_head_only := 'N';
      dbms_output.put_line(' ');
      dbms_output.put_line('Total number of Top lines on this Order '||to_char(:V_line_tot));
  end if;
end;
/

prompt
accept print_lines_summary prompt 'Do you want the Lines Summary to be printed? (Default Y): '


/* Display all line_ids associated with this order number */
select 
    substr(LIN.LINE_ID,1,15)      LINE_ID,
    substr(to_char(LIN.line_number) || 
          decode(LIN.shipment_number, null, null, '.' || to_char(LIN.shipment_number))|| 
          decode(LIN.option_number, null, null, '.' || to_char(LIN.option_number)) ||
          decode(LIN.component_number, null, null, 
                 decode(LIN.option_number, null, '.',null)||
                 '.'||to_char(LIN.component_number))||
          decode(LIN.service_number,null,null,
                 decode(LIN.component_number, null, '.' , null) ||
                        decode(LIN.option_number, null, '.', null ) ||
                        '.'|| to_char(LIN.service_number)),1,10) LINE_NUM,
     ITM.SEGMENT1                  ITEM,
     substr(LIN.LINE_CATEGORY_CODE,1,10)  lin_cat,
     nvl(LIN.ORDERED_QUANTITY,0)   Order_QTY,
     LIN.ORDER_QUANTITY_UOM        uom,
     substr(LIN.FLOW_STATUS_CODE,1,20) Line_status,
     nvl(LIN.SHIPPED_QUANTITY,0)   shipped,
     nvl(LIN.FULFILLED_QUANTITY,0) fulfilled,                  
     nvl(LIN.INVOICED_QUANTITY,0)  invoiced,
     nvl(LIN.CANCELLED_QUANTITY,0) cancelled      
from
     OE_ORDER_LINES                LIN,
     MTL_SYSTEM_ITEMS              ITM 
where 
     LIN.HEADER_ID                  = nvl('&header_id_selected',:v_header_id)
and  LIN.SHIP_FROM_ORG_ID           = ITM.ORGANIZATION_ID(+)
and  LIN.INVENTORY_ITEM_ID          = ITM.INVENTORY_ITEM_ID(+)
and  LIN.OPTION_NUMBER              IS NULL
and  LIN.ITEM_TYPE_CODE             <> 'INCLUDED'
and  substr(UPPER(nvl('&print_lines_summary','Y')),1,1) = 'Y'
order by
     NVL(LIN.ATO_LINE_ID,               LIN.LINE_ID),
     NVL(LIN.SORT_ORDER,                '0000'),
     NVL(LIN.LINK_TO_LINE_ID,           LIN.LINE_ID),
     NVL(LIN.SOURCE_DOCUMENT_LINE_ID,   LIN.LINE_ID),
     NVL(LIN.SERVICE_REFERENCE_LINE_ID, LIN.LINE_ID),
     LIN.LINE_ID;

/* Display any header or line sets associated with this order */
PROMPT
PROMPT OE_SETS (SET)

select ST1.SET_ID                        SET_ID,                         
       ST1.SET_NAME                      SET_NAME,                       
       ST1.SET_TYPE                      SET_TYPE,                       
       ST1.HEADER_ID                     HEADER_ID,
       ST1.INVENTORY_ITEM_ID             ITEM_ID,              
       ST1.ORDERED_QUANTITY_UOM          UOM,           
       ST1.LINE_TYPE_ID                  LINE_TYPE_ID,
       nvl(LST.SYSTEM_REQUIRED_FLAG,'N') SYS_REQD,
       ST1.SET_STATUS                    STATUS,                                                              
       to_char(ST1.SCHEDULE_SHIP_DATE,'DD-MON-RR_HH24:MI:SS')  SCH_SHP_DT,             
       to_char(ST1.SCHEDULE_ARRIVAL_DATE,'DD-MON-RR_HH24:MI:SS')   SCH_ARV_DT,
       ST1.SHIP_FROM_ORG_ID              SHIP_FROM,      
       ST1.SHIP_TO_ORG_ID                SHIP_TO_ID,
       ST1.SHIPMENT_PRIORITY_CODE        SHIP_PRIORITY, 
       ST1.FREIGHT_CARRIER_CODE          CARRIER,
       ST1.SHIPPING_METHOD_CODE          SHIP_METHOD,
       ST1.SHIP_TOLERANCE_ABOVE          STA,           
       ST1.SHIP_TOLERANCE_BELOW          STB
from OE_SETS                       ST1,
     OE_LINE_SETS                  LST
where ST1.SET_ID              = LST.SET_ID(+)
and   ST1.HEADER_ID = nvl('&header_id_selected',:v_header_id)
and   substr(UPPER(nvl('&print_lines_summary','Y')),1,1) = 'Y';

prompt
accept line_id_selected prompt 'Please enter LINE_ID from list above(leave blank for all lines): '

begin
if :v_line_tot > 0 then   -- check for line_id
begin
  select count(*) 
    into :v_line_cnt
    from oe_order_lines_all
   where header_id = nvl('&header_id_selected',:v_header_id)
     and  nvl('&line_id_selected',0)  in (0,line_id);
  
  if :v_line_cnt = 0 then
      RAISE no_data_found;
  end if;

exception
   when no_data_found then
      dbms_output.put_line('ERROR - Invalid line_id entered');
      dbms_output.put_line('ACTION - Please hit CTL-C to exit');
      dbms_output.put_line('.');
      :v_error := 1;
      raise;
   when others then
      dbms_output.put_line('ERROR - Unable to retrieve order line due to error: '||SQLERRM);
      dbms_output.put_line('ACTION - Please hit CTL-C to exit');
      dbms_output.put_line('.');
      :v_error := 1;
      raise;
end;
end if;
end;
/

WHENEVER SQLERROR CONTINUE;

prompt
accept do_analysis prompt 'Do you want validation performed on this order (Default=Y): '
prompt

prompt
accept prt_wf    prompt 'Do you want WorkFlow information printed? (Default=Y): '
accept prt_price prompt 'Do you want Pricing information printed? (Default=Y): '
accept prt_po    prompt 'Do you want Purchasing information printed? (Default=Y): '
accept prt_rec   prompt 'Do you want Receiving information printed? (Default=Y): '
accept prt_wip   prompt 'Do you want Work In Progress information printed? (Default=Y): '
accept prt_inv   prompt 'Do you want Inventory information printed? (Default=Y): '
accept prt_ar    prompt 'Do you want Receivables information printed? (Default=Y): '
prompt
prompt Listing of tables for WMS could be extense,  *** Note Default is No ***
accept prt_wms   prompt 'Do you want WMS information printed? (Default=N): '
prompt
prompt Listing of tables MTL_UNIT_TRANSACTIONS (MUT), WSH_SERIAL_NUMBERS (WSN)
prompt and MTL_SERIAL_NUMBERS could be extense.
accept det_cnt   prompt 'Do you want complete listing (Y-All records  N-only the first 10 records)? (Default=Y): '
prompt

set heading off

def version_code='12.0.4.160324'
def c1='_'
def pref='HTMomse12'
def suff='.html'
def out_file='&pref&c1&order_number_selected&suff'

-- Printout the filename and location
column DUMMY  new_value out_dir noprint
column DUMMY2 new_value host_nam noprint
  select substr(value,1,instr(value||',',',')-1) DUMMY
    from v$parameter where name = 'utl_file_dir';

  select substr(machine,1,20) DUMMY2
    from v$session where upper(program) like '%SMON%';

Prompt Output file created on Database Server: "&host_nam"
Prompt Complete path and filename: "&out_dir/&out_file"


set define '!'
-- Tables
def std='<table width=100% border=1 cellpadding=1 cellspacing=1>'
def et=</Table>
-- headers
def sh='<TR bgcolor="#cccc99"><td><font face="arial" color="#336699"> <b><i>'
def dh='</i></b></font></td><td><font face="arial" color="#336699"> <b><i>'
def dhr='</i></b></font></td><td><font face="arial" color="#ff0000"> <b><i>'
def eh='</i></b></font></td></tr>'
-- def sh='<TR bgcolor="#cccc99"><td><b><i>'
-- def dh='</i></b></td><td><b><i>'
-- def eh='</i></b></td></tr>'
-- lines
def sld='<tr bgcolor="#f7f7e7"><td><font face="arial" size="-1">'
def d='</font></td><td><font face="arial" size="-1">'
def el='</font></td></tr>'
-- def sld='<tr bgcolor="#f7f7e7"><td>'
-- def d='</td><td>'
-- def el='</td></tr>'
-- otros
def f=<br>     -- eol
def b=<b>      -- bold
def eb=</b>    -- end bold
def sp='&nbsp' -- space
set null '&nbsp'
set define '&'


-- Start of printout
DECLARE
  handle UTL_FILE.FILE_TYPE;
  dirname varchar2(1000);
  text    varchar2(1000);

function n(v varchar2) return varchar2 is  -- to print fields
begin
  if v is null then
   return '&sp';
   else
   return v;
  end if;
end n;

function n2(v varchar2,v2 varchar2) return varchar2 is   -- to print parameters
begin
  if v is null then
   return v2;
   else
   return v;
  end if;
end n2;

begin
-- Creation of output file
-- SQL fails when parameter is loo long 
--  select substr(value,1,instr(value||',',',')-1) into dirname
--    from v$parameter where name = 'utl_file_dir';

  select substr(value,1,decode(instr(value,','),0,length(value),instr(value,',')-1)) into dirname
    from v$parameter where name = 'utl_file_dir';

--  handle := UTL_FILE.FOPEN(dirname,dirname||'/&out_file','W',32000);
  handle := UTL_FILE.FOPEN('&out_dir','&out_dir/&out_file','W',32000);

/* HTML header information */

UTL_FILE.PUT_LINE(handle,'<html>');
UTL_FILE.PUT_LINE(handle,'<head>');
UTL_FILE.PUT_LINE(handle,'<title> &out_file </title>');
UTL_FILE.PUT_LINE(handle,'</head>');
UTL_FILE.PUT_LINE(handle,'<body background=#ffffff>');
UTL_FILE.PUT_LINE(handle,'<font face="arial" color="#336699"> <b><i>');

-- DataBase name, creation date
-- User and time script execution
UTL_FILE.PUT_LINE(handle,'&std &sh DATABASE IDENTIFICATION &dh SCRIPT EXECUTION (Ver. &version_code) &eh');
select '&sld &b DB Name: &eb '||name||' - &b Release: &eb '||substr(release_name,1,10)||' &d'||
       '&b Script run at Date/Time: &eb'||to_char(sysdate,'DD-MON-RR_HH24:MI:SS')||'&el'||
       '&sld &b Creation Date: &eb'||to_char(created,'DD-MON-RR_HH24:MI:SS')||'&d'
       into text
  from V$DATABASE, fnd_product_groups;

UTL_FILE.PUT_LINE(handle,text);
select '&b USER is: '||username into text from user_users;
UTL_FILE.PUT_LINE(handle,text);
UTL_FILE.PUT_LINE(handle,'&eb &el &et &f &f');


UTL_FILE.PUT_LINE(handle,'&std &sh The test will be run with the following parameters &dh Sections being printed: &dh &sp &eh');
UTL_FILE.PUT_LINE(handle,'&sld &b Order Number = &eb &order_number_selected &d WorkFlow = '||n2('&prt_wf','Y')||' &d Pricing = '||n2('&prt_price','Y')||' &el');
UTL_FILE.PUT_LINE(handle,'&sld &b Header Id = &eb &header_id_selected &d Purchasing = '||n2('&prt_po','Y')||' &d Receiving = '||n2('&prt_rec','Y')||' &el');
if :v_head_only = 'Y' then
  UTL_FILE.PUT_LINE(handle,'&sld &b Line Id = &eb '||n2('','Header Only')||' &d Work in Progress = '||n2('&prt_wip','Y')||' &d Inventory = '||n2('&prt_inv','Y')||' &el');
 else
  UTL_FILE.PUT_LINE(handle,'&sld &b Line Id = &eb '||n2('&line_id_selected','All Lines')||' &d Work in Progress = '||n2('&prt_wip','Y')||' &d Inventory = '||n2('&prt_inv','Y')||' &el');
end if;
UTL_FILE.PUT_LINE(handle,'&sld &b Validation = &eb '||n2('&do_analysis','Y')||' &d Receivables = '||n2('&prt_ar','Y')||' &d Serial Num. Details = '||n2('&det_cnt','Y')||' &el');
UTL_FILE.PUT_LINE(handle,'&sld &sp &d WMS info = '||n2('&prt_wms','N')||' &d Summary = '||n2('&print_lines_summary','Y')||' &el');
UTL_FILE.PUT_LINE(handle,'&et &f &f');


-- Index for Major tables direct access
UTL_FILE.PUT_LINE(handle,'&std &sh INDEX FOR MAJOR TABLES DIRECT ACCESS &dh &sp &eh');
UTL_FILE.PUT_LINE(handle,'&sld <a HREF="#OE_SETS">OE_SETS (SET)</a>                   &d <a HREF="#WSH_TRIPS">WSH_TRIPS (TRP)</a>  &el');
UTL_FILE.PUT_LINE(handle,'&sld <a HREF="#OE_ORDER_HEADERS">OE_ORDER_HEADERS (ORD)</a> &d <a HREF="#WSH_TRIP_STOPS">WSH_TRIP_STOPS (STP)</a>  &el');
UTL_FILE.PUT_LINE(handle,'&sld <a HREF="#OE_ORDER_LINES">OE_ORDER_LINES (LIN)</a>     &d <a HREF="#WSH_NEW_DELIVERIES">WSH_NEW_DELIVERIES (DEL)</a>  &el');
UTL_FILE.PUT_LINE(handle,'&sld <a HREF="#MTL_RESERVATIONS">MTL_RESERVATIONS (RES)</a> &d <a HREF="#WSH_DELIVERY_DETAILS">WSH_DELIVERY_DETAILS (DET)</a> &el');
UTL_FILE.PUT_LINE(handle,'&sld <a HREF="#MTL_TRANSACTIONS_INTERFACE">MTL_TRANSACTIONS_INTERFACE (MTI)</a> &d');
UTL_FILE.PUT_LINE(handle,'<a HREF="#WMS_RULES">WMS_RULES</a>  &el');
UTL_FILE.PUT_LINE(handle,'&sld <a HREF="#MTL_MATERIAL_TRANSACTIONS_TEMP">MTL_MATERIAL_TRANSACTIONS_TEMP (TMP) - UNPICKED LINES</a> &d');
UTL_FILE.PUT_LINE(handle,'<a HREF="#RA_INTERFACE_LINES">RA_INTERFACE_LINES (RAI)</a> &el');
UTL_FILE.PUT_LINE(handle,'&sld <a HREF="#MTL_MATERIAL_TRANSACTIONS">MTL_MATERIAL_TRANSACTIONS (TRN) - PICKED LINES</a>  &d');
UTL_FILE.PUT_LINE(handle,'<a HREF="#RA_CUSTOMER_TRX">RA_CUSTOMER_TRX (RAH) - INVOICE HEADERS</a> &el');
UTL_FILE.PUT_LINE(handle,'&sld <a HREF="#MTL_TXN_REQUEST_LINES_V">MTL_TXN_REQUEST_LINES_V (MOV)</a> &d ');
UTL_FILE.PUT_LINE(handle,'<a HREF="#RA_CUSTOMER_TRX_LINES">RA_CUSTOMER_TRX_LINES (RAL)</a>  &el');

UTL_FILE.PUT_LINE(handle,'&et &f &f');

-- Parameters, initial listings from screen

UTL_FILE.PUT_LINE(handle,'&std &sh HEADER_ID &dh ORDER_TYPE_NAME &dh CATEGORY &dh ORG_ID &dh ORGANIZATION_NAME &el');

Declare
cursor l_orders is
  select
       ORD.HEADER_ID                 Hea_I,
       TYP.NAME                      OT_NAME,
       ORD.ORDER_CATEGORY_CODE       CAT_co,
       ORD.ORG_ID                    ORG_I
  from
       OE_ORDER_HEADERS_ALL          ORD,
       OE_TRANSACTION_TYPES_TL       TYP,
       FND_LANGUAGES                 FLA
  where
       ORD.ORDER_NUMBER              = :v_order_num
  and  TYP.LANGUAGE                  = FLA.LANGUAGE_CODE 
  and  FLA.INSTALLED_FLAG            = 'B'
  and  ORD.ORDER_TYPE_ID             = TYP.TRANSACTION_TYPE_ID
  and :v_error                       = 0;

or_name varchar2(100);

begin
 for lo in l_orders 
 loop

   -- Organization Name
  select name
    into or_name
    from hr_operating_units
   where organization_id = nvl(lo.ORG_I,-99);

   -- Print line to Output file

   utl_file.put_line(handle,'&sld'||n(lo.hea_i)||'&d'||n(lo.ot_name)||'&d');
   utl_file.put_line(handle,n(lo.Cat_co)||'&d'||n(lo.org_i)||'&d'||n(or_name)||'&el');

 end loop;

end;

UTL_FILE.PUT_LINE(handle,'&et  ');

if substr(UPPER(nvl('&print_lines_summary','Y')),1,1) = 'Y' then

UTL_FILE.PUT_LINE(handle,'&f &f &b LINE_IDs ASSOCIATED WITH THIS ORDER NUMBER &f');

UTL_FILE.PUT_LINE(handle,'&std &sh LINE NUMBER &dh LINE_ID &dh ITEM &dh ORDER QTY &dh UOM &dh LINE STATUS &dh SHIP QTY &dh ');
UTL_FILE.PUT_LINE(handle,'FULFILL QTY &dh INVOICE QTY &dh CANCEL QTY &dh OPEN &dh SHIP &dh CANC &dh SI &dh SHIP FROM &dh ');
UTL_FILE.PUT_LINE(handle,'LINE TYPE &dh LINE CATEG &dh INVOICED &dh ARRIVAL SET_ID &dh SHIP SET_ID &dh LINE SET_ID &eh');

Declare
cursor l_lines_ord is
  select 
     to_char(LIN.line_number) || 
          decode(LIN.shipment_number, null, null, '.' || to_char(LIN.shipment_number))|| 
          decode(LIN.option_number, null, null, '.' || to_char(LIN.option_number)) ||
          decode(LIN.component_number, null, null, 
                 decode(LIN.option_number, null, '.',null)||
                 '.'||to_char(LIN.component_number))||
          decode(LIN.service_number,null,null,
                 decode(LIN.component_number, null, '.' , null) ||
                        decode(LIN.option_number, null, '.', null ) ||
                        '.'|| to_char(LIN.service_number)) LINE_NUM,
     LIN.LINE_ID                    LINE_ID,
     ITM.SEGMENT1                  ITEM,
     nvl(LIN.ORDERED_QUANTITY,0)   ORD_QTY,
     LIN.ORDER_QUANTITY_UOM        Orduom,
     substr(LIN.FLOW_STATUS_CODE,1,22) Line_st,
     nvl(LIN.SHIPPED_QUANTITY,0)   shpq,
     nvl(LIN.FULFILLED_QUANTITY,0) fulq,                  
     nvl(LIN.INVOICED_QUANTITY,0)  invq, 
     nvl(LIN.CANCELLED_QUANTITY,0) canq,      
     nvl(LIN.OPEN_FLAG,'N')        openf,
     nvl(LIN.SHIPPABLE_FLAG,'N')   shipf,
     nvl(LIN.CANCELLED_FLAG,'N')   canf,
     nvl(LIN.SHIPPING_INTERFACED_FLAG,'N')    SI,
     LIN.SHIP_FROM_ORG_ID          shipfrom,
     LIN.LINE_TYPE_ID              LINE_TYPE_ID,
     LIN.LINE_CATEGORY_CODE        lin_cat,
     nvl(LIN.INVOICE_INTERFACE_STATUS_CODE,'N') INV_INT_STAT,
     LIN.ARRIVAL_SET_ID            ARRIV_SET_ID,
     LIN.SHIP_SET_ID               SHIP_SET_ID,
     LIN.LINE_SET_ID               LINE_SET_ID
  from
     OE_ORDER_LINES                LIN,
     MTL_SYSTEM_ITEMS              ITM 
  where 
     LIN.HEADER_ID                  = nvl('&header_id_selected',:v_header_id)
  and  LIN.SHIP_FROM_ORG_ID           = ITM.ORGANIZATION_ID(+)
  and  LIN.INVENTORY_ITEM_ID          = ITM.INVENTORY_ITEM_ID(+)
  and  LIN.OPTION_NUMBER              IS NULL
  and  LIN.ITEM_TYPE_CODE             <> 'INCLUDED'
  order by
     NVL(LIN.ATO_LINE_ID,               LIN.LINE_ID),
     NVL(LIN.SORT_ORDER,                '0000'),
     NVL(LIN.LINK_TO_LINE_ID,           LIN.LINE_ID),
     NVL(LIN.SOURCE_DOCUMENT_LINE_ID,   LIN.LINE_ID),
     NVL(LIN.SERVICE_REFERENCE_LINE_ID, LIN.LINE_ID),
     LIN.LINE_ID;

begin
   for ll in l_lines_ord
   loop
     utl_file.put_line(handle,'&sld'||n(ll.LINE_NUM)||'&d'||n(ll.LINE_ID)||'&d'||n(ll.ITEM)||'&d');
     utl_file.put_line(handle,n(ll.ORD_QTY)||'&d'||n(ll.Orduom)||'&d');
     utl_file.put_line(handle,n(ll.Line_st)||'&d'||n(ll.shpq)||'&d');
     utl_file.put_line(handle,n(ll.fulq)||'&d'||n(ll.invq)||'&d');
     utl_file.put_line(handle,n(ll.canq)||'&d'||n(ll.openf)||'&d');
     utl_file.put_line(handle,n(ll.shipf)||'&d'||n(ll.canf)||'&d');
     utl_file.put_line(handle,n(ll.SI)||'&d'||n(ll.shipfrom)||'&d');
     utl_file.put_line(handle,n(ll.LINE_TYPE_ID)||'&d'||n(ll.LIN_CAT)||'&d');
     utl_file.put_line(handle,n(ll.INV_INT_STAT)||'&d'||n(ll.ARRIV_SET_ID)||'&d');
     utl_file.put_line(handle,n(ll.SHIP_SET_ID)||'&d'||n(ll.LINE_SET_ID)||'&el');
   end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');

/* Display any header or line sets associated with this order */

UTL_FILE.PUT_LINE(handle,'&f &f <a NAME="OE_SETS"> OE_SETS (SET) </a> &f');

UTL_FILE.PUT_LINE(handle,'&std &sh SET_ID &dh SET_NAME &dh SET_TYPE &dh HEADER_ID &dh LINE_ID &dh ITEM_ID &dh UOM &dh LINE_TYPE_ID &dh SYS_REQD &dh ');
UTL_FILE.PUT_LINE(handle,'STATUS &dh SCH_SHP_DT &dh SCH_ARV_DT &dh SHIP_FROM &dh SHIP_TO_ID &dh SHIP_PRIORITY &dh CARRIER &dh ');
UTL_FILE.PUT_LINE(handle,'SHIP_METHOD &dh STA &dh STB &eh');

Declare
cursor l_oe_sets is
select ST1.SET_ID                        SET_ID,
       ST1.SET_NAME                      SET_NAME,
       ST1.SET_TYPE                      SET_TYPE,
       ST1.HEADER_ID                     HEADER_ID,
       LST.LINE_ID                       LINE_ID,
       ST1.INVENTORY_ITEM_ID             ITEM_ID,
       ST1.ORDERED_QUANTITY_UOM          UOM,
       ST1.LINE_TYPE_ID                  LINE_TYPE_ID,
       nvl(LST.SYSTEM_REQUIRED_FLAG,'N') SYS_REQD,
       ST1.SET_STATUS                    STATUS,
       to_char(ST1.SCHEDULE_SHIP_DATE,'DD-MON-RR_HH24:MI:SS')    SCH_SHP_DT,
       to_char(ST1.SCHEDULE_ARRIVAL_DATE,'DD-MON-RR_HH24:MI:SS') SCH_ARV_DT,
       ST1.SHIP_FROM_ORG_ID              SHIP_FROM,
       ST1.SHIP_TO_ORG_ID                SHIP_TO_ID,
       ST1.SHIPMENT_PRIORITY_CODE        SHIP_PRIORITY,
       ST1.FREIGHT_CARRIER_CODE          CARRIER,
       ST1.SHIPPING_METHOD_CODE          SHIP_METHOD,
       ST1.SHIP_TOLERANCE_ABOVE          STA,
       ST1.SHIP_TOLERANCE_BELOW          STB
from OE_SETS                       ST1,
     OE_LINE_SETS                  LST
where ST1.SET_ID              = LST.SET_ID(+)
and   ST1.HEADER_ID = nvl('&header_id_selected',:v_header_id);          

begin
  for ll in l_oe_sets
  loop
   utl_file.put_line(handle,'&sld'||n(ll.SET_ID)||'&d'||n(ll.SET_NAME)||'&d');
   utl_file.put_line(handle,n(ll.SET_TYPE)||'&d'||n(ll.HEADER_ID)||'&d');
   utl_file.put_line(handle,n(ll.LINE_ID)||'&d'||n(ll.ITEM_ID)||'&d');
   utl_file.put_line(handle,n(ll.UOM)||'&d'||n(ll.LINE_TYPE_ID)||'&d');
   utl_file.put_line(handle,n(ll.SYS_REQD)||'&d'||n(ll.STATUS)||'&d');
   utl_file.put_line(handle,n(ll.SCH_SHP_DT)||'&d'||n(ll.SCH_ARV_DT)||'&d');
   utl_file.put_line(handle,n(ll.SHIP_FROM)||'&d'||n(ll.SHIP_TO_ID)||'&d');
   utl_file.put_line(handle,n(ll.SHIP_PRIORITY)||'&d'||n(ll.CARRIER)||'&d');
   utl_file.put_line(handle,n(ll.SHIP_METHOD)||'&d'||n(ll.STA)||'&d');
   utl_file.put_line(handle,n(ll.STB)||'&el');
  end loop;
end;

end if;

UTL_FILE.PUT_LINE(handle,'&et &f &f');

UTL_FILE.FCLOSE(handle);
end;
/

DECLARE
  handle UTL_FILE.FILE_TYPE;
  dirname varchar2(1000);
  text    varchar2(1000);

function n(v varchar2) return varchar2 is
begin
  if v is null then
   return '&sp';
   else
   return v;
  end if;
end n;

begin
-- Append to output file
  handle := UTL_FILE.FOPEN('&out_dir','&out_dir/&out_file','A',32000);
  UTL_FILE.PUT_LINE(handle,'&et'); -- in case last one failed

UTL_FILE.PUT_LINE(handle,'&f &f <a NAME="OE_ORDER_HEADERS"> OE_ORDER_HEADERS (ORD) </a> <a HREF="#OOH">Column Definitions</a> &f');
UTL_FILE.PUT_LINE(handle,'&std &sh WARNING &dh HEADER_ID &dh ORD_NUM &dh TYPE_ID &dh TYPE_NAME &dh FLOW_PROCESS &dh FLOW_CODE &dh BK &dh OP &dh CN &dh SP &dh ');
UTL_FILE.PUT_LINE(handle,'CUST_ID &dh CUST_NAME &dh SHIP_ID &dh SHIP_TO &dh BILL_ID &dh ORD_DATE &dh WH_ID &dh ORG &dh ');
UTL_FILE.PUT_LINE(handle,'CAT &dh PL_ID &dh PL_NAME &dh PO_NUMBER &dh STA &dh STB &dh OPERATING_UNIT &dh ORIG_SYS_DOC_REF &eh');

declare
cursor header1 is
     select distinct ORD.HEADER_ID            head_id,
            ORD.ORDER_NUMBER                  ord_no,
            ORD.ORDER_TYPE_ID                 ord_t,
            TYP.NAME                          ord_tna,
            ORD.SOLD_TO_ORG_ID                cus_id,
            substr(CUS.CUSTOMER_NAME,1,20)    cus_na,
            ORD.SHIP_TO_ORG_ID                ship_t,
            substr(SHP.NAME,1,12)             ship_tn,
            ORD.INVOICE_TO_ORG_ID             bill_t,
            to_char(ORD.ORDERED_DATE,'DD-MON-RR_HH24:MI:SS')  ord_d,
            nvl(ORD.BOOKED_FLAG,'N')          book_g,
            nvl(ORD.OPEN_FLAG,'N')            open_f,
            nvl(ORD.CANCELLED_FLAG,'N')       can_f,
            nvl(ORD.PARTIAL_SHIPMENTS_ALLOWED,'Y') spa,
            ORD.SHIP_FROM_ORG_ID                   or_id,
            PAR.ORGANIZATION_CODE                  or_co,
            ORD.FLOW_STATUS_CODE                   fl_co,
            ORD.ORDER_CATEGORY_CODE                cat_co,
            ORD.PRICE_LIST_ID                      pr_id,
            PRC.NAME                               pr_na,
            ORD.CUST_PO_NUMBER                     po_no,
            ORD.SHIP_TOLERANCE_ABOVE               shta,
            ORD.SHIP_TOLERANCE_BELOW               shtb,
            HR.NAME                                oper_unit,
            ORD.ORIG_SYS_DOCUMENT_REF              orig_sys_ref
from
     OE_ORDER_HEADERS                  ORD,
     MTL_PARAMETERS                    PAR,
     OE_TRANSACTION_TYPES_V            TYP,
     AR_CUSTOMERS                      CUS,
     OE_SHIP_TO_ORGS_V                 SHP,
     QP_LIST_HEADERS_V                 PRC,
     HR_OPERATING_UNITS                HR
where
     ORD.HEADER_ID                 = nvl('&header_id_selected',:v_header_id)
and  ORD.SHIP_FROM_ORG_ID          = PAR.ORGANIZATION_ID(+)
and  ORD.ORDER_TYPE_ID             = TYP.TRANSACTION_TYPE_ID(+)
and  ORD.SHIP_TO_ORG_ID            = SHP.SITE_USE_ID(+)
and  ORD.SOLD_TO_ORG_ID            = CUS.CUSTOMER_ID(+)
and  ORD.PRICE_LIST_ID             = PRC.LIST_HEADER_ID(+)
and  ORD.ORG_ID                    = HR.ORGANIZATION_ID(+);

r_ord_t  varchar2(100);
r_open_l number;
r_canc_l number;
r_canopen_l number;

type     per_record_typ is RECORD
               (flag    varchar2(1),
                descrip varchar2(200));
type     msg_tab is TABLE of per_record_typ INDEX by binary_integer;
msg      msg_tab;

begin
 dbms_output.enable(1000000);
 :r_error := 0;
-- Initialize error messages even if Do Analysis not selected
-- if substr(UPPER(nvl('&do_analysis','Y')),1,1) = 'Y' then
 for i in 1..60
 loop
   msg(i).flag := '0';
   msg(i).descrip := '';
 end loop;

 msg(10).descrip := '   10. Order_Header has associated lines with OPEN and CANCELLED flags set to Y.';
 msg(15).descrip := '   15. Order_Header status is BOOKED but "Book" activity has not been completed.';

 msg(20).descrip := '   20. Order_Header status is CLOSED but "Close" activity has not been completed.';
 msg(21).descrip := '   21. Order_Header status is CLOSED but exist open lines associated to this header.';
 msg(22).descrip := '   22. All associated lines has been closed but Order_Header is not ready to CLOSE.';

 msg(50).descrip := '   50. No WorkFlow Process associated to this line.';
 msg(51).descrip := '   51. Incorrect WorkFlow Process associated to this Order Header.';
 msg(52).descrip := '   52. Incorrect WorkFlow Activity associated to this Order Header.';
 msg(53).descrip := '   53. Incorrect WorkFlow Activity Result for this Order Header.';
 msg(54).descrip := '   54. Incorrect WorkFlow Activity Status for this Order Header.';
-- end if;

 for h in header1 
 loop
   :r_flag := '';

if substr(UPPER(nvl('&do_analysis','Y')),1,1) = 'Y' then
   --
   -- Gather general information
   --
   begin
     -- get Order_Header Flow name
     select WFA1.DISPLAY_NAME
       into r_ord_t
       FROM WF_ITEM_ACTIVITY_STATUSES         WFS,
            WF_PROCESS_ACTIVITIES             WFP,
            WF_ACTIVITIES_VL                  WFA,
            WF_ACTIVITIES_VL                  WFA1
      where WFS.ITEM_TYPE              = 'OEOH'
        and WFS.item_key               = to_Char(h.head_id)
        and WFS.PROCESS_ACTIVITY       = WFP.INSTANCE_ID(+)
        and WFP.PROCESS_NAME           = 'ROOT'
        and WFP.PROCESS_ITEM_TYPE      = WFA.ITEM_TYPE
        and WFP.PROCESS_NAME           = WFA.NAME
        and WFP.PROCESS_VERSION        = WFA.VERSION
        and WFP.ACTIVITY_ITEM_TYPE     = WFA1.ITEM_TYPE
        and WFP.ACTIVITY_NAME          = WFA1.NAME
        and WFA1.VERSION               = (select nvl(max(VERSION),-1)
                                            from WF_ACTIVITIES WF2
                                           where WF2.ITEM_TYPE = WFP.ACTIVITY_ITEM_TYPE
                                             and WF2.NAME      = WFP.ACTIVITY_NAME);

     -- Select current activity
     select WFA.DISPLAY_NAME, WFA1.DISPLAY_NAME,   
            WF_CORE.ACTIVITY_RESULT(WFA1.RESULT_TYPE,WFS.ACTIVITY_RESULT_CODE),
            LKP.MEANING
       into :r_pro_na, :r_act_na, :r_result, :r_act_s
       from WF_ITEM_ACTIVITY_STATUSES WFS,
            WF_PROCESS_ACTIVITIES     WFP,
            WF_ACTIVITIES_VL          WFA,
            WF_ACTIVITIES_VL          WFA1,
            WF_LOOKUPS                LKP
      where WFS.ITEM_TYPE          = 'OEOH'
        and WFS.item_key           = To_Char(h.head_id)
        and WFS.PROCESS_ACTIVITY   = WFP.INSTANCE_ID
        and WFP.PROCESS_ITEM_TYPE  = WFA.ITEM_TYPE
        and WFP.PROCESS_NAME       = WFA.NAME
        and WFP.PROCESS_VERSION    = WFA.VERSION
        and WFP.ACTIVITY_ITEM_TYPE = WFA1.ITEM_TYPE
        and WFP.ACTIVITY_NAME      = WFA1.NAME
        and WFA1.VERSION           = (select max(VERSION)
                                        from WF_ACTIVITIES WF2
                                       where WF2.ITEM_TYPE = WFP.ACTIVITY_ITEM_TYPE
                                         and WF2.NAME      = WFP.ACTIVITY_NAME)
        and LKP.LOOKUP_TYPE        = 'WFENG_STATUS'
        and LKP.LOOKUP_CODE        = WFS.ACTIVITY_STATUS
        and Execution_time         = (Select Max(execution_time) from WF_ITEM_ACTIVITY_STATUSES
                                       where ITEM_TYPE = 'OEOH'
                                         and item_key = To_Char(h.head_id));
   exception
     when no_data_found then
       :r_flag := :r_flag || '50 ';
       msg(50).flag := '1';
   end;

   -- Check if order has open lines and count also cancelled lines
   begin
   select sum(decode(open_flag,'Y',1,0)),sum(decode(cancelled_flag,'Y',1,0)),sum(decode(cancelled_flag,'Y',decode(open_flag,'Y',1,0),0))
     into r_open_l, r_canc_l, r_canopen_l
     from oe_order_lines_all
    where header_id = h.head_id
    group by header_id;
   exception
     when no_data_found then
       r_open_l := 0;
       r_canc_l := 0;
       r_canopen_l := 0;
   end;
   ---
   -- Basic Verifications
   --

   -- Open lines have cancelled flag showing as open
   if r_canopen_l > 0 then
     :r_flag := :r_flag || '10 ';
     msg(10).flag := '1';
   end if;

   -- All lines closed but Order Header status is not CLOSED and WF is NOT waiting to close.
   if r_open_l = 0    -- no open lines
    and h.fl_co <> 'CLOSED'  then
     if :r_pro_na = 'Close - Order'    -- check current status
       and :r_act_s = 'Deferred' then
        null;  -- OK just waiting for month end
     else
       :r_flag := :r_flag || '22 ';
       msg(22).flag := '1';
     end if;
   end if;   

   ---
   -- Verifications for Order Header on ENTERED status
   ---
   If h.fl_co = 'ENTERED' then
     -- verify current WF status
     if :r_pro_na <> 'Book - Order, Manual' then
       :r_flag := :r_flag || '51 ';
       msg(51).flag := '1';
      elsif :r_act_na <> 'Book - Eligible' then
        :r_flag := :r_flag || '52 ';
        msg(52).flag := '1';
      elsif :r_result is not null then
        :r_flag := :r_flag || '53 ';
        msg(53).flag := '1';
      elsif :r_act_s <> 'Notified' then
        :r_flag := :r_flag || '54 ';
        msg(54).flag := '1';
     end if;
   end if; -- ENTERED

   ---
   -- Verifications for Order Header on BOOKED status
   ---
   If h.fl_co = 'BOOKED' then
     -- Check is Book activity has been completed
     begin
       select WFA.DISPLAY_NAME, WFA1.DISPLAY_NAME,
              WF_CORE.ACTIVITY_RESULT(WFA1.RESULT_TYPE,WFS.ACTIVITY_RESULT_CODE),
              LKP.MEANING
         into :r_pro_na, :r_act_na, :r_result, :r_act_s
         from WF_ITEM_ACTIVITY_STATUSES WFS,
              WF_PROCESS_ACTIVITIES     WFP,
              WF_ACTIVITIES_VL          WFA,
              WF_ACTIVITIES_VL          WFA1,
              WF_LOOKUPS                LKP
        where WFS.ITEM_TYPE          = 'OEOH'
          and WFS.item_key           = to_char(h.head_id)
          and WFS.PROCESS_ACTIVITY   = WFP.INSTANCE_ID
          and WFP.PROCESS_ITEM_TYPE  = WFA.ITEM_TYPE
          and WFP.PROCESS_NAME       = WFA.NAME
          and WFP.PROCESS_VERSION    = WFA.VERSION
          and WFP.ACTIVITY_ITEM_TYPE = WFA1.ITEM_TYPE
          and WFP.ACTIVITY_NAME      = WFA1.NAME
          and WFA1.VERSION           = (select max(VERSION)
                                          from WF_ACTIVITIES WF2
                                         where WF2.ITEM_TYPE = WFP.ACTIVITY_ITEM_TYPE
                                           and WF2.NAME      = WFP.ACTIVITY_NAME)
          and LKP.LOOKUP_TYPE        = 'WFENG_STATUS'
          and LKP.LOOKUP_CODE        = WFS.ACTIVITY_STATUS
          and WFA1.DISPLAY_NAME      = 'Book';
     exception
       when no_data_found then
         :r_flag := :r_flag || '15 ';
         msg(15).flag := '1';
     end;

     if :r_act_na = 'Book' then
       if :r_result = 'Complete' 
          and :r_act_s = 'Complete' then
            null;  -- status is OK
        else
         :r_flag := :r_flag || '15 ';
         msg(15).flag := '1';
       end if;
     end if;
   end if; -- BOOKED

   ---
   -- Verifications for Order Header on CLOSED status
   ---
   If h.fl_co = 'CLOSED' then
     -- verify current WF status
     if :r_pro_na <> r_ord_t then
       :r_flag := :r_flag || '51 ';
       msg(51).flag := '1';
      elsif :r_act_na <> 'End' then
        :r_flag := :r_flag || '52 ';
        msg(52).flag := '1';
      elsif :r_result <> 'Null' then
        :r_flag := :r_flag || '53 ';
        msg(53).flag := '1';
      elsif :r_act_s <> 'Complete' then
        :r_flag := :r_flag || '54 ';
        msg(54).flag := '1';
     end if;

     -- Get status for CLOSE activity
     begin
       select WFA.DISPLAY_NAME, WFA1.DISPLAY_NAME,
              WF_CORE.ACTIVITY_RESULT(WFA1.RESULT_TYPE,WFS.ACTIVITY_RESULT_CODE),
              LKP.MEANING
         into :r_pro_na, :r_act_na, :r_result, :r_act_s
         from WF_ITEM_ACTIVITY_STATUSES WFS,
              WF_PROCESS_ACTIVITIES     WFP,
              WF_ACTIVITIES_VL          WFA,
              WF_ACTIVITIES_VL          WFA1,
              WF_LOOKUPS                LKP
        where WFS.ITEM_TYPE          = 'OEOH'
          and WFS.item_key           = To_Char(h.head_id)
          and WFS.PROCESS_ACTIVITY   = WFP.INSTANCE_ID
          and WFP.PROCESS_ITEM_TYPE  = WFA.ITEM_TYPE
          and WFP.PROCESS_NAME       = WFA.NAME
          and WFP.PROCESS_VERSION    = WFA.VERSION
          and WFP.ACTIVITY_ITEM_TYPE = WFA1.ITEM_TYPE
          and WFP.ACTIVITY_NAME      = WFA1.NAME
          and WFA1.VERSION           = (select max(VERSION)
                                          from WF_ACTIVITIES WF2
                                         where WF2.ITEM_TYPE = WFP.ACTIVITY_ITEM_TYPE
                                           and WF2.NAME      = WFP.ACTIVITY_NAME)
          and LKP.LOOKUP_TYPE        = 'WFENG_STATUS'
          and LKP.LOOKUP_CODE        = WFS.ACTIVITY_STATUS
          and WFA1.DISPLAY_NAME      = 'Close';
     exception
       when no_data_found then
         :r_flag := :r_flag || '20 ';
         msg(20).flag := '1';
     end;
     -- verify CLOSE activity completed
     if :r_act_na = 'Close' then
       if :r_result = 'Complete' 
          and :r_act_s = 'Complete' then
            null;  -- status is OK
        else
         :r_flag := :r_flag || '20 ';
         msg(20).flag := '1';
       end if;
     end if;

     -- check for open lines
     if r_open_l > 0 then 
       :r_flag := :r_flag || '21 ';
       msg(21).flag := '1';
     end if;
   end if; -- CLOSED
   
end if; -- do_analysis
  
   -- Print line to Output file
   utl_file.put_line(handle,'&sld &b <a HREF="#OHERR">'||n(:r_flag)||'</a> &eb &d');
   utl_file.put_line(handle,n(h.head_id)||'&d'||n(h.ord_no)||'&d');
   utl_file.put_line(handle,n(h.ord_t)||'&d'||n(h.ord_tna)||'&d');
   utl_file.put_line(handle,n(r_ord_t)||'&d'||n(h.fl_co)||'&d');
   utl_file.put_line(handle,n(h.book_g)||'&d'||n(h.open_f)||'&d');
   utl_file.put_line(handle,n(h.can_f)||'&d'||n(h.spa)||'&d');
   utl_file.put_line(handle,n(h.cus_id)||'&d'||n(h.cus_na)||'&d');
   utl_file.put_line(handle,n(h.ship_t)||'&d'||n(h.ship_tn)||'&d');
   utl_file.put_line(handle,n(h.bill_t)||'&d'||n(h.ord_d)||'&d');
   utl_file.put_line(handle,n(h.or_id)||'&d'||n(h.or_co)||'&d');
   utl_file.put_line(handle,n(h.cat_co)||'&d'||n(h.pr_id)||'&d');
   utl_file.put_line(handle,n(h.pr_na)||'&d'||n(h.po_no)||'&d');
   utl_file.put_line(handle,n(h.shta)||'&d'||n(h.shtb)||'&d');
   utl_file.put_line(handle,n(h.oper_unit)||'&d'||n(h.orig_sys_ref)||'&el');

   if :r_flag is not null then
     :r_error := 1;
   end if;
 end loop;
 utl_file.put_line(handle,'&et');

 if :r_error = 1 then
   utl_file.put_line(handle,'&f &b <a NAME="OHERR">Warning List:</a> &eb &f');
   for i in 1..60
   loop
     if msg(i).flag = '1' then
       utl_file.put_line(handle,msg(i).descrip||'&f');
     end if;
   end loop;
 end if;
end;


UTL_FILE.PUT_LINE(handle,'&et');

UTL_FILE.FCLOSE(handle);
end;
/


DECLARE
  handle UTL_FILE.FILE_TYPE;
  dirname varchar2(1000);
  text    varchar2(1000);

function n(v varchar2) return varchar2 is
begin
  if v is null then
   return '&sp';
   else
   return v;
  end if;
end n;

begin
-- If :v_head_only = 'N' then 

-- Append to output file
  handle := UTL_FILE.FOPEN('&out_dir','&out_dir/&out_file','A',32000);
  UTL_FILE.PUT_LINE(handle,'&et'); -- in case last one failed

if substr(UPPER(nvl('&prt_wf','Y')),1,1) = 'Y' then

UTL_FILE.PUT_LINE(handle,'&f &f WORKFLOW ORDER STATUS  &f');

UTL_FILE.PUT_LINE(handle,'&std');
UTL_FILE.PUT_LINE(handle,'&sh PROCESS_NAME &dh ACTIVITY_NAME &dh RESULT &dh ACT_STATUS &dh NOTIF_ID &dh INT_PROCESS_NAME &dh ');
UTL_FILE.PUT_LINE(handle,'INT_ACTIVITY_NAME &dh INT_RESULT_CODE &dh INT_ACTIVITY_STATUS &dh BEGIN_DATE &dh END_DATE &dh ERROR_NAME &eh');


Declare
cursor l_wf_order is
select WFA.DISPLAY_NAME      Process,
       WFA1.DISPLAY_NAME     Activity,
       WF_CORE.ACTIVITY_RESULT(WFA1.RESULT_TYPE,WFS.ACTIVITY_RESULT_CODE) Result,
       LKP.MEANING           Act_status,
       WFS.NOTIFICATION_ID   Notif,
       WFP.PROCESS_NAME      I_process,
       WFP.ACTIVITY_NAME     I_activity,
       WFS.ACTIVITY_STATUS   I_status,
       WFS.ACTIVITY_RESULT_CODE   I_Act_Res_code,
       to_char(WFS.BEGIN_DATE,'DD-MON-RR_HH24:MI:SS') Begin_d,
       to_char(WFS.END_DATE,'DD-MON-RR_HH24:MI:SS')   End_d,
       WFS.ERROR_NAME        error_name
from WF_ITEM_ACTIVITY_STATUSES WFS,
     WF_PROCESS_ACTIVITIES     WFP,
     WF_ACTIVITIES_VL          WFA,
     WF_ACTIVITIES_VL          WFA1,
     WF_LOOKUPS                LKP
where 
     WFS.ITEM_TYPE          = 'OEOH'
and  WFS.item_key           = nvl('&header_id_selected',to_char(:v_header_id))
and  WFS.PROCESS_ACTIVITY   = WFP.INSTANCE_ID
and  WFP.PROCESS_ITEM_TYPE  = WFA.ITEM_TYPE
and  WFP.PROCESS_NAME       = WFA.NAME
and  WFP.PROCESS_VERSION    = WFA.VERSION
and  WFP.ACTIVITY_ITEM_TYPE = WFA1.ITEM_TYPE
and  WFP.ACTIVITY_NAME      = WFA1.NAME
and  WFA1.VERSION           = 
                             (select max(VERSION)
                              from WF_ACTIVITIES WF2
                              where WF2.ITEM_TYPE = WFP.ACTIVITY_ITEM_TYPE
and   WF2.NAME              = WFP.ACTIVITY_NAME)
and  LKP.LOOKUP_TYPE        = 'WFENG_STATUS'
and  LKP.LOOKUP_CODE        = WFS.ACTIVITY_STATUS
order by WFS.ITEM_KEY, 
         WFS.BEGIN_DATE, 
         EXECUTION_TIME;

begin
 for ll in l_wf_order
 loop
   utl_file.put_line(handle,'&sld'||n(ll.Process)||'&d'||n(ll.Activity)||'&d');
   utl_file.put_line(handle,n(ll.Result)||'&d'||n(ll.Act_status)||'&d');
   utl_file.put_line(handle,n(ll.Notif)||'&d'||n(ll.I_process)||'&d');
   utl_file.put_line(handle,n(ll.I_activity)||'&d'||n(ll.I_Act_Res_code)||'&d');
   utl_file.put_line(handle,n(ll.I_status)||'&d'||n(ll.Begin_d)||'&d');
   utl_file.put_line(handle,n(ll.End_d)||'&d'||n(ll.Error_name)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');

UTL_FILE.PUT_LINE(handle,'&f &f WORKFLOW ORDER NOTIFICATION INFORMATION (WFN)&f');
UTL_FILE.PUT_LINE(handle,'&std &sh NOTIF_ID &dh TO_USER &dh ORIG_RECIP &dh RECIP_ROLE &dh MAIL_STAT &dh MESSAGE_NAME &dh STATUS &dh SUBJECT &eh');


Declare
cursor l_wf_ord_noti is
select WFN.NOTIFICATION_ID         NOTIF_ID,
       WFN.TO_USER                 TO_USER,
       WFN.ORIGINAL_RECIPIENT      ORIG_RECIP,
       WFN.RECIPIENT_ROLE          RECIP_ROLE,
       WFN.MAIL_STATUS             MAIL_STAT,
       WFN.MESSAGE_NAME            MESSAGE_NAME,
       WFN.STATUS                  STATUS,
       WFN.SUBJECT                 SUBJECT
from WF_ITEM_ACTIVITY_STATUSES WFS,
     WF_NOTIFICATIONS          WFN
where WFS.ITEM_TYPE          = 'OEOH'
 and  WFS.item_key           = nvl('&header_id_selected',to_char(:v_header_id))
 and  WFS.NOTIFICATION_ID    is not null
 and  WFN.NOTIFICATION_ID    = WFS.NOTIFICATION_ID
order by WFS.ITEM_KEY, 
         WFS.BEGIN_DATE, 
         EXECUTION_TIME;

begin
 for ll in l_wf_ord_noti
 loop
   utl_file.put_line(handle,'&sld'||n(ll.NOTIF_ID)||'&d'||n(ll.TO_USER)||'&d');
   utl_file.put_line(handle,n(ll.ORIG_RECIP)||'&d'||n(ll.RECIP_ROLE)||'&d');
   utl_file.put_line(handle,n(ll.MAIL_STAT)||'&d'||n(ll.MESSAGE_NAME)||'&d');
   utl_file.put_line(handle,n(ll.STATUS)||'&d'||n(ll.SUBJECT)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');


/*
UTL_FILE.PUT_LINE(handle,'&f &f WORKFLOW ORDER SKIP INFORMATION (WFSKIP)&f');
UTL_FILE.PUT_LINE(handle,'&std &sh NOTIF_ID &dh TO_USER &dh ORIG_RECIP &dh RECIP_ROLE &dh MAIL_STAT &dh MESSAGE_NAME &dh STATUS &dh SUBJECT &eh');

Declare
r_exist   number;

begin
  select count(*)
    into r_exist
    from all_tables
   where table_name = 'ONT_WF_SKIP_LOG';
  if r_exist = 0 then
    utl_file.put_line(handle,'&et Table ONT_WF_SKIP_LOG is not present on this instance: &f');
 end if;
end;

Declare
cursor l_wf_ord_skip is
   select WFS.HEADER_ID      HEADER_ID,
       WFA.DISPLAY_NAME      DISPLAY_NAME,
       to_char(WFS.CREATION_DATE,'DD-MON-RR_HH24:MI:SS') CRE_DATE,
       WFS.USER_ID           USER_ID,
       WFS.RESPONSIBILITY_ID RESPONSIBILITY_ID,
       WFS.APPLICATION_ID    APPLICATION_ID
  from ONT_WF_SKIP_LOG           WFS,
       WF_PROCESS_ACTIVITIES     WFP,
       WF_ACTIVITIES_VL          WFA
 where WFS.LINE_ID            is null
  and  WFS.HEADER_ID          = nvl('&header_id_selected',to_char(:v_header_id))
  and  WFS.ACTIVITY_ID        = WFP.INSTANCE_ID
  and  WFP.PROCESS_ITEM_TYPE  = WFA.ITEM_TYPE
  and  WFP.PROCESS_NAME       = WFA.NAME
  and  WFP.PROCESS_VERSION    = WFA.VERSION
 order by WFS.HEADER_ID, 
          WFS.CREATION_DATE;

begin
 for ll in l_wf_ord_skip
 loop
   utl_file.put_line(handle,'&sld'||n(ll.HEADER_ID)||'&d'||n(ll.DISPLAY_NAME)||'&d');
   utl_file.put_line(handle,n(ll.CRE_DATE)||'&d'||n(ll.USER_ID)||'&d');
   utl_file.put_line(handle,n(ll.RESPONSIBILITY_ID)||'&d'||n(ll.APPLICATION_ID)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');  */


UTL_FILE.PUT_LINE(handle,'&f &f WORKFLOW ORDER LEVEL ERRORS &f');
UTL_FILE.PUT_LINE(handle,'&std &sh PROCESS_NAME &dh ERROR_ACTIVITY_NAME &dh RESULT &dh ACT_STATUS &dh ERROR_NAME &dh ERROR_MESSAGE &dh ERROR_STACK &eh');


Declare
cursor l_wf_ord_err is
select WFA.DISPLAY_NAME    Process,
       WFA1.DISPLAY_NAME   Activity,
       WF_CORE.ACTIVITY_RESULT(WFA1.RESULT_TYPE,WFS.ACTIVITY_RESULT_CODE) result,
       LKP.MEANING         status,
       WFS.ERROR_NAME      err_name,
       WFS.ERROR_MESSAGE   err_mess,
       WFS.ERROR_STACK     err_stack
from WF_ITEM_ACTIVITY_STATUSES WFS,
     WF_PROCESS_ACTIVITIES     WFP,
     WF_ACTIVITIES_VL          WFA,
     WF_ACTIVITIES_VL          WFA1,
     WF_LOOKUPS                LKP
where 
     WFS.ITEM_TYPE          = 'OEOH'
and  WFS.item_key           = nvl('&header_id_selected',to_char(:v_header_id))
and  WFS.PROCESS_ACTIVITY   = WFP.INSTANCE_ID
and  WFP.PROCESS_ITEM_TYPE  = WFA.ITEM_TYPE
and  WFP.PROCESS_NAME       = WFA.NAME
and  WFP.PROCESS_VERSION    = WFA.VERSION
and  WFP.ACTIVITY_ITEM_TYPE = WFA1.ITEM_TYPE
and  WFP.ACTIVITY_NAME      = WFA1.NAME
and  WFA1.VERSION = 
    (select max(VERSION)
     from WF_ACTIVITIES WF2
     where WF2.ITEM_TYPE = WFP.ACTIVITY_ITEM_TYPE
     and   WF2.NAME      = WFP.ACTIVITY_NAME)
and  LKP.LOOKUP_TYPE = 'WFENG_STATUS'
and  LKP.LOOKUP_CODE = WFS.ACTIVITY_STATUS
and  WFS.ACTIVITY_STATUS = 'ERROR'
order by WFS.ITEM_KEY, WFS.BEGIN_DATE, EXECUTION_TIME;

begin
 for ll in l_wf_ord_err
 loop
   utl_file.put_line(handle,'&sld'||n(ll.Process)||'&d'||n(ll.Activity)||'&d');
   utl_file.put_line(handle,n(ll.result)||'&d'||n(ll.status)||'&d');
   utl_file.put_line(handle,n(ll.err_name)||'&d'||n(ll.err_mess)||'&d');
   utl_file.put_line(handle,n(ll.err_stack)||'&el');
 end loop;
end;


UTL_FILE.PUT_LINE(handle,'&et');


-- break on ERR_TYPE_KEY skip 2;

UTL_FILE.PUT_LINE(handle,'&f &f WORKFLOW ACTIVITY STATUS FOR ORDER ERROR PROCESS &f');
UTL_FILE.PUT_LINE(handle,'&std &sh ERR_TYPE_KEY &dh ERR_PROCESS_NAME &dh ERR_ACTIVITY_NAME &dh RESULT &dh ACT_STATUS &dh NOTIF_ID &dh ');
UTL_FILE.PUT_LINE(handle,'ASGND_USER &dh BEGIN_DATE &dh END_DATE &eh');


Declare
cursor l_wf_ord_erp is
select WFS.ITEM_TYPE || '-' || WFS.ITEM_KEY               ERR_TYPE_KEY, 
       WFA.DISPLAY_NAME           ERR_PROCESS_NAME,
       WFA1.DISPLAY_NAME          ERR_ACTIVITY_NAME,
       WF_CORE.ACTIVITY_RESULT(WFA1.RESULT_TYPE,WFS.ACTIVITY_RESULT_CODE) RESULT,
       LKP.MEANING                ACT_STATUS,
       WFS.NOTIFICATION_ID        NOTIF_ID,
       WFS.ASSIGNED_USER          ASGND_USER,
       to_char(WFS.BEGIN_DATE,'DD-MON-RR_HH24:MI:SS') BEGIN_DATE,
       to_char(WFS.END_DATE,'DD-MON-RR_HH24:MI:SS') END_DATE
from WF_ITEM_ACTIVITY_STATUSES WFS,
     WF_PROCESS_ACTIVITIES     WFP,
     WF_ACTIVITIES_VL          WFA,
     WF_ACTIVITIES_VL          WFA1,
     WF_LOOKUPS                LKP,
     WF_ITEMS                  WFI
where 
       WFS.ITEM_TYPE          = WFI.ITEM_TYPE
  and  WFS.item_key           = WFI.ITEM_KEY
  and  WFS.PROCESS_ACTIVITY   = WFP.INSTANCE_ID
  and  WFP.PROCESS_ITEM_TYPE  = WFA.ITEM_TYPE
  and  WFP.PROCESS_NAME       = WFA.NAME
  and  WFP.PROCESS_VERSION    = WFA.VERSION
  and  WFP.ACTIVITY_ITEM_TYPE = WFA1.ITEM_TYPE
  and  WFP.ACTIVITY_NAME      = WFA1.NAME
  and  WFA1.VERSION = 
      (select max(VERSION)
       from WF_ACTIVITIES WF2
       where WF2.ITEM_TYPE = WFP.ACTIVITY_ITEM_TYPE
       and   WF2.NAME      = WFP.ACTIVITY_NAME)
  and  LKP.LOOKUP_TYPE = 'WFENG_STATUS'
  and  LKP.LOOKUP_CODE = WFS.ACTIVITY_STATUS
  and  WFI.PARENT_ITEM_TYPE = 'OEOH'
  and  WFI.PARENT_ITEM_KEY  = nvl('&header_id_selected',to_char(:v_header_id))
  and  WFI.ITEM_TYPE in (select WFAE.ERROR_ITEM_TYPE
                         from WF_ITEM_ACTIVITY_STATUSES WFSE,
                         WF_PROCESS_ACTIVITIES     WFPE,
                         WF_ACTIVITIES_VL          WFAE,
                         WF_ACTIVITIES_VL          WFA1E
                         where 
                                WFSE.ITEM_TYPE = 'OEOH'
                           and  WFSE.ITEM_KEY  = nvl('&header_id_selected',to_char(:v_header_id))
                           and  WFSE.PROCESS_ACTIVITY   = WFPE.INSTANCE_ID
                           and  WFPE.PROCESS_ITEM_TYPE  = WFAE.ITEM_TYPE
                           and  WFPE.PROCESS_NAME       = WFAE.NAME
                           and  WFPE.PROCESS_VERSION    = WFAE.VERSION
                           and  WFPE.ACTIVITY_ITEM_TYPE = WFA1E.ITEM_TYPE
                           and  WFPE.ACTIVITY_NAME      = WFA1E.NAME
                           and  WFA1E.VERSION = 
                               (select max(VERSION)
                                from WF_ACTIVITIES WF2E
                                where WF2E.ITEM_TYPE = WFPE.ACTIVITY_ITEM_TYPE
                                and   WF2E.NAME      = WFPE.ACTIVITY_NAME)
                           and  WFSE.ACTIVITY_STATUS = 'ERROR')
  order by WFS.ITEM_KEY, WFS.BEGIN_DATE, EXECUTION_TIME;

begin
 for ll in l_wf_ord_erp
 loop
   utl_file.put_line(handle,'&sld'||n(ll.ERR_TYPE_KEY)||'&d'||n(ll.ERR_PROCESS_NAME)||'&d');
   utl_file.put_line(handle,n(ll.ERR_ACTIVITY_NAME)||'&d'||n(ll.RESULT)||'&d');
   utl_file.put_line(handle,n(ll.ACT_STATUS)||'&d'||n(ll.NOTIF_ID)||'&d');
   utl_file.put_line(handle,n(ll.ASGND_USER)||'&d'||n(ll.BEGIN_DATE)||'&d');
   utl_file.put_line(handle,n(ll.END_DATE)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');

end if; -- prt_wf

-- end if; -- v_head_only

UTL_FILE.FCLOSE(handle);
end;
/

DECLARE
  handle UTL_FILE.FILE_TYPE;
  dirname varchar2(1000);
  text    varchar2(1000);

function n(v varchar2) return varchar2 is
begin
  if v is null then
   return '&sp';
   else
   return v;
  end if;
end n;

begin
-- If :v_head_only = 'N' then 

-- Append to output file
  handle := UTL_FILE.FOPEN('&out_dir','&out_dir/&out_file','A',32000);
  UTL_FILE.PUT_LINE(handle,'&et'); -- in case last one failed

UTL_FILE.PUT_LINE(handle,'&f &f GENERIC HOLDS (ORDER AND/OR LINES) &f');
UTL_FILE.PUT_LINE(handle,'&std &sh HOLD_ID &dh HOLD_NAME &dh HOLD_TYPE &dh WF_ITEM &dh WF_ACTIVITY &dh ORD_HOLD_ID &dh HLD_SRC_ID &dh HLD_REL_ID &dh ');
UTL_FILE.PUT_LINE(handle,'HEADER_ID &dh H_REL &dh S_REL &dh RELEASE_REASON &dh ENTITY &dh ENTITY_ID &dh ENTITY2 &dh ENTITY_ID2 &dh HOLD_UNTIL  &eh');

Declare
cursor l_ord_hld is
SELECT 
  HDF.HOLD_ID                        ,
  HDF.NAME                           ,
  HDF.TYPE_CODE                      ,
  HDF.ITEM_TYPE                      ,
  HDF.ACTIVITY_NAME                  ,
  HLD.ORDER_HOLD_ID                  ,
  HLD.HOLD_SOURCE_ID                 ,
  HLD.HOLD_RELEASE_ID                ,
  HLD.HEADER_ID                      ,
  HLD.RELEASED_FLAG                  Rel_Flag1,
  HSR.RELEASED_FLAG                  Rel_Flag2,
  HRL.RELEASE_REASON_CODE            Rel_code,
  decode(HSR.HOLD_ENTITY_CODE,
         'B','Bill To',
         'C','Customer',
         'I','Item',
         'O','Order',
         'S','Ship To',
         'W','Warehouse',
         HSR.HOLD_ENTITY_CODE)       entity,
  HSR.HOLD_ENTITY_ID                 entity_id,
  decode(HSR.HOLD_ENTITY_CODE2,
         'B','Bill To',
         'C','Customer',
         'I','Item',
         'O','Order',
         'S','Ship To',
         'W','Warehouse',
         HSR.HOLD_ENTITY_CODE2)      entity2,
  HSR.HOLD_ENTITY_ID2                entity_id2,
  to_char(HSR.HOLD_UNTIL_DATE,'DD-MON-RR_HH24:MI:SS') Hold_until
from OE_ORDER_HOLDS_ALL    HLD,
     OE_HOLD_SOURCES_ALL   HSR,
     OE_HOLD_DEFINITIONS   HDF,
     OE_HOLD_RELEASES      HRL
where        HLD.HEADER_ID                 = nvl('&header_id_selected',:v_header_id)
        and  HLD.HOLD_SOURCE_ID       = HSR.HOLD_SOURCE_ID
        and  HSR.HOLD_ID              = HDF.HOLD_ID
        and  HLD.HOLD_RELEASE_ID      = HRL.HOLD_RELEASE_ID(+)
        and  HLD.LINE_ID              IS NULL;

begin
 for ll in l_ord_hld
 loop
   utl_file.put_line(handle,'&sld'||n(ll.HOLD_ID)||'&d'||n(ll.NAME)||'&d');
   utl_file.put_line(handle,n(ll.TYPE_CODE)||'&d'||n(ll.ITEM_TYPE)||'&d');
   utl_file.put_line(handle,n(ll.ACTIVITY_NAME)||'&d'||n(ll.ORDER_HOLD_ID)||'&d');
   utl_file.put_line(handle,n(ll.HOLD_SOURCE_ID)||'&d'||n(ll.HOLD_RELEASE_ID)||'&d');
   utl_file.put_line(handle,n(ll.HEADER_ID)||'&d'||n(ll.Rel_Flag1)||'&d');
   utl_file.put_line(handle,n(ll.Rel_Flag2)||'&d'||n(ll.Rel_code)||'&d');
   utl_file.put_line(handle,n(ll.entity)||'&d'||n(ll.entity_id)||'&d');
   utl_file.put_line(handle,n(ll.entity2)||'&d'||n(ll.entity_id2)||'&d');
   utl_file.put_line(handle,n(ll.Hold_until)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');

-- This section is commented out because it runs slowly without an index 
--<do not run> CREATE INDEX OE_PROCESSING_MSGS_777
--<do not run> ON ONT.OE_PROCESSING_MSGS
--<do not run> (header_id, line_id);

-- column ACTIVITY   format a15;
-- column msg_Source format a14;  
-- column DESCRIPTION format a30;

-- PROMPT
UTL_FILE.PUT_LINE(handle,'&f &f  HEADER PROCESSING MESSAGES &f');
UTL_FILE.PUT_LINE(handle,'&std &sh WARNING &dh HEADER_ID &dh MSG_SOURCE &dh ACTIVITY &dh REQUEST_ID &dh DESCRIPTION &dh MESSAGE_TEXT &eh');

declare
 cursor h_proc is
 select distinct
        MSG.header_id                   HEADER_ID,
    decode(MSG.MESSAGE_SOURCE_CODE,
           'U','U=On-Line(UI)',
           'C','C=Conc Process',
           'W','W=Workflow',
           MSG.MESSAGE_SOURCE_CODE)     MSG_SOURCE,
    MSG.PROCESS_ACTIVITY                PROCESS_ACTIVITY,       
    MSG.request_id                      REQUEST_ID,
    MST.message_text                    MESSAGE_TEXT
 from oe_processing_msgs_vl     MSG,
      oe_processing_msgs_tl     MST,
      fnd_languages             FLA
 where  MSG.header_id      = nvl('&header_id_selected',:v_header_id)
   and  msg.transaction_id = mst.transaction_id
   and  MST.LANGUAGE                  = FLA.LANGUAGE_CODE 
   and  FLA.INSTALLED_FLAG            = 'B' 
   and  decode(MSG.LINE_ID,9.99E+125,NULL,MSG.LINE_ID) is NULL;

r_activity varchar2(100);
r_descrip  varchar2(100);

type     per_record_typ is RECORD
               (flag    varchar2(1),
                descrip varchar2(200));
type     msg_tab is TABLE of per_record_typ INDEX by binary_integer;
msg      msg_tab;

begin
 :r_error := 0;
-- Initialize error messages even if Do Analysis not selected
-- if substr(UPPER(nvl('&do_analysis','Y')),1,1) = 'Y' then
 for i in 1..10
 loop
   msg(i).flag := '0';
   msg(i).descrip := '';
 end loop;

 msg(1).descrip := '    1. Cannot find Activity name associated to HEADER PROCESSING MESSAGES.';
 msg(2).descrip := '    2. Cannot find Request Description.';
-- end if;

 for hp in h_proc 
 loop
   :r_flag := '';
   :r_error := 0;
   r_activity := '';
   r_descrip  := '';

if substr(UPPER(nvl('&do_analysis','Y')),1,1) = 'Y' then
   -- Get Activity name
   begin
     select WFA1.DISPLAY_NAME
       into r_activity
       from WF_PROCESS_ACTIVITIES  WFP,
            WF_ACTIVITIES_VL       WFA1 
      where WFP.INSTANCE_ID        = hp.PROCESS_ACTIVITY
        and WFP.ACTIVITY_ITEM_TYPE = WFA1.ITEM_TYPE
        and WFP.ACTIVITY_NAME      = WFA1.NAME
        and WFA1.VERSION = (select max(VERSION)
                              from WF_ACTIVITIES WF2
                             where WF2.ITEM_TYPE = WFP.ACTIVITY_ITEM_TYPE
                               and WF2.NAME      = WFP.ACTIVITY_NAME);
    exception
      when no_data_found then
        :r_flag := :r_flag || '1 ';
        msg(1).flag := '1';
   end;

   -- Get Request description
   begin
     select DESCRIPTION         
       into r_descrip
       FROM FND_CONCURRENT_REQUESTS FCR
      where FCR.REQUEST_ID    = hp.REQUEST_ID;
    exception
      when no_data_found then
        :r_flag := :r_flag || '2 ';
        msg(2).flag := '1';
   end;

end if; -- do_analysis
  
   -- Print line to Output file
   utl_file.put_line(handle,'&sld &b <a HREF="#HPMERR">'||n(:r_flag)||'</a> &eb &d');
   utl_file.put_line(handle,n(hp.HEADER_ID)||'&d'||n(hp.MSG_SOURCE)||'&d');
   utl_file.put_line(handle,n(r_activity)||'&d'||n(hp.REQUEST_ID)||'&d');
   utl_file.put_line(handle,n(r_descrip)||'&d'||n(hp.MESSAGE_TEXT)||'&el');

   if :r_flag is not null then
     :r_error := 1;
   end if;
 end loop;
 utl_file.put_line(handle,'&et');

 if :r_error = 1 then
   utl_file.put_line(handle,'&f &b <a NAME="HPMERR">Warning List:</a> &eb &f');
   for i in 1..10
   loop
     if msg(i).flag = '1' then
       utl_file.put_line(handle,msg(i).descrip||'&f');
     end if;
   end loop;
 end if;
end;

UTL_FILE.PUT_LINE(handle,'&et');

-- end if; -- v_head_only

UTL_FILE.FCLOSE(handle);
end;
/

-- <do not run> DROP INDEX OE_PROCESSING_MSGS_777

DECLARE
  handle UTL_FILE.FILE_TYPE;
  dirname varchar2(1000);
  text    varchar2(1000);

function n(v varchar2) return varchar2 is
begin
  if v is null then
   return '&sp';
   else
   return v;
  end if;
end n;

begin
-- If :v_head_only = 'N' then 

-- Append to output file
  handle := UTL_FILE.FOPEN('&out_dir','&out_dir/&out_file','A',32000);
  UTL_FILE.PUT_LINE(handle,'&et'); -- in case last one failed


if substr(UPPER(nvl('&prt_price','Y')),1,1) = 'Y' then

UTL_FILE.PUT_LINE(handle,'&f &f HEADER PRICE ADJUSTMENTS (ADJ)   <a HREF="#ADJC">Column Definitions</a> &f');

UTL_FILE.PUT_LINE(handle,'&std &sh APPLIED &dh PRC_ADJ_ID &dh LST_HD_ID &dh LST_LN_ID &dh LIST_LN_NO &dh MOD_LVL &dh ');
UTL_FILE.PUT_LINE(handle,'LIST_TYPE_CODE &dh CHG_TY_CD &dh ARITH_OP &dh OP_PER_QTY &dh ADJ_AMT_PQ &dh OPERAND &dh ');
UTL_FILE.PUT_LINE(handle,'ADJ_AMT &dh CD &dh AF &dh PI &dh AC &dh IF &dh EF &dh UA &dh UF &dh AP &dh LK &dh ');
UTL_FILE.PUT_LINE(handle,'PERC &dh COST_ID &dh TAX_CODE &dh PP &eh');

declare
 cursor h_prc_adj is
select
ADJ.PRICE_ADJUSTMENT_ID            PRC_ADJ_ID,
nvl(ADJ.APPLIED_FLAG,'N')          APPLIED_FLAG,
ADJ.LIST_HEADER_ID                 LST_HD_ID,
ADJ.LIST_LINE_ID                   LST_LN_ID,
ADJ.LIST_LINE_NO                   LIST_LN_NO,   
ADJ.MODIFIER_LEVEL_CODE            MOD_LVL,     
ADJ.LIST_LINE_TYPE_CODE            LIST_TYPE_CODE,
ADJ.CHARGE_TYPE_CODE               CHG_TY_CD,             
ADJ.ARITHMETIC_OPERATOR            ARITH_OP,  
ADJ.OPERAND_PER_PQTY               OP_PER_QTY,        
ADJ.ADJUSTED_AMOUNT_PER_PQTY       ADJ_AMT_PQ,
ADJ.OPERAND                        OPERAND,                      
ADJ.ADJUSTED_AMOUNT                ADJ_AMT,                             
ADJ.CREDIT_OR_CHARGE_FLAG          CD,                          
ADJ.AUTOMATIC_FLAG                 AF,   
ADJ.PRINT_ON_INVOICE_FLAG          PI,   
ADJ.ACCRUAL_FLAG                   AC,        
ADJ.INVOICED_FLAG                  INF,              
ADJ.ESTIMATED_FLAG                 EF, 
ADJ.UPDATE_ALLOWED                 UA,        
ADJ.UPDATED_FLAG                   UF,         
ADJ.APPLIED_FLAG                   AP, 
ADJ.LOCK_CONTROL                   LK,                                      
ADJ.PERCENT                        PERC,            
ADJ.COST_ID                        COST_ID,               
ADJ.TAX_CODE                       TAX_CODE,               
ADJ.PRICING_PHASE_ID               PP
from OE_PRICE_ADJUSTMENTS   ADJ
where  ADJ.HEADER_ID                 = nvl('&header_id_selected',:v_header_id)
        and  ADJ.LINE_ID              IS NULL
--        and  ADJ.APPLIED_FLAG         = 'Y'
order by ADJ.APPLIED_FLAG,LIST_TYPE_CODE;

begin
 for hh in h_prc_adj
 loop
   utl_file.put_line(handle,'&sld'||n(hh.APPLIED_FLAG)||'&d'||n(hh.PRC_ADJ_ID)||'&d'||n(hh.LST_HD_ID)||'&d');
   utl_file.put_line(handle,n(hh.LST_LN_ID)||'&d'||n(hh.LIST_LN_NO)||'&d'||n(hh.MOD_LVL)||'&d');
   utl_file.put_line(handle,n(hh.LIST_TYPE_CODE)||'&d'||n(hh.CHG_TY_CD)||'&d'||n(hh.ARITH_OP)||'&d');
   utl_file.put_line(handle,n(hh.OP_PER_QTY)||'&d'||n(hh.ADJ_AMT_PQ)||'&d'||n(hh.OPERAND)||'&d');
   utl_file.put_line(handle,n(hh.ADJ_AMT)||'&d'||n(hh.CD)||'&d'||n(hh.AF)||'&d');
   utl_file.put_line(handle,n(hh.PI)||'&d'||n(hh.AC)||'&d'||n(hh.INF)||'&d');
   utl_file.put_line(handle,n(hh.EF)||'&d'||n(hh.UA)||'&d'||n(hh.UF)||'&d');
   utl_file.put_line(handle,n(hh.AP)||'&d'||n(hh.LK)||'&d'||n(hh.PERC)||'&d');
   utl_file.put_line(handle,n(hh.COST_ID)||'&d'||n(hh.TAX_CODE)||'&d'||n(hh.PP)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');

end if; -- prt_price


UTL_FILE.PUT_LINE(handle,'&f &f MTL_SALES_ORDERS (MSO)  &f');
UTL_FILE.PUT_LINE(handle,'&std &sh DS_HEADER_ID &dh HEADER_ID &dh ORDER_NUMBER &dh ORDER_TYPE_NAME &dh CONSTANTOE &eh');

declare
 cursor mtl_sal is
select
     MSO.SALES_ORDER_ID     ,
     ORD.HEADER_ID                 ,
     ORD.ORDER_NUMBER              ,
     TYP.NAME                      ,
     MSO.SEGMENT3
from
     MTL_SALES_ORDERS              MSO,
     OE_ORDER_HEADERS              ORD,
     OE_TRANSACTION_TYPES_VL       TYP
where
     ORD.ORDER_TYPE_ID             = TYP.TRANSACTION_TYPE_ID
and  TO_CHAR(ORD.ORDER_NUMBER)     = MSO.SEGMENT1
and  TYP.NAME(+)                   = MSO.SEGMENT2
-- klr
and  MSO.SEGMENT1                  = '&order_number_selected'
and  ORD.HEADER_ID                 = nvl('&header_id_selected',:v_header_id) 
order by
     ORD.HEADER_ID;

begin
 for ms in mtl_sal
 loop
   utl_file.put_line(handle,'&sld'||n(ms.SALES_ORDER_ID)||'&d'||n(ms.HEADER_ID)||'&d');
   utl_file.put_line(handle,n(ms.ORDER_NUMBER)||'&d'||n(ms.NAME)||'&d');
   utl_file.put_line(handle,n(ms.SEGMENT3)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');

begin
  select distinct(MSO.SALES_ORDER_ID) 
    into :sales_ord_id 
    from MTL_SALES_ORDERS              MSO,
         OE_ORDER_HEADERS              ORD,
         OE_TRANSACTION_TYPES_TL       TYP,
         FND_LANGUAGES                 FLA
   where ORD.HEADER_ID                 = nvl('&header_id_selected',:v_header_id)
     and ORD.ORDER_TYPE_ID             = TYP.TRANSACTION_TYPE_ID
     and TYP.LANGUAGE                  = FLA.LANGUAGE_CODE 
     and FLA.INSTALLED_FLAG            = 'B' 
     -- klr
     and MSO.SEGMENT1                  = '&order_number_selected'
     and TYP.NAME                      = MSO.SEGMENT2;
end;

-- end if; -- v_head_only

UTL_FILE.FCLOSE(handle);
end;
/


DECLARE
  handle UTL_FILE.FILE_TYPE;
  dirname varchar2(1000);
  text    varchar2(1000);
  c_lines number;

function n(v varchar2) return varchar2 is
begin
  if v is null then
   return '&sp';
   else
   return v;
  end if;
end n;

begin
c_lines := 0;
If :v_head_only = 'N' then 

-- Append to output file
  handle := UTL_FILE.FOPEN('&out_dir','&out_dir/&out_file','A',32000);
  UTL_FILE.PUT_LINE(handle,'&et'); -- in case last one failed


UTL_FILE.PUT_LINE(handle,'&f &f <a NAME="OE_ORDER_LINES">OE_ORDER_LINES (LIN)</a> <a HREF="#OOL">Column Definitions</a> &f');

-- Titles will be printed only if lines are selected, titles repeat every 50 lines

declare
 cursor line1 is 
  select 
    to_char(LIN.line_number) || 
          decode(LIN.shipment_number, null, null, '.' || to_char(LIN.shipment_number))|| 
          decode(LIN.option_number, null, null, '.' || to_char(LIN.option_number)) ||
          decode(LIN.component_number, null, null, 
                 decode(LIN.option_number, null, '.',null)||
                 '.'||to_char(LIN.component_number))||
          decode(LIN.service_number,null,null,
                 decode(LIN.component_number, null, '.' , null) ||
                        decode(LIN.option_number, null, '.', null ) ||
                        '.'|| to_char(LIN.service_number))  line_n,
     LIN.LINE_ID                   Line_i,
     LIN.INVENTORY_ITEM_ID         Item_i,
     lpad(' ',length(LIN.SORT_ORDER)/4,rpad('.',80,'....!'))||ITM.SEGMENT1   Item_na,
     substr(LIN.FLOW_STATUS_CODE,1,22) Line_st,
     nvl(LIN.ORDERED_QUANTITY,0)   Ordq,
     LIN.ORDER_QUANTITY_UOM        Orduom,
     nvl(LIN.ORDERED_QUANTITY2,0)  Ordq2,
     LIN.ORDERED_QUANTITY_UOM2     Orduom2,
     LIN.UNIT_SELLING_PRICE        sell,
     LIN.UNIT_LIST_PRICE           lisl,
     nvl(LIN.SHIPPED_QUANTITY,0)   shpq,
     nvl(LIN.SHIPPED_QUANTITY2,0)  shpq2,
     nvl(LIN.SHIPPING_QUANTITY,0)  SHN_Q,
     nvl(LIN.SHIPPING_QUANTITY2,0) SHN_Q2,
     nvl(LIN.SHIPPING_QUANTITY_UOM2,0)  SHN_QUOM2,
     nvl(FULFILLED_QUANTITY,0)     fulq,                  
     nvl(FULFILLED_QUANTITY2,0)    fulq2,
     nvl(LIN.CANCELLED_QUANTITY,0) canq,      
     nvl(LIN.CANCELLED_QUANTITY2,0) canq2,      
     nvl(LIN.INVOICED_QUANTITY,0)  invq, 
     substr(LIN.SCHEDULE_STATUS_CODE,1,5)      schc,
     nvl(LIN.OPEN_FLAG,'N')        openf,
     nvl(LIN.BOOKED_FLAG,'N')      bookf,
     nvl(LIN.SHIPPABLE_FLAG,'N')   shipf,
     nvl(LIN.CANCELLED_FLAG,'N')   canf,
     nvl(LIN.VISIBLE_DEMAND_FLAG,'N')  vdem,
     nvl(LIN.FULFILLED_FLAG, 'N')  fulf,
     nvl(LIN.SHIPPING_INTERFACED_FLAG,'N')    SI,
     decode(nvl(LIN.ATO_LINE_ID,0),0,'N','Y') ato_i,
     nvl(LIN.SHIP_MODEL_COMPLETE_FLAG,'N')  smcf,
     LIN.SHIP_FROM_ORG_ID          shipfrom,
--     PAR.ORGANIZATION_CODE         org_i,
     to_char(LIN.REQUEST_DATE,'DD-MON-RR_HH24:MI:SS')  reqd,
     to_char(LIN.SCHEDULE_SHIP_DATE,'DD-MON-RR_HH24:MI:SS') schd,
--     TYP.NAME                      lin_ty,
     LIN.LINE_TYPE_ID              LINE_TYPE_ID,
     LIN.LINE_CATEGORY_CODE        lin_cat,
     LIN.ITEM_TYPE_CODE            itm_tc,
     LIN.ORDERED_ITEM_ID           ord_it,
     LIN.ORDERED_ITEM              ord_it_name,
     LIN.SOURCE_TYPE_CODE          Src_tc,
     LIN.PRICE_LIST_ID             prlst,
     LIN.DEMAND_CLASS_CODE         demc,
     nvl(LIN.OPTION_FLAG,'N')                 CFG,
     LIN.TOP_MODEL_LINE_ID                    PRT_LN_ID,
     LIN.ATO_LINE_ID                          ATO_LN_ID,
     LIN.LINK_TO_LINE_ID                      LNK_LN_ID,
     LIN.SPLIT_BY                             SPL_BY,
     LIN.SPLIT_FROM_LINE_ID                   SPL_LN_ID,
     LIN.CONFIG_HEADER_ID                     CFG_HD_ID, 
     nvl(LIN.INVOICE_INTERFACE_STATUS_CODE,'N') INVC_INT_STAT,
     LIN.SHIP_TOLERANCE_ABOVE                 STA,
     LIN.SHIP_TOLERANCE_BELOW                 STB,            
     LIN.SHIP_SET_ID                          SH_SET_ID,
     ST2.SET_TYPE                             SH_SET_TY,
     LIN.LINE_SET_ID                          LN_SET_ID,
     ST3.SET_TYPE                             LN_SET_TY,
     LIN.ARRIVAL_SET_ID                       AR_SET_ID,
     ST1.SET_TYPE                             AR_SET_TY,
     LIN.CALCULATE_PRICE_FLAG                 CAL_PR,
     to_char(LIN.ACTUAL_SHIPMENT_DATE,'DD-MON-RR_HH24:MI:SS')  act_shp,
     to_char(LIN.CREATION_DATE,'DD-MON-RR_HH24:MI:SS')  cre_date,
     LIN.ordered_quantity2                    ord_qty2,
     LIN.ordered_quantity_uom2                ord_uom2,
     to_char(LIN.LAST_UPDATE_DATE,'DD-MON-RR_HH24:MI:SS')  upd_date,
     ITM.TRACKING_QUANTITY_IND                trck_qty_ind,
     ITM.PRIMARY_UOM_CODE                     PRM_UOM,
     ITM.DUAL_UOM_CONTROL                     DUAL_UOM,
     ITM.SECONDARY_DEFAULT_IND                SEC_DEF_IND,
     ITM.SECONDARY_UOM_CODE                   SEC_UOM,
     ITM.CHILD_LOT_FLAG                       CHLD_LOT,
     ITM.PARENT_CHILD_GENERATION_FLAG         PAR_CHLD,
     ITM.LOT_DIVISIBLE_FLAG                   LOT_DIV,
     ITM.GRADE_CONTROL_FLAG                   GRAD_CTRL,
     Decode(ITM.ONT_PRICING_QTY_SOURCE,
            'P','Primary', 'Secondary')       ONT_PR_QTY,
     ITM.dual_uom_deviation_high              DUAL_UOM_DEV_H,
     ITM.dual_uom_deviation_low               DUAL_UOM_DEV_L,
     ITM.lot_control_code                     lot_ctl,
     ITM.location_control_code                loct_ctl,
     ITM.lot_status_enabled                   status_ctl,
     Decode(ITM.RESERVABLE_TYPE,1,'Reserv',
       2,'Non-Res',
       'Val: '||to_char(ITM.RESERVABLE_TYPE)) RES_TYPE,
     LIN.source_document_id                   src_doc_id,
     LIN.source_document_line_id              src_doc_lin_id,
     LIN.order_source_id                      ord_src_id,
     LIN.ORIG_SYS_DOCUMENT_REF                o_sys_doc_r,
     LIN.ORIG_SYS_LINE_REF                    o_sys_lin_r
from
     OE_ORDER_LINES                LIN,
--     OE_TRANSACTION_TYPES          TYP,
     MTL_SYSTEM_ITEMS              ITM,
--     MTL_PARAMETERS                PAR,
     OE_SETS                       ST1,
     OE_SETS                       ST2,
     OE_SETS                       ST3
where
     LIN.HEADER_ID                  = nvl('&header_id_selected',:v_header_id)
and  NVL('&line_id_selected',0)     in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID)
-- and  LIN.LINE_TYPE_ID               = TYP.TRANSACTION_TYPE_ID
-- and  LIN.SHIP_FROM_ORG_ID           = PAR.ORGANIZATION_ID(+)
and  LIN.SHIP_FROM_ORG_ID           = ITM.ORGANIZATION_ID(+)
and  LIN.INVENTORY_ITEM_ID          = ITM.INVENTORY_ITEM_ID(+)
-- and  LIN.OPTION_NUMBER              IS NULL
-- and  LIN.ITEM_TYPE_CODE             <> 'INCLUDED'
and  LIN.ARRIVAL_SET_ID            = ST1.SET_ID(+)
and  LIN.SHIP_SET_ID               = ST2.SET_ID(+)
and  LIN.LINE_SET_ID               = ST3.SET_ID(+)
order by
    nvl(LIN.line_number,0), nvl(LIN.shipment_number,0), nvl(LIN.option_number,0),
    nvl(LIN.component_number,0), nvl(LIN.service_number,0);
--     NVL(LIN.ATO_LINE_ID,               LIN.LINE_ID),
--     NVL(LIN.SORT_ORDER,                '0000'),
--     NVL(LIN.LINK_TO_LINE_ID,           LIN.LINE_ID),
--     NVL(LIN.SOURCE_DOCUMENT_LINE_ID,   LIN.LINE_ID),
--     NVL(LIN.SERVICE_REFERENCE_LINE_ID, LIN.LINE_ID),
--     LIN.LINE_ID;  



 cursor setl(lin number) is 
  select os.set_id, os.set_type
    from oe_sets os, oe_line_sets ols
   where os.set_id = ols.set_id
     and line_id = lin;

r_del_det  varchar2(100);
r_pro_nab  varchar2(100);
r_act_nab  varchar2(100);
r_resultb  varchar2(100);
r_act_sb   varchar2(100);
r_sets     varchar2(100);
r_omint    varchar2(100);
r_invint   varchar2(100);
org_i      varchar2(100);
lin_ty     varchar2(100);

type     per_record_typ is RECORD
               (flag    varchar2(1),
                descrip varchar2(200));
type     msg_tab is TABLE of per_record_typ INDEX by binary_integer;
msg      msg_tab;
c_lines  number;


begin
  :r_error := 0;
  c_lines := 51;

-- Initialize error messages even if Do Analysis not selected
-- if substr(UPPER(nvl('&do_analysis','Y')),1,1) = 'Y' then
  for i in 1..70
  loop
    msg(i).flag := '0';
    msg(i).descrip := '';
  end loop;
  msg(1).descrip  := '    1. Ordered Quantity is less than Shipped Quantity.';
  msg(2).descrip  := '    2. Ordered Quantity is less than Fulfilled Quantity.';
  msg(3).descrip  := '    3. Ordered Quantity is less than Invoiced Quantity.';
  msg(4).descrip  := '    4. Interface to WSH set to "N" but exist Delivery Details associated to this Order_line.';
  msg(5).descrip  := '    5. Associated Delivery Details has NOT been interface but Shipped, Fulfilled or Invoiced QTY are updated.';

  msg(10).descrip := '   10. Order_Line shows as OPEN but CANCELLED flag is set to Y.';

  msg(11).descrip := '   11. Order_Line status ENTERED but Shipped Qty is greater than zero.';
  msg(12).descrip := '   12. Order_Line status ENTERED but Fulfilled Qty is greater than zero.';

  msg(15).descrip := '   15. Order_Line status BOOKED but "Book" activity has not been completed.';
  msg(16).descrip := '   16. Order_Line status BOOKED but Shipped Qty is greater than zero.';
  msg(17).descrip := '   17. Order_Line status BOOKED but Fulfilled Qty is greater than zero.';

  msg(20).descrip := '   20. Order_Line status AWAITING SHIPPING but Item is set to Non-Shippable.';
  msg(21).descrip := '   21. Order_Line status AWAITING SHIPPING but Shipped Qty is greater than zero.';
  msg(22).descrip := '   22. Order_Line status AWAITING SHIPPING but Fulfilled Qty is greater than zero.';
  msg(23).descrip := '   23. No Delivery Details associated to this Order_line, Interface to WSH is set to "Y".';
  msg(24).descrip := '   24. Associated Delivery Details have invalid RELEASED status.';
  msg(25).descrip := '   25. Associated Delivery Details have invalid INTERFACE status (Not Shipped but Interfaced to OM or INV).';

  msg(40).descrip := '   40. Line status is CLOSED but associated Delivery Details still not interface to OM.';
  msg(41).descrip := '   41. Line status is CLOSED but associated Delivery Details still not interface to INVentory.';

  msg(45).descrip := '   45. Line status is CANCELLED but associated Delivery Details has been interfaced to OM.';
  msg(46).descrip := '   46. Line status is CANCELLED but associated Delivery Details has been interfaced to INVentory.';

  msg(50).descrip := '   50. No WorkFlow Process associated to this line.';
  msg(51).descrip := '   51. Incorrect WorkFlow Process associated to this line.';
  msg(52).descrip := '   52. Incorrect WorkFlow Activity associated to this line.';
  msg(53).descrip := '   53. Incorrect WorkFlow Activity Result for this line.';
  msg(54).descrip := '   54. Incorrect WorkFlow Activity Status for this line.';
  msg(55).descrip := '   55. Order Line status is BOOKED, WorkFlow Activities shows additional activities for this line.';

  msg(60).descrip := '   60. Organization_id is Null for this line.';
  msg(61).descrip := '   61. Transaction Type is Null for this line.';
-- end if;

 for i in line1 
 loop
   :r_flag := '';
   r_del_det := '';
   r_sets := '';

   -- get Order_line WF name
   begin
     :r_line_t := '';
     select WFA.DISPLAY_NAME
       into :r_line_t
       FROM WF_ITEM_ACTIVITY_STATUSES     WFS,
            WF_PROCESS_ACTIVITIES         WFP,
            WF_ACTIVITIES_VL              WFA
      WHERE WFS.ITEM_TYPE           = 'OEOL'
        -- klr
        and WFS.ITEM_KEY            = to_char(I.LINE_I)
        and WFS.PROCESS_ACTIVITY    = WFP.INSTANCE_ID
        and WFP.PROCESS_NAME        = 'ROOT'
        and WFP.ACTIVITY_ITEM_TYPE  = WFA.ITEM_TYPE
        and WFP.ACTIVITY_NAME       = WFA.NAME
        and nvl(WFA.VERSION,-1)     = (select nvl(max(VERSION),-1)
                                         from WF_ACTIVITIES WF2
                                        where WF2.ITEM_TYPE = WFP.ACTIVITY_ITEM_TYPE
                                          and WF2.NAME      = WFP.ACTIVITY_NAME);
   exception
     when no_data_found then
       :r_flag := :r_flag || '50 ';
       msg(50).flag := '1';
   end;

   -- get organization code
   begin
     select ORGANIZATION_CODE
       into org_i
       from MTL_PARAMETERS 
      where i.shipfrom   = ORGANIZATION_ID(+);
   exception
     when no_data_found then
       org_i := null;
       :r_flag := :r_flag || '60 ';
       msg(60).flag := '1';
   end;

   -- get Transaction Type name
   begin
     select NAME
       into lin_ty
       from OE_TRANSACTION_TYPES
      where i.LINE_TYPE_ID   = TRANSACTION_TYPE_ID;
   exception
     when others then
       lin_ty := null;
       :r_flag := :r_flag || '61 ';
       msg(61).flag := '1';
   end;

   -- Check if exist associated Delivery Detail id regardles of line status and SI flag.
   begin
     select Delivery_detail_id
       into r_del_det
       from WSH_Delivery_Details
      where SOURCE_LINE_ID   = I.LINE_I
        and source_code = 'OE'
        and rownum = 1;
   exception
     when no_data_found then
       null;   -- conditions will be evaluated later
   end;

   -- Get associated sets
   for sl in setl(i.line_i)
   loop
     r_sets := r_sets || to_char(sl.set_id)||'-'||sl.set_type || ' ';
   end loop;

   -- Reservations
   select sum(nvl(RESERVATION_QUANTITY,0)) 
     into :r_res_q
     from MTL_RESERVATIONS RES
    where RES.DEMAND_SOURCE_HEADER_ID = :sales_ord_id
      and RES.DEMAND_SOURCE_TYPE_ID in  (2,8,9,21,22)
      and RES.DEMAND_SOURCE_LINE_ID  = i.LINE_I;

if substr(UPPER(nvl('&do_analysis','Y')),1,1) = 'Y' then
   -- current WF status
   begin
     :r_pro_na := '';
     :r_act_na := '';
     :r_result := '';
     :r_act_s  := '';

     select WFA.DISPLAY_NAME, WFA1.DISPLAY_NAME,
            WF_CORE.ACTIVITY_RESULT(WFA1.RESULT_TYPE,WFS.ACTIVITY_RESULT_CODE),
            LKP.MEANING
       into :r_pro_na, :r_act_na, :r_result, :r_act_s
       from WF_ITEM_ACTIVITY_STATUSES WFS,
            WF_PROCESS_ACTIVITIES     WFP,
            WF_ACTIVITIES_VL          WFA,
            WF_ACTIVITIES_VL          WFA1,
            WF_LOOKUPS                LKP
      where WFS.ITEM_TYPE          = 'OEOL'
        and WFS.item_key           = To_Char(i.line_i)
        and WFS.PROCESS_ACTIVITY   = WFP.INSTANCE_ID
        and WFP.PROCESS_ITEM_TYPE  = WFA.ITEM_TYPE
        and WFP.PROCESS_NAME       = WFA.NAME
        and WFP.PROCESS_VERSION    = WFA.VERSION
        and WFP.ACTIVITY_ITEM_TYPE = WFA1.ITEM_TYPE
        and WFP.ACTIVITY_NAME      = WFA1.NAME
        and rownum = 1                        -- to manage ORA-1422
        and WFA1.VERSION =  (select max(VERSION)  from WF_ACTIVITIES WF2
                             where WF2.ITEM_TYPE = WFP.ACTIVITY_ITEM_TYPE
                               and WF2.NAME      = WFP.ACTIVITY_NAME)
        and LKP.LOOKUP_TYPE        = 'WFENG_STATUS'
        and LKP.LOOKUP_CODE        = WFS.ACTIVITY_STATUS
        and Execution_time         = (Select Max(execution_time) from WF_ITEM_ACTIVITY_STATUSES
                                       where item_type = 'OEOL' and Item_key = To_Char(i.line_i));
   exception
     when no_data_found then
       if msg(50).flag = '1' then 
         null;   -- problem already discovered
       else
         :r_flag := :r_flag || '50 ';
         msg(50).flag := '1';
       end if;
   end;

   ---
   -- Basic Verification
   --
   -- Ordered Qty less than Shipped (with Tolerance)
   If  i.Ordq*(100+nvl(i.sta,0))/100 < i.shpq then  
     :r_flag := :r_flag || '1 ';
     msg(1).flag := '1';
   end if;

   -- Ordered Qty less than Fulfilled (with Tolerance)
   If  i.Ordq*(100+nvl(i.sta,0))/100 < i.fulq then
     :r_flag := :r_flag || '2 ';
     msg(2).flag := '1';
   end if;

   -- Ordered Qty less than Invoiced (with Tolerance)
   If  i.Ordq*(100+nvl(i.sta,0))/100 < i.invq then
     :r_flag := :r_flag || '3 ';
     msg(3).flag := '1';
   end if;

   If i.si = 'N' then     -- Non interfaced to WSH
     select count(*) -- count total wdd for this line
       into :r_wdd
       from WSH_Delivery_Details
      where SOURCE_LINE_ID   = I.LINE_I
        and source_code = 'OE';
     If :r_wdd > 0 then
       :r_flag := :r_flag || '4 ';
       msg(4).flag := '1';
     end if;
   end if;

   -- If line is Shippable
   -- Check if shipped, fulfilled or invoiced quantities has been updated and associated Del.Details still not Interfaced to OM
   if  i.shipf = 'Y' then   -- Shippable line
     if i.shpq > 0
      or i.fulq > 0 
      or i.invq > 0 then
       select count(*)    -- count if any associated Delivery Detail has been OM interfaced
         into r_omint
         from WSH_Delivery_Details
        where SOURCE_LINE_ID   = I.LINE_I
          and source_code = 'OE'
          and OE_INTERFACED_FLAG = 'Y';
       if r_omint = 0 then
         :r_flag := :r_flag || '5 ';
         msg(5).flag := '1';
       end if;
     end if;
   end if;

   -- line open but Cancelled flag is Y
   if i.openf = 'Y' 
    and i.canf = 'Y' then
     :r_flag := :r_flag || '10 ';
     msg(10).flag := '1';
   end if;

   ---
   -- Verifications for Order_Lines on ENTERED status
   ---
   If i.line_st = 'ENTERED' then

     if  i.shpq > 0 then     -- Quantity Shipped
       :r_flag := :r_flag || '11 ';
       msg(11).flag := '1';
     end if;
     if i.fulq > 0 then      -- Quantity Fulfilled
       :r_flag := :r_flag || '12 ';
       msg(12).flag := '1';
     end if;

     -- verify current WF status
     if :r_pro_na <> 'Enter - Line' then
       :r_flag := :r_flag || '51 ';
       msg(51).flag := '1';
      elsif :r_act_na <> 'Wait for Booking' then
        :r_flag := :r_flag || '52 ';
        msg(52).flag := '1';
      elsif :r_result is not null then
        :r_flag := :r_flag || '53 ';
        msg(53).flag := '1';
      elsif :r_act_s <> 'Notified' then
        :r_flag := :r_flag || '54 ';
        msg(54).flag := '1';
     end if;
   end if; -- ENTERED

   ---
   -- Verifications for Order_Lines on BOOKED status
   ---
   If i.line_st = 'BOOKED' then
     -- Check is Book activity has been completed
     begin
       select WFA.DISPLAY_NAME, WFA1.DISPLAY_NAME,
              WF_CORE.ACTIVITY_RESULT(WFA1.RESULT_TYPE,WFS.ACTIVITY_RESULT_CODE),
              LKP.MEANING
         into r_pro_nab, r_act_nab, r_resultb, r_act_sb
         from WF_ITEM_ACTIVITY_STATUSES WFS,
              WF_PROCESS_ACTIVITIES     WFP,
              WF_ACTIVITIES_VL          WFA,
              WF_ACTIVITIES_VL          WFA1,
              WF_LOOKUPS                LKP
        where WFS.ITEM_TYPE          = 'OEOL'
          and WFS.item_key           = To_Char(i.line_i)
          and WFS.PROCESS_ACTIVITY   = WFP.INSTANCE_ID
          and WFP.PROCESS_ITEM_TYPE  = WFA.ITEM_TYPE
          and WFP.PROCESS_NAME       = WFA.NAME
          and WFP.PROCESS_VERSION    = WFA.VERSION
          and WFP.ACTIVITY_ITEM_TYPE = WFA1.ITEM_TYPE
          and WFP.ACTIVITY_NAME      = WFA1.NAME
          and WFA1.VERSION =  (select max(VERSION)  from WF_ACTIVITIES WF2
                               where WF2.ITEM_TYPE = WFP.ACTIVITY_ITEM_TYPE
                                 and WF2.NAME      = WFP.ACTIVITY_NAME)
          and LKP.LOOKUP_TYPE        = 'WFENG_STATUS'
          and LKP.LOOKUP_CODE        = WFS.ACTIVITY_STATUS
          and WFA1.DISPLAY_NAME      = 'Wait for Booking';
     exception
       when no_data_found then
         :r_flag := :r_flag || '15 ';
         msg(15).flag := '1';
     end;

     if r_act_nab = 'Wait for Booking' then
       if r_resultb = 'Null' 
          and r_act_sb = 'Complete' then
            null;  -- status is OK
        else
         :r_flag := :r_flag || '15 ';
         msg(15).flag := '1';
       end if;
     end if;

     if  i.shpq > 0 then     -- Quantity Shipped
       :r_flag := :r_flag || '16 ';
       msg(16).flag := '1';
     end if;
     if i.fulq > 0 then      -- Quantity Fulfilled
       :r_flag := :r_flag || '17 ';
       msg(17).flag := '1';
     end if;

     -- Check if there are other activities after Booking.
     if :r_act_na = 'Wait for Booking' then
       null;   -- OK last activity is Booking
      else
       :r_flag := :r_flag || '55 ';
       msg(55).flag := '1';
     end if;

   end if; -- BOOKED

   ---
   -- Verifications for Order_Lines on AWAITING SHIPPING status
   ---
   If i.line_st = 'AWAITING_SHIPPING' then
     if i.shipf = 'N' then   -- No shippable line
       :r_flag := :r_flag || '20 ';
       msg(20).flag := '1';
     end if;
     if  i.shpq > 0 then     -- Quantity Shipped
       :r_flag := :r_flag || '21 ';
       msg(21).flag := '1';
     end if;
     if i.fulq > 0 then      -- Quantity Fulfilled
       :r_flag := :r_flag || '22 ';
       msg(22).flag := '1';
     end if;
     select count(*) -- count total wdd for this line
       into :r_wdd
       from WSH_Delivery_Details
      where SOURCE_LINE_ID   = I.LINE_I
        and source_code = 'OE';
     If :r_wdd = 0 and i.si = 'Y' then
       :r_flag := :r_flag || '23 ';
       msg(23).flag := '1';
      else
       select count(*)  -- Invalid Release flag status
         into :r_wdd
         from WSH_Delivery_Details
        where SOURCE_LINE_ID   = I.LINE_I
          and source_code = 'OE'
          and RELEASED_STATUS  not in ('Y','R','S','B','P','C','N','D','X');
       If :r_wdd > 0 then
         :r_flag := :r_flag || '24 ';
         msg(24).flag := '1';
       end if;
       select count(*) -- count Released status 'R' but OM or INV interfaced
         into :r_wdd
         from WSH_Delivery_Details
        where SOURCE_LINE_ID   = I.LINE_I
          and source_code = 'OE'
          and RELEASED_STATUS <> 'C'
          and (nvl(INV_INTERFACED_FLAG,'N') not in ( 'N','X')
               or nvl(OE_INTERFACED_FLAG,'N') <> 'N');
       If :r_wdd > 0 then
         :r_flag := :r_flag || '25 ';
         msg(25).flag := '1';
       end if;
     end if; -- no wdd

     -- verify current WF status
     if :r_pro_na <> 'Ship - Line, Manual' then
       :r_flag := :r_flag || '51 ';
       msg(51).flag := '1';
      elsif :r_act_na <> 'Ship' then
        :r_flag := :r_flag || '52 ';
        msg(52).flag := '1';
      elsif :r_result is not null then
        :r_flag := :r_flag || '53 ';
        msg(53).flag := '1';
      elsif :r_act_s <> 'Notified' then
        :r_flag := :r_flag || '54 ';
        msg(54).flag := '1';
     end if;
 
   end if; -- AWAITING SHIPPING

   ---
   -- Verifications for Order_Lines on SHIPPED status
   ---
   If i.line_st = 'SHIPPED' then
     null;  -- nothing yet
     -- :r_flag := :r_flag || '';
     -- msg(30).flag := '1';
   end if; -- SHIPPED

   ---
   -- Verifications for Order_Lines on FULFILLED status
   ---
   If i.line_st = 'FULFILLED' then
     null;  -- nothing yet
     -- :r_flag := :r_flag || '';
     -- msg(40).flag := '1';
   end if; -- FULFILLED

   ---
   -- Verifications for Order_Lines on INVOICED status
   ---
   If i.line_st = 'INVOICED' then
     null; -- nothing yet
     -- :r_flag := :r_flag || '';
     -- msg(40).flag := '1';
   end if; -- INVOICED

   ---
   -- Verifications for Order_Lines on CLOSED status
   ---
   If i.line_st = 'CLOSED' then
     -- If line is Shippable check if associated Del.Details still not Interfaced to OM or INV
     if i.shipf = 'Y' then
       -- count if any associated Delivery Detail has NOT been OM or INV interfaced 
       begin
         select sum(decode(nvl(OE_INTERFACED_FLAG,'N'),'N',1,0)), sum(decode(nvl(INV_INTERFACED_FLAG,'N'),'N',1,0))
           into r_omint, r_invint
           from WSH_Delivery_Details
          where SOURCE_LINE_ID   = I.LINE_I
            and SOURCE_CODE = 'OE'
            and released_status <> 'D'
          group by SOURCE_LINE_ID;
        exception
         when no_data_found then
           r_omint := 0;
           r_invint := 0; 
       end;
       if r_omint > 0 then
         :r_flag := :r_flag || '40 ';
         msg(40).flag := '1';
       end if;
       if r_invint > 0 then
         :r_flag := :r_flag || '41 ';
         msg(41).flag := '1';
       end if;
     end if;
   end if; -- CLOSED

   ---
   -- Verifications for Order_Lines on CANCELLED status
   ---
   If i.line_st = 'CANCELLED' then
     -- If line is Shippable check if associated Del.Details open Pending Interfaced to OM or INV
     if i.shipf = 'Y' then
       -- count if any associated Delivery Detail has been interfaced to OM or INV
       begin
         select sum(decode(nvl(OE_INTERFACED_FLAG,'N'),'Y',1,0)), sum(decode(nvl(INV_INTERFACED_FLAG,'N'),'Y',1,0))
           into r_omint, r_invint
           from WSH_Delivery_Details
          where SOURCE_LINE_ID  = I.LINE_I
            and source_code = 'OE'
            and RELEASED_STATUS <> 'D'
          group by SOURCE_LINE_ID;
        exception
         when no_data_found then
           r_omint := 0;
           r_invint := 0; 
       end; 
       if r_omint > 0 then
         :r_flag := :r_flag || '45 ';
         msg(45).flag := '1';
       end if;
       if r_invint > 0 then
         :r_flag := :r_flag || '46 ';
         msg(46).flag := '1';
       end if;
     end if;
   end if; -- CANCELLED
end if; -- do_analysis

   if c_lines >= 50 then
     if c_lines = 51 then
       UTL_FILE.PUT_LINE(handle,'&std ');
     end if;
     c_lines := 1;
     UTL_FILE.PUT_LINE(handle,'&sh WARNING &dh LINE &dh LINE_ID &dh DELIVERY DETAIL &dh ITEM ID &dh ITEM &dh ');
     UTL_FILE.PUT_LINE(handle,'FLOW PROCESS &dh FLOW CODE &dh OP &dh BK &dh SH &dh CN &dh VD &dh SI &dh FF &dh ATO &dh ');
     UTL_FILE.PUT_LINE(handle,'SMC &dh ORDER QTY &dh UOM &dh SELL PRICE &dh LIST PRICE &dh ');
     UTL_FILE.PUT_LINE(handle,'CALC PRICE &dh RESERV QTY &dh SHIPNG QTY &dh SHIPPD QTY &dh FULFIL QTY &dh CANCEL QTY &dh ');
     UTL_FILE.PUT_LINE(handle,'INVOIC QTY &dh SCHD ST_CD &dh WH_ID &dh ORG &dh CREATE DATE &dh ');
     UTL_FILE.PUT_LINE(handle,'REQUEST DATE &dh SCHED DATE &dh ACTUAL SHIP_DATE &dh LINE TYPE &dh LINE CATEG &dh ');
     UTL_FILE.PUT_LINE(handle,'ITEM TYPE &dh ORDERED ITEM_ID &dh ORDERED ITEM &dh SOURCE TYPE &dh ');
     UTL_FILE.PUT_LINE(handle,'PRICE LIST_ID &dh DEMAND CLASS &dh CFG &dh PRT LN_ID &dh ATO LN_ID &dh LINK LN_ID &dh ');
     UTL_FILE.PUT_LINE(handle,'SPLIT BY &dh SPLIT LN_ID &dh CONFIG HD_ID &dh INVOIC INT_STAT &dh STA &dh STB &dh SHIP SET_ID &dh ');
     UTL_FILE.PUT_LINE(handle,'SHIP SET_TY &dh LINE SET_ID &dh LINE SET_TY &dh ARRIV SET_ID &dh ARRIV SET_TY &dh OTHER SETS &dh ');
     UTL_FILE.PUT_LINE(handle,'DISCRETE ORDER_QTY &dh DISCR UOM &dh DISCRETE SHPNG_QTY &dh DISCRETE SHPNG_UOM &dh ');
     UTL_FILE.PUT_LINE(handle,'DISCRETE SHIPPD_QTY &dh DISCRETE FULFLL_QTY &dh DISCRETE CANCEL_QTY &dh ');
     UTL_FILE.PUT_LINE(handle,'LAST UPD_DATE &dh');
     UTL_FILE.PUT_LINE(handle,'TRACK QTY_IND &dh PRIMARY UOM_COD &dh DUAL_UOM CONTROL &dh SECONDARY DEFAULT_IND &dh ');
     UTL_FILE.PUT_LINE(handle,'SECONDARY UOM_COD &dh CHILD LOT_FLAG &dh PARENT_CHILD GEN_FLAG &dh LOT DIVIS_FLAG &dh ');
     UTL_FILE.PUT_LINE(handle,'GRADE CNTRL_FLAG &dh ONT_PRICING QTY_SOURCE &dh DUAL_UOM DEV_HGH &dh DUAL_UOM DEV_LOW &dh ');
     UTL_FILE.PUT_LINE(handle,'LOT CONTROL &dh LOCATION CTRL_CODE &dh STATUS CONTROL &dh RESERVABLE &dh ');
     UTL_FILE.PUT_LINE(handle,'SOURCE DOC_ID &dh SOURCE DOC_LIN_ID &dh ORDER SRC_ID &dh ORIG_SYS DOC_REF &dh ORIG_SYS LINE_REF &eh');
    else
     c_lines := c_lines + 1;
   end if;

   -- Print line to Output file
   utl_file.put_line(handle,'&sld &b <a HREF="#OLERR">'||n(:r_flag)||'</a> &eb &d'||n(i.line_n)||'&d');
   utl_file.put_line(handle,'<a NAME="OOE'||i.Line_i||'">'||n(i.Line_i)||'</a>'||'&d'||'<a HREF="#WDD'||r_del_det||'">'||n(r_del_det)||'</a>');
   utl_file.put_line(handle,'&d'||n(i.Item_i)||'&d'||n(i.Item_na)||'&d');
   utl_file.put_line(handle,'<a HREF="#WF'||i.Line_i||'">'||n(:r_line_t)||'</a>'||'&d');
   utl_file.put_line(handle,n(i.Line_st)||'&d'||n(i.openf)||'&d');
   utl_file.put_line(handle,n(i.bookf)||'&d'||n(i.shipf)||'&d');
   utl_file.put_line(handle,n(i.canf)||'&d'||n(i.vdem)||'&d');
   utl_file.put_line(handle,n(i.si)||'&d'||n(i.fulf)||'&d');
   utl_file.put_line(handle,n(i.ato_i)||'&d'||n(i.smcf)||'&d');
   utl_file.put_line(handle,n(i.Ordq)||'&d'||n(i.Orduom)||'&d');
   utl_file.put_line(handle,n(i.sell)||'&d');
   utl_file.put_line(handle,n(i.lisl)||'&d'||n(i.cal_pr)||'&d');
   utl_file.put_line(handle,n(:r_res_q)||'&d'||n(i.shn_q)||'&d');
   utl_file.put_line(handle,n(i.shpq)||'&d'||n(i.fulq)||'&d');
   utl_file.put_line(handle,n(i.canq)||'&d');
   utl_file.put_line(handle,n(i.invq)||'&d'||n(i.schc)||'&d');
   utl_file.put_line(handle,n(i.shipfrom)||'&d'||n(org_i)||'&d');
   utl_file.put_line(handle,n(i.cre_date)||'&d');
   utl_file.put_line(handle,n(i.reqd)||'&d'||n(i.schd)||'&d');
   utl_file.put_line(handle,n(i.act_shp)||'&d');
   utl_file.put_line(handle,n(lin_ty)||'&d'||n(i.lin_cat)||'&d');
   utl_file.put_line(handle,n(i.itm_tc)||'&d'||n(i.ord_it)||'&d');
   utl_file.put_line(handle,n(i.ord_it_name)||'&d'||n(i.Src_tc)||'&d');
   utl_file.put_line(handle,n(i.prlst)||'&d'||n(i.demc)||'&d');
   utl_file.put_line(handle,n(i.CFG)||'&d');
   utl_file.put_line(handle,n(i.PRT_LN_ID)||'&d'||n(i.ATO_LN_ID)||'&d');
   utl_file.put_line(handle,n(i.LNK_LN_ID)||'&d'||n(i.SPL_BY)||'&d');
   utl_file.put_line(handle,n(i.SPL_LN_ID)||'&d');
   utl_file.put_line(handle,n(i.CFG_HD_ID)||'&d'||n(i.INVC_INT_STAT)||'&d');
   utl_file.put_line(handle,n(i.STA)||'&d');
   utl_file.put_line(handle,n(i.STB)||'&d'||n(i.SH_SET_ID)||'&d');
   utl_file.put_line(handle,n(i.SH_SET_TY)||'&d'||n(i.LN_SET_ID)||'&d');
   utl_file.put_line(handle,n(i.LN_SET_TY)||'&d'||n(i.AR_SET_ID)||'&d');
   utl_file.put_line(handle,n(i.AR_SET_TY)||'&d'||n(r_sets)||'&d');
   utl_file.put_line(handle,n(i.Ordq2)||'&d'||n(i.Orduom2)||'&d');
   utl_file.put_line(handle,n(i.shn_q2)||'&d'||n(i.shn_quom2)||'&d');
   utl_file.put_line(handle,n(i.shpq2)||'&d'||n(i.fulq2)||'&d');
   utl_file.put_line(handle,n(i.canq2)||'&d');
   utl_file.put_line(handle,n(i.upd_date)||'&d');
   utl_file.put_line(handle,n(i.trck_qty_ind)||'&d'||n(i.PRM_UOM)||'&d');
   utl_file.put_line(handle,n(i.DUAL_UOM)||'&d'||n(i.SEC_DEF_IND)||'&d');
   utl_file.put_line(handle,n(i.SEC_UOM)||'&d'||n(i.CHLD_LOT)||'&d');
   utl_file.put_line(handle,n(i.PAR_CHLD)||'&d'||n(i.LOT_DIV)||'&d');
   utl_file.put_line(handle,n(i.GRAD_CTRL)||'&d'||n(i.ONT_PR_QTY)||'&d');
   utl_file.put_line(handle,n(i.DUAL_UOM_DEV_H)||'&d'||n(i.DUAL_UOM_DEV_L)||'&d');
   utl_file.put_line(handle,n(i.LOT_CTL)||'&d'||n(i.LOCT_CTL)||'&d');
   utl_file.put_line(handle,n(i.STATUS_CTL)||'&d');
   utl_file.put_line(handle,n(i.RES_TYPE)||'&d');
   utl_file.put_line(handle,'<a HREF="#PORH'||i.src_doc_id||'">'||n(i.src_doc_id)||'</a>'||'&d');
   utl_file.put_line(handle,'<a HREF="#PORL'||i.src_doc_lin_id||'">'||n(i.src_doc_lin_id)||'</a>'||'&d');
   utl_file.put_line(handle,n(i.ord_src_id)||'&d');
   utl_file.put_line(handle,n(i.o_sys_doc_r)||'&d'||n(i.o_sys_lin_r)||'&el');

   if :r_flag is not null then
    :r_error := 1;
   end if;
 end loop;

 utl_file.put_line(handle,'&et');

 if :r_error = 1 then
   utl_file.put_line(handle,'&f &b <a NAME="OLERR">Warning List:</a> &eb &f');
   for i in 1..70
   loop
     if msg(i).flag = '1' then
       utl_file.put_line(handle,msg(i).descrip||'&f');
     end if;
   end loop;
 end if;

end;

UTL_FILE.PUT_LINE(handle,'&et');

end if; -- v_head_only

UTL_FILE.FCLOSE(handle);
end;
/
--AQUI

DECLARE
  handle UTL_FILE.FILE_TYPE;
  dirname varchar2(1000);
  text    varchar2(1000);

function n(v varchar2) return varchar2 is
begin
  if v is null then
   return '&sp';
   else
   return v;
  end if;
end n;

begin
If :v_head_only = 'N' then 

-- Append to output file
  handle := UTL_FILE.FOPEN('&out_dir','&out_dir/&out_file','A',32000);
  UTL_FILE.PUT_LINE(handle,'&et'); -- in case last one failed


UTL_FILE.PUT_LINE(handle,'&f &f OE_ORDER_LINES_HISTORY (HIL)  &f');
UTL_FILE.PUT_LINE(handle,'&std &sh LINE_ID &dh LINE &dh ORD_Q &dh LCN_Q &dh SHN_Q &dh SHP_Q &dh FUL_Q &dh CAN_Q &dh INC_Q &dh UOM &dh SSC &dh OP &dh');
UTL_FILE.PUT_LINE(handle,'BK &dh SH &dh CN &dh VD &dh SI &dh FF &dh WF_ACT_CODE &dh WF_RESULT &dh HIST_TYPE &dh HIST_CREAT_DATE &dh FLOW_CODE &dh');
UTL_FILE.PUT_LINE(handle,'PRICE  &dh REASON_CODE &dh HIST_COMMENTS &eh');

Declare
cursor l_lin_his is
select
     HIL.LINE_ID                           Line_id,
     to_char(HIL.line_number) || 
          decode(HIL.shipment_number, null, null, '.' || to_char(HIL.shipment_number))|| 
          decode(HIL.option_number, null, null, '.' || to_char(HIL.option_number)) ||
          decode(HIL.component_number, null, null, 
                 decode(HIL.option_number, null, '.',null)||
                 '.'||to_char(HIL.component_number))||
          decode(HIL.service_number,null,null,
                 decode(HIL.component_number, null, '.' , null) ||
                        decode(HIL.option_number, null, '.', null ) ||
                        '.'|| to_char(HIL.service_number)) line_no,
     nvl(HIL.ORDERED_QUANTITY,0)           Ord_q,
     nvl(HIL.LATEST_CANCELLED_QUANTITY,0)  lCan_q, 
     nvl(HIL.SHIPPING_QUANTITY,0)          SHI_q, 
     nvl(HIL.SHIPPED_QUANTITY,0)           SHP_q, 
     nvl(HIL.FULFILLED_QUANTITY,0)         Ful_q,                   
     nvl(HIL.CANCELLED_QUANTITY,0)         Can_q,       
     nvl(HIL.INVOICED_QUANTITY,0)          INV_q, 
     HIL.ORDER_QUANTITY_UOM                Ord_q_uom,
     substr(HIL.SCHEDULE_STATUS_CODE,1,5)  Sch_code,
     nvl(HIL.OPEN_FLAG,'N')                OP_f,
     nvl(HIL.BOOKED_FLAG,'N')              BK_f,
     nvl(HIL.SHIPPABLE_FLAG,'N')           SH_f,
     nvl(HIL.CANCELLED_FLAG,'N')           CN_f,
     nvl(HIL.VISIBLE_DEMAND_FLAG,'N')      VD_f,
     nvl(HIL.SHIPPING_INTERFACED_FLAG,'N') SI_f,
     nvl(HIL.FULFILLED_FLAG,'N')           FL_f,
     HIL.WF_ACTIVITY_CODE                  Act_code,
     HIL.WF_RESULT_CODE                    Res_code,
     HIL.HIST_TYPE_CODE                    Typ_code,
     to_char(HIL.HIST_CREATION_DATE, 'DD-MON-RR_HH24:MI:SS')  Hist_cre_date,
     HIL.FLOW_STATUS_CODE                  Sta_code,
     HIL.UNIT_SELLING_PRICE                Sell_p, 
     HIL.REASON_CODE                       Reason,
     HIL.HIST_COMMENTS                     comments
from
     OE_ORDER_LINES_HISTORY        HIL
where
     HIL.HEADER_ID                 = nvl('&header_id_selected',:v_header_id)
and  NVL('&line_id_selected',0)    in (0,HIL.LINE_ID,
                                         HIL.TOP_MODEL_LINE_ID,
                                         HIL.ATO_LINE_ID,
                                         HIL.LINK_TO_LINE_ID,
                                         HIL.SERVICE_REFERENCE_LINE_ID)
order by
     NVL(HIL.ATO_LINE_ID,               HIL.LINE_ID),
     NVL(HIL.SORT_ORDER,                '0000'),
     NVL(HIL.LINK_TO_LINE_ID,           HIL.LINE_ID),
     NVL(HIL.SOURCE_DOCUMENT_LINE_ID,   HIL.LINE_ID),
     NVL(HIL.SERVICE_REFERENCE_LINE_ID, HIL.LINE_ID),
     HIL.LINE_ID;

begin
 for ll in l_lin_his
 loop
   utl_file.put_line(handle,'&sld'||n(ll.Line_id)||'&d'||n(ll.line_no)||'&d');
   utl_file.put_line(handle,n(ll.Ord_q)||'&d'||n(ll.lcan_q)||'&d');
   utl_file.put_line(handle,n(ll.SHI_q)||'&d'||n(ll.SHP_q)||'&d');
   utl_file.put_line(handle,n(ll.Ful_q)||'&d'||n(ll.CAN_q)||'&d');
   utl_file.put_line(handle,n(ll.INV_q)||'&d'||n(ll.ord_q_uom)||'&d');
   utl_file.put_line(handle,n(ll.Sch_code)||'&d'||n(ll.OP_f)||'&d');
   utl_file.put_line(handle,n(ll.BK_f)||'&d'||n(ll.SH_f)||'&d');
   utl_file.put_line(handle,n(ll.CN_f)||'&d'||n(ll.VD_f)||'&d');
   utl_file.put_line(handle,n(ll.SI_f)||'&d'||n(ll.FL_f)||'&d');
   utl_file.put_line(handle,n(ll.Act_code)||'&d'||n(ll.Res_code)||'&d');
   utl_file.put_line(handle,n(ll.Typ_code)||'&d'||n(ll.Hist_cre_date)||'&d');
   utl_file.put_line(handle,n(ll.Sta_code)||'&d'||n(ll.Sell_p)||'&d');
   utl_file.put_line(handle,n(ll.Reason)||'&d'||n(ll.comments)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');


if substr(UPPER(nvl('&prt_wf','Y')),1,1) = 'Y' then

UTL_FILE.PUT_LINE(handle,'&f &f WORKFLOW LINE STATUS &f');

-- break on WFS.ITEM_KEY skip 2;

declare 
cursor WF_LINE is
select WFS.ITEM_KEY               ITEM_KEY,
       WFA.DISPLAY_NAME           PROCESS_NAME,
       WFA1.DISPLAY_NAME          ACTIVITY_NAME,
       WF_CORE.ACTIVITY_RESULT(WFA1.RESULT_TYPE,WFS.ACTIVITY_RESULT_CODE) RESULT_CODE,
       LKP.MEANING                MEANING,
       WFS.NOTIFICATION_ID        NOTIFICATION_ID,
       WFP.PROCESS_NAME           INT_Process_name,
       WFP.ACTIVITY_NAME          INT_Activity_name,
       WFS.ACTIVITY_RESULT_CODE   INT_Act_Res_code,
       WFS.ACTIVITY_STATUS        INT_Act_Stat,
       to_char(WFS.BEGIN_DATE,'DD-MON-RR_HH24:MI:SS') B_Date,
       to_char(WFS.END_DATE,'DD-MON-RR_HH24:MI:SS')   E_Date,
       WFS.ERROR_NAME             Err_name,
       WFS.BEGIN_DATE             BEGIN_DATE2,
       WFS.EXECUTION_TIME         EXECUTION_TIME2
from WF_ITEM_ACTIVITY_STATUSES WFS,
     WF_PROCESS_ACTIVITIES     WFP,
     WF_ACTIVITIES_VL          WFA,
     WF_ACTIVITIES_VL          WFA1,
     WF_LOOKUPS                LKP
where 
     WFS.ITEM_TYPE          = 'OEOL'
  and  WFS.item_key           in 
              (select to_char(line_id) from OE_ORDER_LINES LIN
               where LIN.HEADER_ID = nvl('&header_id_selected',:v_header_id)
               and NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                           LIN.TOP_MODEL_LINE_ID,
                                           LIN.ATO_LINE_ID,
                                           LIN.LINK_TO_LINE_ID,
                                           LIN.SERVICE_REFERENCE_LINE_ID)
              ) 
  and  WFS.PROCESS_ACTIVITY   = WFP.INSTANCE_ID
  and  WFP.PROCESS_ITEM_TYPE  = WFA.ITEM_TYPE
  and  WFP.PROCESS_NAME       = WFA.NAME
  and  WFP.PROCESS_VERSION    = WFA.VERSION
  and  WFP.ACTIVITY_ITEM_TYPE = WFA1.ITEM_TYPE
  and  WFP.ACTIVITY_NAME      = WFA1.NAME
  and  WFA1.VERSION = 
      (select max(VERSION)
       from WF_ACTIVITIES WF2
       where WF2.ITEM_TYPE = WFP.ACTIVITY_ITEM_TYPE
       and   WF2.NAME      = WFP.ACTIVITY_NAME)
  and  LKP.LOOKUP_TYPE = 'WFENG_STATUS'
  and  LKP.LOOKUP_CODE = WFS.ACTIVITY_STATUS
UNION ALL
select WFS.ITEM_KEY             ITEM_KEY,
       WFA.DISPLAY_NAME         PROCESS_NAME,
       WFA1.DISPLAY_NAME        ACTIVITY_NAME,
       WF_CORE.ACTIVITY_RESULT(WFA1.RESULT_TYPE,WFS.ACTIVITY_RESULT_CODE) RESULT_CODE,
       LKP.MEANING                MEANING,
       WFS.NOTIFICATION_ID        NOTIFICATION_ID,
       WFP.PROCESS_NAME           INT_Process_name,
       WFP.ACTIVITY_NAME          INT_Activity_name,
       WFS.ACTIVITY_RESULT_CODE   INT_Act_Res_code,
       WFS.ACTIVITY_STATUS        INT_Act_Stat,
       to_char(WFS.BEGIN_DATE,'DD-MON-RR_HH24:MI:SS') B_Date,
       to_char(WFS.END_DATE,'DD-MON-RR_HH24:MI:SS')   E_Date,
       WFS.ERROR_NAME             Err_name,
       WFS.BEGIN_DATE             BEGIN_DATE2,
       WFS.EXECUTION_TIME         EXECUTION_TIME2
from WF_ITEM_ACTIVITY_STATUSES_H WFS,
     WF_PROCESS_ACTIVITIES     WFP,
     WF_ACTIVITIES_VL          WFA,
     WF_ACTIVITIES_VL          WFA1,
     WF_LOOKUPS                LKP
where
     WFS.ITEM_TYPE          = 'OEOL'
  and  WFS.item_key           in
              (select to_char(line_id) from OE_ORDER_LINES LIN
               where LIN.HEADER_ID = nvl('&header_id_selected',:v_header_id)
               and NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                           LIN.TOP_MODEL_LINE_ID,
                                           LIN.ATO_LINE_ID,
                                           LIN.LINK_TO_LINE_ID,
                                           LIN.SERVICE_REFERENCE_LINE_ID)
              )
  and  WFS.PROCESS_ACTIVITY   = WFP.INSTANCE_ID
  and  WFP.PROCESS_ITEM_TYPE  = WFA.ITEM_TYPE
  and  WFP.PROCESS_NAME       = WFA.NAME
  and  WFP.PROCESS_VERSION    = WFA.VERSION
  and  WFP.ACTIVITY_ITEM_TYPE = WFA1.ITEM_TYPE
  and  WFP.ACTIVITY_NAME      = WFA1.NAME
  and  WFA1.VERSION =
      (select max(VERSION)
       from WF_ACTIVITIES WF2
       where WF2.ITEM_TYPE = WFP.ACTIVITY_ITEM_TYPE
       and   WF2.NAME      = WFP.ACTIVITY_NAME)
  and  LKP.LOOKUP_TYPE = 'WFENG_STATUS'
  and  LKP.LOOKUP_CODE = WFS.ACTIVITY_STATUS
  order by 1, 13, 14;

type     per_record_typ is RECORD
               (flag    varchar2(1),
                descrip varchar2(200));
type     msg_tab is TABLE of per_record_typ INDEX by binary_integer;
msg      msg_tab;
current_key  number;
line_no  varchar2(10);


begin
  current_key := '';
  :r_error := 0;
  
-- Initialize error messages even if Do Analysis not selected
-- if substr(UPPER(nvl('&do_analysis','Y')),1,1) = 'Y' then
  for i in 1..60
  loop
    msg(i).flag := '0';
    msg(i).descrip := '';
  end loop;

-- end if; -- do_analysis


 -- cycle for table information
 for wf in wf_LINE 
 loop
   :r_flag := '';

if substr(UPPER(nvl('&do_analysis','Y')),1,1) = 'Y' then
  null;  -- nothing yet
end if; -- do_analysis

   -- Print line to Output file
   if wf.item_key <> nvl(current_key,0) then  -- check if item key changed to place a division
     if current_key is not null then  -- first pass don't need to close table
       utl_file.put_line(handle,'&et &f');
     end if;
     utl_file.put_line(handle,'&std &sh <a NAME="WF'||wf.ITEM_KEY||'">WARNING</a> &dh');
     utl_file.put_line(handle,'LIN_NUM &dh');
     utl_file.put_line(handle,'LIN_ID &dh');
     utl_file.put_line(handle,'PROCESS_NAME &dh');
     utl_file.put_line(handle,'ACTIVITY_NAME &dh');
     utl_file.put_line(handle,'RESULT &dh');
     utl_file.put_line(handle,'ACT_STATUS &dh');
     utl_file.put_line(handle,'NOTIF_ID &dh');
     utl_file.put_line(handle,'INT_PROCESS_NAME &dh');
     utl_file.put_line(handle,'INT_ACTIVITY_NAME &dh');
     utl_file.put_line(handle,'INT_RESULT_CODE &dh');
     utl_file.put_line(handle,'INT_ACTIVITY_STATUS &dh');
     utl_file.put_line(handle,'BEGIN_DATE &dh');
     utl_file.put_line(handle,'END_DATE &dh');
     utl_file.put_line(handle,'ERROR_NAME &eh');
     current_key := wf.item_key;

     select to_char(LIN.line_number) || 
            decode(LIN.shipment_number, null, null, '.' || to_char(LIN.shipment_number))|| 
            decode(LIN.option_number, null, null, '.' || to_char(LIN.option_number)) ||
            decode(LIN.component_number, null, null, 
                   decode(LIN.option_number, null, '.',null)||
                   '.'||to_char(LIN.component_number))||
            decode(LIN.service_number,null,null,
                   decode(LIN.component_number, null, '.' , null) ||
                          decode(LIN.option_number, null, '.', null ) ||
                          '.'|| to_char(LIN.service_number))  
            into line_no
       from OE_ORDER_LINES LIN
      where LIN.Line_id = current_key;

   end if;   
   utl_file.put_line(handle,'&sld &b <a HREF="#WFLERR">'||n(:r_flag)||'</a> &eb &d'||n(line_no)||'&d');
   utl_file.put_line(handle,n(wf.ITEM_KEY)||'&d');
   utl_file.put_line(handle,n(wf.Process_name)||'&d'||n(wf.activity_name)||'&d');
   utl_file.put_line(handle,n(wf.Result_code)||'&d'||n(wf.Meaning)||'&d');
   utl_file.put_line(handle,n(wf.Notification_id)||'&d'||n(wf.INT_Process_name)||'&d');
   utl_file.put_line(handle,n(wf.INT_activity_name)||'&d'||n(wf.INT_Act_Res_code)||'&d');
   utl_file.put_line(handle,n(wf.INT_Act_Stat)||'&d'||n(wf.B_date)||'&d');
   utl_file.put_line(handle,n(wf.E_date)||'&d'||n(wf.Err_name)||'&el');

   if :r_flag is not null then
    :r_error := 1;
   end if;
 end loop;

 utl_file.put_line(handle,'&et');

 if :r_error = 1 then
   utl_file.put_line(handle,'&f &b <a NAME="WFLERR">Warning List:</a> &eb &f');
   for i in 1..60
   loop
     if msg(i).flag = '1' then
       utl_file.put_line(handle,msg(i).descrip||'&f');
     end if;
   end loop;
 end if;

end;

UTL_FILE.PUT_LINE(handle,'&et');


/*
UTL_FILE.PUT_LINE(handle,'&f &f WORKFLOW LINE SKIP INFORMATION (WFSKIPL)&f');

UTL_FILE.PUT_LINE(handle,'&std &sh LINE_ID &dh HEADER_ID &dh DISPLAY_NAME &dh CREATION_DATE &dh USER_ID &dh RESPONSIBILITY_ID &dh APPLICATION_ID &eh');

declare
  r_exist   number;

begin
  
  select count(*)
    into r_exist
    from all_tables
   where table_name = 'ONT_WF_SKIP_LOG';
  if r_exist = 0 then
    utl_file.put_line(handle,'&et Table ONT_WF_SKIP_LOG is not present on this instance: &f');
 end if;

end;

    select '&sld', WFS.LINE_ID,
           WFS.HEADER_ID,
           WFA.DISPLAY_NAME,
           to_char(WFS.CREATION_DATE,'DD-MON-RR_HH24:MI:SS') cre_date,
           WFS.USER_ID,
           WFS.RESPONSIBILITY_ID,
           WFS.APPLICATION_ID,'&el'
      from ONT_WF_SKIP_LOG           WFS,
           WF_PROCESS_ACTIVITIES     WFP,
           WF_ACTIVITIES_VL          WFA
     where WFS.HEADER_ID          = nvl('&header_id_selected',to_char(:v_header_id))
       and WFS.ACTIVITY_ID        = WFP.INSTANCE_ID
       and WFP.PROCESS_ITEM_TYPE  = WFA.ITEM_TYPE
       and WFP.PROCESS_NAME       = WFA.NAME
       and WFP.PROCESS_VERSION    = WFA.VERSION
       and NVL('&line_id_selected',0)  in (0,WFS.LINE_ID)
     order by WFS.HEADER_ID, 
              WFS.LINE_ID,
              WFS.CREATION_DATE;

UTL_FILE.PUT_LINE(handle,'&et'); */


UTL_FILE.PUT_LINE(handle,'&f &f &b WORKFLOW LINE STATUS ERRORS &f');

-- break on LIN_ID skip 2;

UTL_FILE.PUT_LINE(handle,'&std &sh LIN_ID &dh PROCESS_NAME &dh ERROR_ACTIVITY_NAME &dh RESULT &dh ACT_STATUS &dh ERR_RETRY_ROLE &dh ERR_RETRY_USER &dh ');
UTL_FILE.PUT_LINE(handle,'ERROR_NAME &dh ERROR_MESSAGE &dh ERROR_STACK &eh');

Declare
cursor l_wf_err is
select 
       WFS.ITEM_KEY               LIN_ID, 
       WFA.DISPLAY_NAME           PROCESS_NAME,
       WFA1.DISPLAY_NAME          ERROR_ACTIVITY_NAME,
       WF_CORE.ACTIVITY_RESULT(WFA1.RESULT_TYPE,WFS.ACTIVITY_RESULT_CODE) RESULT,
       LKP.MEANING                   ACT_STATUS,
       WFS.ERROR_NAME             ERROR_NAME,
       WFS.ERROR_MESSAGE          ERROR_MESSAGE,
       WFS.ERROR_STACK            ERROR_STACK
from WF_ITEM_ACTIVITY_STATUSES WFS,
     WF_ITEMS                  WFI,
     WF_PROCESS_ACTIVITIES     WFP,
     WF_ACTIVITIES_VL          WFA,
     WF_ACTIVITIES_VL          WFA1,
     WF_LOOKUPS                LKP
where 
     WFS.ITEM_TYPE          = 'OEOL'
  and  WFS.item_key           in 
              (select to_char(line_id) from OE_ORDER_LINES LIN
               where LIN.HEADER_ID = nvl('&header_id_selected',:v_header_id)
               and NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                           LIN.TOP_MODEL_LINE_ID,
                                           LIN.ATO_LINE_ID,
                                           LIN.LINK_TO_LINE_ID,
                                           LIN.SERVICE_REFERENCE_LINE_ID)
              ) 
  and  WFS.PROCESS_ACTIVITY   = WFP.INSTANCE_ID
  and  WFP.PROCESS_ITEM_TYPE  = WFA.ITEM_TYPE
  and  WFP.PROCESS_NAME       = WFA.NAME
  and  WFP.PROCESS_VERSION    = WFA.VERSION
  and  WFP.ACTIVITY_ITEM_TYPE = WFA1.ITEM_TYPE
  and  WFP.ACTIVITY_NAME      = WFA1.NAME
  and  WFA1.VERSION = 
      (select max(VERSION)
       from WF_ACTIVITIES WF2
       where WF2.ITEM_TYPE = WFP.ACTIVITY_ITEM_TYPE
       and   WF2.NAME      = WFP.ACTIVITY_NAME)
  and  LKP.LOOKUP_TYPE = 'WFENG_STATUS'
  and  LKP.LOOKUP_CODE = WFS.ACTIVITY_STATUS
  and  WFS.ERROR_NAME is not NULL
  and  WFI.PARENT_ITEM_TYPE='OEOL' 
  and  WFI.PARENT_ITEM_KEY in 
              (select to_char(line_id) from OE_ORDER_LINES LIN
               where LIN.HEADER_ID = nvl('&header_id_selected',:v_header_id)
               and NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                           LIN.TOP_MODEL_LINE_ID,
                                           LIN.ATO_LINE_ID,
                                           LIN.LINK_TO_LINE_ID,
                                           LIN.SERVICE_REFERENCE_LINE_ID)
              ) 
 order by WFS.ITEM_KEY, WFS.BEGIN_DATE, EXECUTION_TIME;

begin
 for ll in l_wf_err
 loop
   utl_file.put_line(handle,'&sld'||n(ll.LIN_ID)||'&d'||n(ll.PROCESS_NAME)||'&d');
   utl_file.put_line(handle,n(ll.ERROR_ACTIVITY_NAME)||'&d'||n(ll.RESULT)||'&d');
   utl_file.put_line(handle,n(ll.ACT_STATUS)||'&d'||n(ll.ERROR_NAME)||'&d');
   utl_file.put_line(handle,n(ll.ERROR_MESSAGE)||'&d'||n(ll.ERROR_STACK)||'&el');
 end loop;
end;


UTL_FILE.PUT_LINE(handle,'&et');
 
-- break on ERR_TYPE_KEY skip 2;


UTL_FILE.PUT_LINE(handle,'&f &f WORKFLOW ACTIVITY STATUS FOR LINE ERROR PROCESS &f');

UTL_FILE.PUT_LINE(handle,'&std &sh ERR_TYPE_KEY &dh ERR_PROCESS_NAME &dh ERR_ACTIVITY_NAME &dh RESULT &dh ACT_STATUS &dh NOTIF_ID &dh ');
UTL_FILE.PUT_LINE(handle,'ASGND_USER &dh BEGIN_DATE &dh END_DATE &eh');

Declare
cursor l_wf_proc_err is
select 
       WFS.ITEM_TYPE || '-' || WFS.ITEM_KEY               ERR_TYPE_KEY, 
       WFA.DISPLAY_NAME           ERR_PROCESS_NAME,
       WFA1.DISPLAY_NAME          ERR_ACTIVITY_NAME,
       WF_CORE.ACTIVITY_RESULT(WFA1.RESULT_TYPE,WFS.ACTIVITY_RESULT_CODE) RESULT,
       LKP.MEANING                ACT_STATUS,
       WFS.NOTIFICATION_ID        NOTIF_ID,
       WFS.ASSIGNED_USER          ASGND_USER,
       to_char(WFS.BEGIN_DATE,'DD-MON-RR_HH24:MI:SS') BEGIN_DATE,
       to_char(WFS.END_DATE,'DD-MON-RR_HH24:MI:SS') END_DATE
from WF_ITEM_ACTIVITY_STATUSES WFS,
     WF_PROCESS_ACTIVITIES     WFP,
     WF_ACTIVITIES_VL          WFA,
     WF_ACTIVITIES_VL          WFA1,
     WF_LOOKUPS                LKP,
     WF_ITEMS                  WFI
where 
       WFS.ITEM_TYPE          = WFI.ITEM_TYPE
  and  WFS.item_key           = WFI.ITEM_KEY
  and  WFS.PROCESS_ACTIVITY   = WFP.INSTANCE_ID
  and  WFP.PROCESS_ITEM_TYPE  = WFA.ITEM_TYPE
  and  WFP.PROCESS_NAME       = WFA.NAME
  and  WFP.PROCESS_VERSION    = WFA.VERSION
  and  WFP.ACTIVITY_ITEM_TYPE = WFA1.ITEM_TYPE
  and  WFP.ACTIVITY_NAME      = WFA1.NAME
  and  WFA1.VERSION = 
      (select max(VERSION)
       from WF_ACTIVITIES WF2
       where WF2.ITEM_TYPE = WFP.ACTIVITY_ITEM_TYPE
       and   WF2.NAME      = WFP.ACTIVITY_NAME)
  and  LKP.LOOKUP_TYPE = 'WFENG_STATUS'
  and  LKP.LOOKUP_CODE = WFS.ACTIVITY_STATUS
  and  WFI.PARENT_ITEM_TYPE = 'OEOL'
  and  WFI.PARENT_ITEM_KEY  in               
             (select to_char(line_id) from OE_ORDER_LINES LIN
               where LIN.HEADER_ID = nvl('&header_id_selected',:v_header_id)
               and NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                           LIN.TOP_MODEL_LINE_ID,
                                           LIN.ATO_LINE_ID,
                                           LIN.LINK_TO_LINE_ID,
                                           LIN.SERVICE_REFERENCE_LINE_ID)
              ) 
  and  WFI.ITEM_TYPE in (select WFAE.ERROR_ITEM_TYPE
                         from WF_ITEM_ACTIVITY_STATUSES WFSE,
                         WF_PROCESS_ACTIVITIES     WFPE,
                         WF_ACTIVITIES_VL          WFAE,
                         WF_ACTIVITIES_VL          WFA1E
                         where 
                                WFSE.ITEM_TYPE = 'OEOL'
                           and  WFSE.ITEM_KEY  in 
                                (select to_char(line_id) from OE_ORDER_LINES LIN
                                 where LIN.HEADER_ID = nvl('&header_id_selected',:v_header_id)
                                 and NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                           LIN.TOP_MODEL_LINE_ID,
                                           LIN.ATO_LINE_ID,
                                           LIN.LINK_TO_LINE_ID,
                                           LIN.SERVICE_REFERENCE_LINE_ID)
                                 )   
                           and  WFSE.PROCESS_ACTIVITY   = WFPE.INSTANCE_ID
                           and  WFPE.PROCESS_ITEM_TYPE  = WFAE.ITEM_TYPE
                           and  WFPE.PROCESS_NAME       = WFAE.NAME
                           and  WFPE.PROCESS_VERSION    = WFAE.VERSION
                           and  WFPE.ACTIVITY_ITEM_TYPE = WFA1E.ITEM_TYPE
                           and  WFPE.ACTIVITY_NAME      = WFA1E.NAME
                           and  WFA1E.VERSION = 
                               (select max(VERSION)
                                from WF_ACTIVITIES WF2E
                                where WF2E.ITEM_TYPE = WFPE.ACTIVITY_ITEM_TYPE
                                and   WF2E.NAME      = WFPE.ACTIVITY_NAME)
                           and  WFSE.ACTIVITY_STATUS = 'ERROR')
  order by WFS.ITEM_KEY, WFS.BEGIN_DATE, EXECUTION_TIME;

begin
 for ll in l_wf_proc_err
 loop
   utl_file.put_line(handle,'&sld'||n(ll.ERR_TYPE_KEY)||'&d'||n(ll.ERR_PROCESS_NAME)||'&d');
   utl_file.put_line(handle,n(ll.ERR_ACTIVITY_NAME)||'&d'||n(ll.RESULT)||'&d');
   utl_file.put_line(handle,n(ll.ACT_STATUS)||'&d'||n(ll.NOTIF_ID)||'&d');
   utl_file.put_line(handle,n(ll.ASGND_USER)||'&d'||n(ll.BEGIN_DATE)||'&d');
   utl_file.put_line(handle,n(ll.END_DATE)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');

end if; -- prt_wf


UTL_FILE.PUT_LINE(handle,'&f &f LINE SPECIFIC HOLDS (LINES ONLY)');
UTL_FILE.PUT_LINE(handle,'&std &sh LINE &dh HOLD_ID &dh HOLD_NAME &dh HOLD_TYPE &dh WF_ITEM &dh WF_ACTIVITY &dh ORD_HOLD_ID &dh HLD_SRC_ID &dh ');
UTL_FILE.PUT_LINE(handle,'HLD_REL_ID &dh HEADER_ID &dh LINE_ID &dh H_REL &dh S_REL &dh RELEASE_REASON &dh ENTITY &dh ENTITY_ID &dh ENTITY2 &dh ');
UTL_FILE.PUT_LINE(handle,'ENTITY_ID2 &dh HOLD_UNTIL &eh');


Declare
cursor l_holds is
SELECT 
    to_char(LIN.line_number) || 
          decode(LIN.shipment_number, null, null, '.' || to_char(LIN.shipment_number))|| 
          decode(LIN.option_number, null, null, '.' || to_char(LIN.option_number)) ||
          decode(LIN.component_number, null, null, 
                 decode(LIN.option_number, null, '.',null)||
                 '.'||to_char(LIN.component_number))||
          decode(LIN.service_number,null,null,
                 decode(LIN.component_number, null, '.' , null) ||
                        decode(LIN.option_number, null, '.', null ) ||
                        '.'|| to_char(LIN.service_number)) LINE,
  HDF.HOLD_ID                        HOLD_ID,
  HDF.NAME                           HOLD_NAME,
  HDF.TYPE_CODE                      HOLD_TYPE,
  HDF.ITEM_TYPE                      WF_ITEM,
  HDF.ACTIVITY_NAME                  WF_ACTIVITY,
  HLD.ORDER_HOLD_ID                  ORD_HOLD_ID,
  HLD.HOLD_SOURCE_ID                 HLD_SRC_ID,
  HLD.HOLD_RELEASE_ID                HLD_REL_ID,
  HLD.HEADER_ID                      HEADER_ID,
  HLD.LINE_ID                        LINE_ID,
  HLD.RELEASED_FLAG                  H_REL,
  HSR.RELEASED_FLAG                  S_REL,
  HRL.RELEASE_REASON_CODE            RELEASE_REASON,
  decode(HSR.HOLD_ENTITY_CODE,
         'B','Bill To',
         'C','Customer',
         'I','Item',
         'O','Order',
         'S','Ship To',
         'W','Warehouse',
         HSR.HOLD_ENTITY_CODE)       ENTITY,
  HSR.HOLD_ENTITY_ID                 ENTITY_ID,
  decode(HSR.HOLD_ENTITY_CODE2,
         'B','Bill To',
         'C','Customer',
         'I','Item',
         'O','Order',
         'S','Ship To',
         'W','Warehouse',
         HSR.HOLD_ENTITY_CODE2)      ENTITY2,
  HSR.HOLD_ENTITY_ID2                ENTITY_ID2,
  to_char(HSR.HOLD_UNTIL_DATE,'DD-MON-RR_HH24:MI:SS')    HOLD_UNTIL
from OE_ORDER_HOLDS_ALL    HLD,
     OE_HOLD_SOURCES_ALL   HSR,
     OE_HOLD_DEFINITIONS   HDF,
     OE_HOLD_RELEASES      HRL,
     OE_ORDER_LINES_ALL    LIN
where        HLD.HEADER_ID             = nvl('&header_id_selected',:v_header_id)
        and  LIN.HEADER_ID             = nvl('&header_id_selected',:v_header_id)
        and  HLD.LINE_ID               IS NOT NULL
        and  HLD.LINE_ID               = LIN.LINE_ID
        and  NVL('&line_id_selected',0)in (0,LIN.LINE_ID,
                                       LIN.TOP_MODEL_LINE_ID,
                                       LIN.ATO_LINE_ID,
                                       LIN.LINK_TO_LINE_ID,
                                       LIN.REFERENCE_LINE_ID,
                                       LIN.SERVICE_REFERENCE_LINE_ID)
        and  HLD.HOLD_SOURCE_ID       = HSR.HOLD_SOURCE_ID
        and  HSR.HOLD_ID              = HDF.HOLD_ID
        and  HLD.HOLD_RELEASE_ID      = HRL.HOLD_RELEASE_ID(+);

begin
 for ll in l_holds
 loop
   utl_file.put_line(handle,'&sld'||n(ll.LINE)||'&d'||n(ll.HOLD_ID)||'&d');
   utl_file.put_line(handle,n(ll.HOLD_NAME)||'&d'||n(ll.HOLD_TYPE)||'&d');
   utl_file.put_line(handle,n(ll.WF_ITEM)||'&d'||n(ll.WF_ACTIVITY)||'&d');
   utl_file.put_line(handle,n(ll.ORD_HOLD_ID)||'&d'||n(ll.HLD_SRC_ID)||'&d');
   utl_file.put_line(handle,n(ll.HLD_REL_ID)||'&d'||n(ll.HEADER_ID)||'&d');
   utl_file.put_line(handle,n(ll.LINE_ID)||'&d'||n(ll.H_REL)||'&d');
   utl_file.put_line(handle,n(ll.S_REL)||'&d'||n(ll.RELEASE_REASON)||'&d');
   utl_file.put_line(handle,n(ll.ENTITY)||'&d'||n(ll.ENTITY_ID)||'&d');
   utl_file.put_line(handle,n(ll.ENTITY2)||'&d'||n(ll.ENTITY_ID2)||'&d');
   utl_file.put_line(handle,n(ll.HOLD_UNTIL)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');


-- This is commented out because it runs slowly without an index 
--<do not run> CREATE INDEX OE_PROCESSING_MSGS_777
--<do not run>  ON ONT.OE_PROCESSING_MSGS
--<do not run> (header_id, line_id);


UTL_FILE.PUT_LINE(handle,'&f &f LINE PROCESSING MESSAGES &f');
UTL_FILE.PUT_LINE(handle,'&std &sh WARNING &dh LINE &dh HEADER_ID &dh LINE_ID &dh MSG_SOURCE &dh ACTIVITY &dh REQUEST_ID &dh DESCRIPTION &dh MESSAGE_TEXT &eh');

declare
 cursor l_proc is
 select distinct
   to_char(LIN.line_number) || 
           decode(LIN.shipment_number, null, null, '.' || to_char(LIN.shipment_number))|| 
           decode(LIN.option_number, null, null, '.' || to_char(LIN.option_number)) ||
           decode(LIN.component_number, null, null, 
                  decode(LIN.option_number, null, '.',null)||
                  '.'||to_char(LIN.component_number))||
           decode(LIN.service_number,null,null,
                  decode(LIN.component_number, null, '.' , null) ||
                         decode(LIN.option_number, null, '.', null ) ||
                         '.'|| to_char(LIN.service_number)) LINE,
        MSG.header_id                                       HEADER_ID,
        MSG.line_id                                         LINE_ID,
        decode(MSG.MESSAGE_SOURCE_CODE,
               'U','U=On-Line(UI)',
               'C','C=Conc Process',
               'W','W=Workflow',
               MSG.MESSAGE_SOURCE_CODE)                     MSG_SOURCE,
        MSG.PROCESS_ACTIVITY        PROCESS_ACTIVITY,       
        MSG.request_id              REQUEST_ID,
        MST.message_text                                    MESSAGE_TEXT
 from oe_processing_msgs_vl     MSG,
      oe_processing_msgs_tl     MST,
      oe_order_lines_all        LIN,
      fnd_languages             FLA
 where  MSG.header_id    = nvl('&header_id_selected',:v_header_id)
   and  MSG.HEADER_ID      = LIN.HEADER_ID
   and  msg.transaction_id = mst.transaction_id
   and  MST.LANGUAGE                  = FLA.LANGUAGE_CODE 
   and  FLA.INSTALLED_FLAG            = 'B' 
   and  MSG.LINE_ID        = LIN.LINE_ID
   and  MSG.LINE_ID        is not NULL
   and  NVL('&line_id_selected',0)      in (0,LIN.LINE_ID,
                                            LIN.TOP_MODEL_LINE_ID,
                                            LIN.ATO_LINE_ID,
                                            LIN.LINK_TO_LINE_ID,
                                            LIN.SERVICE_REFERENCE_LINE_ID);
r_activity varchar2(100);
r_descrip  varchar2(100);

type     per_record_typ is RECORD
               (flag    varchar2(1),
                descrip varchar2(200));
type     msg_tab is TABLE of per_record_typ INDEX by binary_integer;
msg      msg_tab;


begin
 :r_error := 0;
-- Initialize error messages even if Do Analysis not selected
-- if substr(UPPER(nvl('&do_analysis','Y')),1,1) = 'Y' then
 for i in 1..10
 loop
   msg(i).flag := '0';
   msg(i).descrip := '';
 end loop;

 msg(1).descrip := '    1. Cannot find Activity name associated to LINE PROCESSING MESSAGES.';
 msg(2).descrip := '    2. Cannot find Request Description.';
-- end if;

 for lp in l_proc 
 loop
   :r_flag := '';
   :r_error := 0;
   r_activity := '';
   r_descrip  := '';

if substr(UPPER(nvl('&do_analysis','Y')),1,1) = 'Y' then
   -- Get Activity name
   begin
     select WFA1.DISPLAY_NAME
       into r_activity
       from WF_PROCESS_ACTIVITIES  WFP,
            WF_ACTIVITIES_VL       WFA1 
      where WFP.INSTANCE_ID        = lp.PROCESS_ACTIVITY
        and WFP.ACTIVITY_ITEM_TYPE = WFA1.ITEM_TYPE
        and WFP.ACTIVITY_NAME      = WFA1.NAME
        and WFA1.VERSION = (select max(VERSION)
                              from WF_ACTIVITIES WF2
                             where WF2.ITEM_TYPE = WFP.ACTIVITY_ITEM_TYPE
                               and WF2.NAME      = WFP.ACTIVITY_NAME);
    exception
      when no_data_found then
        :r_flag := :r_flag || '1 ';
        msg(1).flag := '1';
   end;

   -- Get Request description
   begin
     select DESCRIPTION         
       into r_descrip
       FROM FND_CONCURRENT_REQUESTS FCR
      where FCR.REQUEST_ID    = lp.REQUEST_ID;
    exception
      when no_data_found then
        :r_flag := :r_flag || '2 ';
        msg(2).flag := '1';
   end;

end if; -- do_analysis

   -- Print line to Output file
  
   -- Print line to Output file
   utl_file.put_line(handle,'&sld &b <a HREF="#LPMERR">'||n(:r_flag)||'</a> &eb &d');
   utl_file.put_line(handle,n(lp.LINE)||'&d'||n(lp.HEADER_ID)||'&d');
   utl_file.put_line(handle,n(lp.LINE_ID)||'&d'||n(lp.MSG_SOURCE)||'&d');
   utl_file.put_line(handle,n(r_activity)||'&d'||n(lp.REQUEST_ID)||'&d');
   utl_file.put_line(handle,n(r_descrip)||'&d'||n(lp.MESSAGE_TEXT)||'&el');

   if :r_flag is not null then
    :r_error := 1;
   end if;
 end loop;

 utl_file.put_line(handle,'&et');

 if :r_error = 1 then
   utl_file.put_line(handle,'&f &b <a NAME="LPMERR">Warning List:</a> &eb &f');
   for i in 1..10
   loop
     if msg(i).flag = '1' then
       utl_file.put_line(handle,msg(i).descrip||'&f');
     end if;
   end loop;
 end if;

end;

UTL_FILE.PUT_LINE(handle,'&et');


--<do not run> DROP INDEX OE_PROCESSING_MSGS_777

if substr(UPPER(nvl('&prt_price','Y')),1,1) = 'Y' then

UTL_FILE.PUT_LINE(handle,'&f &f <a NAME="APPLIED LINE PRICE ADJUSTMENTS">APPLIED LINE PRICE ADJUSTMENTS (ADJ)</a> <a HREF="#ALA">Column Definitions</a> &f');
UTL_FILE.PUT_LINE(handle,'&std &sh PRC_ADJ_ID &dh LINE &dh LST_HD_ID &dh LST_LN_ID &dh LIST_LN_NO &dh MOD_LVL &dh LIST_TYPE_CODE &dh CHG_TY_CD &dh');
UTL_FILE.PUT_LINE(handle,'ARITH_OP &dh OP_PER_QTY &dh ADJ_AMT_PQ &dh OPERAND &dh ADJ_AMT &dh CD &dh AF &dh PI &dh AC &dh IF &dh EF &dh UA &dh UF &dh');
UTL_FILE.PUT_LINE(handle,'AP &dh LK &dh PERC &dh COST_ID &dh TAX_CODE &dh PP &eh');


Declare
cursor l_prc_adj is
select
    ADJ.PRICE_ADJUSTMENT_ID            PRC_ADJ_ID,
    to_char(LIN.line_number) || 
          decode(LIN.shipment_number, null, null, '.' || to_char(LIN.shipment_number))|| 
          decode(LIN.option_number, null, null, '.' || to_char(LIN.option_number)) ||
          decode(LIN.component_number, null, null, 
                 decode(LIN.option_number, null, '.',null)||
                 '.'||to_char(LIN.component_number))||
          decode(LIN.service_number,null,null,
                 decode(LIN.component_number, null, '.' , null) ||
                        decode(LIN.option_number, null, '.', null ) ||
                        '.'|| to_char(LIN.service_number)) LINE,
     ADJ.LIST_HEADER_ID                 LST_HD_ID,             
     ADJ.LIST_LINE_ID                   LST_LN_ID,  
     ADJ.LIST_LINE_NO                   LIST_LN_NO,   
     ADJ.MODIFIER_LEVEL_CODE            MOD_LVL,     
     ADJ.LIST_LINE_TYPE_CODE            LIST_TYPE_CODE,
     ADJ.CHARGE_TYPE_CODE               CHG_TY_CD,             
     ADJ.ARITHMETIC_OPERATOR            ARITH_OP,  
     ADJ.OPERAND_PER_PQTY               OP_PER_QTY,        
     ADJ.ADJUSTED_AMOUNT_PER_PQTY       ADJ_AMT_PQ,
     ADJ.OPERAND                        OPERAND,                      
     ADJ.ADJUSTED_AMOUNT                ADJ_AMT,                             
     ADJ.CREDIT_OR_CHARGE_FLAG          CD,                          
     ADJ.AUTOMATIC_FLAG                 AF,   
     ADJ.PRINT_ON_INVOICE_FLAG          PI,   
     ADJ.ACCRUAL_FLAG                   AC,        
     ADJ.INVOICED_FLAG                  INF,              
     ADJ.ESTIMATED_FLAG                 EF,
     ADJ.UPDATE_ALLOWED                 UA,        
     ADJ.UPDATED_FLAG                   UF,         
     ADJ.APPLIED_FLAG                   AP, 
     ADJ.LOCK_CONTROL                   LK,                                      
     ADJ.PERCENT                        PERC,            
     ADJ.COST_ID                        COST_ID,               
     ADJ.TAX_CODE                       TAX_CODE,               
     ADJ.PRICING_PHASE_ID               PP
from OE_PRICE_ADJUSTMENTS   ADJ,
     OE_ORDER_LINES_ALL     LIN
where  ADJ.HEADER_ID             = nvl('&header_id_selected',:v_header_id)
  and  LIN.HEADER_ID             = nvl('&header_id_selected',:v_header_id)
  and  ADJ.LINE_ID               IS NOT NULL
  and  ADJ.LINE_ID               = LIN.LINE_ID
  and  NVL('&line_id_selected',0)in (0,LIN.LINE_ID,
                                       LIN.TOP_MODEL_LINE_ID,
                                       LIN.ATO_LINE_ID,
                                       LIN.LINK_TO_LINE_ID,
                                       LIN.REFERENCE_LINE_ID,
                                       LIN.SERVICE_REFERENCE_LINE_ID)
  and  ADJ.APPLIED_FLAG         = 'Y'
order by LINE,
         LIST_LINE_TYPE_CODE;

begin
 for ll in l_prc_adj
 loop
   utl_file.put_line(handle,'&sld'||n(ll.PRC_ADJ_ID)||'&d'||n(ll.LINE)||'&d');
   utl_file.put_line(handle,n(ll.LST_HD_ID)||'&d'||n(ll.LST_LN_ID)||'&d');
   utl_file.put_line(handle,n(ll.LIST_LN_NO)||'&d'||n(ll.MOD_LVL)||'&d');
   utl_file.put_line(handle,n(ll.LIST_TYPE_CODE)||'&d'||n(ll.CHG_TY_CD)||'&d');
   utl_file.put_line(handle,n(ll.ARITH_OP)||'&d'||n(ll.OP_PER_QTY)||'&d');
   utl_file.put_line(handle,n(ll.ADJ_AMT_PQ)||'&d'||n(ll.OPERAND)||'&d');
   utl_file.put_line(handle,n(ll.ADJ_AMT)||'&d'||n(ll.CD)||'&d');
   utl_file.put_line(handle,n(ll.AF)||'&d'||n(ll.PI)||'&d');
   utl_file.put_line(handle,n(ll.AC)||'&d'||n(ll.INF)||'&d');
   utl_file.put_line(handle,n(ll.EF)||'&d'||n(ll.UA)||'&d');
   utl_file.put_line(handle,n(ll.UF)||'&d'||n(ll.AP)||'&d');
   utl_file.put_line(handle,n(ll.LK)||'&d'||n(ll.PERC)||'&d');
   utl_file.put_line(handle,n(ll.COST_ID)||'&d'||n(ll.TAX_CODE)||'&d');
   utl_file.put_line(handle,n(ll.PP)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');

UTL_FILE.PUT_LINE(handle,'&f &f <a NAME="UN-APPLIED LINE PRICE ADJUSTMENTS">UN-APPLIED LINE PRICE ADJUSTMENTS (ADJ)</a> <a HREF="#ALA">Column Definitions</a> &f');

UTL_FILE.PUT_LINE(handle,'&std &sh PRC_ADJ_ID &dh LINE &dh LST_HD_ID &dh LST_LN_ID &dh LIST_LN_NO &dh MOD_LVL &dh LIST_TYPE_CODE &dh CHG_TY_CD &dh');
UTL_FILE.PUT_LINE(handle,'ARITH_OP &dh OP_PER_QTY &dh ADJ_AMT_PQ &dh OPERAND &dh ADJ_AMT &dh CD &dh AF &dh PI &dh AC &dh IF &dh EF &dh UA &dh UF &dh');
UTL_FILE.PUT_LINE(handle,'AP &dh LK &dh PERC &dh COST_ID &dh TAX_CODE &dh PP &eh');


Declare
cursor l_prc_adj_un is
select
    ADJ.PRICE_ADJUSTMENT_ID            PRC_ADJ_ID,
    to_char(LIN.line_number) || 
          decode(LIN.shipment_number, null, null, '.' || to_char(LIN.shipment_number))|| 
          decode(LIN.option_number, null, null, '.' || to_char(LIN.option_number)) ||
          decode(LIN.component_number, null, null, 
                 decode(LIN.option_number, null, '.',null)||
                 '.'||to_char(LIN.component_number))||
          decode(LIN.service_number,null,null,
                 decode(LIN.component_number, null, '.' , null) ||
                        decode(LIN.option_number, null, '.', null ) ||
                        '.'|| to_char(LIN.service_number)) LINE,
     ADJ.LIST_HEADER_ID                 LST_HD_ID,             
     ADJ.LIST_LINE_ID                   LST_LN_ID,  
     ADJ.LIST_LINE_NO                   LIST_LN_NO,   
     ADJ.MODIFIER_LEVEL_CODE            MOD_LVL,     
     ADJ.LIST_LINE_TYPE_CODE            LIST_TYPE_CODE,
     ADJ.CHARGE_TYPE_CODE               CHG_TY_CD,             
     ADJ.ARITHMETIC_OPERATOR            ARITH_OP,  
     ADJ.OPERAND_PER_PQTY               OP_PER_QTY,        
     ADJ.ADJUSTED_AMOUNT_PER_PQTY       ADJ_AMT_PQ,
     ADJ.OPERAND                        OPERAND,                      
     ADJ.ADJUSTED_AMOUNT                ADJ_AMT,                             
     ADJ.CREDIT_OR_CHARGE_FLAG          CD,                          
     ADJ.AUTOMATIC_FLAG                 AF,   
     ADJ.PRINT_ON_INVOICE_FLAG          PI,   
     ADJ.ACCRUAL_FLAG                   AC,        
     ADJ.INVOICED_FLAG                  INF,              
     ADJ.ESTIMATED_FLAG                 EF, 
     ADJ.UPDATE_ALLOWED                 UA,        
     ADJ.UPDATED_FLAG                   UF,         
     ADJ.APPLIED_FLAG                   AP, 
     ADJ.LOCK_CONTROL                   LK,                                      
     ADJ.PERCENT                        PERC,            
     ADJ.COST_ID                        COST_ID,               
     ADJ.TAX_CODE                       TAX_CODE,               
     ADJ.PRICING_PHASE_ID               PP
from OE_PRICE_ADJUSTMENTS   ADJ,
     OE_ORDER_LINES_ALL     LIN
where  ADJ.HEADER_ID             = nvl('&header_id_selected',:v_header_id)
  and  LIN.HEADER_ID             = nvl('&header_id_selected',:v_header_id)
  and  ADJ.LINE_ID               IS NOT NULL
  and  ADJ.LINE_ID               = LIN.LINE_ID
  and  NVL('&line_id_selected',0)in (0,LIN.LINE_ID,
                                       LIN.TOP_MODEL_LINE_ID,
                                       LIN.ATO_LINE_ID,
                                       LIN.LINK_TO_LINE_ID,
                                       LIN.REFERENCE_LINE_ID,
                                       LIN.SERVICE_REFERENCE_LINE_ID)
  and  nvl(ADJ.APPLIED_FLAG,'N') = 'N'
order by LINE,
         LIST_LINE_TYPE_CODE;

begin
 for ll in l_prc_adj_un
 loop
   utl_file.put_line(handle,'&sld'||n(ll.PRC_ADJ_ID)||'&d'||n(ll.LINE)||'&d');
   utl_file.put_line(handle,n(ll.LST_HD_ID)||'&d'||n(ll.LST_LN_ID)||'&d');
   utl_file.put_line(handle,n(ll.LIST_LN_NO)||'&d'||n(ll.MOD_LVL)||'&d');
   utl_file.put_line(handle,n(ll.LIST_TYPE_CODE)||'&d'||n(ll.CHG_TY_CD)||'&d');
   utl_file.put_line(handle,n(ll.ARITH_OP)||'&d'||n(ll.OP_PER_QTY)||'&d');
   utl_file.put_line(handle,n(ll.ADJ_AMT_PQ)||'&d'||n(ll.OPERAND)||'&d');
   utl_file.put_line(handle,n(ll.ADJ_AMT)||'&d'||n(ll.CD)||'&d');
   utl_file.put_line(handle,n(ll.AF)||'&d'||n(ll.PI)||'&d');
   utl_file.put_line(handle,n(ll.AC)||'&d'||n(ll.INF)||'&d');
   utl_file.put_line(handle,n(ll.EF)||'&d'||n(ll.UA)||'&d');
   utl_file.put_line(handle,n(ll.UF)||'&d'||n(ll.AP)||'&d');
   utl_file.put_line(handle,n(ll.LK)||'&d'||n(ll.PERC)||'&d');
   utl_file.put_line(handle,n(ll.COST_ID)||'&d'||n(ll.TAX_CODE)||'&d');
   utl_file.put_line(handle,n(ll.PP)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');

end if; -- prt_price

if substr(UPPER(nvl('&prt_po','Y')),1,1) = 'Y' then

UTL_FILE.PUT_LINE(handle,'&f &f OE_DROP_SHIP_SOURCES (SRC) &f');
UTL_FILE.PUT_LINE(handle,'&std &sh DROP_SHIP_ID &dh HEADER_ID &dh LINE_ID &dh LINE &dh ORG_ID &dh DEST_ID &dh DEST_ORG &dh REQ_HEADER_ID &dh ');
UTL_FILE.PUT_LINE(handle,'REQ_LINE_ID &dh PO_HEAD_ID &dh PO_LINE_ID &dh LINE_LOC_ID &dh PO_RELEASE_ID &eh');

Declare
cursor l_drop_ship is
select 
 SRC.DROP_SHIP_SOURCE_ID                      DROP_SHIP_ID,
 SRC.HEADER_ID                                HEADER_ID,
 SRC.LINE_ID                                  LINE_ID,
    to_char(LIN.line_number) || 
          decode(LIN.shipment_number, null, null, '.' || to_char(LIN.shipment_number))|| 
          decode(LIN.option_number, null, null, '.' || to_char(LIN.option_number)) ||
          decode(LIN.component_number, null, null, 
                 decode(LIN.option_number, null, '.',null)||
                 '.'||to_char(LIN.component_number))||
          decode(LIN.service_number,null,null,
                 decode(LIN.component_number, null, '.' , null) ||
                        decode(LIN.option_number, null, '.', null ) ||
                        '.'|| to_char(LIN.service_number)) LINE,
 SRC.ORG_ID                                   ORG_ID,
 SRC.DESTINATION_ORGANIZATION_ID              DEST_ID,
 PAR.ORGANIZATION_CODE                        DEST_ORG,
 SRC.REQUISITION_HEADER_ID                    REQ_HEADER_ID,
 SRC.REQUISITION_LINE_ID                      REQ_LINE_ID,
 SRC.PO_HEADER_ID                             PO_HEAD_ID,
 SRC.PO_LINE_ID                               PO_LINE_ID,
 SRC.LINE_LOCATION_ID                         LINE_LOC_ID,
 SRC.PO_RELEASE_ID                            PO_RELEASE_ID
FROM OE_DROP_SHIP_SOURCES    SRC,
     OE_ORDER_LINES          LIN, 
     MTL_PARAMETERS          PAR
WHERE
     SRC.LINE_ID                      = LIN.LINE_ID
and  SRC.DESTINATION_ORGANIZATION_ID  = PAR.ORGANIZATION_ID(+)
and  LIN.HEADER_ID                 = nvl('&header_id_selected',:v_header_id)
and  NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID)
order by
     NVL(LIN.TOP_MODEL_LINE_ID,            LIN.LINE_ID),
     NVL(LIN.ATO_LINE_ID,               LIN.LINE_ID),
     NVL(LIN.SORT_ORDER,                '0000'),
     NVL(LIN.LINK_TO_LINE_ID,           LIN.LINE_ID),
     NVL(LIN.SOURCE_DOCUMENT_LINE_ID,   LIN.LINE_ID),
     LIN.LINE_ID;

begin
 for ll in l_drop_ship
 loop
   utl_file.put_line(handle,'&sld'||n(ll.DROP_SHIP_ID)||'&d'||n(ll.HEADER_ID)||'&d');
   utl_file.put_line(handle,n(ll.LINE_ID)||'&d'||n(ll.LINE)||'&d');
   utl_file.put_line(handle,n(ll.ORG_ID)||'&d'||n(ll.DEST_ID)||'&d');
   utl_file.put_line(handle,n(ll.DEST_ORG)||'&d'||n(ll.REQ_HEADER_ID)||'&d');
   utl_file.put_line(handle,n(ll.REQ_LINE_ID)||'&d'||n(ll.PO_HEAD_ID)||'&d');
   utl_file.put_line(handle,n(ll.PO_LINE_ID)||'&d'||n(ll.LINE_LOC_ID)||'&d');
   utl_file.put_line(handle,n(ll.PO_RELEASE_ID)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');

end if; -- prt_po

end if; -- :v_head_only

   UTL_FILE.FCLOSE(handle);
end;
/


DECLARE
  handle UTL_FILE.FILE_TYPE;
  dirname varchar2(1000);
  text    varchar2(1000);

function n(v varchar2) return varchar2 is
begin
  if v is null then
   return '&sp';
   else
   return v;
  end if;
end n;

begin
If :v_head_only = 'N' then 

-- Append to output file
  handle := UTL_FILE.FOPEN('&out_dir','&out_dir/&out_file','A',32000);
  UTL_FILE.PUT_LINE(handle,'&et'); -- in case last one failed

if substr(UPPER(nvl('&prt_po','Y')),1,1) = 'Y' then

UTL_FILE.PUT_LINE(handle,'&f &f PO_REQUISITIONS_INTERFACE_ALL (RQI) &f');
UTL_FILE.PUT_LINE(handle,'&std &sh DROP_SHIP_ID &dh LINE &dh AUTH_STATUS &dh DELIV_LOC &dh PREPARER &dh DEST_ORG_ID &dh DEST_TYPE &dh ');
UTL_FILE.PUT_LINE(handle,'SRC_CODE &dh SRC_TYPE_CODE &dh ITEM_ID &dh NEED_BY &dh QTY &dh PRICE &el');


Declare
cursor po_req_int is
Select  /* DROP SHIP */
     RQI.INTERFACE_SOURCE_LINE_ID           DROP_SHIP_ID,
     to_char(LIN.line_number) || 
          decode(LIN.shipment_number, null, null, '.' || to_char(LIN.shipment_number))|| 
          decode(LIN.option_number, null, null, '.' || to_char(LIN.option_number)) ||
          decode(LIN.component_number, null, null, 
                 decode(LIN.option_number, null, '.',null)||
                 '.'||to_char(LIN.component_number))||
          decode(LIN.service_number,null,null,
                 decode(LIN.component_number, null, '.' , null) ||
                        decode(LIN.option_number, null, '.', null ) ||
                        '.'|| to_char(LIN.service_number)) LINE,
     RQI.AUTHORIZATION_STATUS               AUTH_STATUS,   
     RQI.DELIVER_TO_LOCATION_ID             DELIV_LOC,
     RQI.PREPARER_ID                        PREPARER,
     RQI.DESTINATION_ORGANIZATION_ID        DEST_ORG_ID,
     RQI.DESTINATION_TYPE_CODE              DEST_TYPE,
     RQI.INTERFACE_SOURCE_CODE              SRC_CODE,
     RQI.SOURCE_TYPE_CODE                   SRC_TYPE_CODE,
     RQI.ITEM_ID                            ITEM_ID,
     to_char(RQI.NEED_BY_DATE,'DD-MON-RR_HH24:MI:SS')  NEED_BY,                               
     RQI.QUANTITY                           QTY,                  
     RQI.UNIT_PRICE                         PRICE
from  PO_REQUISITIONS_INTERFACE_ALL   RQI,
      OE_DROP_SHIP_SOURCES            SRC,
      OE_ORDER_LINES                  LIN
where  SRC.LINE_ID                      = LIN.LINE_ID
  and  SRC.DROP_SHIP_SOURCE_ID          = RQI.INTERFACE_SOURCE_LINE_ID
  and  LIN.HEADER_ID                 = nvl('&header_id_selected',:v_header_id)
  and  NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                           LIN.TOP_MODEL_LINE_ID,
                                           LIN.ATO_LINE_ID,
                                           LIN.LINK_TO_LINE_ID,
                                           LIN.SERVICE_REFERENCE_LINE_ID)
order by
     NVL(LIN.TOP_MODEL_LINE_ID,            LIN.LINE_ID),
     NVL(LIN.ATO_LINE_ID,               LIN.LINE_ID),
     NVL(LIN.SORT_ORDER,                '0000'),
     NVL(LIN.LINK_TO_LINE_ID,           LIN.LINE_ID),
     NVL(LIN.SOURCE_DOCUMENT_LINE_ID,   LIN.LINE_ID),
     LIN.LINE_ID;

begin
 for po in po_req_int
 loop
   utl_file.put_line(handle,'&sld'||n(po.DROP_SHIP_ID)||'&d'||n(po.LINE)||'&d');
   utl_file.put_line(handle,n(po.AUTH_STATUS)||'&d'||n(po.DELIV_LOC)||'&d');
   utl_file.put_line(handle,n(po.PREPARER)||'&d'||n(po.DEST_ORG_ID)||'&d');
   utl_file.put_line(handle,n(po.DEST_TYPE)||'&d'||n(po.SRC_CODE)||'&d');
   utl_file.put_line(handle,n(po.SRC_TYPE_CODE)||'&d'||n(po.ITEM_ID)||'&d');
   utl_file.put_line(handle,n(po.NEED_BY)||'&d'||n(po.QTY)||'&d');
   utl_file.put_line(handle,n(po.PRICE)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');

UTL_FILE.PUT_LINE(handle,'&f &f PO_INTERFACE_ERRORS_ALL (POE) &f');
UTL_FILE.PUT_LINE(handle,'&std &sh INTF_TRANS_ID &dh LINE &dh COLUMN_NAME &dh ERROR &dh INTF_TYPE &dh REQUEST_ID &dh TABLE_NAME &eh');


Declare 
cursor po_int_err is
select
  POE.INTERFACE_TRANSACTION_ID     INTF_TRANS_ID,   
  to_char(LIN.line_number) || 
          decode(LIN.shipment_number, null, null, '.' || to_char(LIN.shipment_number))|| 
          decode(LIN.option_number, null, null, '.' || to_char(LIN.option_number)) ||
          decode(LIN.component_number, null, null, 
                 decode(LIN.option_number, null, '.',null)||
                 '.'||to_char(LIN.component_number))||
          decode(LIN.service_number,null,null,
                 decode(LIN.component_number, null, '.' , null) ||
                        decode(LIN.option_number, null, '.', null ) ||
                        '.'|| to_char(LIN.service_number)) LINE,
  POE.COLUMN_NAME                  COLUMN_NAME,                  
  POE.ERROR_MESSAGE                ERRORM,  
  POE.INTERFACE_TYPE               INTF_TYPE,         
  POE.REQUEST_ID                   REQUEST_ID,
  POE.TABLE_NAME                   TABLE_NAME
from  
  PO_INTERFACE_ERRORS             POE,
  OE_DROP_SHIP_SOURCES            SRC,
  OE_ORDER_LINES                  LIN,
  PO_REQUISITIONS_INTERFACE_ALL   RQI
where 
     SRC.LINE_ID                      = LIN.LINE_ID
and  SRC.DROP_SHIP_SOURCE_ID          = RQI.INTERFACE_SOURCE_LINE_ID
and  RQI.TRANSACTION_ID               = POE.INTERFACE_TRANSACTION_ID
and  LIN.HEADER_ID                    = nvl('&header_id_selected',:v_header_id)
and  NVL('&line_id_selected',0)       in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID)
order by
     NVL(LIN.TOP_MODEL_LINE_ID,         LIN.LINE_ID),
     NVL(LIN.ATO_LINE_ID,               LIN.LINE_ID),
     NVL(LIN.SORT_ORDER,                '0000'),
     NVL(LIN.LINK_TO_LINE_ID,           LIN.LINE_ID),
     NVL(LIN.SOURCE_DOCUMENT_LINE_ID,   LIN.LINE_ID),
     LIN.LINE_ID;

begin
 for po in po_int_err
 loop
   utl_file.put_line(handle,'&sld'||n(po.INTF_TRANS_ID)||'&d'||n(po.LINE)||'&d');
   utl_file.put_line(handle,n(po.COLUMN_NAME)||'&d'||n(po.ERRORM)||'&d');
   utl_file.put_line(handle,n(po.INTF_TYPE)||'&d'||n(po.REQUEST_ID)||'&d');
   utl_file.put_line(handle,n(po.TABLE_NAME)||'&el');
 end loop;
end;


UTL_FILE.PUT_LINE(handle,'&et');

UTL_FILE.PUT_LINE(handle,'&f &f PO_REQUISITION_HEADERS_ALL (RQH) &f');
UTL_FILE.PUT_LINE(handle,'&std &sh REQ_HEADER_ID &dh REQ_NUMBER &dh DROP_SHIP_ID &dh AUTH_STATUS &dh ENABLED &dh SRC_CODE &dh SUMMARY &dh ');
UTL_FILE.PUT_LINE(handle,'XFR_OE_FLAG &dh REQ_TYPE &dh ITEM_TYPE &dh ITEM_KEY &eh');

Declare
cursor po_req_hdr is
select distinct 
  RQH.REQUISITION_HEADER_ID         REQ_HEADER_ID ,
  RQH.SEGMENT1                            REQ_NUMBER,
  RQH.INTERFACE_SOURCE_LINE_ID            DROP_SHIP_ID,
  RQH.AUTHORIZATION_STATUS                AUTH_STATUS,               
  RQH.ENABLED_FLAG                        ENABLED,  
  RQH.INTERFACE_SOURCE_CODE               SRC_CODE,
  RQH.SUMMARY_FLAG                        SUMMARY,
  RQH.TRANSFERRED_TO_OE_FLAG              XFR_OE_FLAG,
  RQH.TYPE_LOOKUP_CODE                    REQ_TYPE,
  RQH.WF_ITEM_TYPE                        ITEM_TYPE,
  RQH.WF_ITEM_KEY                         ITEM_KEY
from 
 PO_REQUISITION_HEADERS_ALL      RQH,
 OE_DROP_SHIP_SOURCES            SRC,
 OE_ORDER_LINES                  LIN
where 
     SRC.LINE_ID                      = LIN.LINE_ID
and SRC.REQUISITION_HEADER_ID         = RQH.REQUISITION_HEADER_ID
and  LIN.HEADER_ID                    = nvl('&header_id_selected',:v_header_id)
and  NVL('&line_id_selected',0)      in (0,LIN.LINE_ID,
                                           LIN.TOP_MODEL_LINE_ID,
                                           LIN.ATO_LINE_ID,
                                           LIN.LINK_TO_LINE_ID,
                                           LIN.SERVICE_REFERENCE_LINE_ID)
UNION ALL
select distinct  /* INTERNAL REQ */
  RQH.REQUISITION_HEADER_ID        REQ_HEADER_ID ,
  RQH.SEGMENT1                            REQ_NUMBER,
  RQH.INTERFACE_SOURCE_LINE_ID            DROP_SHIP_ID,
  RQH.AUTHORIZATION_STATUS                AUTH_STATUS,               
  RQH.ENABLED_FLAG                        ENABLED,  
  RQH.INTERFACE_SOURCE_CODE               SRC_CODE,
  RQH.SUMMARY_FLAG                        SUMMARY,
  RQH.TRANSFERRED_TO_OE_FLAG              XFR_OE_FLAG,
  RQH.TYPE_LOOKUP_CODE                    REQ_TYPE,
  RQH.WF_ITEM_TYPE                        ITEM_TYPE,
  RQH.WF_ITEM_KEY                         ITEM_KEY
from 
 PO_REQUISITION_HEADERS_ALL      RQH,
 OE_ORDER_LINES                  LIN
where 
     LIN.SOURCE_DOCUMENT_ID           = RQH.REQUISITION_HEADER_ID
and  LIN.SOURCE_DOCUMENT_TYPE_ID      = 10                        --INTERNAL REQUISITION
and  LIN.HEADER_ID                    = nvl('&header_id_selected',:v_header_id)
and  NVL('&line_id_selected',0)      in (0,LIN.LINE_ID,
                                           LIN.TOP_MODEL_LINE_ID,
                                           LIN.ATO_LINE_ID,
                                           LIN.LINK_TO_LINE_ID,
                                           LIN.SERVICE_REFERENCE_LINE_ID)
UNION ALL
select distinct  /* ATO BUY ITEM */
  RQH.REQUISITION_HEADER_ID        REQ_HEADER_ID ,
  RQH.SEGMENT1                            REQ_NUMBER,
  RQH.INTERFACE_SOURCE_LINE_ID            DROP_SHIP_ID,
  RQH.AUTHORIZATION_STATUS                AUTH_STATUS,
  RQH.ENABLED_FLAG                        ENABLED,
  RQH.INTERFACE_SOURCE_CODE               SRC_CODE,
  RQH.SUMMARY_FLAG                        SUMMARY,
  RQH.TRANSFERRED_TO_OE_FLAG              XFR_OE_FLAG,
  RQH.TYPE_LOOKUP_CODE                    REQ_TYPE,
  RQH.WF_ITEM_TYPE                        ITEM_TYPE,
  RQH.WF_ITEM_KEY                         ITEM_KEY
from
  PO_REQUISITION_HEADERS_ALL      RQH,
  MTL_RESERVATIONS                RES
where
     :sales_ord_id                 = RES.DEMAND_SOURCE_HEADER_ID
and  RES.DEMAND_SOURCE_TYPE_ID     = 2   -- SO
and  RES.SUPPLY_SOURCE_TYPE_ID     = 17  -- Req
and  RES.SUPPLY_SOURCE_HEADER_ID   = RQH.REQUISITION_HEADER_ID
UNION ALL
SELECT DISTINCT  /* BACK TO BACK REQ */
 RQH.REQUISITION_HEADER_ID         REQ_HEADER_ID ,
 RQH.SEGMENT1                         REQ_NUMBER,
 RQH.INTERFACE_SOURCE_LINE_ID         DROP_SHIP_ID,
 RQH.AUTHORIZATION_STATUS             AUTH_STATUS,
 RQH.ENABLED_FLAG                     ENABLED,
 RQH.INTERFACE_SOURCE_CODE            SRC_CODE,
 RQH.SUMMARY_FLAG                     SUMMARY,
 RQH.TRANSFERRED_TO_OE_FLAG           XFR_OE_FLAG,
 RQH.TYPE_LOOKUP_CODE                 REQ_TYPE,
 RQH.WF_ITEM_TYPE                     ITEM_TYPE,
 RQH.WF_ITEM_KEY                      ITEM_KEY
FROM
 PO_REQUISITION_HEADERS_ALL     RQH,
 OE_ORDER_LINES_ALL             LIN
WHERE RQH.INTERFACE_SOURCE_LINE_ID = LIN.LINE_ID
 AND RQH.INTERFACE_SOURCE_CODE      = 'CTO'
 and LIN.HEADER_ID                  = nvl('&header_id_selected',:v_header_id);

begin
 for po in po_req_hdr
 loop
   utl_file.put_line(handle,'&sld'||'<a NAME="PORH'||po.REQ_HEADER_ID||'">'||n(po.REQ_HEADER_ID)||'</a>'||'&d'||n(po.REQ_NUMBER)||'&d');
   utl_file.put_line(handle,n(po.DROP_SHIP_ID)||'&d'||n(po.AUTH_STATUS)||'&d');
   utl_file.put_line(handle,n(po.ENABLED)||'&d'||n(po.SRC_CODE)||'&d');
   utl_file.put_line(handle,n(po.SUMMARY)||'&d'||n(po.XFR_OE_FLAG)||'&d');
   utl_file.put_line(handle,n(po.REQ_TYPE)||'&d'||n(po.ITEM_TYPE)||'&d');
   utl_file.put_line(handle,n(po.ITEM_KEY)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');


UTL_FILE.PUT_LINE(handle,'&f &f PO_REQUISITION_LINES_ALL (RQL) &f');

UTL_FILE.PUT_LINE(handle,'&std &sh REQ_HEAD_ID &dh REQ_LINE_ID &dh DOC_TYPE &dh REQ_LINE &dh LINE &dh ITEM_ID &dh ITEM_DESC &dh UOM &dh PRICE &dh ');
UTL_FILE.PUT_LINE(handle,'QTY &dh QTY_CNC &dh QTY_DLV &dh CANC &dh SRC_TYPE &dh SRC_ORG &dh DEST_TYPE &dh DEST_ORG &dh ENC_FL &dh LINE_TYPE_ID &dh ');
UTL_FILE.PUT_LINE(handle,'NEED_BY &dh RFQ &dh BUYER_ID &eh');


Declare 
cursor po_req_lin is
select /* DROP SHIPMENT */
  RQL.REQUISITION_HEADER_ID        REQ_HEAD_ID,
  RQL.REQUISITION_LINE_ID          REQ_LINE_ID, 
  'DROP SHIP'                      DOC_TYPE,             
  RQL.LINE_NUM                     REQ_LINE,
  to_char(LIN.line_number) || 
          decode(LIN.shipment_number, null, null, '.' || to_char(LIN.shipment_number))|| 
          decode(LIN.option_number, null, null, '.' || to_char(LIN.option_number)) ||
          decode(LIN.component_number, null, null, 
                 decode(LIN.option_number, null, '.',null)||
                 '.'||to_char(LIN.component_number))||
          decode(LIN.service_number,null,null,
                 decode(LIN.component_number, null, '.' , null) ||
                        decode(LIN.option_number, null, '.', null ) ||
                        '.'|| to_char(LIN.service_number)) LINE,
  RQL.ITEM_ID                      ITEM_ID,    
  RQL.ITEM_DESCRIPTION             ITEM_DESC,
  RQL.UNIT_MEAS_LOOKUP_CODE        UOM, 
  RQL.UNIT_PRICE                   PRICE,
  RQL.QUANTITY                     QTY,           
  RQL.QUANTITY_CANCELLED           QTY_CNC,          
  RQL.QUANTITY_DELIVERED           QTY_DLV,                  
  RQL.CANCEL_FLAG                  CANC,   
  RQL.SOURCE_TYPE_CODE             SRC_TYPE,
  RQL.SOURCE_ORGANIZATION_ID       SRC_ORG,     
  RQL.DESTINATION_CONTEXT          DEST_TYPE,     
  RQL.DESTINATION_ORGANIZATION_ID  DEST_ORG,
  RQL.ENCUMBERED_FLAG              ENC_FL,                 
  RQL.LINE_TYPE_ID                 LINE_TYPE_ID,
  to_char(RQL.NEED_BY_DATE,'DD-MON-RR_HH24:MI:SS')  NEED_BY,
  RQL.ON_RFQ_FLAG                  RFQ ,                                          
  RQL.SUGGESTED_BUYER_ID           BUYER_ID
from 
  PO_REQUISITION_LINES_ALL         RQL,
  OE_DROP_SHIP_SOURCES             SRC,
  OE_ORDER_LINES                   LIN
where 
     SRC.LINE_ID                      = LIN.LINE_ID
and  RQL.REQUISITION_LINE_ID          = SRC.REQUISITION_LINE_ID
and  LIN.HEADER_ID                 = nvl('&header_id_selected',:v_header_id)
and  NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID)
UNION ALL
select  /* INTERNAL SALES ORDER */
  RQL.REQUISITION_HEADER_ID        REQ_HEAD_ID,
  RQL.REQUISITION_LINE_ID          REQ_LINE_ID,    
  'INTERNAL SO'                    DOC_TYPE,                       
  RQL.LINE_NUM                     REQ_LINE,
  to_char(LIN.line_number) || 
          decode(LIN.shipment_number, null, null, '.' || to_char(LIN.shipment_number))|| 
          decode(LIN.option_number, null, null, '.' || to_char(LIN.option_number)) ||
          decode(LIN.component_number, null, null, 
                 decode(LIN.option_number, null, '.',null)||
                 '.'||to_char(LIN.component_number))||
          decode(LIN.service_number,null,null,
                 decode(LIN.component_number, null, '.' , null) ||
                        decode(LIN.option_number, null, '.', null ) ||
                        '.'|| to_char(LIN.service_number)) LINE,
  RQL.ITEM_ID                      ITEM_ID,    
  RQL.ITEM_DESCRIPTION             ITEM_DESC,
  RQL.UNIT_MEAS_LOOKUP_CODE        UOM, 
  RQL.UNIT_PRICE                   PRICE,
  RQL.QUANTITY                     QTY,           
  RQL.QUANTITY_CANCELLED           QTY_CNC,          
  RQL.QUANTITY_DELIVERED           QTY_DLV,                  
  RQL.CANCEL_FLAG                  CANC,      
  RQL.SOURCE_TYPE_CODE             SRC_TYPE,
  RQL.SOURCE_ORGANIZATION_ID       SRC_ORG,       
  RQL.DESTINATION_CONTEXT          DEST_TYPE,     
  RQL.DESTINATION_ORGANIZATION_ID  DEST_ORG,
  RQL.ENCUMBERED_FLAG              ENC_FL ,                 
  RQL.LINE_TYPE_ID                 LINE_TYPE_ID,
  to_char(RQL.NEED_BY_DATE,'DD-MON-RR_HH24:MI:SS')  NEED_BY,
  RQL.ON_RFQ_FLAG                  RFQ ,                                          
  RQL.SUGGESTED_BUYER_ID           BUYER_ID
from 
  PO_REQUISITION_LINES_ALL         RQL,
  OE_ORDER_LINES                   LIN
where 
     LIN.SOURCE_DOCUMENT_LINE_ID      = RQL.REQUISITION_LINE_ID 
and  LIN.SOURCE_DOCUMENT_TYPE_ID      = 10                        --INTERNAL REQUISITION
and  LIN.HEADER_ID                    = nvl('&header_id_selected',:v_header_id)
and  NVL('&line_id_selected',0)       in (0,LIN.LINE_ID,
                                            LIN.TOP_MODEL_LINE_ID,
                                            LIN.ATO_LINE_ID,
                                            LIN.LINK_TO_LINE_ID,
                                            LIN.SERVICE_REFERENCE_LINE_ID)
UNION ALL
SELECT DISTINCT  /* BACK TO BACK REQ */
 RQL.REQUISITION_HEADER_ID        REQ_HEAD_ID,
 RQL.REQUISITION_LINE_ID          REQ_LINE_ID,
 'BACK TO BACK'                   DOC_TYPE,
 RQL.LINE_NUM                     REQ_LINE,
 TO_CHAR(LIN.line_number)
 || DECODE(LIN.shipment_number, NULL, NULL, '.'
 || TO_CHAR(LIN.shipment_number))
 || DECODE(LIN.option_number, NULL, NULL, '.'
 || TO_CHAR(LIN.option_number))
 || DECODE(LIN.component_number, NULL, NULL, DECODE(LIN.option_number, NULL, '.',NULL)
 || '.'
 ||TO_CHAR(LIN.component_number))
 || DECODE(LIN.service_number,NULL,NULL, DECODE(LIN.component_number, NULL, '.' , NULL)
 || DECODE(LIN.option_number, NULL, '.', NULL )
 || '.'
 || TO_CHAR(LIN.service_number))  LINE,
 RQL.ITEM_ID                      ITEM_ID,
 RQL.ITEM_DESCRIPTION             ITEM_DESC,
 RQL.UNIT_MEAS_LOOKUP_CODE        UOM,
 RQL.UNIT_PRICE                   PRICE,
 RQL.QUANTITY                     QTY,
 RQL.QUANTITY_CANCELLED           QTY_CNC,
 RQL.QUANTITY_DELIVERED           QTY_DLV,
 RQL.CANCEL_FLAG                  CANC,
 RQL.SOURCE_TYPE_CODE             SRC_TYPE,
 RQL.SOURCE_ORGANIZATION_ID       SRC_ORG,
 RQL.DESTINATION_CONTEXT          DEST_TYPE,
 RQL.DESTINATION_ORGANIZATION_ID  DEST_ORG,
 RQL.ENCUMBERED_FLAG              ENC_FL ,
 RQL.LINE_TYPE_ID                 LINE_TYPE_ID,
 TO_CHAR(RQL.NEED_BY_DATE,'DD-MON-RR_HH24:MI:SS') NEED_BY,
 RQL.ON_RFQ_FLAG                  RFQ ,
 RQL.SUGGESTED_BUYER_ID           BUYER_ID
FROM
 PO_REQUISITION_HEADERS_ALL    RQH,
 PO_REQUISITION_LINES_ALL      RQL,
 OE_ORDER_LINES_ALL            LIN
WHERE RQH.INTERFACE_SOURCE_LINE_ID = LIN.LINE_ID
 AND RQH.INTERFACE_SOURCE_CODE      = 'CTO'
 AND RQH.REQUISITION_HEADER_ID      = RQL.REQUISITION_HEADER_ID
 and LIN.HEADER_ID                    = nvl('&header_id_selected',:v_header_id)
 and NVL('&line_id_selected',0)       in (0,LIN.LINE_ID,
                                            LIN.TOP_MODEL_LINE_ID,
                                            LIN.ATO_LINE_ID,
                                            LIN.LINK_TO_LINE_ID,
                                            LIN.SERVICE_REFERENCE_LINE_ID);

begin
 for po in po_req_lin
 loop
   utl_file.put_line(handle,'&sld'||n(po.REQ_HEAD_ID)||'&d'||'<a NAME="PORL'||po.REQ_LINE_ID||'">'||n(po.REQ_LINE_ID)||'</a>'||'&d');
   utl_file.put_line(handle,n(po.DOC_TYPE)||'&d'||n(po.REQ_LINE)||'&d');
   utl_file.put_line(handle,n(po.LINE)||'&d'||n(po.ITEM_ID)||'&d');
   utl_file.put_line(handle,n(po.ITEM_DESC)||'&d'||n(po.UOM)||'&d');
   utl_file.put_line(handle,n(po.PRICE)||'&d'||n(po.QTY)||'&d');
   utl_file.put_line(handle,n(po.QTY_CNC)||'&d'||n(po.QTY_DLV)||'&d');
   utl_file.put_line(handle,n(po.CANC)||'&d'||n(po.SRC_TYPE)||'&d');
   utl_file.put_line(handle,n(po.SRC_ORG)||'&d'||n(po.DEST_TYPE)||'&d');
   utl_file.put_line(handle,n(po.DEST_ORG)||'&d'||n(po.ENC_FL)||'&d');
   utl_file.put_line(handle,n(po.LINE_TYPE_ID)||'&d'||n(po.NEED_BY)||'&d');
   utl_file.put_line(handle,n(po.RFQ)||'&d'||n(po.BUYER_ID)||'&el');
 end loop;
end;
        
UTL_FILE.PUT_LINE(handle,'&et');


UTL_FILE.PUT_LINE(handle,'&f &f WORKFLOW REQUISITION APPROVAL STATUS &f');

-- break on REQ_NUM_IK skip 2;

UTL_FILE.PUT_LINE(handle,'&std &sh REQ_NUM_IK &dh PROCESS_NAME &dh ACTIVITY_NAME &dh RESULT &dh ACT_STATUS &dh NOTIF_ID &dh _BEGIN_DATE_ &dh ');
UTL_FILE.PUT_LINE(handle,'_END_DATE_ &dh ERROR &eh');

Declare 
cursor po_req_apprv is
select WFS.item_key               REQ_NUM_IK,
       WFA.DISPLAY_NAME           PROCESS_NAME,
       WFA1.DISPLAY_NAME          ACTIVITY_NAME,
       WF_CORE.ACTIVITY_RESULT(WFA1.RESULT_TYPE,WFS.ACTIVITY_RESULT_CODE) RESULT,
       LKP.MEANING                ACT_STATUS,
       WFS.NOTIFICATION_ID        NOTIF_ID,
       to_char(WFS.BEGIN_DATE,'DD-MON-RR_HH24:MI:SS') BEGIN_DATE,
       to_char(WFS.END_DATE,'DD-MON-RR_HH24:MI:SS') END_DATE,
       WFS.ERROR_NAME             ERROR_apprv
from WF_ITEM_ACTIVITY_STATUSES WFS,
     WF_PROCESS_ACTIVITIES     WFP,
     WF_ACTIVITIES_VL          WFA,
     WF_ACTIVITIES_VL          WFA1,
     WF_LOOKUPS                LKP
where 
     WFS.ITEM_TYPE          = 'REQAPPRV'
and  WFS.item_key       in (select wf_item_key /*DROP SHIPMENTS*/
                           from 
                           PO_REQUISITION_HEADERS  REQ,
                           OE_DROP_SHIP_SOURCES    SRC,
                           OE_ORDER_LINES          LIN
                           where 
                               SRC.LINE_ID                   = LIN.LINE_ID
                           and SRC.REQUISITION_HEADER_ID     = REQ.REQUISITION_HEADER_ID
                           and  LIN.HEADER_ID                = nvl('&header_id_selected',:v_header_id)
                           and  NVL('&line_id_selected',0)   in (0,LIN.LINE_ID,
                                     LIN.TOP_MODEL_LINE_ID,
                                     LIN.ATO_LINE_ID,
                                     LIN.LINK_TO_LINE_ID,
                                     LIN.SERVICE_REFERENCE_LINE_ID)
                           UNION ALL
                           select wf_item_key /* INTERNAL SALES ORDERSS */
                           from 
                           PO_REQUISITION_HEADERS  REQ,
                           OE_ORDER_LINES          LIN
                           where 
                                LIN.SOURCE_DOCUMENT_ID           = REQ.REQUISITION_HEADER_ID
                           and  LIN.SOURCE_DOCUMENT_TYPE_ID      = 10                        
                           and  LIN.HEADER_ID                = nvl('&header_id_selected',:v_header_id)
                           and  NVL('&line_id_selected',0)   in (0,LIN.LINE_ID,
                                     LIN.TOP_MODEL_LINE_ID,
                                     LIN.ATO_LINE_ID,
                                     LIN.LINK_TO_LINE_ID,
                                     LIN.SERVICE_REFERENCE_LINE_ID))                               
and  WFS.PROCESS_ACTIVITY   = WFP.INSTANCE_ID
and  WFP.PROCESS_ITEM_TYPE  = WFA.ITEM_TYPE
and  WFP.PROCESS_NAME       = WFA.NAME
and  WFP.PROCESS_VERSION    = WFA.VERSION
and  WFP.ACTIVITY_ITEM_TYPE = WFA1.ITEM_TYPE
and  WFP.ACTIVITY_NAME      = WFA1.NAME
and  WFA1.VERSION = 
    (select max(VERSION)
     from WF_ACTIVITIES WF2
     where WF2.ITEM_TYPE = WFP.ACTIVITY_ITEM_TYPE
     and   WF2.NAME      = WFP.ACTIVITY_NAME)
and  LKP.LOOKUP_TYPE = 'WFENG_STATUS'
and  LKP.LOOKUP_CODE = WFS.ACTIVITY_STATUS
order by WFS.ITEM_KEY, WFS.BEGIN_DATE, EXECUTION_TIME;

begin
 for po in po_req_apprv
 loop
   utl_file.put_line(handle,'&sld'||n(po.REQ_NUM_IK)||'&d'||n(po.PROCESS_NAME)||'&d');
   utl_file.put_line(handle,n(po.ACTIVITY_NAME)||'&d'||n(po.RESULT)||'&d');
   utl_file.put_line(handle,n(po.ACT_STATUS)||'&d'||n(po.NOTIF_ID)||'&d');
   utl_file.put_line(handle,n(po.BEGIN_DATE)||'&d'||n(po.END_DATE)||'&d');
   utl_file.put_line(handle,n(po.ERROR_APPRV)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');

UTL_FILE.PUT_LINE(handle,'&f &f WORKFLOW REQUISITION APPROVAL NOTIFICATIONS &f');
UTL_FILE.PUT_LINE(handle,'&std &sh NOTIF_ID &dh TO_USER &dh ORIG_RECIP &dh RECIP_ROLE &dh MAIL_STAT &dh MESSAGE_NAME &dh STATUS &dh SUBJECT &eh');


Declare 
cursor po_req_apprv_not is
select   WFN.NOTIFICATION_ID         NOTIF_ID,
         WFN.TO_USER                 TO_USER,   
         WFN.ORIGINAL_RECIPIENT      ORIG_RECIP,           
         WFN.RECIPIENT_ROLE          RECIP_ROLE,                     
         WFN.MAIL_STATUS             MAIL_STAT,             
         WFN.MESSAGE_NAME            MESSAGE_NAME,      
         WFN.STATUS                  STATUS,                    
         WFN.SUBJECT                 SUBJECT
from WF_ITEM_ACTIVITY_STATUSES WFS,
     WF_NOTIFICATIONS          WFN
where 
     WFS.ITEM_TYPE          = 'REQAPPRV'
and  WFS.item_key       in (select wf_item_key  /* DROP SHIPMENTS */
                           from 
                           PO_REQUISITION_HEADERS  REQ,
                           OE_DROP_SHIP_SOURCES    SRC,
                           OE_ORDER_LINES          LIN
                           where 
                               SRC.LINE_ID                   = LIN.LINE_ID
                           and SRC.REQUISITION_HEADER_ID     = REQ.REQUISITION_HEADER_ID
                           and  LIN.HEADER_ID                = nvl('&header_id_selected',:v_header_id)
                           and  NVL('&line_id_selected',0)   in (0,LIN.LINE_ID,
                                     LIN.TOP_MODEL_LINE_ID,
                                     LIN.ATO_LINE_ID,
                                     LIN.LINK_TO_LINE_ID,
                                     LIN.SERVICE_REFERENCE_LINE_ID)
                           UNION ALL
                           select wf_item_key  /* INTERNAL SALES ORDERSS */
                           from 
                           PO_REQUISITION_HEADERS  REQ,
                           OE_ORDER_LINES          LIN
                           where 
                                LIN.SOURCE_DOCUMENT_ID           = REQ.REQUISITION_HEADER_ID
                           and  LIN.SOURCE_DOCUMENT_TYPE_ID      = 10                        
                           and  LIN.HEADER_ID                = nvl('&header_id_selected',:v_header_id)
                           and  NVL('&line_id_selected',0)   in (0,LIN.LINE_ID,
                                     LIN.TOP_MODEL_LINE_ID,
                                     LIN.ATO_LINE_ID,
                                     LIN.LINK_TO_LINE_ID,
                                     LIN.SERVICE_REFERENCE_LINE_ID))                               
and  WFS.NOTIFICATION_ID is not null
and  WFN.NOTIFICATION_ID = WFS.NOTIFICATION_ID
order by WFS.ITEM_KEY, WFS.BEGIN_DATE, EXECUTION_TIME;

begin
 for po in po_req_apprv_Not
 loop
   utl_file.put_line(handle,'&sld'||n(po.NOTIF_ID)||'&d'||n(po.To_USER)||'&d');
   utl_file.put_line(handle,n(po.ORIG_RECIP)||'&d'||n(po.RECIP_ROLE)||'&d');
   utl_file.put_line(handle,n(po.MAIL_STAT)||'&d'||n(po.MESSAGE_NAME)||'&d');
   utl_file.put_line(handle,n(po.STATUS)||'&d'||n(po.SUBJECT)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et ');


UTL_FILE.PUT_LINE(handle,'&f &f WORKFLOW REQUISITION APPROVAL ERRORS &f');
UTL_FILE.PUT_LINE(handle,'&std &sh PROCESS_NAME &dh ACTIVITY_NAME &dh RESULT &dh ACT_STATUS &dh ERROR_NAME &dh ERROR_MESSAGE &dh ERROR_STACK &eh');

Declare 
cursor po_req_apprv_err is
select WFA.DISPLAY_NAME           PROCESS_NAME,
       WFA1.DISPLAY_NAME          ACTIVITY_NAME,
       WF_CORE.ACTIVITY_RESULT(WFA1.RESULT_TYPE,WFS.ACTIVITY_RESULT_CODE) RESULT,
       LKP.MEANING                ACT_STATUS,
       WFS.ERROR_NAME             ERROR_NAME,
       WFS.ERROR_MESSAGE          ERROR_MESSAGE,
       WFS.ERROR_STACK            ERROR_STACK
from WF_ITEM_ACTIVITY_STATUSES WFS,
     WF_PROCESS_ACTIVITIES     WFP,
     WF_ACTIVITIES_VL          WFA,
     WF_ACTIVITIES_VL          WFA1,
     WF_LOOKUPS                LKP
where 
     WFS.ITEM_TYPE          = 'REQAPPRV'
and  WFS.item_key       in (select wf_item_key /* DROP SHIPMENTS */
                           from 
                           PO_REQUISITION_HEADERS  REQ,
                           OE_DROP_SHIP_SOURCES    SRC,
                           OE_ORDER_LINES          LIN
                           where 
                               SRC.LINE_ID                   = LIN.LINE_ID
                           and SRC.REQUISITION_HEADER_ID     = REQ.REQUISITION_HEADER_ID
                           and  LIN.HEADER_ID                = nvl('&header_id_selected',:v_header_id)
                           and  NVL('&line_id_selected',0)   in (0,LIN.LINE_ID,
                                     LIN.TOP_MODEL_LINE_ID,
                                     LIN.ATO_LINE_ID,
                                     LIN.LINK_TO_LINE_ID,
                                     LIN.SERVICE_REFERENCE_LINE_ID)
                           UNION ALL
                           select wf_item_key  /* INTERNAL SALES ORDERSS */
                           from 
                           PO_REQUISITION_HEADERS  REQ,
                           OE_ORDER_LINES          LIN
                           where 
                                LIN.SOURCE_DOCUMENT_ID           = REQ.REQUISITION_HEADER_ID
                           and  LIN.SOURCE_DOCUMENT_TYPE_ID      = 10                        
                           and  LIN.HEADER_ID                = nvl('&header_id_selected',:v_header_id)
                           and  NVL('&line_id_selected',0)   in (0,LIN.LINE_ID,
                                     LIN.TOP_MODEL_LINE_ID,
                                     LIN.ATO_LINE_ID,
                                     LIN.LINK_TO_LINE_ID,
                                     LIN.SERVICE_REFERENCE_LINE_ID))                                
and  WFS.PROCESS_ACTIVITY   = WFP.INSTANCE_ID
and  WFP.PROCESS_ITEM_TYPE  = WFA.ITEM_TYPE
and  WFP.PROCESS_NAME       = WFA.NAME
and  WFP.PROCESS_VERSION    = WFA.VERSION
and  WFP.ACTIVITY_ITEM_TYPE = WFA1.ITEM_TYPE
and  WFP.ACTIVITY_NAME      = WFA1.NAME
and  WFA1.VERSION = 
    (select max(VERSION)
     from WF_ACTIVITIES WF2
     where WF2.ITEM_TYPE = WFP.ACTIVITY_ITEM_TYPE
     and   WF2.NAME      = WFP.ACTIVITY_NAME)
and  LKP.LOOKUP_TYPE = 'WFENG_STATUS'
and  LKP.LOOKUP_CODE = WFS.ACTIVITY_STATUS
and  WFS.ERROR_NAME is not NULL
order by WFS.ITEM_KEY, WFS.BEGIN_DATE, EXECUTION_TIME;

begin
 for po in po_req_apprv_err
 loop
   utl_file.put_line(handle,'&sld'||n(po.PROCESS_NAME)||'&d'||n(po.ACTIVITY_NAME)||'&d');
   utl_file.put_line(handle,n(po.RESULT)||'&d'||n(po.ACT_STATUS)||'&d');
   utl_file.put_line(handle,n(po.ERROR_NAME)||'&d'||n(po.ERROR_MESSAGE)||'&d');
   utl_file.put_line(handle,n(po.ERROR_STACK)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');

UTL_FILE.PUT_LINE(handle,'&f &f PO_HEADERS_INTERFACE (PHI) &f');
UTL_FILE.PUT_LINE(handle,'&std &sh INTF_HEAD_ID &dh BATCH_ID &dh INTF_SRC_CODE &dh PROCESS_CODE &dh ACTION &dh PO_HEADER_ID &dh REL_NUM &dh');
UTL_FILE.PUT_LINE(handle,'PO_RELEASE_ID &dh VENDOR_NAME &dh APPRV_STAT &dh FIRM &dh FROZEN &dh CLOSE_CODE &dh CLOSE_DATE &dh APPRV_REQD &dh'); 
UTL_FILE.PUT_LINE(handle,'REF_NUM &dh VEND_NUM &dh WF_GROUP_ID &eh');

Declare
cursor po_hdr_int is
select 
  PHI.INTERFACE_HEADER_ID            INTF_HEAD_ID,      
  PHI.BATCH_ID                       BATCH_ID,     
  PHI.INTERFACE_SOURCE_CODE          INTF_SRC_CODE,    
  PHI.PROCESS_CODE                   PROCESS_CODE,   
  PHI.ACTION                         ACTION,  
  PHI.PO_HEADER_ID                   PO_HEADER_ID, 
  PHI.RELEASE_NUM                    REL_NUM,
  PHI.PO_RELEASE_ID                  PO_RELEASE_ID,
  PHI.VENDOR_NAME                    VENDOR_NAME,
  PHI.APPROVAL_STATUS                APPRV_STAT,
  PHI.FIRM_FLAG                      FIRM,
  PHI.FROZEN_FLAG                    FROZEN,
  PHI.CLOSED_CODE                    CLOSE_CODE,
  to_char(PHI.CLOSED_DATE,'DD-MON-RR_HH24:MI:SS')  CLOSE_DATE,
  PHI.APPROVAL_REQUIRED_FLAG         APPRV_REQD,                                         
  PHI.REFERENCE_NUM                  REF_NUM,    
  PHI.VENDOR_NUM                     VEND_NUM,               
  PHI.WF_GROUP_ID                    WF_GROUP_ID
from 
     PO_HEADERS_INTERFACE            PHI,
     PO_LINES_INTERFACE              PLI,
     OE_DROP_SHIP_SOURCES            SRC,
     OE_ORDER_LINES                  LIN
where 
     SRC.LINE_ID                   = LIN.LINE_ID
and  SRC.REQUISITION_LINE_ID       = PLI.REQUISITION_LINE_ID
and  PHI.INTERFACE_HEADER_ID       = PLI.INTERFACE_HEADER_ID
and  LIN.HEADER_ID                 = nvl('&header_id_selected',:v_header_id)
and  NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID)order by
     NVL(LIN.TOP_MODEL_LINE_ID,            LIN.LINE_ID),
     NVL(LIN.ATO_LINE_ID,               LIN.LINE_ID),
     NVL(LIN.SORT_ORDER,                '0000'),
     NVL(LIN.LINK_TO_LINE_ID,           LIN.LINE_ID),
     NVL(LIN.SOURCE_DOCUMENT_LINE_ID,   LIN.LINE_ID),
     LIN.LINE_ID;

begin
 for po in po_hdr_int
 loop
   utl_file.put_line(handle,'&sld'||n(po.INTF_HEAD_ID)||'&d'||n(po.BATCH_ID)||'&d');
   utl_file.put_line(handle,n(po.INTF_SRC_CODE)||'&d'||n(po.PROCESS_CODE)||'&d');
   utl_file.put_line(handle,n(po.ACTION)||'&d'||n(po.PO_HEADER_ID)||'&d');
   utl_file.put_line(handle,n(po.REL_NUM)||'&d'||n(po.PO_RELEASE_ID)||'&d');
   utl_file.put_line(handle,n(po.VENDOR_NAME)||'&d'||n(po.APPRV_STAT)||'&d');
   utl_file.put_line(handle,n(po.FIRM)||'&d'||n(po.FROZEN)||'&d');
   utl_file.put_line(handle,n(po.CLOSE_CODE)||'&d'||n(po.CLOSE_DATE)||'&d');
   utl_file.put_line(handle,n(po.APPRV_REQD)||'&d'||n(po.REF_NUM)||'&d');
   utl_file.put_line(handle,n(po.VEND_NUM)||'&d'||n(po.WF_GROUP_ID)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');


UTL_FILE.PUT_LINE(handle,'&f &f PO_LINES_INTERFACE (PLI) &f');
UTL_FILE.PUT_LINE(handle,'&std &sh INTF_LINE_ID &dh INTF_HEAD_ID &dh ACTION &dh PO_LINE &dh LINE &dh PO_LINE_ID &dh SHIP_NUM &dh SHIP_TYPE &dh ');
UTL_FILE.PUT_LINE(handle,'REQ_LINE_ID &dh PO_HEADER_ID &dh LINE_TYPE &dh ITEM &dh UOM &dh QUANTITY &dh PRICE &dh FIRM &dh SHIP_ORG_CODE &dh ');
UTL_FILE.PUT_LINE(handle,'SHIP_ORG_ID &dh SHIP_TO_LOC &dh SHIP_TO_LOC_ID &dh NEED_BY &dh PROMISE &dh WAREH_ID &eh');

Declare
cursor po_lin_int is
select 
  PLI.INTERFACE_LINE_ID              INTF_LINE_ID,
  PLI.INTERFACE_HEADER_ID            INTF_HEAD_ID,
  PLI.ACTION                         ACTION,
  PLI.LINE_NUM                       PO_LINE,
  to_char(LIN.line_number) || 
          decode(LIN.shipment_number, null, null, '.' || to_char(LIN.shipment_number))|| 
          decode(LIN.option_number, null, null, '.' || to_char(LIN.option_number)) ||
          decode(LIN.component_number, null, null, 
                 decode(LIN.option_number, null, '.',null)||
                 '.'||to_char(LIN.component_number))||
          decode(LIN.service_number,null,null,
                 decode(LIN.component_number, null, '.' , null) ||
                        decode(LIN.option_number, null, '.', null ) ||
                        '.'|| to_char(LIN.service_number)) LINE,
  PLI.PO_LINE_ID                     PO_LINE_ID,
  PLI.SHIPMENT_NUM                   SHIP_NUM,            
  PLI.SHIPMENT_TYPE                  SHIP_TYPE,
  PLI.REQUISITION_LINE_ID            REQ_LINE_ID,
  PLI.PO_HEADER_ID                   PO_HEADER_ID,
  PLI.LINE_TYPE                      LINE_TYPE,
  PLI.ITEM                           ITEM,
  PLI.UNIT_OF_MEASURE                UOM,
  PLI.QUANTITY                       QUANTITY,
  PLI.UNIT_PRICE                     PRICE,
  PLI.FIRM_FLAG                      FIRM,
  PLI.SHIP_TO_ORGANIZATION_CODE      SHIP_ORG_CODE,
  PLI.SHIP_TO_ORGANIZATION_ID        SHIP_ORG_ID,
  PLI.SHIP_TO_LOCATION               SHIP_TO_LOC,
  PLI.SHIP_TO_LOCATION_ID            SHIP_TO_LOC_ID,
  to_char(PLI.NEED_BY_DATE,'DD-MON-RR_HH24:MI:SS')  NEED_BY,
  to_char(PLI.PROMISED_DATE,'DD-MON-RR_HH24:MI:SS')  PROMISE,
  PLI.ORGANIZATION_ID                WAREH_ID
from PO_LINES_INTERFACE              PLI,
     OE_DROP_SHIP_SOURCES            SRC,
     OE_ORDER_LINES                  LIN
where 
     SRC.LINE_ID                      = LIN.LINE_ID
and  SRC.REQUISITION_LINE_ID          = PLI.REQUISITION_LINE_ID
and  LIN.HEADER_ID                 = nvl('&header_id_selected',:v_header_id)
and  NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID)
order by
     NVL(LIN.TOP_MODEL_LINE_ID,            LIN.LINE_ID),
     NVL(LIN.ATO_LINE_ID,               LIN.LINE_ID),
     NVL(LIN.SORT_ORDER,                '0000'),
     NVL(LIN.LINK_TO_LINE_ID,           LIN.LINE_ID),
     NVL(LIN.SOURCE_DOCUMENT_LINE_ID,   LIN.LINE_ID),
     LIN.LINE_ID;

begin
 for po in po_lin_int
 loop
   utl_file.put_line(handle,'&sld'||n(po.INTF_LINE_ID)||'&d'||n(po.INTF_HEAD_ID)||'&d');
   utl_file.put_line(handle,n(po.ACTION)||'&d'||n(po.PO_LINE)||'&d');
   utl_file.put_line(handle,n(po.LINE)||'&d'||n(po.PO_LINE_ID)||'&d');
   utl_file.put_line(handle,n(po.SHIP_NUM)||'&d'||n(po.SHIP_TYPE)||'&d');
   utl_file.put_line(handle,n(po.REQ_LINE_ID)||'&d'||n(po.PO_HEADER_ID)||'&d');
   utl_file.put_line(handle,n(po.LINE_TYPE)||'&d'||n(po.ITEM)||'&d');
   utl_file.put_line(handle,n(po.UOM)||'&d'||n(po.QUANTITY)||'&d');
   utl_file.put_line(handle,n(po.PRICE)||'&d'||n(po.FIRM)||'&d');
   utl_file.put_line(handle,n(po.SHIP_ORG_CODE)||'&d'||n(po.SHIP_ORG_ID)||'&d');
   utl_file.put_line(handle,n(po.SHIP_TO_LOC)||'&d'||n(po.SHIP_TO_LOC_ID)||'&d');
   utl_file.put_line(handle,n(po.NEED_BY)||'&d'||n(po.PROMISE)||'&d');
   utl_file.put_line(handle,n(po.WAREH_ID)||'&el');
 end loop;
end;
UTL_FILE.PUT_LINE(handle,'&et');


UTL_FILE.PUT_LINE(handle,'&f &f PO_INTERFACE_ERRORS_ALL    (POE) &f');
UTL_FILE.PUT_LINE(handle,'&std &sh INF_TRANS_ID &dh COLUMN_NAME &dh ERROR &dh INF_TYPE &dh REQUEST_ID &dh TABLE_NAME &eh');

Declare
cursor po_int_err is
select 
     POE.INTERFACE_TRANSACTION_ID      INF_TRANS_ID,
     POE.COLUMN_NAME                   COLUMN_NAME,
     POE.ERROR_MESSAGE                 ERROR_M,
     POE.INTERFACE_TYPE                INF_TYPE,
     POE.REQUEST_ID                    REQUEST_ID,
     POE.TABLE_NAME                    TABLE_N
from  
     PO_INTERFACE_ERRORS             POE,
     PO_HEADERS_INTERFACE            PHI,
     PO_LINES_INTERFACE              PLI,
     OE_DROP_SHIP_SOURCES            SRC,
     OE_ORDER_LINES                  LIN
where 
     SRC.LINE_ID                   = LIN.LINE_ID
and  SRC.REQUISITION_LINE_ID       = PLI.REQUISITION_LINE_ID
and  PHI.INTERFACE_HEADER_ID       = PLI.INTERFACE_HEADER_ID
and  (POE.INTERFACE_TRANSACTION_ID = PLI.INTERFACE_LINE_ID
 or   POE.INTERFACE_TRANSACTION_ID = PHI.INTERFACE_HEADER_ID)
and  LIN.HEADER_ID                 = nvl('&header_id_selected',:v_header_id)
and  NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID)order by
     NVL(LIN.TOP_MODEL_LINE_ID,            LIN.LINE_ID),
     NVL(LIN.ATO_LINE_ID,               LIN.LINE_ID),
     NVL(LIN.SORT_ORDER,                '0000'),
     NVL(LIN.LINK_TO_LINE_ID,           LIN.LINE_ID),
     NVL(LIN.SOURCE_DOCUMENT_LINE_ID,   LIN.LINE_ID),
     LIN.LINE_ID;

begin
 for po in po_int_err
 loop
   utl_file.put_line(handle,'&sld'||n(po.INF_TRANS_ID)||'&d'||n(po.COLUMN_NAME)||'&d');
   utl_file.put_line(handle,n(po.ERROR_M)||'&d'||n(po.INF_TYPE)||'&d');
   utl_file.put_line(handle,n(po.REQUEST_ID)||'&d'||n(po.TABLE_N)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');


UTL_FILE.PUT_LINE(handle,'&f &f PO_HEADERS_ALL (POH) &f');

UTL_FILE.PUT_LINE(handle,'&std &sh PO_HEADER_ID &dh PO_NUM &dh ACCEPT_REQD &dh BILL_TO &dh SHIP_TO &dh CLS_STAT &dh CONF_ORD &dh CURR &dh ');
UTL_FILE.PUT_LINE(handle,'ENABLED &dh FROZEN &dh SUMM &dh TYPE &dh VEND_CNCACT &dh VEND_ID &dh VEND_SITE &dh ITEM_TYPE &dh ITEM_KEY &eh');


Declare
cursor po_hdr is
select 
  POH.PO_HEADER_ID                PO_HEADER_ID,         
  POH.SEGMENT1                    PO_NUM,
  POH.ACCEPTANCE_REQUIRED_FLAG    ACCEPT_REQD,
  POH.BILL_TO_LOCATION_ID         BILL_TO,
  POH.SHIP_TO_LOCATION_ID         SHIP_TO,
  POH.CLOSED_CODE                 CLS_STAT,
  POH.CONFIRMING_ORDER_FLAG       CONF_ORD,
  POH.CURRENCY_CODE               CURR,
  POH.ENABLED_FLAG                ENABLED,
  POH.FROZEN_FLAG                 FROZEN,                   
  POH.SUMMARY_FLAG                SUMM,               
  POH.TYPE_LOOKUP_CODE            TYPE,
  POH.VENDOR_CONTACT_ID           VEND_CONT,
  POH.VENDOR_ID                   VEND_ID,
  POH.VENDOR_SITE_ID              VEND_SITE,   
  POH.WF_ITEM_TYPE                ITEM_TYPE,
  POH.WF_ITEM_KEY                 ITEM_KEY
from 
    PO_HEADERS           POH,
    OE_DROP_SHIP_SOURCES SRC,
    OE_ORDER_LINES       LIN
where 
     SRC.LINE_ID                   = LIN.LINE_ID
and  SRC.PO_HEADER_ID              = POH.PO_HEADER_ID
and  LIN.HEADER_ID                 = nvl('&header_id_selected',:v_header_id)
and  NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID)
UNION ALL
select distinct     /* ATO BUY ITEM */
  POH.PO_HEADER_ID                PO_HEADER_ID,
  POH.SEGMENT1                    PO_NUM,
  POH.ACCEPTANCE_REQUIRED_FLAG    ACCEPT_REQD,
  POH.BILL_TO_LOCATION_ID         BILL_TO,
  POH.SHIP_TO_LOCATION_ID         SHIP_TO,
  POH.CLOSED_CODE                 CLS_STAT,
  POH.CONFIRMING_ORDER_FLAG       CONF_ORD,
  POH.CURRENCY_CODE               CURR,
  POH.ENABLED_FLAG                ENABLED,
  POH.FROZEN_FLAG                 FROZEN,
  POH.SUMMARY_FLAG                SUMM,
  POH.TYPE_LOOKUP_CODE            TYPE,
  POH.VENDOR_CONTACT_ID           VEND_CONT,
  POH.VENDOR_ID                   VEND_ID,
  POH.VENDOR_SITE_ID              VEND_SITE,
  POH.WF_ITEM_TYPE                ITEM_TYPE,
  POH.WF_ITEM_KEY                 ITEM_KEY
from
    PO_HEADERS           POH,
    MTL_RESERVATIONS     RES
where
     :sales_ord_id                 = RES.DEMAND_SOURCE_HEADER_ID
and  RES.DEMAND_SOURCE_TYPE_ID     = 2                         -- SO
and  RES.SUPPLY_SOURCE_TYPE_ID     in (1,13)                   -- PO or INV
and  RES.SUPPLY_SOURCE_HEADER_ID   = POH.PO_HEADER_ID;

begin
 for po in po_hdr
 loop
   utl_file.put_line(handle,'&sld'||n(po.PO_HEADER_ID)||'&d'||n(po.PO_NUM)||'&d');
   utl_file.put_line(handle,n(po.ACCEPT_REQD)||'&d'||n(po.BILL_TO)||'&d');
   utl_file.put_line(handle,n(po.SHIP_TO)||'&d'||n(po.CLS_STAT)||'&d');
   utl_file.put_line(handle,n(po.CONF_ORD)||'&d'||n(po.CURR)||'&d');
   utl_file.put_line(handle,n(po.ENABLED)||'&d'||n(po.FROZEN)||'&d');
   utl_file.put_line(handle,n(po.SUMM)||'&d'||n(po.TYPE)||'&d');
   utl_file.put_line(handle,n(po.VEND_CONT)||'&d'||n(po.VEND_ID)||'&d');
   utl_file.put_line(handle,n(po.VEND_SITE)||'&d'||n(po.ITEM_TYPE)||'&d');
   utl_file.put_line(handle,n(po.ITEM_KEY)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');

UTL_FILE.PUT_LINE(handle,'&f &f PO_LINES (POL) &f');


UTL_FILE.PUT_LINE(handle,'&std &sh PO_HEADER_ID &dh PO_LINE_ID &dh PO_LINE &dh LINE &dh CATEGORY_ID &dh CLS_STAT &dh FIRM &dh ITEM_DESC &dh ');
UTL_FILE.PUT_LINE(handle,'ITEM_ID &dh LINE_TYPE_ID &dh QTY &dh PRICE &eh');

Declare
cursor po_lines is
select
  POL.PO_HEADER_ID                PO_HEADER_ID,
  POL.PO_LINE_ID                  PO_LINE_ID,
  POL.LINE_NUM                    PO_LINE,
  to_char(LIN.line_number) || 
          decode(LIN.shipment_number, null, null, '.' || to_char(LIN.shipment_number))|| 
          decode(LIN.option_number, null, null, '.' || to_char(LIN.option_number)) ||
          decode(LIN.component_number, null, null, 
                 decode(LIN.option_number, null, '.',null)||
                 '.'||to_char(LIN.component_number))||
          decode(LIN.service_number,null,null,
                 decode(LIN.component_number, null, '.' , null) ||
                        decode(LIN.option_number, null, '.', null ) ||
                        '.'|| to_char(LIN.service_number)) LINE,
  POL.CATEGORY_ID                 CATEGORY_ID,
  POL.CLOSED_CODE                 CLS_STAT,
  POL.FIRM_STATUS_LOOKUP_CODE     FIRM,
  POL.ITEM_DESCRIPTION            ITEM_DESC,
  POL.ITEM_ID                     ITEM_ID,                  
  POL.LINE_TYPE_ID                LINE_TYPE_ID,
  POL.QUANTITY                    QTY,
  POL.UNIT_PRICE                  PRICE
from 
    PO_LINES             POL,
    OE_DROP_SHIP_SOURCES SRC,
    OE_ORDER_LINES       LIN
where 
     SRC.LINE_ID                   = LIN.LINE_ID
and  SRC.PO_LINE_ID                = POL.PO_LINE_ID
and  LIN.HEADER_ID                 = nvl('&header_id_selected',:v_header_id)
and  NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID)
order by
     NVL(LIN.TOP_MODEL_LINE_ID,            LIN.LINE_ID),
     NVL(LIN.ATO_LINE_ID,               LIN.LINE_ID),
     NVL(LIN.SORT_ORDER,                '0000'),
     NVL(LIN.LINK_TO_LINE_ID,           LIN.LINE_ID),
     NVL(LIN.SOURCE_DOCUMENT_LINE_ID,   LIN.LINE_ID),
     LIN.LINE_ID;

begin
 for po in po_lines
 loop
   utl_file.put_line(handle,'&sld'||n(po.PO_HEADER_ID)||'&d'||n(po.PO_LINE_ID)||'&d');
   utl_file.put_line(handle,n(po.PO_LINE)||'&d'||n(po.LINE)||'&d');
   utl_file.put_line(handle,n(po.CATEGORY_ID)||'&d'||n(po.CLS_STAT)||'&d');
   utl_file.put_line(handle,n(po.FIRM)||'&d'||n(po.ITEM_DESC)||'&d');
   utl_file.put_line(handle,n(po.ITEM_ID)||'&d'||n(po.LINE_TYPE_ID)||'&d');
   utl_file.put_line(handle,n(po.QTY)||'&d'||n(po.PRICE)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');


UTL_FILE.PUT_LINE(handle,'&f &f WORKFLOW PURCHASE ORDER APPROVAL STATUS &f');

UTL_FILE.PUT_LINE(handle,'&std &sh PO_NUM_IK &dh PROCESS_NAME &dh ACTIVITY_NAME &dh RESULT &dh ACT_STATUS &dh NOTIF_ID &dh ');
UTL_FILE.PUT_LINE(handle,'BEGIN_DATE &dh END_DATE &dh ERROR &eh');

Declare
cursor po_wf_po_apr is
select WFS.item_key               PO_NUM_IK,
       WFA.DISPLAY_NAME           PROCESS_NAME,
       WFA1.DISPLAY_NAME          ACTIVITY_NAME,
       WF_CORE.ACTIVITY_RESULT(WFA1.RESULT_TYPE,WFS.ACTIVITY_RESULT_CODE) RESULT,
       LKP.MEANING                ACT_STATUS,
       WFS.NOTIFICATION_ID        NOTIF_ID,
       to_char(WFS.BEGIN_DATE,'DD-MON-RR_HH24:MI:SS') BEGIN_DATE,
       to_char(WFS.END_DATE,'DD-MON-RR_HH24:MI:SS') END_DATE,
       WFS.ERROR_NAME             ERROR_N
from WF_ITEM_ACTIVITY_STATUSES WFS,
     WF_PROCESS_ACTIVITIES     WFP,
     WF_ACTIVITIES_VL          WFA,
     WF_ACTIVITIES_VL          WFA1,
     WF_LOOKUPS                LKP
where 
     WFS.ITEM_TYPE          = 'POAPPRV'
and  WFS.item_key           in (select wf_item_key
                               from 
                               PO_HEADERS           POH,
                               OE_DROP_SHIP_SOURCES SRC,
                               OE_ORDER_LINES       LIN
                               where 
                               SRC.LINE_ID                   = LIN.LINE_ID
                               and  SRC.PO_HEADER_ID              = POH.PO_HEADER_ID
                               and  LIN.HEADER_ID                 = nvl('&header_id_selected',:v_header_id)
                               and  NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID))                               
and  WFS.PROCESS_ACTIVITY   = WFP.INSTANCE_ID
and  WFP.PROCESS_ITEM_TYPE  = WFA.ITEM_TYPE
and  WFP.PROCESS_NAME       = WFA.NAME
and  WFP.PROCESS_VERSION    = WFA.VERSION
and  WFP.ACTIVITY_ITEM_TYPE = WFA1.ITEM_TYPE
and  WFP.ACTIVITY_NAME      = WFA1.NAME
and  WFA1.VERSION = 
    (select max(VERSION)
     from WF_ACTIVITIES WF2
     where WF2.ITEM_TYPE = WFP.ACTIVITY_ITEM_TYPE
     and   WF2.NAME      = WFP.ACTIVITY_NAME)
and  LKP.LOOKUP_TYPE = 'WFENG_STATUS'
and  LKP.LOOKUP_CODE = WFS.ACTIVITY_STATUS
order by WFS.ITEM_KEY, WFS.BEGIN_DATE, EXECUTION_TIME;

begin
 for po in po_wf_po_apr
 loop
   utl_file.put_line(handle,'&sld'||n(po.PO_NUM_IK)||'&d'||n(po.PROCESS_NAME)||'&d');
   utl_file.put_line(handle,n(po.ACTIVITY_NAME)||'&d'||n(po.RESULT)||'&d');
   utl_file.put_line(handle,n(po.ACT_STATUS)||'&d'||n(po.NOTIF_ID)||'&d');
   utl_file.put_line(handle,n(po.BEGIN_DATE)||'&d'||n(po.END_DATE)||'&d');
   utl_file.put_line(handle,n(po.ERROR_N)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');


UTL_FILE.PUT_LINE(handle,'&f &f WORKFLOW PURCHASE APPROVAL NOTIFICATIONS  &f');
UTL_FILE.PUT_LINE(handle,'&std &sh NOTIF_ID &dh TO_USER &dh ORIG_RECIP &dh RECIP_ROLE &dh MAIL_STAT &dh MESSAGE_NAME &dh STATUS &dh SUBJECT &eh');

Declare
cursor po_wf_not is
select   WFN.NOTIFICATION_ID         NOTIF_ID,
         WFN.TO_USER                 TO_USER,
         WFN.ORIGINAL_RECIPIENT      ORIG_RECIP,
         WFN.RECIPIENT_ROLE          RECIP_ROLE,
         WFN.MAIL_STATUS             MAIL_STAT,
         WFN.MESSAGE_NAME            MESSAGE_NAME,
         WFN.STATUS                  STATUS,
         WFN.SUBJECT                 SUBJECT
from WF_ITEM_ACTIVITY_STATUSES WFS,
     WF_NOTIFICATIONS          WFN
where 
     WFS.ITEM_TYPE          = 'POAPPRV'
and  WFS.item_key           in (select wf_item_key
                               from 
                               PO_HEADERS           POH,
                               OE_DROP_SHIP_SOURCES SRC,
                               OE_ORDER_LINES       LIN
                               where 
                               SRC.LINE_ID                   = LIN.LINE_ID
                               and  SRC.PO_HEADER_ID              = POH.PO_HEADER_ID
                               and  LIN.HEADER_ID                 = nvl('&header_id_selected',:v_header_id)
                               and  NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID))                               
and  WFS.NOTIFICATION_ID is not null
and  WFN.NOTIFICATION_ID = WFS.NOTIFICATION_ID
order by WFS.ITEM_KEY, WFS.BEGIN_DATE, EXECUTION_TIME;

begin
 for po in po_wf_not
 loop
   utl_file.put_line(handle,'&sld'||n(po.NOTIF_ID)||'&d'||n(po.TO_USER)||'&d');
   utl_file.put_line(handle,n(po.ORIG_RECIP)||'&d'||n(po.RECIP_ROLE)||'&d');
   utl_file.put_line(handle,n(po.MAIL_STAT)||'&d'||n(po.MESSAGE_NAME)||'&d');
   utl_file.put_line(handle,n(po.STATUS)||'&d'||n(po.SUBJECT)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');


UTL_FILE.PUT_LINE(handle,'&f &f WORKFLOW PURCHASE APPROVAL ERRORS   &f');
UTL_FILE.PUT_LINE(handle,'&std &sh PROCESS_NAME &dh ACTIVITY_NAME &dh RESULT &dh ACT_STATUS &dh ERROR_NAME &dh ERROR_MESSAGE &dh ERROR_STACK &eh');

Declare
cursor po_hdr_int is
select WFA.DISPLAY_NAME           PROCESS_NAME,
       WFA1.DISPLAY_NAME          ACTIVITY_NAME,
       WF_CORE.ACTIVITY_RESULT(WFA1.RESULT_TYPE,WFS.ACTIVITY_RESULT_CODE) RESULT,
       LKP.MEANING                ACT_STATUS,
       WFS.ERROR_NAME             ERROR_NAME,
       WFS.ERROR_MESSAGE          ERROR_MESSAGE,
       WFS.ERROR_STACK            ERROR_STACK
from WF_ITEM_ACTIVITY_STATUSES WFS,
     WF_PROCESS_ACTIVITIES     WFP,
     WF_ACTIVITIES_VL          WFA,
     WF_ACTIVITIES_VL          WFA1,
     WF_LOOKUPS                LKP
where 
     WFS.ITEM_TYPE          = 'POAPPRV'
and  WFS.item_key           in (select wf_item_key
                               from 
                               PO_HEADERS           POH,
                               OE_DROP_SHIP_SOURCES SRC,
                               OE_ORDER_LINES       LIN
                               where 
                               SRC.LINE_ID                   = LIN.LINE_ID
                               and  SRC.PO_HEADER_ID              = POH.PO_HEADER_ID
                               and  LIN.HEADER_ID                 = nvl('&header_id_selected',:v_header_id)
                               and  NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID))                               
and  WFS.PROCESS_ACTIVITY   = WFP.INSTANCE_ID
and  WFP.PROCESS_ITEM_TYPE  = WFA.ITEM_TYPE
and  WFP.PROCESS_NAME       = WFA.NAME
and  WFP.PROCESS_VERSION    = WFA.VERSION
and  WFP.ACTIVITY_ITEM_TYPE = WFA1.ITEM_TYPE
and  WFP.ACTIVITY_NAME      = WFA1.NAME
and  WFA1.VERSION = 
    (select max(VERSION)
     from WF_ACTIVITIES WF2
     where WF2.ITEM_TYPE = WFP.ACTIVITY_ITEM_TYPE
     and   WF2.NAME      = WFP.ACTIVITY_NAME)
and  LKP.LOOKUP_TYPE = 'WFENG_STATUS'
and  LKP.LOOKUP_CODE = WFS.ACTIVITY_STATUS
and  WFS.ERROR_NAME is not NULL
order by WFS.ITEM_KEY, WFS.BEGIN_DATE, EXECUTION_TIME;

begin
 for po in po_hdr_int
 loop
   utl_file.put_line(handle,'&sld'||n(po.PROCESS_NAME)||'&d'||n(po.ACTIVITY_NAME)||'&d');
   utl_file.put_line(handle,n(po.RESULT)||'&d'||n(po.ACT_STATUS)||'&d');
   utl_file.put_line(handle,n(po.ERROR_NAME)||'&d'||n(po.ERROR_MESSAGE)||'&d');
   utl_file.put_line(handle,n(po.ERROR_STACK)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');

end if; -- prt_po

if substr(UPPER(nvl('&prt_rec','Y')),1,1) = 'Y' then

UTL_FILE.PUT_LINE(handle,'&f &f RCV_SHIPMENT_LINES (SHL) &f');
UTL_FILE.PUT_LINE(handle,'&std &sh SHP_LN_ID &dh SHP_HD_ID &dh SHP_LINE_STATUS &dh RCV_Q &dh SHP_Q &dh DEST_TYPE &dh OE_HD_ID &dh OE_LN_ID &dh ');
UTL_FILE.PUT_LINE(handle,'ITEM_ID &dh TO_ORG &dh SRC_DOC &dh REQUEST_ID &eh');

Declare
cursor rcv_shp_lin is
select  /* DROP SHIPMENTS */  
  SHL.SHIPMENT_LINE_ID                  SHP_LN_ID, 
  SHL.SHIPMENT_HEADER_ID                SHP_HD_ID,    
  SHL.SHIPMENT_LINE_STATUS_CODE         SHP_LINE_STATUS,     
  SHL.QUANTITY_RECEIVED                 RCV_Q,
  SHL.QUANTITY_SHIPPED                  SHP_Q,  
  SHL.DESTINATION_TYPE_CODE             DEST_TYPE,     
  SHL.OE_ORDER_HEADER_ID                OE_HD_ID,     
  SHL.OE_ORDER_LINE_ID                  OE_LN_ID,    
  SHL.ITEM_ID                           ITEM_ID,         
  SHL.SHIP_TO_LOCATION_ID               TO_ORG, 
  SHL.SOURCE_DOCUMENT_CODE              SRC_DOC,     
  SHL.REQUEST_ID                        REQUEST_ID
from
   RCV_SHIPMENT_LINES         SHL,
   OE_DROP_SHIP_SOURCES       SRC,
   OE_ORDER_LINES             LIN
where 
     SRC.LINE_ID                   = LIN.LINE_ID
and  SRC.PO_LINE_ID                = SHL.PO_LINE_ID
and  LIN.HEADER_ID                 = nvl('&header_id_selected',:v_header_id)
and  NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID)
 UNION ALL
select      /* RETURNS */
  SHL.SHIPMENT_LINE_ID                  SHP_LN_ID, 
  SHL.SHIPMENT_HEADER_ID                SHP_HD_ID,    
  SHL.SHIPMENT_LINE_STATUS_CODE         SHP_LINE_STATUS,     
  SHL.QUANTITY_RECEIVED                 RCV_Q,
  SHL.QUANTITY_SHIPPED                  SHP_Q,  
  SHL.DESTINATION_TYPE_CODE             DEST_TYPE,     
  SHL.OE_ORDER_HEADER_ID                OE_HD_ID,     
  SHL.OE_ORDER_LINE_ID                  OE_LN_ID,    
  SHL.ITEM_ID                           ITEM_ID,         
  SHL.SHIP_TO_LOCATION_ID               TO_ORG, 
  SHL.SOURCE_DOCUMENT_CODE              SRC_DOC,     
  SHL.REQUEST_ID                        REQUEST_ID
from
   RCV_SHIPMENT_LINES             SHL,
   OE_ORDER_LINES                 LIN
where 
     SHL.OE_ORDER_LINE_ID          = LIN.LINE_ID
and  LIN.HEADER_ID                 = nvl('&header_id_selected',:v_header_id)
and  NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID)
UNION ALL
select    /* INTERNAL SALES ORDER */ 
  SHL.SHIPMENT_LINE_ID                  SHP_LN_ID, 
  SHL.SHIPMENT_HEADER_ID                SHP_HD_ID,    
  SHL.SHIPMENT_LINE_STATUS_CODE         SHP_LINE_STATUS,     
  SHL.QUANTITY_RECEIVED                 RCV_Q,
  SHL.QUANTITY_SHIPPED                  SHP_Q,  
  SHL.DESTINATION_TYPE_CODE             DEST_TYPE,     
  SHL.OE_ORDER_HEADER_ID                OE_HD_ID,     
  SHL.OE_ORDER_LINE_ID                  OE_LN_ID,    
  SHL.ITEM_ID                           ITEM_ID,         
  SHL.SHIP_TO_LOCATION_ID               TO_ORG, 
  SHL.SOURCE_DOCUMENT_CODE              SRC_DOC,     
  SHL.REQUEST_ID                        REQUEST_ID
from
   RCV_SHIPMENT_LINES         SHL,
   OE_ORDER_LINES             LIN
where 
     LIN.SOURCE_DOCUMENT_LINE_ID       = SHL.REQUISITION_LINE_ID
and  LIN.SOURCE_DOCUMENT_TYPE_ID       = 10                        
and  LIN.HEADER_ID                     = nvl('&header_id_selected',:v_header_id)
and  NVL('&line_id_selected',0)        in (0,LIN.LINE_ID,
                                             LIN.TOP_MODEL_LINE_ID,
                                             LIN.ATO_LINE_ID,
                                             LIN.LINK_TO_LINE_ID,
                                             LIN.SERVICE_REFERENCE_LINE_ID)
UNION ALL
select  /* ATO BUY ITEM */
  SHL.SHIPMENT_LINE_ID                  SHP_LN_ID,
  SHL.SHIPMENT_HEADER_ID                SHP_HD_ID,
  SHL.SHIPMENT_LINE_STATUS_CODE         SHP_LINE_STATUS,
  SHL.QUANTITY_RECEIVED                 RCV_Q,
  SHL.QUANTITY_SHIPPED                  SHP_Q,
  SHL.DESTINATION_TYPE_CODE             DEST_TYPE,
  SHL.OE_ORDER_HEADER_ID                OE_HD_ID,
  SHL.OE_ORDER_LINE_ID                  OE_LN_ID,
  SHL.ITEM_ID                           ITEM_ID,
  SHL.SHIP_TO_LOCATION_ID               TO_ORG,
  SHL.SOURCE_DOCUMENT_CODE              SRC_DOC,
  SHL.REQUEST_ID                        REQUEST_ID
from
   RCV_SHIPMENT_LINES         SHL,
   MTL_RESERVATIONS           RES,
   PO_HEADERS_ALL             POH
where
     :sales_ord_id                 = RES.DEMAND_SOURCE_HEADER_ID
and  RES.DEMAND_SOURCE_LINE_ID     = NVL('&line_id_selected',RES.DEMAND_SOURCE_LINE_ID)
and  RES.DEMAND_SOURCE_TYPE_ID     = 2                         -- SO
and  RES.SUPPLY_SOURCE_TYPE_ID     in (1,13)                   -- PO or INV
and  RES.SUPPLY_SOURCE_HEADER_ID   = POH.PO_HEADER_ID          --
and  POH.PO_HEADER_ID              = SHL.PO_HEADER_ID;

begin
 for rcv in rcv_shp_lin
 loop
   utl_file.put_line(handle,'&sld'||n(rcv.SHP_LN_ID)||'&d'||n(rcv.SHP_HD_ID)||'&d');
   utl_file.put_line(handle,n(rcv.SHP_LINE_STATUS)||'&d'||n(rcv.RCV_Q)||'&d');
   utl_file.put_line(handle,n(rcv.SHP_Q)||'&d'||n(rcv.DEST_TYPE)||'&d');
   utl_file.put_line(handle,n(rcv.OE_HD_ID)||'&d'||n(rcv.OE_LN_ID)||'&d');
   utl_file.put_line(handle,n(rcv.ITEM_ID)||'&d'||n(rcv.TO_ORG)||'&d');
   utl_file.put_line(handle,n(rcv.SRC_DOC)||'&d'||n(rcv.REQUEST_ID)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et ');


UTL_FILE.PUT_LINE(handle,'&f &f RCV_TRANSACTIONS (RCV) &f');

UTL_FILE.PUT_LINE(handle,'&std &sh TRANS_ID &dh PRNT_TRANS_ID &dh LINE &dh TRANS_TYPE &dh TRANS_DATE &dh DEST_TYPE &dh INSPECT_STAT &dh ');
UTL_FILE.PUT_LINE(handle,'INTF_SRC &dh INTF_TRANS_ID &dh LOC_ID &dh MVT_STAT &dh ORG_ID &dh OE_HEAD_ID &dh OE_LINE_ID &dh PO_HEAD_ID &dh ');
UTL_FILE.PUT_LINE(handle,'PO_LINE_ID &dh LINE_LOC_ID &dh UNIT_PRICE &dh UOM &dh QTY &dh REQUEST_ID &dh SHIP_HEAD_ID &dh SHIP_LINE_ID &dh ');
UTL_FILE.PUT_LINE(handle,'SRC_DOC_CODE &dh VEND_ID &dh VEND_SITE_ID &eh');

Declare
cursor rcv_trx is
select /* DROP SHIPMENTS */
  RCV.TRANSACTION_ID              TRANS_ID,
  RCV.PARENT_TRANSACTION_ID       PRNT_TRANS_ID,
  to_char(LIN.line_number) || 
          decode(LIN.shipment_number, null, null, '.' || to_char(LIN.shipment_number))|| 
          decode(LIN.option_number, null, null, '.' || to_char(LIN.option_number)) ||
          decode(LIN.component_number, null, null, 
                 decode(LIN.option_number, null, '.',null)||
                 '.'||to_char(LIN.component_number))||
          decode(LIN.service_number,null,null,
                 decode(LIN.component_number, null, '.' , null) ||
                        decode(LIN.option_number, null, '.', null ) ||
                        '.'|| to_char(LIN.service_number)) LINE,
  RCV.TRANSACTION_TYPE            TRANS_TYPE,
  to_char(RCV.TRANSACTION_DATE,'DD-MON-RR_HH24:MI:SS')  TRANS_DATE,
  RCV.DESTINATION_TYPE_CODE       DEST_TYPE,   
  RCV.INSPECTION_STATUS_CODE      INSPECT_STAT,
  RCV.INTERFACE_SOURCE_CODE       INTF_SRC, 
  RCV.INTERFACE_TRANSACTION_ID    INTF_TRANS_ID, 
  RCV.LOCATION_ID                 LOC_ID,
  RCV.MVT_STAT_STATUS             MVT_STAT,
  RCV.ORGANIZATION_ID             ORG_ID,
  RCV.OE_ORDER_HEADER_ID          OE_HEAD_ID,
  RCV.OE_ORDER_LINE_ID            OE_LINE_ID,
  RCV.PO_HEADER_ID                PO_HEAD_ID,
  RCV.PO_LINE_ID                  PO_LINE_ID,
  RCV.PO_LINE_LOCATION_ID         LINE_LOC_ID,
  RCV.PO_UNIT_PRICE               UNIT_PRICE,
  RCV.PRIMARY_UNIT_OF_MEASURE     UOM,
  RCV.QUANTITY                    QTY,
  RCV.REQUEST_ID                  REQUEST_ID,
  RCV.SHIPMENT_HEADER_ID          SHIP_HEAD_ID,
  RCV.SHIPMENT_LINE_ID            SHIP_LINE_ID,
  RCV.SOURCE_DOCUMENT_CODE        SRC_DOC_CODE, 
  RCV.VENDOR_ID                   VEND_ID,    
  RCV.VENDOR_SITE_ID              VEND_SITE_ID  
from
   RCV_TRANSACTIONS     RCV,
   OE_DROP_SHIP_SOURCES SRC,
   OE_ORDER_LINES       LIN
where 
     SRC.LINE_ID                   = LIN.LINE_ID
and  SRC.PO_LINE_ID                = RCV.PO_LINE_ID
and  LIN.HEADER_ID                 = nvl('&header_id_selected',:v_header_id)
and  NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID)
 UNION ALL
select    /* RETURNS */
  RCV.TRANSACTION_ID              TRANS_ID,
  RCV.PARENT_TRANSACTION_ID       PRNT_TRANS_ID,
  to_char(LIN.line_number) || 
          decode(LIN.shipment_number, null, null, '.' || to_char(LIN.shipment_number))|| 
          decode(LIN.option_number, null, null, '.' || to_char(LIN.option_number)) ||
          decode(LIN.component_number, null, null, 
                 decode(LIN.option_number, null, '.',null)||
                 '.'||to_char(LIN.component_number))||
          decode(LIN.service_number,null,null,
                 decode(LIN.component_number, null, '.' , null) ||
                        decode(LIN.option_number, null, '.', null ) ||
                        '.'|| to_char(LIN.service_number)) LINE,
  RCV.TRANSACTION_TYPE            TRANS_TYPE,
  to_char(RCV.TRANSACTION_DATE,'DD-MON-RR_HH24:MI:SS') TRANS_DATE,         
  RCV.DESTINATION_TYPE_CODE       DEST_TYPE,                    
  RCV.INSPECTION_STATUS_CODE      INSPECT_STAT,   
  RCV.INTERFACE_SOURCE_CODE       INTF_SRC_CODE,  
  RCV.INTERFACE_TRANSACTION_ID    INTF_TRANS_ID, 
  RCV.LOCATION_ID                 LOC_ID,
  RCV.MVT_STAT_STATUS             MVT_STAT,
  RCV.ORGANIZATION_ID             ORG_ID,
  RCV.OE_ORDER_HEADER_ID          OE_HEAD_ID,
  RCV.OE_ORDER_LINE_ID            OE_LINE_ID,
  RCV.PO_HEADER_ID                PO_HEAD_ID,
  RCV.PO_LINE_ID                  PO_LINE_ID,
  RCV.PO_LINE_LOCATION_ID         LINE_LOC_ID,
  RCV.PO_UNIT_PRICE               UNIT_PRICE,
  RCV.PRIMARY_UNIT_OF_MEASURE     UOM,
  RCV.QUANTITY                    QTY,
  RCV.REQUEST_ID                  REQUEST_ID,
  RCV.SHIPMENT_HEADER_ID          SHIP_HEAD_ID,
  RCV.SHIPMENT_LINE_ID            SHIP_LINE_ID,
  RCV.SOURCE_DOCUMENT_CODE        SRC_DOC_CODE,        
  RCV.VENDOR_ID                   VEND_ID,
  RCV.VENDOR_SITE_ID              VEND_SITE_ID
from
   RCV_TRANSACTIONS     RCV,
   OE_ORDER_LINES       LIN
where 
     RCV.OE_ORDER_LINE_ID          = LIN.LINE_ID
and  LIN.HEADER_ID                 = nvl('&header_id_selected',:v_header_id)
and  NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID)
UNION ALL
select      /* INTERNAL SALES ORDERS */
  RCV.TRANSACTION_ID              TRANS_ID,
  RCV.PARENT_TRANSACTION_ID       PRNT_TRANS_ID,
  to_char(LIN.line_number) || 
          decode(LIN.shipment_number, null, null, '.' || to_char(LIN.shipment_number))|| 
          decode(LIN.option_number, null, null, '.' || to_char(LIN.option_number)) ||
          decode(LIN.component_number, null, null, 
                 decode(LIN.option_number, null, '.',null)||
                 '.'||to_char(LIN.component_number))||
          decode(LIN.service_number,null,null,
                 decode(LIN.component_number, null, '.' , null) ||
                        decode(LIN.option_number, null, '.', null ) ||
                        '.'|| to_char(LIN.service_number)) LINE,
  RCV.TRANSACTION_TYPE            TRANS_TYPE,
  to_char(RCV.TRANSACTION_DATE,'DD-MON-RR_HH24:MI:SS') TRANS_DATE,         
  RCV.DESTINATION_TYPE_CODE       DEST_TYPE,                    
  RCV.INSPECTION_STATUS_CODE      INSPECT_STAT,   
  RCV.INTERFACE_SOURCE_CODE       INTF_SRC_CODE,  
  RCV.INTERFACE_TRANSACTION_ID    INTF_TRANS_ID, 
  RCV.LOCATION_ID                 LOC_ID,
  RCV.MVT_STAT_STATUS             MVT_STAT,
  RCV.ORGANIZATION_ID             ORG_ID,
  RCV.OE_ORDER_HEADER_ID          OE_HEAD_ID,
  RCV.OE_ORDER_LINE_ID            OE_LINE_ID,
  RCV.PO_HEADER_ID                PO_HEAD_ID,
  RCV.PO_LINE_ID                  PO_LINE_ID,
  RCV.PO_LINE_LOCATION_ID         LINE_LOC_ID,
  RCV.PO_UNIT_PRICE               UNIT_PRICE,
  RCV.PRIMARY_UNIT_OF_MEASURE     UOM,
  RCV.QUANTITY                    QTY,
  RCV.REQUEST_ID                  REQUEST_ID,
  RCV.SHIPMENT_HEADER_ID          SHIP_HEAD_ID,
  RCV.SHIPMENT_LINE_ID            SHIP_LINE_ID,
  RCV.SOURCE_DOCUMENT_CODE        SRC_DOC_CODE,        
  RCV.VENDOR_ID                   VEND_ID,
  RCV.VENDOR_SITE_ID              VEND_SITE_ID
from
   RCV_TRANSACTIONS     RCV,
   OE_ORDER_LINES       LIN
where 
     LIN.SOURCE_DOCUMENT_LINE_ID       = RCV.REQUISITION_LINE_ID
and  LIN.SOURCE_DOCUMENT_TYPE_ID       = 10                        
and  LIN.HEADER_ID                 = nvl('&header_id_selected',:v_header_id)
and  NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID)
UNION ALL
select /* ATO BUY ITEM */
  RCV.TRANSACTION_ID              TRANS_ID,
  RCV.PARENT_TRANSACTION_ID       PRNT_TRANS_ID, 
  to_char(RES.DEMAND_SOURCE_LINE_ID) LINE, 
  RCV.TRANSACTION_TYPE            TRANS_TYPE,
  to_char(RCV.TRANSACTION_DATE,'DD-MON-RR_HH24:MI:SS') TRANS_DATE,         
  RCV.DESTINATION_TYPE_CODE       DEST_TYPE,
  RCV.INSPECTION_STATUS_CODE      INSPECT_STAT,
  RCV.INTERFACE_SOURCE_CODE       INTF_SRC_CODE,
  RCV.INTERFACE_TRANSACTION_ID    INTF_TRANS_ID,
  RCV.LOCATION_ID                 LOC_ID,
  RCV.MVT_STAT_STATUS             MVT_STAT,
  RCV.ORGANIZATION_ID             ORG_ID,
  RCV.OE_ORDER_HEADER_ID          OE_HEAD_ID,
  RCV.OE_ORDER_LINE_ID            OE_LINE_ID,
  RCV.PO_HEADER_ID                PO_HEAD_ID,
  RCV.PO_LINE_ID                  PO_LINE_ID,
  RCV.PO_LINE_LOCATION_ID         LINE_LOC_ID,
  RCV.PO_UNIT_PRICE               UNIT_PRICE,
  RCV.PRIMARY_UNIT_OF_MEASURE     UOM,
  RCV.QUANTITY                    QTY,
  RCV.REQUEST_ID                  REQUEST_ID,
  RCV.SHIPMENT_HEADER_ID          SHIP_HEAD_ID,
  RCV.SHIPMENT_LINE_ID            SHIP_LINE_ID,
  RCV.SOURCE_DOCUMENT_CODE        SRC_DOC_CODE,
  RCV.VENDOR_ID                   VEND_ID,
  RCV.VENDOR_SITE_ID              VEND_SITE_ID
from
   RCV_TRANSACTIONS           RCV,
   MTL_RESERVATIONS           RES,
   PO_HEADERS_ALL             POH
where
     :sales_ord_id                 = RES.DEMAND_SOURCE_HEADER_ID
and  RES.DEMAND_SOURCE_LINE_ID     = NVL('&line_id_selected',RES.DEMAND_SOURCE_LINE_ID)
and  RES.DEMAND_SOURCE_TYPE_ID     = 2                         -- SO
and  RES.SUPPLY_SOURCE_TYPE_ID     in (1,13)                   -- PO or INV
and  RES.SUPPLY_SOURCE_HEADER_ID   = POH.PO_HEADER_ID          --
and  POH.PO_HEADER_ID              = RCV.PO_HEADER_ID;

begin
 for rcv in rcv_trx
 loop
   utl_file.put_line(handle,'&sld'||n(rcv.TRANS_ID)||'&d'||n(rcv.PRNT_TRANS_ID)||'&d');
   utl_file.put_line(handle,n(rcv.LINE)||'&d'||n(rcv.TRANS_TYPE)||'&d');
   utl_file.put_line(handle,n(rcv.TRANS_DATE)||'&d'||n(rcv.DEST_TYPE)||'&d');
   utl_file.put_line(handle,n(rcv.INSPECT_STAT)||'&d'||n(rcv.INTF_SRC)||'&d');
   utl_file.put_line(handle,n(rcv.INTF_TRANS_ID)||'&d'||n(rcv.LOC_ID)||'&d');
   utl_file.put_line(handle,n(rcv.MVT_STAT)||'&d'||n(rcv.ORG_ID)||'&d');
   utl_file.put_line(handle,n(rcv.OE_HEAD_ID)||'&d'||n(rcv.OE_LINE_ID)||'&d');
   utl_file.put_line(handle,n(rcv.PO_HEAD_ID)||'&d'||n(rcv.PO_LINE_ID)||'&d');
   utl_file.put_line(handle,n(rcv.LINE_LOC_ID)||'&d'||n(rcv.UNIT_PRICE)||'&d');
   utl_file.put_line(handle,n(rcv.UOM)||'&d'||n(rcv.QTY)||'&d');
   utl_file.put_line(handle,n(rcv.REQUEST_ID)||'&d'||n(rcv.SHIP_HEAD_ID)||'&d');
   utl_file.put_line(handle,n(rcv.SHIP_LINE_ID)||'&d'||n(rcv.SRC_DOC_CODE)||'&d');
   utl_file.put_line(handle,n(rcv.VEND_ID)||'&d'||n(rcv.VEND_SITE_ID)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');


UTL_FILE.PUT_LINE(handle,'&f &f RCV_TRANSACTIONS_INTERFACE (RTI) &f');
UTL_FILE.PUT_LINE(handle,'&std &sh INTF_TRNS_ID &dh PROC_MODE &dh PROC_STAT &dh TRNS_STAT &dh TRNS_DATE &dh TRNS_TYPE &dh DEST_TYPE &dh INSP_STAT &dh');
UTL_FILE.PUT_LINE(handle,'INSP_SRC &dh OE_HEAD_ID &dh OE_LINE_ID &dh ITEM_ID &dh QTY &dh PRY_Q &dh PO_UNIT_PRC &dh SUB &dh TO_ORG_ID &dh RCPT_SRC &dh');
UTL_FILE.PUT_LINE(handle,'PRNT_TRN_ID &dh SHP_HEAD_ID &dh SHP_LINE_ID &dh SRC_DOC &dh REQUEST_ID &eh');

Declare
cursor rcv_trx_int is
select   /* DROP SHIPMENTS */
  RTI.INTERFACE_TRANSACTION_ID          INTF_TRNS_ID,
  RTI.PROCESSING_MODE_CODE              PROC_MODE,     
  RTI.PROCESSING_STATUS_CODE            PROC_STAT,     
  RTI.TRANSACTION_STATUS_CODE           TRNS_STAT,      
  to_char(RTI.TRANSACTION_DATE,'DD-MON-RR_HH24:MI:SS')  TRNS_DATE,     
  RTI.TRANSACTION_TYPE                  TRNS_TYPE,   
  RTI.DESTINATION_TYPE_CODE             DEST_TYPE,     
  RTI.INSPECTION_STATUS_CODE            INSP_STAT,     
  RTI.INTERFACE_SOURCE_CODE             INSP_SRC,     
  RTI.OE_ORDER_HEADER_ID                OE_HEAD_ID,      
  RTI.OE_ORDER_LINE_ID                  OE_LINE_ID,    
  RTI.ITEM_ID                           ITEM_ID,         
  RTI.QUANTITY                          QTY,        
  RTI.PRIMARY_QUANTITY                  PRY_Q,   
  RTI.PO_UNIT_PRICE                     PO_UNIT_PRC,   
  RTI.SUBINVENTORY                      SUB,     
  RTI.TO_ORGANIZATION_ID                TO_ORG_ID, 
  RTI.RECEIPT_SOURCE_CODE               RCPT_SRC,    
  RTI.PARENT_TRANSACTION_ID             PRNT_TRN_ID,  
  RTI.SHIPMENT_HEADER_ID                SHP_HEAD_ID,     
  RTI.SHIPMENT_LINE_ID                  SHP_LINE_ID,     
  RTI.SOURCE_DOCUMENT_CODE              SRC_DOC,     
  RTI.PROCESSING_REQUEST_ID             REQUEST_ID
from
   RCV_TRANSACTIONS_INTERFACE RTI,
   OE_DROP_SHIP_SOURCES       SRC,
   OE_ORDER_LINES             LIN
where 
     SRC.LINE_ID                   = LIN.LINE_ID
and  SRC.PO_LINE_ID                = RTI.PO_LINE_ID
and  LIN.HEADER_ID                 = nvl('&header_id_selected',:v_header_id)
and  NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID)
 UNION ALL
select      /* RETURNS */ 
  RTI.INTERFACE_TRANSACTION_ID          INTF_TRNS_ID,
  RTI.PROCESSING_MODE_CODE              PROC_MODE,     
  RTI.PROCESSING_STATUS_CODE            PROC_STAT,     
  RTI.TRANSACTION_STATUS_CODE           TRNS_STAT,      
  to_char(RTI.TRANSACTION_DATE,'DD-MON-RR_HH24:MI:SS') TRNS_DATE,     
  RTI.TRANSACTION_TYPE                  TRNS_TYPE,   
  RTI.DESTINATION_TYPE_CODE             DEST_TYPE,     
  RTI.INSPECTION_STATUS_CODE            INSP_STAT,     
  RTI.INTERFACE_SOURCE_CODE             INSP_SRC,     
  RTI.OE_ORDER_HEADER_ID                OE_HEAD_ID,      
  RTI.OE_ORDER_LINE_ID                  OE_LINE_ID,    
  RTI.ITEM_ID                           ITEM_ID,         
  RTI.QUANTITY                          QTY,        
  RTI.PRIMARY_QUANTITY                  PRY_Q,   
  RTI.PO_UNIT_PRICE                     PO_UNIT_PRC,   
  RTI.SUBINVENTORY                      SUB,     
  RTI.TO_ORGANIZATION_ID                TO_ORG_ID, 
  RTI.RECEIPT_SOURCE_CODE               RCPT_SRC,    
  RTI.PARENT_TRANSACTION_ID             PRNT_TRN_ID,  
  RTI.SHIPMENT_HEADER_ID                SHP_HEAD_ID,     
  RTI.SHIPMENT_LINE_ID                  SHP_LINE_ID,     
  RTI.SOURCE_DOCUMENT_CODE              SRC_DOC,     
  RTI.PROCESSING_REQUEST_ID             REQUEST_ID
from
   RCV_TRANSACTIONS_INTERFACE     RTI,
   OE_ORDER_LINES                 LIN
where 
     RTI.OE_ORDER_LINE_ID          = LIN.LINE_ID
and  LIN.HEADER_ID                 = nvl('&header_id_selected',:v_header_id)
and  NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID)
UNION ALL
select    /* INTERNAL SALES ORDER  */ 
  RTI.INTERFACE_TRANSACTION_ID          INTF_TRNS_ID,
  RTI.PROCESSING_MODE_CODE              PROC_MODE,     
  RTI.PROCESSING_STATUS_CODE            PROC_STAT,     
  RTI.TRANSACTION_STATUS_CODE           TRNS_STAT,      
  to_char(RTI.TRANSACTION_DATE,'DD-MON-RR_HH24:MI:SS')  TRNS_DATE,     
  RTI.TRANSACTION_TYPE                  TRNS_TYPE,   
  RTI.DESTINATION_TYPE_CODE             DEST_TYPE,     
  RTI.INSPECTION_STATUS_CODE            INSP_STAT,     
  RTI.INTERFACE_SOURCE_CODE             INSP_SRC,     
  RTI.OE_ORDER_HEADER_ID                OE_HEAD_ID,      
  RTI.OE_ORDER_LINE_ID                  OE_LINE_ID,    
  RTI.ITEM_ID                           ITEM_ID,         
  RTI.QUANTITY                          QTY,        
  RTI.PRIMARY_QUANTITY                  PRY_Q,   
  RTI.PO_UNIT_PRICE                     PO_UNIT_PRC,   
  RTI.SUBINVENTORY                      SUB,     
  RTI.TO_ORGANIZATION_ID                TO_ORG_ID, 
  RTI.RECEIPT_SOURCE_CODE               RCPT_SRC,    
  RTI.PARENT_TRANSACTION_ID             PRNT_TRN_ID,  
  RTI.SHIPMENT_HEADER_ID                SHP_HEAD_ID,     
  RTI.SHIPMENT_LINE_ID                  SHP_LINE_ID,     
  RTI.SOURCE_DOCUMENT_CODE              SRC_DOC,     
  RTI.PROCESSING_REQUEST_ID             REQUEST_ID
from
   RCV_TRANSACTIONS_INTERFACE     RTI,
   OE_ORDER_LINES                 LIN
where 
     LIN.SOURCE_DOCUMENT_LINE_ID       = RTI.REQUISITION_LINE_ID
and  LIN.SOURCE_DOCUMENT_TYPE_ID       = 10                        
and  LIN.HEADER_ID                 = nvl('&header_id_selected',:v_header_id)
and  NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID);

begin
 for rcv in rcv_trx_int
 loop
   utl_file.put_line(handle,'&sld'||n(rcv.INTF_TRNS_ID)||'&d'||n(rcv.PROC_MODE)||'&d'||n(rcv.PROC_STAT)||'&d');
   utl_file.put_line(handle,n(rcv.TRNS_STAT)||'&d'||n(rcv.TRNS_DATE)||'&d'||n(rcv.TRNS_TYPE)||'&d');
   utl_file.put_line(handle,n(rcv.DEST_TYPE)||'&d'||n(rcv.INSP_STAT)||'&d'||n(rcv.INSP_SRC)||'&d');
   utl_file.put_line(handle,n(rcv.OE_HEAD_ID)||'&d'||n(rcv.OE_LINE_ID)||'&d'||n(rcv.ITEM_ID)||'&d');
   utl_file.put_line(handle,n(rcv.QTY)||'&d'||n(rcv.PRY_Q)||'&d'||n(rcv.PO_UNIT_PRC)||'&d');
   utl_file.put_line(handle,n(rcv.SUB)||'&d'||n(rcv.TO_ORG_ID)||'&d'||n(rcv.RCPT_SRC)||'&d');
   utl_file.put_line(handle,n(rcv.PRNT_TRN_ID)||'&d'||n(rcv.SHP_HEAD_ID)||'&d'||n(rcv.SHP_LINE_ID)||'&d');
   utl_file.put_line(handle,n(rcv.SRC_DOC)||'&d'||n(rcv.REQUEST_ID)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');

end if; -- prt_rec

if substr(UPPER(nvl('&prt_po','Y')),1,1) = 'Y' then

UTL_FILE.PUT_LINE(handle,'&f &f MTL_SUPPLY (SUP) &f');
UTL_FILE.PUT_LINE(handle,'&std &sh SUP_TYPE &dh REQ_HEAD_ID &dh REQ_LINE_ID &dh PO_HEAD_ID &dh PO_REL_ID &dh PO_LINE_ID &dh PO_LINE_LOC_ID &dh ');
UTL_FILE.PUT_LINE(handle,'PO_DIST_ID &dh SHP_HEAD_ID &dh SHP_LINE_ID &dh RCV_TRANS_ID &dh ITEM_ID &dh QTY &dh RECPT_DT &dh NEED_BY_DT &dh ');
UTL_FILE.PUT_LINE(handle,'DEST_TYPE &dh FROM_ORG &dh FROM_SUB &dh TO_ORG &dh TO_SUB &dh INTRNS_OWN_ORG &eh');

Declare
cursor mtl_supply is
select  /* DROP SHIPMENTS */
  SUP.SUPPLY_TYPE_CODE               SUP_TYPE,
  SUP.REQ_HEADER_ID                  REQ_HEAD_ID,
  SUP.REQ_LINE_ID                    REQ_LINE_ID,
  SUP.PO_HEADER_ID                   PO_HEAD_ID,
  SUP.PO_RELEASE_ID                  PO_REL_ID,
  SUP.PO_LINE_ID                     PO_LINE_ID,
  SUP.PO_LINE_LOCATION_ID            PO_LINE_LOC_ID,
  SUP.PO_DISTRIBUTION_ID             PO_DIST_ID,
  SUP.SHIPMENT_HEADER_ID             SHP_HEAD_ID,
  SUP.SHIPMENT_LINE_ID               SHP_LINE_ID,
  SUP.RCV_TRANSACTION_ID             RCV_TRANS_ID,
  SUP.ITEM_ID                        ITEM_ID,
  SUP.QUANTITY                       QTY,
  to_char(SUP.RECEIPT_DATE,'DD-MON-RR_HH24:MI:SS')  RECPT_DT,
  to_char(SUP.NEED_BY_DATE,'DD-MON-RR_HH24:MI:SS')  NEED_BY_DT,
  SUP.DESTINATION_TYPE_CODE          DEST_TYPE,
  SUP.FROM_ORGANIZATION_ID           FROM_ORG,
  SUP.FROM_SUBINVENTORY              FROM_SUB,
  SUP.TO_ORGANIZATION_ID             TO_ORG,
  SUP.TO_SUBINVENTORY                TO_SUB,
  SUP.INTRANSIT_OWNING_ORG_ID        INTRNS_OWN_ORG
from
   MTL_SUPPLY           SUP,
   OE_DROP_SHIP_SOURCES SRC,
   OE_ORDER_LINES       LIN
where 
     SRC.LINE_ID                   = LIN.LINE_ID
and  SRC.PO_LINE_ID                = SUP.PO_LINE_ID
and  LIN.HEADER_ID                 = nvl('&header_id_selected',:v_header_id)
and  NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID)
UNION ALL
select         /* INTERNAL SALES ORDER */ 
  SUP.SUPPLY_TYPE_CODE               SUP_TYPE,
  --SUP.SUPPLY_SOURCE_ID               SUP_SRC,
  SUP.REQ_HEADER_ID                  REQ_HEAD_ID,
  SUP.REQ_LINE_ID                    REQ_LINE_ID,
  SUP.PO_HEADER_ID                   PO_HEAD_ID,
  SUP.PO_RELEASE_ID                  PO_REL_ID,
  SUP.PO_LINE_ID                     PO_LINE_ID,
  SUP.PO_LINE_LOCATION_ID            PO_LINE_LOC_ID,
  SUP.PO_DISTRIBUTION_ID             PO_DIST_ID,
  SUP.SHIPMENT_HEADER_ID             SHP_HEAD_ID,
  SUP.SHIPMENT_LINE_ID               SHP_LINE_ID,
  SUP.RCV_TRANSACTION_ID             RCV_TRANS_ID,
  SUP.ITEM_ID                        ITEM_ID,
  SUP.QUANTITY                       QTY,
  to_char(SUP.RECEIPT_DATE,'DD-MON-RR_HH24:MI:SS') RECPT_DT,
  to_char(SUP.NEED_BY_DATE,'DD-MON-RR_HH24:MI:SS') NEED_BY_DT,
  SUP.DESTINATION_TYPE_CODE          DEST_TYPE,
  SUP.FROM_ORGANIZATION_ID           FROM_ORG,
  SUP.FROM_SUBINVENTORY              FROM_SUB,
  SUP.TO_ORGANIZATION_ID             TO_ORG,
  SUP.TO_SUBINVENTORY                TO_SUB,
  SUP.INTRANSIT_OWNING_ORG_ID        INTRNS_OWN_ORG
from
   MTL_SUPPLY           SUP,
   OE_ORDER_LINES       LIN
where 
     LIN.SOURCE_DOCUMENT_LINE_ID       = SUP.REQ_LINE_ID
and  LIN.SOURCE_DOCUMENT_TYPE_ID       = 10                        
and  LIN.HEADER_ID                 = nvl('&header_id_selected',:v_header_id)
and  NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID);

begin
 for mtl in mtl_supply
 loop
   utl_file.put_line(handle,'&sld'||n(mtl.SUP_TYPE)||'&d'||n(mtl.REQ_HEAD_ID)||'&d'||n(mtl.REQ_LINE_ID)||'&d');
   utl_file.put_line(handle,n(mtl.PO_HEAD_ID)||'&d'||n(mtl.PO_REL_ID)||'&d'||n(mtl.PO_LINE_ID)||'&d');
   utl_file.put_line(handle,n(mtl.PO_LINE_LOC_ID)||'&d'||n(mtl.PO_DIST_ID)||'&d'||n(mtl.SHP_HEAD_ID)||'&d');
   utl_file.put_line(handle,n(mtl.SHP_LINE_ID)||'&d'||n(mtl.RCV_TRANS_ID)||'&d'||n(mtl.ITEM_ID)||'&d');
   utl_file.put_line(handle,n(mtl.QTY)||'&d'||n(mtl.RECPT_DT)||'&d'||n(mtl.NEED_BY_DT)||'&d');
   utl_file.put_line(handle,n(mtl.DEST_TYPE)||'&d'||n(mtl.FROM_ORG)||'&d'||n(mtl.FROM_SUB)||'&d');
   utl_file.put_line(handle,n(mtl.TO_ORG)||'&d'||n(mtl.TO_SUB)||'&d'||n(mtl.INTRNS_OWN_ORG)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');

end if; -- prt_po

if substr(UPPER(nvl('&prt_inv','Y')),1,1) = 'Y' then

UTL_FILE.PUT_LINE(handle,'&f &f <a NAME="MTL_RESERVATIONS">MTL_RESERVATIONS (RES) </a> <a HREF="#MR">Column Definitions</a> &f');
UTL_FILE.PUT_LINE(handle,'&std &sh RESERV_ID &dh SHIP_READY &dh DS_HEAD_ID &dh DS_LINE_ID &dh DS_DELIV &dh LINE &dh ITEM_ID &dh ITEM &dh ');
UTL_FILE.PUT_LINE(handle,'RES_Q &dh DET_Q &dh UOM &dh REQUIRD_D &dh DS_TYPE &dh WH_ID &dh SUBINV &dh LOT &dh REV &dh LOC_ID &dh SERIAL_NUM &dh ');
UTL_FILE.PUT_LINE(handle,'SS_TYPE_ID &dh WIP_ID &dh JOB_NAME &dh JOB_STAT &dh SS_HEADER_ID &dh SS_SOURCE_LINE_DET &dh SS_SOURCE_LINE &dh ');
UTL_FILE.PUT_LINE(handle,'SECONDARY RES_QTY &dh CALC_SEC RES_QTY &dh SECONDARY UOM &dh SECONDARY DET_QTY &eh');

Declare
cursor mtl_res is
select 
     RES.RESERVATION_ID               RESERV_ID,
     decode(RES.SHIP_READY_FLAG,
        1,'1=Released',
        2,'2=Submitted',
        to_char(RES.SHIP_READY_FLAG)) SHIP_READY, 
     RES.DEMAND_SOURCE_HEADER_ID      DS_HEAD_ID,
     RES.DEMAND_SOURCE_LINE_ID        DS_LINE_ID,
     RES.DEMAND_SOURCE_DELIVERY       DS_DELIV,
     to_char(LIN.line_number) || 
          decode(LIN.shipment_number, null, null, '.' || to_char(LIN.shipment_number))|| 
          decode(LIN.option_number, null, null, '.' || to_char(LIN.option_number)) ||
          decode(LIN.component_number, null, null, 
                 decode(LIN.option_number, null, '.',null)||
                 '.'||to_char(LIN.component_number))||
          decode(LIN.service_number,null,null,
                 decode(LIN.component_number, null, '.' , null) ||
                        decode(LIN.option_number, null, '.', null ) ||
                        '.'|| to_char(LIN.service_number)) LINE,
     RES.INVENTORY_ITEM_ID            ITEM_ID,
     ITM.SEGMENT1                     ITEM,
     RES.PRIMARY_RESERVATION_QUANTITY RES_Q,
     RES.DETAILED_QUANTITY            DET_Q,
     RES.PRIMARY_UOM_CODE             UOM,
     To_char(RES.REQUIREMENT_DATE,'DD-MON-RR_HH24:MI:SS') REQUIRD_D,
     RES.DEMAND_SOURCE_TYPE_ID        DS_TYPE,
     RES.ORGANIZATION_ID              WH_ID,
     RES.SUBINVENTORY_CODE            SUBINV,
     RES.LOT_NUMBER                   LOT,
     RES.REVISION                     REV,
     RES.LOCATOR_ID                   LOC_ID,
     RES.SERIAL_NUMBER                SERIAL_NUM,
     decode(RES.SUPPLY_SOURCE_TYPE_ID,
            5,'5=WIP DJ',
            RES.SUPPLY_SOURCE_TYPE_ID) SS_TYPE_ID,
     WIP.WIP_ENTITY_ID                WIP_ID,
     WIP.WIP_ENTITY_NAME              JOB_NAME,
     JOB.STATUS_TYPE_DISP             JOB_STAT,
     RES.SUPPLY_SOURCE_HEADER_ID      SS_HEADER_ID,      
     RES.SUPPLY_SOURCE_LINE_DETAIL    SS_SOURCE_LINE_DET,
     RES.SUPPLY_SOURCE_LINE_ID        SS_SOURCE_LINE,
     res.secondary_reservation_quantity sec_res_q,
     inv_convert.inv_um_convert(
                            res.inventory_item_id,
                            res.lot_number,
                            res.organization_id,
                            5,
                            res.primary_reservation_quantity,
                            res.primary_uom_code,
                            res.secondary_uom_code,
                            null,
                            null) calc_sec_res_q,
     res.secondary_uom_code uom2,
     res.secondary_detailed_quantity  sec_dtl_q
from
     MTL_RESERVATIONS              RES,
     OE_ORDER_LINES                LIN,
     MTL_SYSTEM_ITEMS              ITM,
     WIP_ENTITIES                  WIP,
     WIP_DISCRETE_JOBS_V           JOB
where
     :sales_ord_id                 = RES.DEMAND_SOURCE_HEADER_ID
and  RES.DEMAND_SOURCE_TYPE_ID     in  (2,8,9,21,22)
and  RES.DEMAND_SOURCE_LINE_ID     = LIN.LINE_ID(+)
and  NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.REFERENCE_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID)
and  RES.ORGANIZATION_ID           = ITM.ORGANIZATION_ID(+)
and  RES.INVENTORY_ITEM_ID         = ITM.INVENTORY_ITEM_ID(+)
and  RES.SUPPLY_SOURCE_HEADER_ID   = WIP.WIP_ENTITY_ID(+)
and  WIP.WIP_ENTITY_ID             = JOB.WIP_ENTITY_ID(+)
order by
     NVL(LIN.TOP_MODEL_LINE_ID,         LIN.LINE_ID),
     NVL(LIN.ATO_LINE_ID,               LIN.LINE_ID),
     NVL(LIN.SORT_ORDER,                '0000'),
     NVL(LIN.LINK_TO_LINE_ID,           LIN.LINE_ID),
     NVL(LIN.SOURCE_DOCUMENT_LINE_ID,   LIN.LINE_ID),
     LIN.LINE_ID,
     RES.RESERVATION_ID; 

begin
 for mtl in mtl_res
 loop
   utl_file.put_line(handle,'&sld'||n(mtl.RESERV_ID)||'&d'||n(mtl.SHIP_READY)||'&d'||n(mtl.DS_HEAD_ID)||'&d');
   utl_file.put_line(handle,n(mtl.DS_LINE_ID)||'&d'||n(mtl.DS_DELIV)||'&d'||n(mtl.LINE)||'&d');
   utl_file.put_line(handle,n(mtl.ITEM_ID)||'&d'||n(mtl.ITEM)||'&d'||n(mtl.RES_Q)||'&d');
   utl_file.put_line(handle,n(mtl.DET_Q)||'&d'||n(mtl.UOM)||'&d'||n(mtl.REQUIRD_D)||'&d');
   utl_file.put_line(handle,n(mtl.DS_TYPE)||'&d'||n(mtl.WH_ID)||'&d'||n(mtl.SUBINV)||'&d');
   utl_file.put_line(handle,n(mtl.LOT)||'&d'||n(mtl.REV)||'&d'||n(mtl.LOC_ID)||'&d');
   utl_file.put_line(handle,n(mtl.SERIAL_NUM)||'&d'||n(mtl.SS_TYPE_ID)||'&d'||n(mtl.WIP_ID)||'&d');
   utl_file.put_line(handle,n(mtl.JOB_NAME)||'&d'||n(mtl.JOB_STAT)||'&d'||n(mtl.SS_HEADER_ID)||'&d');
   utl_file.put_line(handle,n(mtl.SS_SOURCE_LINE_DET)||'&d'||n(mtl.SS_SOURCE_LINE)||'&d');
   utl_file.put_line(handle,n(mtl.sec_res_q)||'&d'||n(mtl.calc_sec_res_q)||'&d');
   utl_file.put_line(handle,n(mtl.uom2)||'&d'||n(mtl.sec_dtl_q)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');

end if; -- prt_inv

if substr(UPPER(nvl('&prt_wip','Y')),1,1) = 'Y' then

UTL_FILE.PUT_LINE(handle,'&f &f WIP_JOB_SCHEDULE_INTERFACE (WJS) &f');
UTL_FILE.PUT_LINE(handle,'&std &sh INTERF_ID &dh SRC_LINE_ID &dh LINE &dh REQUEST_ID &dh GROUP_ID &dh SRC_CODE &dh PHS &dh STAT_TY &dh ');
UTL_FILE.PUT_LINE(handle,'STAT &dh ORG &dh LOAD &dh ITEM_ID &dh WIP_SUP_TY &dh START_QTY &eh');

Declare
cursor wip_sch_int is
select 
     WJS.INTERFACE_ID                   INTERF_ID,
     WJS.SOURCE_LINE_ID                 SRC_LINE_ID,
     to_char(LIN.line_number) ||
          decode(LIN.shipment_number, null, null, '.' || to_char(LIN.shipment_number))||
          decode(LIN.option_number, null, null, '.' || to_char(LIN.option_number)) ||
          decode(LIN.component_number, null, null,
                 decode(LIN.option_number, null, '.',null)||
                 '.'||to_char(LIN.component_number))||
          decode(LIN.service_number,null,null,
                 decode(LIN.component_number, null, '.' , null) ||
                        decode(LIN.option_number, null, '.', null ) ||
                        '.'|| to_char(LIN.service_number)) LINE,
     WJS.REQUEST_ID                     REQUEST_ID,
     WJS.GROUP_ID                       GROUP_ID,
     WJS.SOURCE_CODE                    SRC_CODE,
     WJS.PROCESS_PHASE                  PHS,
     WJS.STATUS_TYPE                    STAT_TY,
     WJS.PROCESS_STATUS                 STAT,
     WJS.ORGANIZATION_CODE              ORG,
     WJS.LOAD_TYPE                      LOAD,
     WJS.PRIMARY_ITEM_ID                ITEM_ID,
     WJS.WIP_SUPPLY_TYPE                WIP_SUP_TY,
     WJS.START_QUANTITY                 START_QTY
from WIP_JOB_SCHEDULE_INTERFACE WJS,
     OE_ORDER_LINES_ALL         LIN
where WJS.SOURCE_LINE_ID      =  LIN.LINE_ID
  and LIN.HEADER_ID           = nvl('&header_id_selected',:v_header_id)
  and NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.REFERENCE_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID);

begin
 for wip in wip_sch_int
 loop
   utl_file.put_line(handle,'&sld'||n(wip.INTERF_ID)||'&d'||n(wip.SRC_LINE_ID)||'&d'||n(wip.LINE)||'&d');
   utl_file.put_line(handle,n(wip.REQUEST_ID)||'&d'||n(wip.GROUP_ID)||'&d'||n(wip.SRC_CODE)||'&d');
   utl_file.put_line(handle,n(wip.PHS)||'&d'||n(wip.STAT_TY)||'&d'||n(wip.STAT)||'&d');
   utl_file.put_line(handle,n(wip.ORG)||'&d'||n(wip.LOAD)||'&d'||n(wip.ITEM_ID)||'&d');
   utl_file.put_line(handle,n(wip.WIP_SUP_TY)||'&d'||n(wip.START_QTY)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');


UTL_FILE.PUT_LINE(handle,'&f &f WIP_INTERFACE_ERRORS (WIE) &f');
UTL_FILE.PUT_LINE(handle,'&std &sh INTERF_ID &dh LINE &dh ERROR_TYPE &dh ERROR &eh');

Declare
cursor wip_int_err is
select
     WIE.INTERFACE_ID                 INTERF_ID,
     to_char(LIN.line_number) ||
          decode(LIN.shipment_number, null, null, '.' || to_char(LIN.shipment_number))||
          decode(LIN.option_number, null, null, '.' || to_char(LIN.option_number)) ||
          decode(LIN.component_number, null, null,
                 decode(LIN.option_number, null, '.',null)||
                 '.'||to_char(LIN.component_number))||
          decode(LIN.service_number,null,null,
                 decode(LIN.component_number, null, '.' , null) ||
                        decode(LIN.option_number, null, '.', null ) ||
                        '.'|| to_char(LIN.service_number)) LINE,
     WIE.ERROR_TYPE                   ERROR_TYPE,
     WIE.ERROR                        ERROR_WIE
from WIP_INTERFACE_ERRORS       WIE,
     WIP_JOB_SCHEDULE_INTERFACE WJS,
     OE_ORDER_LINES_ALL         LIN
where WIE.INTERFACE_ID        = WJS.INTERFACE_ID
  and WJS.SOURCE_LINE_ID      =  LIN.LINE_ID
  and LIN.HEADER_ID           = nvl('&header_id_selected',:v_header_id)
  and NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.REFERENCE_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID);

begin
 for wip in wip_int_err
 loop
   utl_file.put_line(handle,'&sld'||n(wip.INTERF_ID)||'&d'||n(wip.LINE)||'&d');
   utl_file.put_line(handle,n(wip.ERROR_TYPE)||'&d'||n(wip.ERROR_WIE)||'&el');
 end loop;
end;
UTL_FILE.PUT_LINE(handle,'&et');


UTL_FILE.PUT_LINE(handle,'&f &f WIP_DISCRETE_JOBS (WIP) &f');
UTL_FILE.PUT_LINE(handle,'&std &sh WIP_ENT_ID &dh JOB_NAME &dh WH_ID &dh REQUEST_ID &dh LINE_ID &dh LINE &dh STATUS &dh PRY_ITEM_ID &dh ');
UTL_FILE.PUT_LINE(handle,'ITEM &dh FIRM &dh JOB_TY &dh WIP_SUP_TYPE &dh SCH_STRT &dh SCH_CMPL &dh RLS_DATE &dh DUE_DATE &dh COMPL_DT &dh ');
UTL_FILE.PUT_LINE(handle,'CLSD_DT &dh STRT_Q &dh COMP_Q &dh SCRP_Q &dh NET_Q &dh LINE_ID &eh');

Declare
cursor wip_d_jobs is
select
     WIP.WIP_ENTITY_ID                  WIP_ENT_ID,
     WIV.WIP_ENTITY_NAME                JOB_NAME,
     WIP.ORGANIZATION_ID                WH_ID,
     WIP.REQUEST_ID                     REQUEST_ID,
     WIP.SOURCE_LINE_ID                 LINE_ID,
     to_char(LIN.line_number) ||
          decode(LIN.shipment_number, null, null, '.' || to_char(LIN.shipment_number))||
          decode(LIN.option_number, null, null, '.' || to_char(LIN.option_number)) ||
          decode(LIN.component_number, null, null,
                 decode(LIN.option_number, null, '.',null)||
                 '.'||to_char(LIN.component_number))||
          decode(LIN.service_number,null,null,
                 decode(LIN.component_number, null, '.' , null) ||
                        decode(LIN.option_number, null, '.', null ) ||
                        '.'|| to_char(LIN.service_number)) LINE,
     WIV.STATUS_TYPE_DISP               STATUS,
     WIP.PRIMARY_ITEM_ID                PRY_ITEM_ID,
     ITM.SEGMENT1                       ITEM,
     WIP.FIRM_PLANNED_FLAG              FIRM,
     WIP.JOB_TYPE                       JOB_TY,
     WIV.WIP_SUPPLY_TYPE_DISP           WIP_SUP_TYPE,
     WIP.SCHEDULED_START_DATE           SCH_STRT,
     WIP.SCHEDULED_COMPLETION_DATE      SCH_CMPL,
     WIP.DATE_RELEASED                  RLS_DATE,
     WIP.DUE_DATE                       DUE_DATE,
     WIP.DATE_COMPLETED                 COMPL_DT,
     WIP.DATE_CLOSED                    CLSD_DT,
     WIP.START_QUANTITY                 STRT_Q,
     WIP.QUANTITY_COMPLETED             COMP_Q,
     WIP.QUANTITY_SCRAPPED              SCRP_Q,
     WIP.NET_QUANTITY                   NET_Q,
     WIP.LINE_ID                        WIP_LINE_ID
from WIP_DISCRETE_JOBS   WIP,
     WIP_DISCRETE_JOBS_V WIV,
     MTL_SYSTEM_ITEMS_B  ITM,
     OE_ORDER_LINES_ALL  LIN
where WIP.WIP_ENTITY_ID       = WIV.WIP_ENTITY_ID
  and WIP.PRIMARY_ITEM_ID     = ITM.INVENTORY_ITEM_ID
  and WIP.ORGANIZATION_ID     = ITM.ORGANIZATION_ID
  and WIP.SOURCE_LINE_ID      =  LIN.LINE_ID
  and LIN.HEADER_ID           = nvl('&header_id_selected',:v_header_id)
  and NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.REFERENCE_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID);

begin
 for wip in wip_d_jobs
 loop
   utl_file.put_line(handle,'&sld'||n(wip.WIP_ENT_ID)||'&d'||n(wip.JOB_NAME)||'&d');
   utl_file.put_line(handle,n(wip.WH_ID)||'&d'||n(wip.REQUEST_ID)||'&d');
   utl_file.put_line(handle,n(wip.LINE_ID)||'&d'||n(wip.LINE)||'&d');
   utl_file.put_line(handle,n(wip.STATUS)||'&d'||n(wip.PRY_ITEM_ID)||'&d');
   utl_file.put_line(handle,n(wip.ITEM)||'&d'||n(wip.FIRM)||'&d');
   utl_file.put_line(handle,n(wip.JOB_TY)||'&d'||n(wip.WIP_SUP_TYPE)||'&d');
   utl_file.put_line(handle,n(wip.SCH_STRT)||'&d'||n(wip.SCH_CMPL)||'&d');
   utl_file.put_line(handle,n(wip.RLS_DATE)||'&d'||n(wip.DUE_DATE)||'&d');
   utl_file.put_line(handle,n(wip.COMPL_DT)||'&d'||n(wip.CLSD_DT)||'&d');
   utl_file.put_line(handle,n(wip.STRT_Q)||'&d'||n(wip.COMP_Q)||'&d');
   utl_file.put_line(handle,n(wip.SCRP_Q)||'&d'||n(wip.NET_Q)||'&d');
   utl_file.put_line(handle,n(wip.WIP_LINE_ID)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');


UTL_FILE.PUT_LINE(handle,'&f &f DISCRETE JOB TRANSACTIONS (DJT) &f');
UTL_FILE.PUT_LINE(handle,'&std &sh JOB_NAME &dh LINE &dh ITEM_ID &dh ITEM &dh QTY &dh MTL_TRNS_ID &dh TRANS_TYPE &dh OP_SEQ &dh SERIAL &eh');

Declare
cursor wip_job_trx is
select
      WIE.wip_entity_name                    JOB_NAME,
      to_char(LIN.line_number) ||
          decode(LIN.shipment_number, null, null, '.' || to_char(LIN.shipment_number))||
          decode(LIN.option_number, null, null, '.' || to_char(LIN.option_number)) ||
          decode(LIN.component_number, null, null,
                 decode(LIN.option_number, null, '.',null)||
                 '.'||to_char(LIN.component_number))||
          decode(LIN.service_number,null,null,
                 decode(LIN.component_number, null, '.' , null) ||
                        decode(LIN.option_number, null, '.', null ) ||
                        '.'|| to_char(LIN.service_number)) LINE,
      TRN.inventory_item_id                  ITEM_ID,
      ITM.segment1                           ITEM,
      TRN.TRANSACTION_QUANTITY               QTY,
      TRN.TRANSACTION_ID                     MTL_TRNS_ID,
      decode(TRN.TRANSACTION_TYPE_ID,
             35, 'WIP Component Issue',
             44, 'WIP Assy Completion',
             TRN.TRANSACTION_TYPE_ID)        TRANS_TYPE,
      TRN.OPERATION_SEQ_NUM                  OP_SEQ,
      UNT.serial_number                      SERIAL
from    wip_discrete_jobs  WIP,
      OE_ORDER_LINES_ALL LIN,
      wip_entities WIE,
      mtl_material_transactions TRN,
      mtl_system_items_b ITM,
      mtl_unit_transactions UNT
where WIE.wip_entity_id              = WIP.wip_entity_id
  and LIN.LINE_ID                    = WIP.source_line_id
  and TRN.transaction_source_id      = WIP.wip_entity_id
  and TRN.transaction_source_type_id = 5
  and TRN.inventory_item_id          = ITM.inventory_item_id
  and TRN.organization_id            = ITM.organization_id
  and TRN.transaction_id             = UNT.transaction_id(+)
  and LIN.HEADER_ID           = nvl('&header_id_selected',:v_header_id)
  and NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.REFERENCE_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID);

begin
 for wip in wip_job_trx
 loop
   utl_file.put_line(handle,'&sld'||n(wip.JOB_NAME)||'&d'||n(wip.LINE)||'&d');
   utl_file.put_line(handle,n(wip.ITEM_ID)||'&d'||n(wip.ITEM)||'&d');
   utl_file.put_line(handle,n(wip.QTY)||'&d'||n(wip.MTL_TRNS_ID)||'&d');
   utl_file.put_line(handle,n(wip.TRANS_TYPE)||'&d'||n(wip.OP_SEQ)||'&d');
   utl_file.put_line(handle,n(wip.SERIAL)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');

end if; -- prt_wip

end if; -- :v_head_only

   UTL_FILE.FCLOSE(handle);
end;
/


DECLARE
  handle UTL_FILE.FILE_TYPE;
  dirname varchar2(1000);
  text    varchar2(1000);

function n(v varchar2) return varchar2 is
begin
  if v is null then
   return '&sp';
   else
   return v;
  end if;
end n;

begin
-- Append to output file
  handle := UTL_FILE.FOPEN('&out_dir','&out_dir/&out_file','A',32000);
  UTL_FILE.PUT_LINE(handle,'&et'); -- in case last one failed

If :v_head_only = 'N' then 

UTL_FILE.PUT_LINE(handle,'&f &f <a NAME="WSH_TRIPS"> WSH_TRIPS (TRP) </a> &f');
UTL_FILE.PUT_LINE(handle,'&std &sh WARNING &dh TRIP_ID &dh NAME &dh PLND &dh STATUS_CODE &dh VEH_NUM &dh CARR_ID &dh SHIP_METH &dh ROUTE_ID &dh VEH_ORG_ID &eh');


Declare
 cursor w_trips is
select distinct 
  TRP.TRIP_ID                      TRIP_ID,
  TRP.NAME                         NAME,
  TRP.PLANNED_FLAG                 PLND,
  TRP.STATUS_CODE                  STATUS_CODE,
  TRP.VEHICLE_NUMBER               VEH_NUM,
  TRP.CARRIER_ID                   CARR_ID,
  TRP.SHIP_METHOD_CODE             SHIP_METH,
  TRP.ROUTE_ID                     ROUTE_ID,
  TRP.VEHICLE_ORGANIZATION_ID      VEH_ORG_ID
FROM 
  WSH_TRIPS                        TRP
where
  TRIP_ID in 
      (select distinct(STP.TRIP_ID)
       FROM
            OE_ORDER_LINES                   LIN,
            WSH_DELIVERY_DETAILS             DET,
            WSH_NEW_DELIVERIES               DEL,
            WSH_DELIVERY_LEGS                LEG,
            WSH_TRIP_STOPS                   STP,
            WSH_DELIVERY_ASSIGNMENTS         ASG
       WHERE
            DEL.DELIVERY_ID                 = ASG.DELIVERY_ID AND
            ASG.DELIVERY_DETAIL_ID          = DET.DELIVERY_DETAIL_ID AND
            DET.SOURCE_LINE_ID              = LIN.LINE_ID AND  
            STP.STOP_ID                     = LEG.PICK_UP_STOP_ID AND
            LEG.DELIVERY_ID                 = DEL.DELIVERY_ID AND 
            LIN.HEADER_ID                   = nvl('&header_id_selected',:v_header_id) AND
            NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.REFERENCE_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID)
       )
order by TRP.TRIP_ID;

type     per_record_typ is RECORD
               (flag    varchar2(1),
                descrip varchar2(200));
type     msg_tab is TABLE of per_record_typ INDEX by binary_integer;
msg      msg_tab;

begin
  :r_error := 0;

-- Initialize error messages even if Do Analysis not selected
-- if substr(UPPER(nvl('&do_analysis','Y')),1,1) = 'Y' then
  for i in 1..30
  loop
    msg(i).flag := '0';
    msg(i).descrip := '';
  end loop;
  msg(1).descrip   := '    1. XX.';
-- end if;

 for tt in w_trips
 loop
   :r_flag := '';

if substr(UPPER(nvl('&do_analysis','Y')),1,1) = 'Y' then
  null;
end if;

   -- Print line to Output file
   utl_file.put_line(handle,'&sld &b <a HREF="#WTERR">'||n(:r_flag)||'</a> &eb &d');
   utl_file.put_line(handle,'<a NAME="T'||tt.TRIP_ID||'">'||n(tt.TRIP_ID)||'</a>'||'&d'||n(tt.NAME)||'&d');
   utl_file.put_line(handle,n(tt.PLND)||'&d'||n(tt.STATUS_CODE)||'&d'||n(tt.VEH_NUM)||'&d'||n(tt.CARR_ID)||'&d');
   utl_file.put_line(handle,n(tt.SHIP_METH)||'&d'||n(tt.ROUTE_ID)||'&d'||n(tt.VEH_ORG_ID)||'&el');

   if :r_flag is not null then
    :r_error := 1;
   end if;
 end loop;
 utl_file.put_line(handle,'&et');

 if :r_error = 1 then
   utl_file.put_line(handle,'&f &b <a NAME="WTERR">Warning List:</a> &eb &f');
   for i in 1..30
   loop
     if msg(i).flag = '1' then
       utl_file.put_line(handle,msg(i).descrip||'&f');
     end if;
   end loop;
 end if;

end;

UTL_FILE.PUT_LINE(handle,'&et');


UTL_FILE.PUT_LINE(handle,'&f &f <a NAME="WSH_TRIP_STOPS">WSH_TRIP_STOPS (STP) </a> &f ');
UTL_FILE.PUT_LINE(handle,'&std &sh WARNING &dh STOP_ID &dh STOP_DESCRIPTION &dh TRIP_ID &dh TRIP_NAME &dh STOP_LOC_ID &dh STATUS &dh LOCK_ST_ID &dh ');
UTL_FILE.PUT_LINE(handle,'PEND_INTERF &dh PLN_DEP_DATE &dh ACT_DEP_DATE &eh');

Declare cursor w_trip_stops is
select distinct 
  STP.STOP_ID                      STOP_ID,
  STP1.STOP_DESCRIPTION            STOP_DESCRIPTION,
  STP.TRIP_ID                      TRIP_ID,
  TRP.NAME                         TRIP_NAME,
  STP.STOP_LOCATION_ID             STOP_LOC_ID,
  STP.STATUS_CODE                  STATUS,
  STP.LOCK_STOP_ID                 LOCK_ST_ID,
  STP.PENDING_INTERFACE_FLAG       PEND_INTERF,
  to_char(STP.PLANNED_DEPARTURE_DATE,'DD-MON-RR_HH24:MI:SS')  PLN_DEP_DATE,
  to_char(STP.ACTUAL_DEPARTURE_DATE,'DD-MON-RR_HH24:MI:SS')   ACT_DEP_DATE
FROM 
  WSH_TRIP_STOPS                   STP,
  WSH_SRS_TRIP_STOPS_V             STP1,
  WSH_TRIPS                        TRP
where
  STP.TRIP_ID                     = TRP.TRIP_ID(+) AND
  STP1.STOP_ID                    = STP.STOP_ID AND
  STP.STOP_ID in 
        (
         (select distinct(LEG.PICK_UP_STOP_ID)
         from   
            OE_ORDER_LINES                   LIN,
            WSH_DELIVERY_DETAILS             DET,
            WSH_NEW_DELIVERIES               DEL,
            WSH_DELIVERY_LEGS                LEG,
            WSH_DELIVERY_ASSIGNMENTS         ASG
         where
         DEL.DELIVERY_ID                 = ASG.DELIVERY_ID AND
         ASG.DELIVERY_DETAIL_ID          = DET.DELIVERY_DETAIL_ID AND
         DET.SOURCE_LINE_ID              = LIN.LINE_ID AND  
         LEG.DELIVERY_ID                 = DEL.DELIVERY_ID AND 
         LIN.HEADER_ID                   = nvl('&header_id_selected',:v_header_id) AND
         NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.REFERENCE_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID)
         )
         UNION
         (select distinct(LEG.DROP_OFF_STOP_ID)
         from   
            OE_ORDER_LINES                   LIN,
            WSH_DELIVERY_DETAILS             DET,
            WSH_NEW_DELIVERIES               DEL,
            WSH_DELIVERY_LEGS                LEG,
            WSH_DELIVERY_ASSIGNMENTS         ASG
         where
         DEL.DELIVERY_ID                 = ASG.DELIVERY_ID AND
         ASG.DELIVERY_DETAIL_ID          = DET.DELIVERY_DETAIL_ID AND
         DET.SOURCE_LINE_ID              = LIN.LINE_ID AND  
         LEG.DELIVERY_ID                 = DEL.DELIVERY_ID AND 
         LIN.HEADER_ID                   = nvl('&header_id_selected',:v_header_id) AND
         NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.REFERENCE_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID)
         )
        );

type     per_record_typ is RECORD
               (flag    varchar2(1),
                descrip varchar2(200));
type     msg_tab is TABLE of per_record_typ INDEX by binary_integer;
msg      msg_tab;

begin
  :r_error := 0;
-- Initialize error messages even if Do Analysis not selected
-- if substr(UPPER(nvl('&do_analysis','Y')),1,1) = 'Y' then
  for i in 1..30
  loop
    msg(i).flag := '0';
    msg(i).descrip := '';
  end loop;
  msg(1).descrip   := '    1. XX.';
-- end if;

 for ts in w_trip_stops
 loop
   :r_flag := '';

if substr(UPPER(nvl('&do_analysis','Y')),1,1) = 'Y' then
  null;
end if;

   -- Print line to Output file
   utl_file.put_line(handle,'&sld &b <a HREF="#WTSERR">'||n(:r_flag)||'</a> &eb &d');
   utl_file.put_line(handle,'<a NAME="TS'||ts.STOP_ID||'">'||n(ts.STOP_ID)||'</a>'||'&d'||n(ts.STOP_DESCRIPTION)||'&d');
   utl_file.put_line(handle,'<a HREF="#T'||ts.TRIP_ID||'">'||n(ts.TRIP_ID)||'</a>'||'&d');
   utl_file.put_line(handle,n(ts.TRIP_NAME)||'&d'||'<a NAME="PU'||n(ts.STOP_LOC_ID)||'">'||n(ts.STOP_LOC_ID)||'</a>'||'&d');
   utl_file.put_line(handle,n(ts.STATUS)||'&d'||n(ts.LOCK_ST_ID)||'&d');
   utl_file.put_line(handle,n(ts.PEND_INTERF)||'&d'||n(ts.PLN_DEP_DATE)||'&d'||n(ts.ACT_DEP_DATE)||'&el');

   if :r_flag is not null then
    :r_error := 1;
   end if;
 end loop;
 utl_file.put_line(handle,'&et');

 if :r_error = 1 then
   utl_file.put_line(handle,'&f &b <a NAME="WTSERR">Warning List:</a> &eb &f');
   for i in 1..30
   loop
     if msg(i).flag = '1' then
       utl_file.put_line(handle,msg(i).descrip||'&f');
     end if;
   end loop;
 end if;

end;

UTL_FILE.PUT_LINE(handle,'&et '); 


UTL_FILE.PUT_LINE(handle,'&f &f WSH_DELIVERY_LEGS (LEG) &f');
UTL_FILE.PUT_LINE(handle,'&std &sh LEG_ID &dh SEQ_NUM &dh DELIVERY_ID &dh PICKUP_STOP_ID &dh DROPOFF_STOP_ID &eh');

Declare
cursor w_del_leg is
select distinct 
  LEG.DELIVERY_LEG_ID              LEG_ID,
  LEG.SEQUENCE_NUMBER              SEQ_NUM,
  LEG.DELIVERY_ID                  DELIVERY_ID,
  LEG.PICK_UP_STOP_ID              PICKUP_STOP_ID,
  LEG.DROP_OFF_STOP_ID             DROPOFF_STOP_ID
  --LEG.LOAD_TENDER_STATUS           LOAD_TENDER_STAT
  --ENABLE_TIMESTAMP ,to_char(LEG.CREATION_DATE,'DD-MON-RR_HH24:MI:SS')    CREATE_DT
  --ENABLE_TIMESTAMP ,to_char(LEG.LAST_UPDATE_DATE,'DD-MON-RR_HH24:MI:SS') UPDATE_DT
  --ENABLE_TIMESTAMP ,LEG.REQUEST_ID                                    REQUEST_ID
FROM 
  OE_ORDER_LINES                   LIN,
  WSH_DELIVERY_DETAILS             DET,
  WSH_NEW_DELIVERIES               DEL,
  WSH_DELIVERY_LEGS                LEG,
  WSH_TRIP_STOPS                   STP,
  WSH_DELIVERY_ASSIGNMENTS         ASG,
  WSH_TRIPS                        TRP
where
  DEL.DELIVERY_ID                 = ASG.DELIVERY_ID AND
  ASG.DELIVERY_DETAIL_ID          = DET.DELIVERY_DETAIL_ID AND
  DET.SOURCE_LINE_ID              = LIN.LINE_ID AND  
  STP.STOP_ID(+)                  = LEG.PICK_UP_STOP_ID AND
  STP.TRIP_ID                     = TRP.TRIP_ID AND 
  LEG.DELIVERY_ID(+)              = DEL.DELIVERY_ID AND 
  LIN.HEADER_ID                = nvl('&header_id_selected',:v_header_id) AND
  NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.REFERENCE_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID)
order by
     LEG.DELIVERY_LEG_ID;

begin
 for wdl in w_del_leg
 loop
   utl_file.put_line(handle,'&sld'||n(wdl.LEG_ID)||'&d'||n(wdl.SEQ_NUM)||'&d');
   utl_file.put_line(handle,n(wdl.DELIVERY_ID)||'&d'||n(wdl.PICKUP_STOP_ID)||'&d');
   utl_file.put_line(handle,n(wdl.DROPOFF_STOP_ID)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');

UTL_FILE.PUT_LINE(handle,'&f &f <a NAME="WSH_NEW_DELIVERIES"> WSH_NEW_DELIVERIES (DEL) </a> &f');
UTL_FILE.PUT_LINE(handle,'&std &sh WARNING &dh DELIVERY_ID &dh DEL_NAME &dh STATUS_CODE &dh WAYBILL &dh PLND &dh PICKUP_DT &dh PICKUP_LOC &dh');
UTL_FILE.PUT_LINE(handle,'DROPOFF_DT &dh DROPOFF_LOC &dh SHIP_METHOD_CODE &dh CONFIRM_DATE &eh');


Declare cursor w_deliveries is
select distinct 
  DEL.DELIVERY_ID                  DELIVERY_ID,
  DEL.NAME                         DEL_NAME,
  DEL.STATUS_CODE                  STATUS_CODE,
  DEL.WAYBILL                      WAYBILL,
  DEL.PLANNED_FLAG                 PLND,
  to_char(DEL.INITIAL_PICKUP_DATE,'DD-MON-RR_HH24:MI:SS') PICKUP_DT,
  DEL.INITIAL_PICKUP_LOCATION_ID   PICKUP_LOC,
  to_char(DEL.ULTIMATE_DROPOFF_DATE,'DD-MON-RR_HH24:MI:SS')  DROPOFF_DT,
  DEL.ULTIMATE_DROPOFF_LOCATION_ID DROPOFF_LOC,
  DEL.SHIP_METHOD_CODE             SHP_METH,
  to_char(DEL.CONFIRM_DATE,'DD-MON-RR_HH24:MI:SS')  CONF_DATE
  --DEL.BOOKING_NUMBER               BOOKING_NUM,
  --DEL.ACCEPTANCE_FLAG              ACCEPTED
  --ENABLE_TIMESTAMP ,to_char(DEL.CREATION_DATE,'DD-MON-RR_HH24:MI:SS')    CREATE_DT
  --ENABLE_TIMESTAMP ,to_char(DEL.LAST_UPDATE_DATE,'DD-MON-RR_HH24:MI:SS') UPDATE_DT
  --ENABLE_TIMESTAMP ,DEL.REQUEST_ID                                    REQUEST_ID
FROM 
  OE_ORDER_LINES                   LIN,
  WSH_DELIVERY_DETAILS             DET,
  WSH_NEW_DELIVERIES               DEL,
  WSH_DELIVERY_ASSIGNMENTS         ASG 
where
  DEL.DELIVERY_ID                 = ASG.DELIVERY_ID AND
  ASG.DELIVERY_DETAIL_ID          = DET.DELIVERY_DETAIL_ID AND
  DET.SOURCE_LINE_ID              = LIN.LINE_ID AND  
  LIN.HEADER_ID                   = nvl('&header_id_selected',:v_header_id) AND
  NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.REFERENCE_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID)
order by Del.DELIVERY_ID;
type     per_record_typ is RECORD
               (flag    varchar2(1),
                descrip varchar2(200));
type     msg_tab is TABLE of per_record_typ INDEX by binary_integer;
msg      msg_tab;

function n(v varchar2) return varchar2 is
begin
  if v is null then
   return '&sp';
   else
   return v;
  end if;
end n;

begin
  :r_error := 0;
-- Initialize error messages even if Do Analysis not selected
-- if substr(UPPER(nvl('&do_analysis','Y')),1,1) = 'Y' then
  for i in 1..30
  loop
    msg(i).flag := '0';
    msg(i).descrip := '';
  end loop;
  msg(1).descrip   := '    1. XX.';
-- end if;

 for nd in w_deliveries
 loop
   :r_flag := '';

if substr(UPPER(nvl('&do_analysis','Y')),1,1) = 'Y' then
  null;
end if; -- do_analysis

   -- Print line to Output file
   utl_file.put_line(handle,'&sld &b <a HREF="#WNDERR">'||n(:r_flag)||'</a> &eb &d');
   utl_file.put_line(handle,'<a NAME="D'||nd.DELIVERY_ID||'">'||n(nd.DELIVERY_ID)||'</a>'||'&d'||n(nd.DEL_NAME)||'&d');
   utl_file.put_line(handle,n(nd.STATUS_CODE)||'&d'||n(nd.WAYBILL)||'&d'||n(nd.PLND)||'&d'||n(nd.PICKUP_DT)||'&d');
   utl_file.put_line(handle,'<a HREF="#PU'||nd.PICKUP_LOC||'">'||n(nd.PICKUP_LOC)||'</a>'||'&d');
   utl_file.put_line(handle,n(nd.DROPOFF_DT)||'&d'||n(nd.DROPOFF_LOC)||'&d');
   utl_file.put_line(handle,n(nd.SHP_METH)||'&d'||n(nd.CONF_DATE)||'&el');

   if :r_flag is not null then
    :r_error := 1;
   end if;
 end loop;
 utl_file.put_line(handle,'&et');

 if :r_error = 1 then
   utl_file.put_line(handle,'&f &b <a NAME="WNDERR">Warning List:</a> &eb &f');
   for i in 1..30
   loop
     if msg(i).flag = '1' then
       utl_file.put_line(handle,msg(i).descrip||'&f');
     end if;
   end loop;
 end if;

end;

UTL_FILE.PUT_LINE(handle,'&et ');

UTL_FILE.PUT_LINE(handle,'&f &f WSH_DELIVERY_ASSIGNMENTS (ASG) &f ');
UTL_FILE.PUT_LINE(handle,'&std &sh DEL_ASGN_ID &dh DELIVERY_ID &dh DEL_DETAIL_ID &dh PAR_DEL_ID &dh PAR_DETAIL_ID &eh ');


Declare
cursor w_del_asig is
select distinct 
  ASG.DELIVERY_ASSIGNMENT_ID       DEL_ASGN_ID,
  ASG.DELIVERY_ID                  DELIVERY_ID,
  ASG.DELIVERY_DETAIL_ID           DEL_DETAIL_ID,
  ASG.PARENT_DELIVERY_ID           PAR_DEL_ID,
  ASG.PARENT_DELIVERY_DETAIL_ID    PAR_DETAIL_ID
  --  ASG.ACTIVE_FLAG                  ACTIVE
  --ENABLE_TIMESTAMP ,to_char(ASG.CREATION_DATE,'DD-MON-RR_HH24:MI:SS')    CREATE_DT
  --ENABLE_TIMESTAMP ,to_char(ASG.LAST_UPDATE_DATE,'DD-MON-RR_HH24:MI:SS') UPDATE_DT
  --ENABLE_TIMESTAMP ,ASG.REQUEST_ID                                    REQUEST_ID
FROM 
  OE_ORDER_LINES                   LIN,
  WSH_DELIVERY_DETAILS             DET,
  WSH_DELIVERY_ASSIGNMENTS         ASG  
where
  ASG.DELIVERY_DETAIL_ID          = DET.DELIVERY_DETAIL_ID AND
  DET.SOURCE_LINE_ID              = LIN.LINE_ID AND  
  LIN.HEADER_ID                   = nvl('&header_id_selected',:v_header_id) AND
  NVL('&line_id_selected',0)       in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.REFERENCE_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID);

begin
 for wda in w_del_asig
 loop
   utl_file.put_line(handle,'&sld'||n(wda.DEL_ASGN_ID)||'&d'||n(wda.DELIVERY_ID)||'&d');
   utl_file.put_line(handle,n(wda.DEL_DETAIL_ID)||'&d'||n(wda.PAR_DEL_ID)||'&d');
   utl_file.put_line(handle,n(wda.PAR_DETAIL_ID)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');


UTL_FILE.PUT_LINE(handle,'&f &f <a NAME="ORGANIZATIONS"> ORGANIZATIONS (ORG) </a> &f');
UTL_FILE.PUT_LINE(handle,'&std &sh ORG_ID &dh ORGANIZATION CODE &dh WMS_ENABLED &dh OPM_ENABLED &dh NEGATIVE BAL_ALLOW ');
UTL_FILE.PUT_LINE(handle,'&dh PRIMARY COST_METH &dh COST ORG_ID &dh MASTER ORG_ID &dh DEFAULT COST_GRP &dh PROJECT REF_ENABLED ');
UTL_FILE.PUT_LINE(handle,'&dh COST CUT_DATE &dh EAM ENABLED &dh ENCUMBR REVERSAL &eh ');

Declare cursor l_orgs is
SELECT distinct 
       mp.organization_id                org_id,
       mp.organization_code              org,
       decode(mp.primary_cost_method,
              1, 'Standard',
              2, 'Average',
              5, 'FIFO',
              6, 'LIFO')              PCM,
       decode(mp.wms_enabled_flag,
              'Y', 'Yes',
              'N', 'No')              WMS,
       decode(mp.NEGATIVE_INV_RECEIPT_CODE,
              1, 'Yes',
              2, 'No')                neg_bal,
       mp.cost_organization_id           c_org,
       mp.master_organization_id         m_org,
       mp.default_cost_group_id          dcg,
       decode(mp.project_reference_enabled,
              1, 'Yes',
              2, 'No')                PRE,
       to_char(mp.cost_cutoff_date,'dd-mon-rrrr') ccd,
       decode(mp.eam_enabled_flag,
              'Y','Yes',
              'N','No')               EAM,
       decode (mp.encumbrance_reversal_flag,
               1, 'Yes',
               2, 'No')               ENC
  FROM MTL_PARAMETERS                   MP,
       WSH_DELIVERY_DETAILS             DET
where DET.SOURCE_CODE                 = 'OE'
  and MP.ORGANIZATION_ID              = DET.ORGANIZATION_ID
  and DET.SOURCE_HEADER_ID            = nvl('&header_id_selected',:v_header_id);


function n(v varchar2) return varchar2 is
begin
  if v is null then
   return '&sp';
   else
   return v;
  end if;
end n;

begin

 for orgi in l_orgs
 loop

if substr(UPPER(nvl('&do_analysis','Y')),1,1) = 'Y' then
  null;
end if; -- do_analysis

   -- Print line to Output file
   utl_file.put_line(handle,'&sld '||n(orgi.org_id)||' &d ');
   utl_file.put_line(handle,n(orgi.org)||' &d '||n(orgi.WMS)||' &d ');
   if INV_GMI_RSV_BRANCH.PROCESS_BRANCH(orgi.org_id) then
     utl_file.put_line(handle,'Yes ');
     :is_opm := 'Y';
    else
     utl_file.put_line(handle,'No ');
     :is_opm := 'N';
   end if;
   utl_file.put_line(handle,' &d '||n(orgi.neg_bal)||' &d '||n(orgi.PCM)||' &d ');
   utl_file.put_line(handle,n(orgi.c_org)||' &d '||n(orgi.m_org)||' &d ');
   utl_file.put_line(handle,n(orgi.dcg)||' &d '||n(orgi.PRE)||' &d ');
   utl_file.put_line(handle,n(orgi.ccd)||' &d '||n(orgi.EAM)||' &d ');
   utl_file.put_line(handle,n(orgi.ENC)||' &el ');

 end loop;

end;

UTL_FILE.PUT_LINE(handle,'&et '); 


UTL_FILE.PUT_LINE(handle,'&f &f <a NAME="ACCOUNTING PERIODS"> ACCOUNTING PERIODS (ACC) </a> &f');
UTL_FILE.PUT_LINE(handle,'&std &sh ORG_ID &dh ORGANIZATION CODE &dh PLANNED_DEPART_DT (Shipped Line) &dh ');
UTL_FILE.PUT_LINE(handle,'SCHEDULE_SHIP_DT (Not Shipped Line) &dh ACCOUNT PERIOD_ID &dh PERIOD NAME ');
UTL_FILE.PUT_LINE(handle,'&dh OPEN FLAG &dh PERIOD START_DT &dh PERIOD CLOSE_DT &dh SCHEDULE CLOSE_DT &eh ');

Declare cursor l_orgs is
SELECT distinct 
       mp.organization_id               org_id,
       mp.organization_code             org,
       to_char(STP.PLANNED_DEPARTURE_DATE,'DD-MON-RR_HH24:MI:SS')  plan_dep_date,
       ''                               schdate,
       oac.acct_period_id               acc_per_id,
       oac.period_name                  per_name,
       oac.open_flag                    open_fl,
       to_char(oac.period_start_date,'DD-MON-RR_HH24:MI:SS')   per_sd,
       to_char(oac.period_close_date,'DD-MON-RR_HH24:MI:SS')   per_cd,
       to_char(oac.schedule_close_date,'DD-MON-RR_HH24:MI:SS') sch_cd
  FROM MTL_PARAMETERS                   MP,
       ORG_ACCT_PERIODS                 OAC,
       WSH_DELIVERY_DETAILS             DET,
       WSH_DELIVERY_LEGS                LEG,
       WSH_TRIP_STOPS                   STP,
       WSH_DELIVERY_ASSIGNMENTS         ASG,
       WSH_TRIPS                        TRP
where DET.RELEASED_STATUS             = 'C'
  and ASG.DELIVERY_DETAIL_ID          = DET.DELIVERY_DETAIL_ID
  and STP.STOP_ID(+)                  = LEG.PICK_UP_STOP_ID
  and STP.TRIP_ID                     = TRP.TRIP_ID(+)
  and LEG.DELIVERY_ID(+)              = ASG.DELIVERY_ID
  and DET.SOURCE_CODE                 = 'OE'
  and MP.ORGANIZATION_ID              = OAC.ORGANIZATION_ID
  and MP.ORGANIZATION_ID              = DET.ORGANIZATION_ID
  and STP.PLANNED_DEPARTURE_DATE      BETWEEN OAC.PERIOD_START_DATE and NVL(OAC.PERIOD_CLOSE_DATE,OAC.SCHEDULE_CLOSE_DATE)
  and DET.SOURCE_HEADER_ID            = nvl('&header_id_selected',:v_header_id)
union
SELECT distinct 
       mp.organization_id               org_id,
       mp.organization_code             org,
       ''                               plan_dep_date,
       to_char(LIN.SCHEDULE_SHIP_DATE,'DD-MON-RR_HH24:MI:SS')   schdate,
       oac.acct_period_id               acc_per_id,
       oac.period_name                  per_name,
       oac.open_flag                    open_fl,
       to_char(oac.period_start_date,'DD-MON-RR_HH24:MI:SS')   per_sd,
       to_char(oac.period_close_date,'DD-MON-RR_HH24:MI:SS')   per_cd,
       to_char(oac.schedule_close_date,'DD-MON-RR_HH24:MI:SS') sch_cd
  FROM MTL_PARAMETERS                   MP,
       ORG_ACCT_PERIODS                 OAC,
       WSH_DELIVERY_DETAILS             DET,
       OE_ORDER_LINES                   LIN
where DET.RELEASED_STATUS             <> 'C'
  and DET.SOURCE_LINE_ID              = LIN.LINE_ID
  and DET.SOURCE_CODE                 = 'OE'
  and MP.ORGANIZATION_ID              = OAC.ORGANIZATION_ID
  and MP.ORGANIZATION_ID              = DET.ORGANIZATION_ID
  and LIN.SCHEDULE_SHIP_DATE          BETWEEN OAC.PERIOD_START_DATE and NVL(OAC.PERIOD_CLOSE_DATE,OAC.SCHEDULE_CLOSE_DATE)
  and DET.SOURCE_HEADER_ID            = nvl('&header_id_selected',:v_header_id);


function n(v varchar2) return varchar2 is
begin
  if v is null then
   return '&sp';
   else
   return v;
  end if;
end n;

begin

 for orgi in l_orgs
 loop

if substr(UPPER(nvl('&do_analysis','Y')),1,1) = 'Y' then
  null;
end if; -- do_analysis

   -- Print line to Output file
   utl_file.put_line(handle,'&sld '||n(orgi.org_id)||' &d ');
   utl_file.put_line(handle,n(orgi.org)||' &d '||n(orgi.plan_dep_date)||' &d ');
   utl_file.put_line(handle,n(orgi.schdate)||' &d '||n(orgi.acc_per_id)||' &d ');
   utl_file.put_line(handle,n(orgi.per_name)||' &d '||n(orgi.open_fl)||' &d ');
   utl_file.put_line(handle,n(orgi.per_sd)||' &d '||n(orgi.per_cd)||' &d ');
   utl_file.put_line(handle,n(orgi.sch_cd)||' &el ');

 end loop;

end;

UTL_FILE.PUT_LINE(handle,'&et '); 


UTL_FILE.PUT_LINE(handle,'&f &f <a NAME="WSH_DELIVERY_DETAILS"> WSH_DELIVERY_DETAILS (DET)</a> <a HREF="#WDD">Column Definitions</a> &f');


Declare cursor w_del_details is
select distinct 
  DET.DELIVERY_DETAIL_ID           DEL_DET_ID,
  DET.RELEASED_STATUS              REL_STATUS_C,
  decode(DET.RELEASED_STATUS,
    'Y','Y=Staged',
    'R','R=Ready to Release',
    'S','S=Rel to Warhouse',
    'B','B=Backorder',
    'P','P=Pending Inv',
    'C','C=Shipped',
    'N','N=Not Ready',
    'D','D=Cancelled',
    'X','X=Not Applicable','Unknown: '||DET.RELEASED_STATUS) REL_STATUS,
  DET.MOVE_ORDER_LINE_ID            MO_LINE_ID,
  ASG.DELIVERY_ID                   DELIV_ID,
  TRP.TRIP_ID                       TRIP_ID,
  to_char(LIN.line_number) || 
          decode(LIN.shipment_number, null, null, '.' || to_char(LIN.shipment_number))|| 
          decode(LIN.option_number, null, null, '.' || to_char(LIN.option_number)) ||
          decode(LIN.component_number, null, null, 
                 decode(LIN.option_number, null, '.',null)||
                 '.'||to_char(LIN.component_number))||
          decode(LIN.service_number,null,null,
                 decode(LIN.component_number, null, '.' , null) ||
                        decode(LIN.option_number, null, '.', null ) ||
                        '.'|| to_char(LIN.service_number)) LINE,
  DET.SOURCE_LINE_ID                LINE_ID,
  DET.INVENTORY_ITEM_ID             ITEM_ID,
  ITM.SEGMENT1                      ITEM,
  nvl(DET.SRC_REQUESTED_QUANTITY,0) SRQ_Q,
  DET.SRC_REQUESTED_QUANTITY_UOM    SRQ_U,
  nvl(DET.SRC_REQUESTED_QUANTITY2,0) SRQ_Q2,
  DET.SRC_REQUESTED_QUANTITY_UOM2   SRQ_U2,
  nvl(DET.REQUESTED_QUANTITY,0)     REQ_Q,
  DET.REQUESTED_QUANTITY_UOM        REQ_U,
  nvl(DET.REQUESTED_QUANTITY2,0)    REQ_Q2,
  DET.REQUESTED_QUANTITY_UOM2       REQ_U2,
  nvl(DET.SHIPPED_QUANTITY,0)       SHP_Q,
  nvl(DET.SHIPPED_QUANTITY2,0)      SHP_Q2,
  nvl(DET.DELIVERED_QUANTITY,0)     DLV_Q,
  nvl(DET.DELIVERED_QUANTITY2,0)    DLV_Q2,
  nvl(DET.CANCELLED_QUANTITY,0)     CAN_Q,
  nvl(DET.CANCELLED_QUANTITY2,0)    CAN_Q2,
  nvl(DET.INV_INTERFACED_FLAG,'N')  INI,
  nvl(DET.OE_INTERFACED_FLAG,'N')   OMI,
  DET.SHIP_TOLERANCE_ABOVE          STA,
  DET.SHIP_TOLERANCE_BELOW          STB,
  DET.SHIP_FROM_LOCATION_ID         SH_FROM_ID,
  DET.SHIP_TO_LOCATION_ID           SH_TO_ID,
--  DET.MVT_STAT_STATUS               MVT_STATUS,
  DET.ORGANIZATION_ID               WH_ID,
  DET.SUBINVENTORY                  CUR_SUB,
  DET.ATTRIBUTE15                   ORG_SUB,
  DET.REVISION                      REV,
  DET.LOT_NUMBER                    LOT,
  DET.SERIAL_NUMBER                 SERIAL,
  DET.LOCATOR_ID                    LOC_ID,
  DET.SHIP_METHOD_CODE              SHIP_METH,
--  DET.MOVEMENT_ID                   MVMT_ID,
  DET.SPLIT_FROM_DELIVERY_DETAIL_ID SPL_DEL_DET_ID,
  DET.PICKABLE_FLAG                 PICKABLE_FLAG,
  nvl(DET.PICKED_QUANTITY,0)        PICKED_QUANTITY,
  nvl(DET.PICKED_QUANTITY2,0)       PICKED_QUANTITY2,
  DET.SHIP_SET_ID                   SHIP_SET_ID,
  DET.SHIP_MODEL_COMPLETE_FLAG      SHIP_MODEL_COMPLETE_FLAG,
  DET.TRANSACTION_TEMP_ID           TRX_TEMP_ID,
  DET.TOP_MODEL_LINE_ID             TOP_MO_LIN,
  DET.SOURCE_LINE_SET_ID            SRC_LIN_SET,
  to_char(DET.CREATION_DATE,'DD-MON-RR_HH24:MI:SS')  cre_date,
  DET.sublot_number                 slotno,
  DET.CYCLE_COUNT_QUANTITY2         cycle_qty,
  DET.QUALITY_CONTROL_QUANTITY      QC_qty,
  DET.QUALITY_CONTROL_QUANTITY2     QC_qty2,
  DET.SCHEDULED_QUANTITY2           sch_qty2,
  to_char(DET.LAST_UPDATE_DATE,'DD-MON-RR_HH24:MI:SS')  upd_date,
  DET.source_line_set_id            SRC_LIN_SET_ID,
  DET.BATCH_ID                      BATCH
FROM 
  OE_ORDER_LINES                   LIN,
  WSH_DELIVERY_DETAILS             DET,
  WSH_NEW_DELIVERIES               DEL,
  WSH_DELIVERY_LEGS                LEG,
  WSH_TRIP_STOPS                   STP,
  MTL_SYSTEM_ITEMS                 ITM,
  WSH_DELIVERY_ASSIGNMENTS         ASG,
  WSH_TRIPS                        TRP
where
  DEL.DELIVERY_ID(+)              = ASG.DELIVERY_ID AND
  ASG.DELIVERY_DETAIL_ID          = DET.DELIVERY_DETAIL_ID AND
  DET.SOURCE_LINE_ID              = LIN.LINE_ID AND  
  STP.STOP_ID(+)                  = LEG.PICK_UP_STOP_ID AND
  STP.TRIP_ID                     = TRP.TRIP_ID(+) AND 
  LEG.DELIVERY_ID(+)              = DEL.DELIVERY_ID AND 
  LIN.SHIP_FROM_ORG_ID            = ITM.ORGANIZATION_ID(+) AND
  LIN.INVENTORY_ITEM_ID           = ITM.INVENTORY_ITEM_ID(+) AND
  DET.SOURCE_CODE                 = 'OE' AND
  LIN.HEADER_ID                   = nvl('&header_id_selected',:v_header_id) AND
  NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.REFERENCE_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID)
Order by
  DET.SOURCE_LINE_ID, DET.DELIVERY_DETAIL_ID;

r_uom_lin        varchar2(10);
r_qshp_lin       number;
r_sta_lin        varchar2(100);
r_mtl_trn        varchar2(100);
r_mtl_res        number;
c_lines          number;
type     per_record_typ is RECORD
               (flag    varchar2(1),
                descrip varchar2(200));
type     msg_tab is TABLE of per_record_typ INDEX by binary_integer;
msg      msg_tab;

function n(v varchar2) return varchar2 is
begin
  if v is null then
   return '&sp';
   else
   return v;
  end if;
end n;


begin
  :r_error := 0;
  c_lines := 51;

-- Initialize error messages even if Do Analysis not selected
-- if substr(UPPER(nvl('&do_analysis','Y')),1,1) = 'Y' then
  for i in 1..35
  loop
    msg(i).flag := '0';
    msg(i).descrip := '';
  end loop;
  msg(1).descrip   := '    1. Source Requested Quantity is less than Shipped Quantity.';

  msg(10).descrip  := '   10. Pickable_Flag on Delivery Detail does Not Match with Mtl_Transactions_Enabled_Flag on Mtl_System_Items.';

  msg(20).descrip  := '   20. Delivery Detail have no Order Lines Associated.';
  msg(21).descrip  := '   21. Order Line associated to this Delivery Detail have INVALID status.';
  msg(24).descrip  := '   24. Delivery Detail have invalid RELEASED status.';
  msg(25).descrip  := '   25. Delivery Detail has NOT been Shipped but has been INTERFACED to OM or INV.';
  msg(26).descrip  := '   26. Delivery Detail has NOT been Shipped but associated Order Line shows Shipped Quantity.';
  msg(27).descrip  := '   27. Delivery Detail has NOT been Shipped but associated Order Line is Not on AWAITING SHIPPING status.';
  msg(28).descrip  := '   28. Delivery Detail has been Shipped, NOT OM Interfaced but associated Order Line Shipped Qty has been updated.';
  msg(29).descrip  := '   29. Delivery Detail has been Shipped, NOT OM Interfaced but associated Order Line is Not on AWAITING SHIPPING status.';
  msg(30).descrip  := '   30. Delivery Detail has been Shipped and OM Interfaced but associated Order Line Shipped Qty has NOT been updated.';
  msg(31).descrip  := '   31. Delivery Detail has been Shipped and OM Interfaced but associated Order Line still on AWAITING SHIPPING status.';
  msg(32).descrip  := '   32. Delivery Detail has been Shipped and OM interfaced, Shipped Qty on order_line does not match Delivery Detail Shp Qty (maybe a Split line).';
  msg(33).descrip  := '   33. Delivery Detail is Cancelled but has been INTERFACED to OM or INV.';
-- end if;


 for dd in w_del_details
 loop
   :r_flag := '';

if substr(UPPER(nvl('&do_analysis','Y')),1,1) = 'Y' then
   -- Check Pickable_flag against mtl_system_items.mtl_transactions_enabled_flag
   Select nvl(mtl_transactions_enabled_flag,'N')
     into r_mtl_trn
     from Mtl_System_items
    where Inventory_item_Id = dd.ITEM_ID
      and Organization_Id   = dd.WH_ID;
   if dd.PICKABLE_FLAG <> r_mtl_trn then
     :r_flag := :r_flag || '10 ';
     msg(10).flag := '1';
   end if;

   begin
     select ORDER_QUANTITY_UOM, nvl(SHIPPED_QUANTITY,0), substr(FLOW_STATUS_CODE,1,22)
       into r_uom_lin, r_qshp_lin, r_sta_lin
      from oe_order_lines_all
     where line_id = dd.LINE_ID;
     exception
       when no_data_found then
         :r_flag := :r_flag || '20 ';
         msg(20).flag := '1';
   end;
   ---
   -- Basic Verification
   --
   If  (dd.SRQ_Q*(100+nvl(dd.STA,0))/100 < dd.SHP_Q) AND (dd.SRQ_U = dd.REQ_U) then
     :r_flag := :r_flag || '1 ';
     msg(1).flag := '1';
   end if;

   if dd.REL_STATUS_C not in ('Y','R','S','B','P','C','N','D','X') then    -- invalid Released status
     :r_flag := :r_flag || '24 ';
     msg(24).flag := '1';
   end if;

   if dd.REL_STATUS_C not in ('C','D')  -- not shipped or cancelled
      and (dd.INI not in ( 'N','X')
           or dd.OMI <> 'N') then    --  but interfaced INV or OM
     :r_flag := :r_flag || '25 ';
     msg(25).flag := '1';
   end if;

   if dd.REL_STATUS_C not in ('C','D')   -- not shipped nor cancelled
      and dd.OMI = 'N'
      and r_qshp_lin > 0  then   -- not OM interfaced but Shipped qty on order line has been updated
       :r_flag := :r_flag || '26 ';
       msg(26).flag := '1';
   end if;

   if dd.REL_STATUS_C not in ('C','D')   -- not shipped nor cancelled
      and dd.OMI = 'N'
      and r_sta_lin <> 'AWAITING_SHIPPING'  then   --  not OM interfaced but order line is not Awaiting Shipping
       :r_flag := :r_flag || '27 ';
       msg(27).flag := '1';
   end if;

   -- Verifications for Shipped Delivery Details
   if dd.REL_STATUS_C = 'C' then
     if dd.OMI = 'N'
      and r_qshp_lin > 0  then   -- Shipped, not OM interfaced but order lines Qty has been updated
       :r_flag := :r_flag || '28 ';
       msg(28).flag := '1';
     end if;

     if dd.OMI = 'N'
      and r_sta_lin <> 'AWAITING_SHIPPING'  then   -- Shipped, not OM interfaced but order line not on Awaiting Shipping
       :r_flag := :r_flag || '29 ';
       msg(29).flag := '1';
     end if;

     if dd.OMI = 'Y'
      and r_qshp_lin = 0  then    -- Shipped and OM interfaced, order_line shipped Qty not updated
       :r_flag := :r_flag || '30 ';
       msg(30).flag := '1';
     end if;

     if dd.OMI = 'Y'
      and r_sta_lin = 'AWAITING_SHIPPING'  then    -- Shipped and OM interfaced, order_line status not updated
       :r_flag := :r_flag || '31 ';
       msg(31).flag := '1';
     end if;

     if dd.OMI = 'Y'
      and r_uom_lin = dd.REQ_U            -- same UOM, no conversion
      and r_qshp_lin <> dd.SHP_Q  then    -- Shipped and OM interfaced, Shipped Qty on order_line does not match Delivery Detail Shp Qty
       :r_flag := :r_flag || '32 ';
       msg(32).flag := '1';
     end if;
   end if; -- Shipped Del.Detail

   -- Verifications for Cancelled Delivery Details
   if dd.REL_STATUS_C = 'D' then
     if (dd.INI = 'Y' 
      or dd.OMI = 'Y') then   -- Delivery Detail has been cancelled but INV or OM flags are set to Y
       :r_flag := :r_flag || '33 ';
       msg(33).flag := '1';
     end if;
   end if; -- Cancelled Del.Detail

end if; -- do_analysis
   ---
   if c_lines >= 35 then
     if c_lines = 51 then
       UTL_FILE.PUT_LINE(handle,'&std ');
     end if;
     c_lines := 1;
     UTL_FILE.PUT_LINE(handle,'&sh WARNING &dh DELIVERY DETAIL_ID &dh RELEASE STATUS &dhr INI &dh OMI &dh MOVE_ORDER LINE_ID &dh ');
     UTL_FILE.PUT_LINE(handle,'DELIVERY ID &dh TRIP_ID &dh LINE &dh LINE_ID &dh ITEM_ID &dh ITEM &dh SOURCE REQ_QTY &dh ');
     UTL_FILE.PUT_LINE(handle,'SOURCE REQ_UOM &dh REQUEST QTY &dh REQUEST UOM &dh PICKED QTY &dh SHIPPED QTY &dh ');
     UTL_FILE.PUT_LINE(handle,'DELIVER QTY &dh CANCEL QTY &dh PICK FLAG &dh STA &dh STB &dh SHIP FROM_ID &dh BATCH_ID &dh ');
     UTL_FILE.PUT_LINE(handle,'SHIP TO_ID &dh WH_ID &dh CURRENT SUBINV &dh ORG SUBINV &dh REV &dh LOT &dh SERIAL &dh ');
     UTL_FILE.PUT_LINE(handle,'TRANSAC TEMP_ID &dh LOCATOR ID &dh SHIP METHOD &dh CREATE DATE &dh SPLIT_FROM DEL_DET_ID &dh ');
     UTL_FILE.PUT_LINE(handle,'SHIP SET &dh SHIP MOD_CMP &dh TOP_MODEL LINE_ID &dh SOURCE LINE_SET &dh DISCRETE SRC_REQ_QTY &dh ');
     UTL_FILE.PUT_LINE(handle,'DISCRETE SRC_REQ_UOM &dh DISCRETE REQ_QTY &dh DISCRETE REQ_UOM &dh DISCRETE PICK_QTY &dh ');
     UTL_FILE.PUT_LINE(handle,'DISCRETE SHP_QTY &dh DISCRETE DEL_QTY &dh DISCRETE CANC_QTY &dh ');
     UTL_FILE.PUT_LINE(handle,'CYCLE COUNT_QTY &dh QUALITY CTRL_QTY &dh ');
     UTL_FILE.PUT_LINE(handle,'QUALITY CTRL_QTY2 &dh DISCRETE SCHD_QTY &dh SUBLOT NUMBER &dh LAST UPDATE_DATE &dh');
     UTL_FILE.PUT_LINE(handle,'SRC_LIN SET_ID &eh');
    else
     c_lines := c_lines + 1;
   end if;

   -- Print line to Output file
   utl_file.put_line(handle,'&sld &b <a HREF="#WDDERR">'||n(:r_flag)||'</a> &eb &d');
   utl_file.put_line(handle,'<a NAME="WDD'||dd.DEL_DET_ID||'">'||n(dd.DEL_DET_ID)||'</a> &d');
   utl_file.put_line(handle,n(dd.REL_STATUS)||'&d');
   utl_file.put_line(handle,n(dd.INI)||'&d'||n(dd.OMI)||'&d');
   utl_file.put_line(handle,'<a HREF="#MO'||dd.MO_LINE_ID||'">'||n(dd.MO_LINE_ID)||'</a>'||'&d');
   utl_file.put_line(handle,'<a HREF="#D'||dd.DELIV_ID||'">'||n(dd.DELIV_ID)||'</a>'||'&d');
   utl_file.put_line(handle,'<a HREF="#T'||dd.TRIP_ID||'">'||n(dd.TRIP_ID)||'</a>'||'&d'||n(dd.LINE)||'&d');
   utl_file.put_line(handle,'<a HREF="#OOE'||dd.LINE_ID||'">'||n(dd.LINE_ID)||'</a> &d');
   utl_file.put_line(handle,n(dd.ITEM_ID)||'&d'||n(dd.ITEM)||'&d');
   utl_file.put_line(handle,n(dd.SRQ_Q)||'&d'||n(dd.SRQ_U)||'&d');
   utl_file.put_line(handle,n(dd.REQ_Q)||'&d'||n(dd.REQ_U)||'&d');
   utl_file.put_line(handle,n(dd.PICKED_QUANTITY)||'&d'||n(dd.SHP_Q)||'&d');
   utl_file.put_line(handle,n(dd.DLV_Q)||'&d'||n(dd.CAN_Q)||'&d');
   utl_file.put_line(handle,n(dd.PICKABLE_FLAG)||'&d');
   utl_file.put_line(handle,n(dd.STA)||'&d');
   utl_file.put_line(handle,n(dd.STB)||'&d'||n(dd.SH_FROM_ID)||'&d');
   utl_file.put_line(handle,n(dd.BATCH)||'&d'||n(dd.SH_TO_ID)||'&d');
   utl_file.put_line(handle,n(dd.WH_ID)||'&d'||n(dd.CUR_SUB)||'&d');
   utl_file.put_line(handle,n(dd.ORG_SUB)||'&d'||n(dd.REV)||'&d');
   utl_file.put_line(handle,n(dd.LOT)||'&d'||n(dd.SERIAL)||'&d');
   utl_file.put_line(handle,n(dd.TRX_TEMP_ID)||'&d');
   utl_file.put_line(handle,n(dd.LOC_ID)||'&d'||n(dd.SHIP_METH)||'&d');
   utl_file.put_line(handle,n(dd.CRE_DATE)||'&d');
   utl_file.put_line(handle,n(dd.SPL_DEL_DET_ID)||'&d');
   utl_file.put_line(handle,n(dd.SHIP_SET_ID)||'&d'||n(dd.SHIP_MODEL_COMPLETE_FLAG)||'&d');
   utl_file.put_line(handle,n(dd.TOP_MO_LIN)||'&d'||n(dd.SRC_LIN_SET)||'&d');
   utl_file.put_line(handle,n(dd.SRQ_Q2)||'&d'||n(dd.SRQ_U2)||'&d');
   utl_file.put_line(handle,n(dd.REQ_Q2)||'&d'||n(dd.REQ_U2)||'&d');
   utl_file.put_line(handle,n(dd.PICKED_QUANTITY2)||'&d'||n(dd.SHP_Q2)||'&d');
   utl_file.put_line(handle,n(dd.DLV_Q2)||'&d'||n(dd.CAN_Q2)||'&d');
   utl_file.put_line(handle,n(dd.cycle_qty)||'&d');
   utl_file.put_line(handle,n(dd.QC_qty)||'&d'||n(dd.QC_qty2)||'&d');
   utl_file.put_line(handle,n(dd.sch_qty2)||'&d');
   utl_file.put_line(handle,n(dd.slotno)||'&d'||n(dd.upd_date)||'&d');
   utl_file.put_line(handle,n(dd.SRC_LIN_SET_ID)||'&el');
   if :r_flag is not null then
    :r_error := 1;
   end if;
 end loop;
 utl_file.put_line(handle,'&et');

 if :r_error = 1 then
   utl_file.put_line(handle,'&f &b <a NAME="WDDERR">Warnings List:</a> &eb &f');
   for i in 1..35
   loop
     if msg(i).flag = '1' then
       utl_file.put_line(handle,msg(i).descrip||'&f');
     end if;
   end loop;
 end if;

end;

UTL_FILE.PUT_LINE(handle,'&et '); 


UTL_FILE.PUT_LINE(handle,'&f &f WSH_DELIVERY_DETAILS (CONTAINERS) &f');
UTL_FILE.PUT_LINE(handle,'&std &sh DEL_DET_ID &dh REL_STATUS &dh DELIV_ID &dh TRIP_ID &dh ITEM_ID &dh ITEM &dh CF &dh CONT_NAME &dh CONT_TYPE &dh ');
UTL_FILE.PUT_LINE(handle,'FL_PER &dh GRS_WT &dh NET_WT &dh WT_UOM &dh VOL &dh VOL_UOM &dh SRQ_Q &dh REQ_Q &dh SHP_Q &dh DLV_Q &dh CAN_Q &dh ');
UTL_FILE.PUT_LINE(handle,'INI &dh OMI &dh SH_FROM_ID &dh SH_TO_ID &dh WH_ID &dh SHIP_METH &dh CREATE_D &dh FOB_CODE &dh FRT_TERMS &dh');
UTL_FILE.PUT_LINE(handle,'SPL_DEL_DET_ID &dh LPN_ID &eh');


Declare
cursor w_del_det_c is
select distinct
  DET.DELIVERY_DETAIL_ID           DEL_DET_ID,
  decode(DET.RELEASED_STATUS,
    'Y','Y=Staged',
    'R','R=Ready to Relese',
    'S','S=Rel to Warhouse',
    'B','B=Backorder',
    'P','P=Pending Inv',
    'C','C=Shipped',
    'N','N=Not Ready',
    'D','D=Cancelled',
    'X','X=Not Applicable','Unknown: '||DET.RELEASED_STATUS) REL_STATUS,
  ASG.DELIVERY_ID                   DELIV_ID,
  TRP.TRIP_ID                       TRIP_ID,
  DET.INVENTORY_ITEM_ID             ITEM_ID,
  ITM.SEGMENT1                      ITEM,
  DET.CONTAINER_FLAG                CF,
  DET.CONTAINER_NAME                CONT_NAME,
  DET.CONTAINER_TYPE_CODE           CONT_TYPE,
  DET.FILL_PERCENT                  FL_PER,
  DET.GROSS_WEIGHT                  GRS_WT,
  DET.NET_WEIGHT                    NET_WT,
  DET.WEIGHT_UOM_CODE               WT_UOM,
  DET.VOLUME                        VOL,
  DET.VOLUME_UOM_CODE               VOL_UOM,
  nvl(DET.SRC_REQUESTED_QUANTITY,0) SRQ_Q,
  nvl(DET.REQUESTED_QUANTITY,0)     REQ_Q,
  nvl(DET.SHIPPED_QUANTITY,0)       SHP_Q,
  nvl(DET.DELIVERED_QUANTITY,0)     DLV_Q,
  nvl(DET.CANCELLED_QUANTITY,0)     CAN_Q,
  nvl(DET.INV_INTERFACED_FLAG,'N')  INI,
  nvl(DET.OE_INTERFACED_FLAG,'N')   OMI,
  DET.SHIP_FROM_LOCATION_ID         SH_FROM_ID,
  DET.SHIP_TO_LOCATION_ID           SH_TO_ID,
  DET.ORGANIZATION_ID               WH_ID,
  DET.SHIP_METHOD_CODE              SHIP_METH,
  to_char(DET.CREATION_DATE,'DD-MON-RR_HH24:MI:SS') CRE_DT,
  DET.FOB_CODE                      FOB_CODE,
  DET.FREIGHT_TERMS_CODE            FRT_TERMS,
  --DET.MOVEMENT_ID                   MVMT_ID,
  DET.SPLIT_FROM_DELIVERY_DETAIL_ID SPL_DEL_DET_ID,
  DET.LPN_ID                        LPN_ID
  --ENABLE_TIMESTAMP ,DET.ORG_ID                                        DEL_ORG_ID
  --ENABLE_TIMESTAMP ,to_char(DET.CREATION_DATE,'DD-MON-RR_HH24:MI:SS')    CREATE_DT
  --ENABLE_TIMESTAMP ,to_char(DET.LAST_UPDATE_DATE,'DD-MON-RR_HH24:MI:SS') UPDATE_DT
  --ENABLE_TIMESTAMP ,DET.REQUEST_ID                                    REQUEST_ID
FROM 
  --OE_ORDER_LINES                   LIN,
  WSH_DELIVERY_DETAILS             DET,
  --WSH_NEW_DELIVERIES               DEL,
  WSH_DELIVERY_LEGS                LEG,
  WSH_TRIP_STOPS                   STP,
  MTL_SYSTEM_ITEMS                 ITM,
  WSH_DELIVERY_ASSIGNMENTS         ASG,
  WSH_TRIPS                        TRP
where
  --DEL.DELIVERY_ID(+)              = ASG.DELIVERY_ID AND
  ASG.DELIVERY_DETAIL_ID          = DET.DELIVERY_DETAIL_ID AND
  DET.ORGANIZATION_ID             = ITM.ORGANIZATION_ID(+) AND
  DET.INVENTORY_ITEM_ID           = ITM.INVENTORY_ITEM_ID(+) AND
  --DET.SOURCE_LINE_ID              = LIN.LINE_ID AND  
  STP.STOP_ID(+)                  = LEG.PICK_UP_STOP_ID AND
  STP.TRIP_ID                     = TRP.TRIP_ID(+) AND 
  LEG.DELIVERY_ID(+)              = ASG.DELIVERY_ID AND 
  --LIN.SHIP_FROM_ORG_ID            = ITM.ORGANIZATION_ID(+) AND
  --LIN.INVENTORY_ITEM_ID           = ITM.INVENTORY_ITEM_ID(+) AND
  DET.SOURCE_CODE                 = 'WSH' AND
  ASG.DELIVERY_ID IN (select ASG1.DELIVERY_ID
                  from   WSH_DELIVERY_ASSIGNMENTS ASG1,
                         WSH_DELIVERY_DETAILS     DET1,
                         OE_ORDER_LINES_ALL       LIN1
                  where DET1.SOURCE_LINE_ID = LIN1.LINE_ID AND
                        DET1.DELIVERY_DETAIL_ID = ASG1.DELIVERY_DETAIL_ID AND
                        DET1.SOURCE_CODE = 'OE' AND
  LIN1.HEADER_ID                   = nvl('&header_id_selected',:v_header_id) AND
  NVL('&line_id_selected',0)    in (0,LIN1.LINE_ID,
                                         LIN1.TOP_MODEL_LINE_ID,
                                         LIN1.ATO_LINE_ID,
                                         LIN1.LINK_TO_LINE_ID,
                                         LIN1.REFERENCE_LINE_ID,
                                         LIN1.SERVICE_REFERENCE_LINE_ID));

begin
 for wddc in w_del_det_c
 loop
   utl_file.put_line(handle,'&sld'||n(wddc.DEL_DET_ID)||'&d'||n(wddc.REL_STATUS)||'&d');
   utl_file.put_line(handle,n(wddc.DELIV_ID)||'&d'||n(wddc.TRIP_ID)||'&d');
   utl_file.put_line(handle,n(wddc.ITEM_ID)||'&d'||n(wddc.ITEM)||'&d');
   utl_file.put_line(handle,n(wddc.CF)||'&d'||n(wddc.CONT_NAME)||'&d');
   utl_file.put_line(handle,n(wddc.CONT_TYPE)||'&d'||n(wddc.FL_PER)||'&d');
   utl_file.put_line(handle,n(wddc.GRS_WT)||'&d'||n(wddc.NET_WT)||'&d');
   utl_file.put_line(handle,n(wddc.WT_UOM)||'&d'||n(wddc.VOL)||'&d');
   utl_file.put_line(handle,n(wddc.VOL_UOM)||'&d'||n(wddc.SRQ_Q)||'&d');
   utl_file.put_line(handle,n(wddc.REQ_Q)||'&d'||n(wddc.SHP_Q)||'&d');
   utl_file.put_line(handle,n(wddc.DLV_Q)||'&d'||n(wddc.CAN_Q)||'&d');
   utl_file.put_line(handle,n(wddc.INI)||'&d'||n(wddc.OMI)||'&d');
   utl_file.put_line(handle,n(wddc.SH_FROM_ID)||'&d'||n(wddc.SH_TO_ID)||'&d');
   utl_file.put_line(handle,n(wddc.WH_ID)||'&d'||n(wddc.SHIP_METH)||'&d');
   utl_file.put_line(handle,n(wddc.CRE_DT)||'&d'||n(wddc.FOB_CODE)||'&d');
   utl_file.put_line(handle,n(wddc.FRT_TERMS)||'&d'||n(wddc.SPL_DEL_DET_ID)||'&d');
   utl_file.put_line(handle,n(wddc.LPN_ID)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');


UTL_FILE.PUT_LINE(handle,'&f &f <a NAME="WSH_SERIAL_NUMBERS"> WSH_SERIAL_NUMBERS (WSN)</a> <a HREF="#WSN">Column Definitions</a> &f');

UTL_FILE.PUT_LINE(handle,'&std &sh ERR_FLAG &dh DEL_DET_ID &dh REL_STATUS &dh LINE &dh LINE_ID &dh ITEM_ID &dh SRQ_Q &dh SRQ_U &dh REQ_Q &dh');
UTL_FILE.PUT_LINE(handle,'REQ_U &dh PIK_Q &dh SHP_Q &dh DLV_Q &dh CAN_Q &dh INI &dh OMI &dh STA &dh STB &dh WH_ID &dh CUR_SUB &dh');
UTL_FILE.PUT_LINE(handle,'ORG_SUB &dh REV &dh LOT &dh SERIAL &dh LOC_ID &dh SPL_DEL_DET_ID &dh FROM_SERIAL &dh TO_SERIAL &dh SERIAL_QTY &dh ');
UTL_FILE.PUT_LINE(handle,'CREATION_DATE &eh');

Declare cursor w_serial_n is
select distinct 
  DET.DELIVERY_DETAIL_ID           DEL_DET_ID,
  DET.RELEASED_STATUS              REL_STATUS_C,
  decode(DET.RELEASED_STATUS,
    'Y','Y=Staged',
    'R','R=Ready to Release',
    'S','S=Rel to Warhouse',
    'B','B=Backorder',
    'P','P=Pending Inv',
    'C','C=Shipped',
    'N','N=Not Ready',
    'D','D=Cancelled',
    'X','X=Not Applicable','Unknown: '||DET.RELEASED_STATUS) REL_STATUS,
  to_char(LIN.line_number) || 
          decode(LIN.shipment_number, null, null, '.' || to_char(LIN.shipment_number))|| 
          decode(LIN.option_number, null, null, '.' || to_char(LIN.option_number)) ||
          decode(LIN.component_number, null, null, 
                 decode(LIN.option_number, null, '.',null)||
                 '.'||to_char(LIN.component_number))||
          decode(LIN.service_number,null,null,
                 decode(LIN.component_number, null, '.' , null) ||
                        decode(LIN.option_number, null, '.', null ) ||
                        '.'|| to_char(LIN.service_number)) LINE,
  DET.SOURCE_LINE_ID                LINE_ID,
  DET.INVENTORY_ITEM_ID             ITEM_ID,
  nvl(DET.SRC_REQUESTED_QUANTITY,0) SRQ_Q,
  SRC_REQUESTED_QUANTITY_UOM        SRQ_U,
  nvl(DET.REQUESTED_QUANTITY,0)     REQ_Q,
  SRC_REQUESTED_QUANTITY_UOM        REQ_U,
  nvl(DET.SHIPPED_QUANTITY,0)       SHP_Q,
  nvl(DET.DELIVERED_QUANTITY,0)     DLV_Q,
  nvl(DET.CANCELLED_QUANTITY,0)     CAN_Q,
  nvl(DET.INV_INTERFACED_FLAG,'N')  INI,
  nvl(DET.OE_INTERFACED_FLAG,'N')   OMI,
  DET.SHIP_TOLERANCE_ABOVE          STA,
  DET.SHIP_TOLERANCE_BELOW          STB,
  DET.ORGANIZATION_ID               WH_ID,
  DET.SUBINVENTORY                  CUR_SUB,
  DET.ATTRIBUTE15                   ORG_SUB,
  DET.REVISION                      REV,
  DET.LOT_NUMBER                    LOT,
  DET.SERIAL_NUMBER                 SERIAL,
  DET.LOCATOR_ID                    LOC_ID,
  DET.SPLIT_FROM_DELIVERY_DETAIL_ID SPL_DEL_DET_ID,
  DET.PICKED_QUANTITY               PICKED_QUANTITY,
  substr(wsn.fm_serial_number,1,15)  FROM_SERIAL,
  substr(wsn.to_serial_number,1,15)  TO_SERIAL,
  wsn.quantity                      WSN_QTY,
  to_char(wsn.creation_date,'DD-MON-RR_HH24:MI:SS') WSN_CRE_DATE
FROM 
  OE_ORDER_LINES                   LIN,
  WSH_DELIVERY_DETAILS             DET,
  WSH_SERIAL_NUMBERS               WSN
where DET.DELIVERY_DETAIL_ID          = WSN.DELIVERY_DETAIL_ID
  and DET.SOURCE_LINE_ID              = LIN.LINE_ID 
  and DET.SOURCE_CODE                 = 'OE'
  and LIN.HEADER_ID                   = nvl('&header_id_selected',:v_header_id)
  and NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.REFERENCE_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID)
  and (substr(UPPER(nvl('&det_cnt','Y')),1,1) = 'Y' or rownum <= 10);

r_uom_lin        varchar2(10);
r_qshp_lin       number;
r_sta_lin        varchar2(100);
r_mtl_trn        varchar2(100);
type     per_record_typ is RECORD
               (flag    varchar2(1),
                descrip varchar2(200));
type     msg_tab is TABLE of per_record_typ INDEX by binary_integer;
msg      msg_tab;

function n(v varchar2) return varchar2 is
begin
  if v is null then
   return '&sp';
   else
   return v;
  end if;
end n;

begin
  :r_error := 0;
-- Initialize error messages even if Do Analysis not selected
-- if substr(UPPER(nvl('&do_analysis','Y')),1,1) = 'Y' then
  for i in 1..1
  loop
    msg(i).flag := '0';
    msg(i).descrip := '';
  end loop;
  msg(1).descrip   := '    1. No verifications yet.';
-- end if;

 for dd in w_serial_n
 loop
   :r_flag := '';

-- if substr(UPPER(nvl('&do_analysis','Y')),1,1) = 'Y' then
   -- Include verifications here
--   Select nvl(mtl_transactions_enabled_flag,'N')
--     into r_mtl_trn
--     from Mtl_System_items
--    where Inventory_item_Id = dd.ITEM_ID
--      and Organization_Id   = dd.WH_ID;
--   if dd.PICKABLE_FLAG <> r_mtl_trn then
--     :r_flag := :r_flag || '1 ';
--     msg(1).flag := '1';
--   end if;


-- end if; -- do_analysis
   ---
   -- Print line to Output file
   utl_file.put_line(handle,'&sld &b <a HREF="#WSNERR">'||n(:r_flag)||'</a> &eb &d ');
   utl_file.put_line(handle,n(dd.DEL_DET_ID)||' &d ');
   utl_file.put_line(handle,n(dd.REL_STATUS)||' &d '||n(dd.LINE)||' &d ');
   utl_file.put_line(handle,'<a HREF="#'||dd.LINE_ID||'">'||n(dd.LINE_ID)||'</a> &d ');
   utl_file.put_line(handle,n(dd.ITEM_ID)||' &d');
   utl_file.put_line(handle,n(dd.SRQ_Q)||'&d'||n(dd.SRQ_U)||' &d ');
   utl_file.put_line(handle,n(dd.REQ_Q)||'&d'||n(dd.REQ_U)||' &d ');
   utl_file.put_line(handle,n(dd.PICKED_QUANTITY)||' &d');
   utl_file.put_line(handle,n(dd.SHP_Q)||'&d'||n(dd.DLV_Q)||' &d');
   utl_file.put_line(handle,n(dd.CAN_Q)||'&d'||n(dd.INI)||' &d');
   utl_file.put_line(handle,n(dd.OMI)||'&d'||n(dd.STA)||' &d');
   utl_file.put_line(handle,n(dd.STB)||'&d');
   utl_file.put_line(handle,n(dd.WH_ID)||'&d'||n(dd.CUR_SUB)||' &d');
   utl_file.put_line(handle,n(dd.ORG_SUB)||'&d'||n(dd.REV)||' &d');
   utl_file.put_line(handle,n(dd.LOT)||'&d'||n(dd.SERIAL)||' &d');
   utl_file.put_line(handle,n(dd.LOC_ID)||'&d'||n(dd.SPL_DEL_DET_ID)||' &d');
   utl_file.put_line(handle,n(dd.FROM_SERIAL)||'&d'||n(dd.TO_SERIAL)||' &d');
   utl_file.put_line(handle,n(dd.WSN_QTY)||'&d'||n(dd.WSN_CRE_DATE)||' &el');

   if :r_flag is not null then
    :r_error := 1;
   end if;
 end loop;
 utl_file.put_line(handle,'&et');

 if :r_error = 1 then
   utl_file.put_line(handle,'&f &b <a NAME="WSNERR">Error List:</a> &eb &f');
   for i in 1..1
   loop
     if msg(i).flag = '1' then
       utl_file.put_line(handle,msg(i).descrip||'&f');
     end if;
   end loop;
 end if;

end;

UTL_FILE.PUT_LINE(handle,'&et '); 

UTL_FILE.PUT_LINE(handle,'&f &f WSH_FREIGHT_COSTS (CST) &f');
UTL_FILE.PUT_LINE(handle,'&std &sh FRT_CST_ID &dh FRT_NAME &dh FRT_TYPE &dh FRT_LEVEL &dh QTY &dh UNIT_AMT &dh TOT_AMT &dh ENTITY_ID &eh');

Declare
cursor w_fre_cst is
select distinct 
wfc.FREIGHT_COST_ID         FRT_CST_ID,                
wfc.FREIGHT_COST_TYPE       FRT_NAME,
lkp.Meaning                 FRT_TYPE,
'DELIV_DETAIL'              FRT_LEVEL,
wfc.QUANTITY                QTY,       
wfc.UNIT_AMOUNT             UNIT_AMT,   
wfc.TOTAL_AMOUNT            TOT_AMT,                                    
wfc.DELIVERY_DETAIL_ID      ENTITY_ID
from wsh_freight_costs_v              wfc,
     wsh_freight_cost_types           wft,
     fnd_lookup_values                lkp,
     OE_ORDER_LINES                   LIN,
     WSH_DELIVERY_DETAILS             DET,
     fnd_languages                    FLA
 WHERE
            wfc.freight_cost_type_id      = wft.freight_cost_type_id AND
            wft.freight_cost_type_code    = lkp.lookup_code AND
            lkp.lookup_type               = 'FREIGHT_COST_TYPE' AND
            lkp.LANGUAGE                  = FLA.LANGUAGE_CODE AND
            FLA.INSTALLED_FLAG            = 'B' AND
            DET.SOURCE_LINE_ID            = LIN.LINE_ID AND  
            LIN.HEADER_ID                 = nvl('&header_id_selected',:v_header_id) AND
            NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.REFERENCE_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID) AND
            WFC.DELIVERY_DETAIL_ID        = DET.DELIVERY_DETAIL_ID 
UNION ALL
select distinct
wfc.FREIGHT_COST_ID         FRT_CST_ID,                
wfc.FREIGHT_COST_TYPE       FRT_NAME,
lkp.Meaning                 FRT_TYPE,
'DELIVERY'                  FRT_LEVEL,
wfc.QUANTITY                QTY,       
wfc.UNIT_AMOUNT             UNIT_AMT,   
wfc.TOTAL_AMOUNT            TOT_AMT,                                    
wfc.DELIVERY_ID             ENTITY_ID
from wsh_freight_costs_v              wfc,
     wsh_freight_cost_types           wft,
     fnd_lookup_values                lkp,
     OE_ORDER_LINES                   LIN,
     WSH_DELIVERY_DETAILS             DET,
     --WSH_NEW_DELIVERIES               DEL,
     --WSH_DELIVERY_LEGS                LEG,
     --WSH_TRIP_STOPS                   STP,
     WSH_DELIVERY_ASSIGNMENTS         ASG,
     fnd_languages                    FLA
 WHERE
            wfc.freight_cost_type_id      = wft.freight_cost_type_id AND
            wft.freight_cost_type_code    = lkp.lookup_code AND
            lkp.lookup_type               = 'FREIGHT_COST_TYPE' AND
            lkp.LANGUAGE                  = FLA.LANGUAGE_CODE AND
            FLA.INSTALLED_FLAG            = 'B' AND
            --DEL.DELIVERY_ID               = ASG.DELIVERY_ID AND
            ASG.DELIVERY_DETAIL_ID        = DET.DELIVERY_DETAIL_ID AND
            DET.SOURCE_LINE_ID            = LIN.LINE_ID AND  
           -- STP.STOP_ID                   = LEG.PICK_UP_STOP_ID AND
           -- LEG.DELIVERY_ID               = DEL.DELIVERY_ID AND 
            LIN.HEADER_ID                 = nvl('&header_id_selected',:v_header_id) AND
            NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.REFERENCE_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID) AND
            WFC.DELIVERY_ID              = ASG.DELIVERY_ID AND
            WFC.DELIVERY_DETAIL_ID       IS NULL
UNION ALL
select distinct 
wfc.FREIGHT_COST_ID         FRT_CST_ID,                
wfc.FREIGHT_COST_TYPE       FRT_NAME,
lkp.Meaning                 FRT_TYPE,
'LEG'                       FRT_LEVEL,
wfc.QUANTITY                QTY,       
wfc.UNIT_AMOUNT             UNIT_AMT,   
wfc.TOTAL_AMOUNT            TOT_AMT,                                    
wfc.DELIVERY_LEG_ID         ENTITY_ID
from wsh_freight_costs_v              wfc,
     wsh_freight_cost_types           wft,
     fnd_lookup_values                lkp,
     OE_ORDER_LINES                   LIN,
     WSH_DELIVERY_DETAILS             DET,
     WSH_NEW_DELIVERIES               DEL,
     WSH_DELIVERY_LEGS                LEG,
     --WSH_TRIP_STOPS                   STP,
     WSH_DELIVERY_ASSIGNMENTS         ASG,
     fnd_languages                    FLA
 WHERE
            wfc.freight_cost_type_id      = wft.freight_cost_type_id AND
            wft.freight_cost_type_code    = lkp.lookup_code AND
            lkp.lookup_type               = 'FREIGHT_COST_TYPE' AND
            lkp.LANGUAGE                  = FLA.LANGUAGE_CODE AND
            FLA.INSTALLED_FLAG            = 'B' AND
            DEL.DELIVERY_ID               = ASG.DELIVERY_ID AND
            ASG.DELIVERY_DETAIL_ID        = DET.DELIVERY_DETAIL_ID AND
            DET.SOURCE_LINE_ID            = LIN.LINE_ID AND  
           -- STP.STOP_ID                   = LEG.PICK_UP_STOP_ID AND
            LEG.DELIVERY_ID               = DEL.DELIVERY_ID AND 
            LIN.HEADER_ID                 = nvl('&header_id_selected',:v_header_id) AND
            NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.REFERENCE_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID) AND
            WFC.DELIVERY_LEG_ID          = LEG.DELIVERY_LEG_ID AND
            WFC.DELIVERY_DETAIL_ID       IS NULL AND
            WFC.DELIVERY_ID              IS NULL
UNION ALL
select distinct
wfc.FREIGHT_COST_ID         FRT_CST_ID,                
wfc.FREIGHT_COST_TYPE       FRT_NAME,
lkp.Meaning                 FRT_TYPE,
'STOP'                      FRT_LEVEL,
wfc.QUANTITY                QTY,       
wfc.UNIT_AMOUNT             UNIT_AMT,   
wfc.TOTAL_AMOUNT            TOT_AMT,                                    
wfc.STOP_ID                 ENTITY_ID
from wsh_freight_costs_v              wfc,
     wsh_freight_cost_types           wft,
     fnd_lookup_values                lkp,
     OE_ORDER_LINES                   LIN,
     WSH_DELIVERY_DETAILS             DET,
     WSH_NEW_DELIVERIES               DEL,
     WSH_DELIVERY_LEGS                LEG,
     WSH_TRIP_STOPS                   STP,
     WSH_DELIVERY_ASSIGNMENTS         ASG,
     fnd_languages                    FLA
 WHERE
            wfc.freight_cost_type_id      = wft.freight_cost_type_id AND
            wft.freight_cost_type_code    = lkp.lookup_code AND
            lkp.lookup_type               = 'FREIGHT_COST_TYPE' AND
            lkp.LANGUAGE                  = FLA.LANGUAGE_CODE AND
            FLA.INSTALLED_FLAG            = 'B' AND
            DEL.DELIVERY_ID               = ASG.DELIVERY_ID AND
            ASG.DELIVERY_DETAIL_ID        = DET.DELIVERY_DETAIL_ID AND
            DET.SOURCE_LINE_ID            = LIN.LINE_ID AND  
            STP.STOP_ID                   = LEG.PICK_UP_STOP_ID AND
            LEG.DELIVERY_ID               = DEL.DELIVERY_ID AND 
            LIN.HEADER_ID                 = nvl('&header_id_selected',:v_header_id) AND
            NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.REFERENCE_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID) AND
            WFC.STOP_ID                  = STP.STOP_ID AND
            WFC.DELIVERY_DETAIL_ID       IS NULL AND
            WFC.DELIVERY_ID              IS NULL AND
            WFC.DELIVERY_LEG_ID          IS NULL
UNION ALL
select distinct
wfc.FREIGHT_COST_ID         FRT_CST_ID,                
wfc.FREIGHT_COST_TYPE       FRT_NAME,
lkp.Meaning                 FRT_TYPE,
'TRIP'                      FRT_LEVEL,
wfc.QUANTITY                QTY,       
wfc.UNIT_AMOUNT             UNIT_AMT,   
wfc.TOTAL_AMOUNT            TOT_AMT,                                    
wfc.TRIP_ID                 ENTITY_ID
from wsh_freight_costs_v              wfc,
     wsh_freight_cost_types           wft,
     fnd_lookup_values                lkp,
     OE_ORDER_LINES                   LIN,
     WSH_DELIVERY_DETAILS             DET,
     WSH_NEW_DELIVERIES               DEL,
     WSH_DELIVERY_LEGS                LEG,
     WSH_TRIP_STOPS                   STP,
     WSH_DELIVERY_ASSIGNMENTS         ASG,
     fnd_languages                    FLA
 WHERE
            wfc.freight_cost_type_id      = wft.freight_cost_type_id AND
            wft.freight_cost_type_code    = lkp.lookup_code AND
            lkp.lookup_type               = 'FREIGHT_COST_TYPE' AND
            lkp.LANGUAGE                  = FLA.LANGUAGE_CODE AND
            FLA.INSTALLED_FLAG            = 'B' AND
            DEL.DELIVERY_ID               = ASG.DELIVERY_ID AND
            ASG.DELIVERY_DETAIL_ID        = DET.DELIVERY_DETAIL_ID AND
            DET.SOURCE_LINE_ID            = LIN.LINE_ID AND  
            STP.STOP_ID                   = LEG.PICK_UP_STOP_ID AND
            LEG.DELIVERY_ID               = DEL.DELIVERY_ID AND 
            LIN.HEADER_ID                 = nvl('&header_id_selected',:v_header_id) AND
            NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.REFERENCE_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID) AND
            WFC.TRIP_ID                  = STP.TRIP_ID AND
            WFC.DELIVERY_DETAIL_ID       IS NULL AND
            WFC.DELIVERY_ID              IS NULL AND
            WFC.STOP_ID                  IS NULL AND
            WFC.DELIVERY_LEG_ID          IS NULL;

begin
 for wfc in w_fre_cst
 loop
   utl_file.put_line(handle,'&sld'||n(wfc.FRT_CST_ID)||'&d'||n(wfc.FRT_NAME)||'&d');
   utl_file.put_line(handle,n(wfc.FRT_TYPE)||'&d'||n(wfc.FRT_LEVEL)||'&d');
   utl_file.put_line(handle,n(wfc.QTY)||'&d'||n(wfc.UNIT_AMT)||'&d');
   utl_file.put_line(handle,n(wfc.TOT_AMT)||'&d'||n(wfc.ENTITY_ID)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');

-- Output for table WSH_EXCEPTIONS being commented by Dev. request to improve performance 
--
--UTL_FILE.PUT_LINE(handle,'&f &f WSH_EXCEPTIONS (EXC) &f
--UTL_FILE.PUT_LINE(handle,'NOTE: THESE ARE BASED STRICTLY ON REQUEST_ID, SO MESSAGES MAY NOT APPLY TO THIS SHIPPING TRANSACTION &f
--
--column EXCEPT_NAME   format a20;
--column EXC_STATUS    format a18;
--column ERROR_MSG     format a30;
--
--UTL_FILE.PUT_LINE(handle,'&std &sh EXCEPT_ID &dh EXCEPT_NAME &dh SEV &dh EXC_STATUS &dh TRIP_ID &dh STOP_ID &dh DELIV_ID &dh DEL_DET_ID &dh
--UTL_FILE.PUT_LINE(handle,'ERROR_MSG &dh REQUEST_ID &dh MESSAGE &eh
--
--select distinct   '&sld',
--     EXCEPTION_ID             EXCEPT_ID,        
--     EXCEPTION_NAME           EXCEPT_NAME,      
--     SEVERITY                 SEV,      
--     STATUS                   EXC_STATUS,      
--     TRIP_ID                  TRIP_ID,      
--     TRIP_STOP_ID             STOP_ID,      
--     DELIVERY_ID              DELIV_ID,      
--     DELIVERY_DETAIL_ID       DEL_DET_ID,      
--     ERROR_MESSAGE            ERROR_MSG,
--     REQUEST_ID               REQUEST_ID,
--     MESSAGE                  MESSAGE,'&el'
--FROM WSH_EXCEPTIONS   EXC
--where STATUS <> 'NO_ACTION_REQUIRED'
--  and EXC.REQUEST_ID IN (
--     select DET.request_id
--     from OE_ORDER_LINES                   LIN,
--          WSH_DELIVERY_DETAILS             DET
--      WHERE
--                 DET.SOURCE_LINE_ID            = LIN.LINE_ID AND  
--                 LIN.HEADER_ID                 = nvl('&header_id_selected',:v_header_id) AND
--                 NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
--                                              LIN.TOP_MODEL_LINE_ID,
--                                              LIN.ATO_LINE_ID,
--                                              LIN.LINK_TO_LINE_ID,
--                                              LIN.REFERENCE_LINE_ID,
--                                              LIN.SERVICE_REFERENCE_LINE_ID)
--     UNION ALL
--     select ASG.REQUEST_ID
--     from OE_ORDER_LINES                   LIN,
--          WSH_DELIVERY_DETAILS             DET,
--          WSH_DELIVERY_ASSIGNMENTS         ASG
--      WHERE
--                 ASG.DELIVERY_DETAIL_ID        = DET.DELIVERY_DETAIL_ID AND
--                 DET.SOURCE_LINE_ID            = LIN.LINE_ID AND  
--                 LIN.HEADER_ID                 = nvl('&header_id_selected',:v_header_id) AND
--                 NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
--                                              LIN.TOP_MODEL_LINE_ID,
--                                              LIN.ATO_LINE_ID,
--                                              LIN.LINK_TO_LINE_ID,
--                                              LIN.REFERENCE_LINE_ID,
--                                              LIN.SERVICE_REFERENCE_LINE_ID) 
--     UNION ALL
--     select LEG.REQUEST_ID
--     from OE_ORDER_LINES                   LIN,
--          WSH_DELIVERY_DETAILS             DET,
--          WSH_NEW_DELIVERIES               DEL,
--          WSH_DELIVERY_LEGS                LEG,
--          WSH_DELIVERY_ASSIGNMENTS         ASG
--      WHERE
--                 DEL.DELIVERY_ID               = ASG.DELIVERY_ID AND
--                 ASG.DELIVERY_DETAIL_ID        = DET.DELIVERY_DETAIL_ID AND
--                 DET.SOURCE_LINE_ID            = LIN.LINE_ID AND  
--                 LEG.DELIVERY_ID               = DEL.DELIVERY_ID AND 
--                 LIN.HEADER_ID                 = nvl('&header_id_selected',:v_header_id) AND
--                 NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
--                                              LIN.TOP_MODEL_LINE_ID,
--                                              LIN.ATO_LINE_ID,
--                                              LIN.LINK_TO_LINE_ID,
--                                         LIN.REFERENCE_LINE_ID,
--                                              LIN.SERVICE_REFERENCE_LINE_ID) 
--     UNION ALL
--     select STP.REQUEST_ID
--     from OE_ORDER_LINES                   LIN,
--          WSH_DELIVERY_DETAILS             DET,
--          WSH_NEW_DELIVERIES               DEL,          WSH_DELIVERY_LEGS                LEG,
--          WSH_TRIP_STOPS                   STP,
--          WSH_DELIVERY_ASSIGNMENTS         ASG
--      WHERE      DEL.DELIVERY_ID               = ASG.DELIVERY_ID AND
--                 ASG.DELIVERY_DETAIL_ID        = DET.DELIVERY_DETAIL_ID AND
--                 DET.SOURCE_LINE_ID            = LIN.LINE_ID AND  
--                 STP.STOP_ID                   = LEG.PICK_UP_STOP_ID AND
--                 LEG.DELIVERY_ID               = DEL.DELIVERY_ID AND 
--                 LIN.HEADER_ID                 = nvl('&header_id_selected',:v_header_id) AND
--                 NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
--                                              LIN.TOP_MODEL_LINE_ID,
--                                              LIN.ATO_LINE_ID,
--                                              LIN.LINK_TO_LINE_ID,
--                                              LIN.REFERENCE_LINE_ID,
--                                              LIN.SERVICE_REFERENCE_LINE_ID)
--     UNION ALL
--     select TRP.REQUEST_ID
--     from OE_ORDER_LINES                   LIN,
--          WSH_DELIVERY_DETAILS             DET,
--          WSH_NEW_DELIVERIES               DEL,
--          WSH_DELIVERY_LEGS                LEG,
--          WSH_TRIP_STOPS                   STP,
--          WSH_TRIPS                        TRP,
--          WSH_DELIVERY_ASSIGNMENTS         ASG
--      WHERE      DEL.DELIVERY_ID               = ASG.DELIVERY_ID AND
--                 ASG.DELIVERY_DETAIL_ID        = DET.DELIVERY_DETAIL_ID AND
--                 DET.SOURCE_LINE_ID            = LIN.LINE_ID AND  
--                 STP.STOP_ID                   = LEG.PICK_UP_STOP_ID AND
--                 TRP.TRIP_ID                   = STP.TRIP_ID AND
--                 LEG.DELIVERY_ID               = DEL.DELIVERY_ID AND 
--                 LIN.HEADER_ID                 = nvl('&header_id_selected',:v_header_id) AND
--                 NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
--                                              LIN.TOP_MODEL_LINE_ID,
--                                              LIN.ATO_LINE_ID,
--                                              LIN.LINK_TO_LINE_ID,
--                                              LIN.REFERENCE_LINE_ID,
--                                              LIN.SERVICE_REFERENCE_LINE_ID) 
--);
--



end if; -- :v_head_only

   UTL_FILE.FCLOSE(handle);
end;
/


DECLARE
  handle UTL_FILE.FILE_TYPE;
  dirname varchar2(1000);
  text    varchar2(1000);

function n(v varchar2) return varchar2 is
begin
  if v is null then
   return '&sp';
   else
   return v;
  end if;
end n;

begin
-- Append to output file
  handle := UTL_FILE.FOPEN('&out_dir','&out_dir/&out_file','A',32000);
  UTL_FILE.PUT_LINE(handle,'&et'); -- in case last one failed

If :v_head_only = 'N' then 

if substr(UPPER(nvl('&prt_wms','Y')),1,1) = 'Y' then

UTL_FILE.PUT_LINE(handle,'&f &f <a NAME="WMS_RULES"> WMS_RULES </a> &f');
UTL_FILE.PUT_LINE(handle,'&std &sh RULE ID &dh CREATE DATE &dh ORG_ID &dh TYPE CODE &dh NAME &dh ');
UTL_FILE.PUT_LINE(handle,'DESCRIPTION &dh QTY_FUNCT PARAM &dh ENABLED FLAG &dh USER FLAG &dh TYPE HRD_ID &dh ');
UTL_FILE.PUT_LINE(handle,'RULE WEIGHT &dh ATTR CATEG &dh MIN_PICK TASK &dh ALLOCAT MODE &dh WMS_ENABLED FLAG &eh ');

Declare cursor wms_r is
Select
       WMSR.RULE_ID             ruleid,
       WMSR.CREATION_DATE       credt,
       WMSR.ORGANIZATION_ID     orgid,
       WMSR.TYPE_CODE           typc,
       WMSR.NAME                nam,
       WMSR.DESCRIPTION         des,
       WMSR.QTY_FUNCTION_PARAMETER_ID qty_funct,
       WMSR.ENABLED_FLAG        enabf,
       WMSR.USER_DEFINED_FLAG   userf,
       WMSR.TYPE_HDR_ID         typh,
       WMSR.RULE_WEIGHT         rulew,
       WMSR.ATTRIBUTE_CATEGORY  attrcat,
       WMSR.MIN_PICK_TASKS_FLAG minpick,
       WMSR.ALLOCATION_MODE_ID  allom,
       WMSR.WMS_ENABLED_FLAG    wmsf
  FROM WMS_RULES                WMSR,
       WSH_DELIVERY_DETAILS     DET,
       OE_ORDER_LINES           LIN
where DET.SOURCE_LINE_ID      = LIN.LINE_ID
  and DET.SOURCE_CODE         = 'OE'
  and WMSR.ORGANIZATION_ID    = DET.ORGANIZATION_ID
  and DET.SOURCE_HEADER_ID    = nvl('&header_id_selected',:v_header_id);


begin

 for wms in wms_r
 loop

  if substr(UPPER(nvl('&do_analysis','Y')),1,1) = 'Y' then
    null;
  end if; -- do_analysis

   -- Print line to Output file
   utl_file.put_line(handle,'&sld '||n(wms.ruleid)||' &d ');
   utl_file.put_line(handle,n(wms.credt)||' &d ');
   utl_file.put_line(handle,n(wms.orgid)||' &d '||n(wms.typc)||' &d ');
   utl_file.put_line(handle,n(wms.nam)||' &d '||n(wms.des)||' &d ');
   utl_file.put_line(handle,n(wms.qty_funct)||' &d '||n(wms.enabf)||' &d ');
   utl_file.put_line(handle,n(wms.userf)||' &d '||n(wms.typh)||' &d ');
   utl_file.put_line(handle,n(wms.rulew)||' &d '||n(wms.attrcat)||' &d ');
   utl_file.put_line(handle,n(wms.minpick)||' &d '||n(wms.allom)||' &d ');
   utl_file.put_line(handle,n(wms.wmsf)||' &el ');

 end loop;

end;

UTL_FILE.PUT_LINE(handle,'&et '); 


UTL_FILE.PUT_LINE(handle,'&f &f <a NAME="WMS_RULE_CONSISTENCIES"> WMS_RULE_CONSISTENCIES </a> &f');
UTL_FILE.PUT_LINE(handle,'&std &sh RULE ID &dh CONSISTENCY ID &dh PARAMETER ID &dh CREATE DATE &dh ATTR CATEG &eh ');

Declare cursor wms_rc is
Select WRC.RULE_ID              ruleid,
       WRC.CONSISTENCY_ID       consid,
       WRC.PARAMETER_ID         paramid,
       WRC.CREATION_DATE        credt,
       WRC.ATTRIBUTE_CATEGORY   attrcat
  FROM WMS_RULE_CONSISTENCIES   WRC,
       WMS_RULES                WMSR,
       WSH_DELIVERY_DETAILS     DET,
       OE_ORDER_LINES           LIN
 WHERE WRC.RULE_ID             = WMSR.RULE_ID
   and DET.SOURCE_LINE_ID      = LIN.LINE_ID
   and DET.SOURCE_CODE         = 'OE'
   and WMSR.ORGANIZATION_ID    = DET.ORGANIZATION_ID
   and DET.SOURCE_HEADER_ID    = nvl('&header_id_selected',:v_header_id)
 ORDER BY 1;


begin

 for wms in wms_rc
 loop

  if substr(UPPER(nvl('&do_analysis','Y')),1,1) = 'Y' then
    null;
  end if; -- do_analysis

   -- Print line to Output file
   utl_file.put_line(handle,'&sld '||n(wms.ruleid)||' &d ');
   utl_file.put_line(handle,n(wms.consid)||' &d '||n(wms.paramid)||' &d ');
   utl_file.put_line(handle,n(wms.credt)||' &d '||n(wms.attrcat)||' &el ');

 end loop;

end;

UTL_FILE.PUT_LINE(handle,'&et '); 


UTL_FILE.PUT_LINE(handle,'&f &f <a NAME="WMS_RESTRICTIONS"> WMS_RESTRICTIONS </a> &f');
UTL_FILE.PUT_LINE(handle,'&std &sh RULE ID &dh SEQUENCE NUMBER &dh CREATE DATE &dh PARAMETER ID &dh ');
UTL_FILE.PUT_LINE(handle,'OPERAND CODE &dh OPERAND TYPE &dh OPER_NUMBER CONSTANT &dh OPER_CHAR CONSTANT &dh ');
UTL_FILE.PUT_LINE(handle,'OPER_DATE CONSTANT &dh OPER PARAM_ID &dh OPER EXPRESSION &dh OPER_FLEX VALUE_SET &dh ');
UTL_FILE.PUT_LINE(handle,'LOGICAL OPER_CODE &dh BRACKET OPEN &dh BRACKET CLOSE &dh ATTR CATEG &eh ');

Declare cursor wms_res is
Select WRES.RULE_ID                      ruleid,
       WRES.SEQUENCE_NUMBER              seqno, 
       WRES.CREATION_DATE                credt,
       WRES.PARAMETER_ID                 paramid,
       WRES.OPERATOR_CODE                opcode,
       WRES.OPERAND_TYPE_CODE            optype,
       WRES.OPERAND_CONSTANT_NUMBER      opnum,
       WRES.OPERAND_CONSTANT_CHARACTER   opchar,
       WRES.OPERAND_CONSTANT_DATE        opdate,
       WRES.OPERAND_PARAMETER_ID         oppar,
       WRES.OPERAND_EXPRESSION           opexp,
       WRES.OPERAND_FLEX_VALUE_SET_ID    opfvs,
       WRES.LOGICAL_OPERATOR_CODE        logop,
       WRES.BRACKET_OPEN                 braop,
       WRES.BRACKET_CLOSE                bracl,
       WRES.ATTRIBUTE_CATEGORY           attrcat
  FROM WMS_RESTRICTIONS         WRES,
       WMS_RULES                WMSR,
       WSH_DELIVERY_DETAILS     DET,
       OE_ORDER_LINES           LIN
 WHERE WRES.RULE_ID            = WMSR.RULE_ID
   and DET.SOURCE_LINE_ID      = LIN.LINE_ID
   and DET.SOURCE_CODE         = 'OE'
   and WMSR.ORGANIZATION_ID    = DET.ORGANIZATION_ID
   and DET.SOURCE_HEADER_ID    = nvl('&header_id_selected',:v_header_id)
 ORDER BY 1;


begin

 for wms in wms_res
 loop

  if substr(UPPER(nvl('&do_analysis','Y')),1,1) = 'Y' then
    null;
  end if; -- do_analysis

   -- Print line to Output file
   utl_file.put_line(handle,'&sld '||n(wms.ruleid)||' &d ');
   utl_file.put_line(handle,n(wms.seqno)||' &d '||n(wms.credt)||' &d ');
   utl_file.put_line(handle,n(wms.paramid)||' &d '||n(wms.opcode)||' &d ');
   utl_file.put_line(handle,n(wms.optype)||' &d '||n(wms.opnum)||' &d ');
   utl_file.put_line(handle,n(wms.opchar)||' &d '||n(wms.opdate)||' &d ');
   utl_file.put_line(handle,n(wms.oppar)||' &d '||n(wms.opexp)||' &d ');
   utl_file.put_line(handle,n(wms.opfvs)||' &d '||n(wms.logop)||' &d ');
   utl_file.put_line(handle,n(wms.braop)||' &d '||n(wms.bracl)||' &d ');
   utl_file.put_line(handle,n(wms.attrcat)||' &el ');

 end loop;

end;

UTL_FILE.PUT_LINE(handle,'&et '); 


UTL_FILE.PUT_LINE(handle,'&f &f <a NAME="WMS_SORT_CRITERIA"> WMS_SORT_CRITERIA </a> &f');
UTL_FILE.PUT_LINE(handle,'&std &sh RULE ID &dh SEQUENCE NUMBER &dh CREATE DATE &dh PARAMETER ID &dh ');
UTL_FILE.PUT_LINE(handle,'OPERAND CODE &dh ATTR CATEG &eh ');

Declare cursor wms_sc is
Select WSOC.RULE_ID             ruleid,
       WSOC.SEQUENCE_NUMBER     seqno,
       WSOC.CREATION_DATE       credt,
       WSOC.PARAMETER_ID        paramid,
       WSOC.ORDER_CODE          opcode,
       WSOC.ATTRIBUTE_CATEGORY  attrcat
  FROM WMS_SORT_CRITERIA        WSOC,
       WMS_RULES                WMSR,
       WSH_DELIVERY_DETAILS     DET,
       OE_ORDER_LINES           LIN
 WHERE WSOC.RULE_ID            = WMSR.RULE_ID
   and DET.SOURCE_LINE_ID      = LIN.LINE_ID
   and DET.SOURCE_CODE         = 'OE'
   and WMSR.ORGANIZATION_ID    = DET.ORGANIZATION_ID
   and DET.SOURCE_HEADER_ID    = nvl('&header_id_selected',:v_header_id)
 ORDER BY 1;


begin

 for wms in wms_sc
 loop

  if substr(UPPER(nvl('&do_analysis','Y')),1,1) = 'Y' then
    null;
  end if; -- do_analysis

   -- Print line to Output file
   utl_file.put_line(handle,'&sld '||n(wms.ruleid)||' &d ');
   utl_file.put_line(handle,n(wms.seqno)||' &d '||n(wms.credt)||' &d ');
   utl_file.put_line(handle,n(wms.paramid)||' &d '||n(wms.opcode)||' &d ');
   utl_file.put_line(handle,n(wms.attrcat)||' &el ');

 end loop;

end;

UTL_FILE.PUT_LINE(handle,'&et '); 


UTL_FILE.PUT_LINE(handle,'&f &f <a NAME="WMS_SELECTION_CRITERIA_TXN_V"> WMS_SELECTION_CRITERIA_TXN_V </a> &f');
UTL_FILE.PUT_LINE(handle,'&std &sh SEQUENCE NUMBER &dh RULE TYPE_CODE &dh RULE TYPE &dh RETURN TYPE &dh ');
UTL_FILE.PUT_LINE(handle,'RETURN TYPE_ID &dh RETURN TYPE_NAME &dh ENABLED FLAG &dh DATE TYPE_COD &dh ');
UTL_FILE.PUT_LINE(handle,'DATE TYPE_FROM &dh DATE TYPE_TO &dh DATE_TYPE LOOKUP_TYPE &dh EFFECTIVE FROM &dh ');
UTL_FILE.PUT_LINE(handle,'EFFECTIVE TO &dh FROM ORGANIZATION_ID &dh FROM ORGANIZATION_CODE &dh FROM SUBINVENT_NAME &dh ');
UTL_FILE.PUT_LINE(handle,'TO ORGANIZATION_ID &dh TO ORGANIZATION_CODE &dh TO SUBINVENT_NAME &dh CUSTOMER ID &dh ');
UTL_FILE.PUT_LINE(handle,'CUSTOMER NAME &dh FREIGHT CODE &dh FREIGHT CODE_NAME &dh INVENTORY ITEM_ID &dh ITEM &dh ');
UTL_FILE.PUT_LINE(handle,'ITEM TYPE &dh ITEM TYPE_NAME &dh ASSIGNMENT GROUP_ID &dh ABC CLASS_ID &dh ');
UTL_FILE.PUT_LINE(handle,'ABC_GROUP CLASS_NAME &dh CATEGORY SET_ID &dh CATEGORY ID &dh CATEGORY SET_NAME &dh ');
UTL_FILE.PUT_LINE(handle,'ORDER TYPE_ID &dh ORDER TYPE_NAME &dh VENDOR ID &dh VENDOR NAME &dh PROJECT ID &dh ');
UTL_FILE.PUT_LINE(handle,'PROJECT NAME &dh TASK ID &dh TASK NAME &dh USER ID &dh USER NAME &dh TRANSACTION ACTION_ID &dh ');
UTL_FILE.PUT_LINE(handle,'TRANSACTION ACTION_NAME &dh REASON ID &dh REASON NAME &dh TRANSACTION SOURCE_TYPE_ID &dh ');
UTL_FILE.PUT_LINE(handle,'TRANSACTION_SOURCE TYPE_NAME &dh TRANSACTION TYPE_ID &dh TRANSACTION TYPE_NAME &dh ');
UTL_FILE.PUT_LINE(handle,'UOM_CODE &dh UNIT_OF MEASURE &dh UOM CLASS &dh UOM_CLASS NAME &dh CREATION DATE &dh ');
UTL_FILE.PUT_LINE(handle,'WMS_ENABLED FLAG &dh LOCATION ID &dh LOCATION NAME &eh ');

Declare cursor wms_sct is
Select WSCT.STG_ASSIGNMENT_ID            stgasg,
       WSCT.SEQUENCE_NUMBER              seqno,
       WSCT.RULE_TYPE_CODE               rultyco,
       WSCT.RULE_TYPE                    rulty,
       WSCT.RETURN_TYPE                  retty,
       WSCT.RETURN_TYPE_ID               rettyid,
       WSCT.RETURN_TYPE_NAME             rettyna,
       WSCT.ENABLED_FLAG                 enaf,
       WSCT.DATE_TYPE_CODE               dttyco,
       WSCT.DATE_TYPE_FROM               dttyfr,
       WSCT.DATE_TYPE_TO                 dttyto,
       WSCT.DATE_TYPE_LOOKUP_TYPE        dttylty,
       WSCT.EFFECTIVE_FROM               efffr,
       WSCT.EFFECTIVE_TO                 effto,
       WSCT.FROM_ORGANIZATION_ID         frorgid,
       WSCT.FROM_ORGANIZATION_CODE       frorgco,
       WSCT.FROM_SUBINVENTORY_NAME       frsubna,
       WSCT.TO_ORGANIZATION_ID           toorgid,
       WSCT.TO_ORGANIZATION_CODE         toorgco,
       WSCT.TO_SUBINVENTORY_NAME         tosubna,
       WSCT.CUSTOMER_ID                  cusid,
       WSCT.CUSTOMER_NAME                cusna,
       WSCT.FREIGHT_CODE                 fcod,
       WSCT.FREIGHT_CODE_NAME            fcna,
       WSCT.INVENTORY_ITEM_ID            inviid,
       WSCT.ITEM                         it,
       WSCT.ITEM_TYPE                    ityp,
       WSCT.ITEM_TYPE_NAME               itypna,
       WSCT.ASSIGNMENT_GROUP_ID          assgid,
       WSCT.ABC_CLASS_ID                 abcid,
       WSCT.ABC_GROUP_CLASS_NAME         abcna,
       WSCT.CATEGORY_SET_ID              csid,
       WSCT.CATEGORY_ID                  catid,
       WSCT.CATEGORY_SET_NAME            csna,
       WSCT.ORDER_TYPE_ID                otid,
       WSCT.ORDER_TYPE_NAME              otna,
       WSCT.VENDOR_ID                    vid,
       WSCT.VENDOR_NAME                  vna,
       WSCT.PROJECT_ID                   pid,
       WSCT.PROJECT_NAME                 pna,
       WSCT.TASK_ID                      tid,
       WSCT.TASK_NAME                    tna,
       WSCT.USER_ID                      usid,
       WSCT.USER_NAME                    usna,
       WSCT.TRANSACTION_ACTION_ID        taid,
       WSCT.TRANSACTION_ACTION_NAME      tana,
       WSCT.REASON_ID                    resid,
       WSCT.REASON_NAME                  resna,
       WSCT.TRANSACTION_SOURCE_TYPE_ID   tstid, 
       WSCT.TRANSACTION_SOURCE_TYPE_NAME tstna,
       WSCT.TRANSACTION_TYPE_ID          ttid,
       WSCT.TRANSACTION_TYPE_NAME        ttna,
       WSCT.UOM_CODE                     uomco,
       WSCT.UNIT_OF_MEASURE              uomna,
       WSCT.UOM_CLASS                    uomcl,
       WSCT.UOM_CLASS_NAME               uomclna,
       WSCT.CREATION_DATE                credt,
       WSCT.WMS_ENABLED_FLAG             wmsef,
       WSCT.LOCATION_ID                  locid,
       WSCT.LOCATION_NAME                locna
  FROM WMS_SELECTION_CRITERIA_TXN_V WSCT,
       WSH_DELIVERY_DETAILS         DET,
       OE_ORDER_LINES               LIN
 where DET.SOURCE_LINE_ID         = LIN.LINE_ID
   and DET.SOURCE_CODE            = 'OE'
   and WSCT.FROM_ORGANIZATION_ID  = DET.ORGANIZATION_ID
   and DET.SOURCE_HEADER_ID       = nvl('&header_id_selected',:v_header_id);


begin

 for wms in wms_sct
 loop

  if substr(UPPER(nvl('&do_analysis','Y')),1,1) = 'Y' then
    null;
  end if; -- do_analysis

   -- Print line to Output file
   utl_file.put_line(handle,'&sld '||n(wms.stgasg)||' &d ');
   utl_file.put_line(handle,n(wms.seqno)||' &d '||n(wms.rultyco)||' &d ');
   utl_file.put_line(handle,n(wms.rulty)||' &d '||n(wms.retty)||' &d ');
   utl_file.put_line(handle,n(wms.rettyid)||' &d '||n(wms.rettyna)||' &d ');
   utl_file.put_line(handle,n(wms.enaf)||' &d '||n(wms.dttyco)||' &d ');
   utl_file.put_line(handle,n(wms.dttyfr)||' &d '||n(wms.dttyto)||' &d ');
   utl_file.put_line(handle,n(wms.dttylty)||' &d '||n(wms.efffr)||' &d ');
   utl_file.put_line(handle,n(wms.effto)||' &d '||n(wms.frorgid)||' &d ');
   utl_file.put_line(handle,n(wms.frorgco)||' &d '||n(wms.frsubna)||' &d ');
   utl_file.put_line(handle,n(wms.toorgid)||' &d '||n(wms.toorgco)||' &d ');
   utl_file.put_line(handle,n(wms.tosubna)||' &d '||n(wms.cusid)||' &d ');
   utl_file.put_line(handle,n(wms.cusna)||' &d '||n(wms.fcod)||' &d ');
   utl_file.put_line(handle,n(wms.fcna)||' &d '||n(wms.inviid)||' &d ');
   utl_file.put_line(handle,n(wms.it)||' &d '||n(wms.ityp)||' &d ');
   utl_file.put_line(handle,n(wms.itypna)||' &d '||n(wms.assgid)||' &d ');
   utl_file.put_line(handle,n(wms.abcid)||' &d '||n(wms.abcna)||' &d ');
   utl_file.put_line(handle,n(wms.csid)||' &d '||n(wms.catid)||' &d ');
   utl_file.put_line(handle,n(wms.csna)||' &d '||n(wms.vid)||' &d ');
   utl_file.put_line(handle,n(wms.otna)||' &d '||n(wms.pid)||' &d ');
   utl_file.put_line(handle,n(wms.vna)||' &d '||n(wms.tid)||' &d ');
   utl_file.put_line(handle,n(wms.pna)||' &d '||n(wms.usid)||' &d ');
   utl_file.put_line(handle,n(wms.usna)||' &d '||n(wms.taid)||' &d ');
   utl_file.put_line(handle,n(wms.tana)||' &d '||n(wms.resid)||' &d ');
   utl_file.put_line(handle,n(wms.resna)||' &d '||n(wms.tstid)||' &d ');
   utl_file.put_line(handle,n(wms.tstna)||' &d '||n(wms.ttid)||' &d ');
   utl_file.put_line(handle,n(wms.ttna)||' &d '||n(wms.uomco)||' &d ');
   utl_file.put_line(handle,n(wms.uomna)||' &d '||n(wms.uomcl)||' &d ');
   utl_file.put_line(handle,n(wms.uomclna)||' &d '||n(wms.credt)||' &d ');
   utl_file.put_line(handle,n(wms.wmsef)||' &d '||n(wms.locid)||' &d ');
   utl_file.put_line(handle,n(wms.locna)||' &el ');

 end loop;

end;

UTL_FILE.PUT_LINE(handle,'&et '); 


end if; --prt_wms



if substr(UPPER(nvl('&prt_inv','Y')),1,1) = 'Y' then

UTL_FILE.PUT_LINE(handle,'&f &f <a NAME="MTL_TRANSACTIONS_INTERFACE"> MTL_TRANSACTIONS_INTERFACE (MTI) </a> <a HREF="#MTI">Column Definitions</a> &f');
UTL_FILE.PUT_LINE(handle,'&std &sh TRX_INT_ID &dh LINE &dh LINE_ID &dh DELIVERY DETAIL_ID &dh ITEM &dh PICKING LINE &dh TRANSACT TYPE &dh ');
UTL_FILE.PUT_LINE(handle,'TRANSACTION DATE &dh PRIM QTY &dh FROM_SUB &dh FROM LOC_ID &dh PROCESS &dh LOCK &dh TRANSACTION MODE &dh LPN_ID &dh ');
UTL_FILE.PUT_LINE(handle,'ERROR CODE &dh ERROR EXPLANATION &dh SECONDARY UOM_CODE &dh SECONDARY TRANS_QTY &DH ');
UTL_FILE.PUT_LINE(handle,'ACCOUNT PERIOD_ID &dh PERIOD NAME &dh PERIOD OPEN &dh PERIOD START_DT &dh PERIOD CLOSE_DT ');
UTL_FILE.PUT_LINE(handle,'&dh SCHEDULE CLOSE_DT &eh');

Declare
cursor mtl_trx_int is
SELECT   
 TMP.TRANSACTION_INTERFACE_ID     TXN_ID,
 to_char(LIN.line_number) || 
          decode(LIN.shipment_number, null, null, '.' || to_char(LIN.shipment_number))|| 
          decode(LIN.option_number, null, null, '.' || to_char(LIN.option_number)) ||
          decode(LIN.component_number, null, null, 
                 decode(LIN.option_number, null, '.',null)||
                 '.'||to_char(LIN.component_number))||
          decode(LIN.service_number,null,null,
                 decode(LIN.component_number, null, '.' , null) ||
                        decode(LIN.option_number, null, '.', null ) ||
                        '.'|| to_char(LIN.service_number)) LINE,
 LIN.LINE_ID                      LINE_ID,
 DET.DELIVERY_DETAIL_ID           DEL_DETAIL_ID,
 ITM.SEGMENT1                     ITEM,
 TMP.PICKING_LINE_ID              PICK_LN_ID,
 decode(TMP.TRANSACTION_TYPE_ID,
	 52,'52-Stage Trans',
	 33,'33-SO Issue',
	 15,'15-RMA Receipt',
	 18,'18-PO Receipt',
	 TMP.TRANSACTION_TYPE_ID||'-Unknown')    TXN_TYPE,
 TMP.TRANSACTION_DATE             TXN_DATE,
 TMP.PRIMARY_QUANTITY             PRM_Q,
 TMP.SUBINVENTORY_CODE            FROM_SUB,
 TMP.LOCATOR_ID                   FROM_LOC_ID,
 TMP.PROCESS_FLAG                 PROCESS,
 TMP.LOCK_FLAG                    LCK,
 TMP.TRANSACTION_MODE             TRANS_MODE,
 TMP.CONTENT_LPN_ID               LPN_ID,
 TMP.ERROR_CODE                   ERROR_CODE,    
 TMP.ERROR_EXPLANATION            ERROR_EXPL,
 TMP.SECONDARY_UOM_CODE           SEC_UOM_CODE,
 TMP.SECONDARY_TRANSACTION_QUANTITY SEC_TRN_QTY,
 TMP.ACCT_PERIOD_ID               ACCT_PER,
 OAC.period_name                  per_name,
 OAC.open_flag                    open_fl,
 to_char(oac.period_start_date,'DD-MON-RR_HH24:MI:SS')   per_sd,
 to_char(oac.period_close_date,'DD-MON-RR_HH24:MI:SS')   per_cd,
 to_char(oac.schedule_close_date,'DD-MON-RR_HH24:MI:SS') sch_cd
FROM 
    MTL_TRANSACTIONS_INTERFACE        TMP,
    WSH_DELIVERY_DETAILS              DET,
    OE_ORDER_LINES                    LIN,
    MTL_SYSTEM_ITEMS                  ITM,
    ORG_ACCT_PERIODS                  OAC
WHERE 
      TMP.SOURCE_LINE_ID = LIN.LINE_ID
  and LIN.LINE_CATEGORY_CODE = 'ORDER' 
  and LIN.SHIP_FROM_ORG_ID   = ITM.ORGANIZATION_ID(+) 
  and LIN.INVENTORY_ITEM_ID  = ITM.INVENTORY_ITEM_ID(+)  
  and DET.SOURCE_LINE_ID     = LIN.LINE_ID
  and TMP.PICKING_LINE_ID    = DET.DELIVERY_DETAIL_ID(+)
  and TMP.ACCT_PERIOD_ID     = OAC.acct_period_id
  and LIN.HEADER_ID          = nvl('&header_id_selected',:v_header_id) 
  and NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.REFERENCE_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID)
UNION ALL
SELECT 
 TMP.TRANSACTION_INTERFACE_ID     TXN_ID,
 to_char(LIN.line_number) || 
          decode(LIN.shipment_number, null, null, '.' || to_char(LIN.shipment_number))|| 
          decode(LIN.option_number, null, null, '.' || to_char(LIN.option_number)) ||
          decode(LIN.component_number, null, null, 
                 decode(LIN.option_number, null, '.',null)||
                 '.'||to_char(LIN.component_number))||
          decode(LIN.service_number,null,null,
                 decode(LIN.component_number, null, '.' , null) ||
                        decode(LIN.option_number, null, '.', null ) ||
                        '.'|| to_char(LIN.service_number)) LINE,
 LIN.LINE_ID                      LINE_ID,
 DET.DELIVERY_DETAIL_ID           DEL_DETAIL_ID,
 ITM.SEGMENT1                     ITEM,
 TMP.PICKING_LINE_ID              PICK_LN_ID,
 decode(TMP.TRANSACTION_TYPE_ID,
	 52,'Stage Trans',
	 33,'SO Issue',
	 15,'RMA Receipt',
	 18,'PO Receipt',
	TMP.TRANSACTION_TYPE_ID)    TXN_TYPE,
 TMP.TRANSACTION_DATE             TXN_DATE,
 TMP.PRIMARY_QUANTITY             PRM_Q,
 TMP.SUBINVENTORY_CODE            FROM_SUB,
 TMP.LOCATOR_ID                   FROM_LOC_ID,
 TMP.PROCESS_FLAG                 PROCESS,
 TMP.LOCK_FLAG                    LCK,
 TMP.TRANSACTION_MODE             TRANS_MODE,
 TMP.CONTENT_LPN_ID               LPN_ID,
 TMP.ERROR_CODE                   ERROR_CODE,    
 TMP.ERROR_EXPLANATION            ERROR_EXPL,
 TMP.SECONDARY_UOM_CODE           SEC_UOM_CODE,
 TMP.SECONDARY_TRANSACTION_QUANTITY SEC_TRN_QTY,
 TMP.ACCT_PERIOD_ID               ACCT_PER,
 OAC.period_name                  per_name,
 OAC.open_flag                    open_fl,
 to_char(oac.period_start_date,'DD-MON-RR_HH24:MI:SS')   per_sd,
 to_char(oac.period_close_date,'DD-MON-RR_HH24:MI:SS')   per_cd,
 to_char(oac.schedule_close_date,'DD-MON-RR_HH24:MI:SS') sch_cd
FROM 
    MTL_TRANSACTIONS_INTERFACE        TMP,
    WSH_DELIVERY_DETAILS              DET,
    OE_ORDER_LINES                    LIN,
    MTL_SYSTEM_ITEMS                  ITM,
    ORG_ACCT_PERIODS                  OAC
WHERE 
      TMP.TRX_SOURCE_LINE_ID = LIN.LINE_ID
  and LIN.LINE_CATEGORY_CODE = 'RETURN' 
  and LIN.SHIP_FROM_ORG_ID   = ITM.ORGANIZATION_ID(+) 
  and LIN.INVENTORY_ITEM_ID  = ITM.INVENTORY_ITEM_ID(+)  
  and DET.SOURCE_LINE_ID     = LIN.LINE_ID
  and TMP.PICKING_LINE_ID    = DET.DELIVERY_DETAIL_ID(+)
  and TMP.ACCT_PERIOD_ID     = OAC.acct_period_id
  and LIN.HEADER_ID          = nvl('&header_id_selected',:v_header_id) 
  and NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.REFERENCE_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID);

begin
 for mti in mtl_trx_int
 loop
   utl_file.put_line(handle,'&sld'||n(mti.TXN_ID)||' &d ');
   utl_file.put_line(handle,n(mti.LINE)||' &d '||n(mti.LINE_ID)||' &d ');
   utl_file.put_line(handle,n(mti.DEL_DETAIL_ID)||' &d'||n(mti.ITEM)||' &d ');
   utl_file.put_line(handle,n(mti.PICK_LN_ID)||' &d'||n(mti.TXN_TYPE)||' &d ');
   utl_file.put_line(handle,n(mti.TXN_DATE)||' &d ');
   utl_file.put_line(handle,n(mti.PRM_Q)||' &d '||n(mti.FROM_SUB)||' &d ');
   utl_file.put_line(handle,n(mti.FROM_LOC_ID)||' &d '||n(mti.PROCESS)||' &d ');
   utl_file.put_line(handle,n(mti.LCK)||' &d '||n(mti.TRANS_MODE)||' &d ');
   utl_file.put_line(handle,n(mti.LPN_ID)||' &d '||n(mti.ERROR_CODE)||' &d ');
   utl_file.put_line(handle,n(mti.ERROR_EXPL)||' &d ');
   utl_file.put_line(handle,n(mti.SEC_UOM_CODE)||' &d '||n(mti.SEC_TRN_QTY)||' &d ');
   utl_file.put_line(handle,n(mti.ACCT_PER)||' &d '||n(mti.PER_NAME)||' &d ');
   utl_file.put_line(handle,n(mti.open_fl)||' &d '||n(mti.per_sd)||' &d ');
   utl_file.put_line(handle,n(mti.per_cd)||' &d '||n(mti.sch_cd)||' &el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');


UTL_FILE.PUT_LINE(handle,'&f &f <a NAME="MTL_MATERIAL_TRANSACTIONS_TEMP"> MTL_MATERIAL_TRANSACTIONS_TEMP (TMP) - UNPICKED LINES </a> ');
UTL_FILE.PUT_LINE(handle,'<a HREF="#TMP">Column Definitions</a> &f');
UTL_FILE.PUT_LINE(handle,'&std &sh MTL_TRNS_ID &dh TRANSACTION DATE &dh MOVE_LINE_ID &dh PICK_SLIP &dh LINE &dh LINE_ID &dh ITEM ');
UTL_FILE.PUT_LINE(handle,'&dh PRM_Q &dh FROM_SUB &dh FROM_LOC_ID &dh ');
UTL_FILE.PUT_LINE(handle,'TO_SUB &dh TO_LOC_ID &dh PROCESS &dh LCK &dh TRANS_MODE &dh ERROR_CODE &dh ERROR_EXPL &dh ');
UTL_FILE.PUT_LINE(handle,'SECONDARY UOM_CODE &dh SECONDARY TRANS_QTY &dh LOT_NUM &dh LOT PRM_QTY &dh LOT SEC_QTY &dh ');
UTL_FILE.PUT_LINE(handle,'LOT_CAL SEC_QTY &eh');


Declare
cursor mtl_trx_tmp is
SELECT distinct 
 TMP.TRANSACTION_TEMP_ID               MTL_TRNS_ID,
 TMP.TRANSACTION_DATE                  MTL_TRNS_DATE,
 TMP.MOVE_ORDER_LINE_ID                MOVE_LINE_ID,
 --'UNPICKED'                            LINE_STATUS,
 TMP.PICK_SLIP_NUMBER                  PICK_SLIP,
 to_char(LIN.line_number) || 
          decode(LIN.shipment_number, null, null, '.' || to_char(LIN.shipment_number))|| 
          decode(LIN.option_number, null, null, '.' || to_char(LIN.option_number)) ||
          decode(LIN.component_number, null, null, 
                 decode(LIN.option_number, null, '.',null)||
                 '.'||to_char(LIN.component_number))||
          decode(LIN.service_number,null,null,
                 decode(LIN.component_number, null, '.' , null) ||
                        decode(LIN.option_number, null, '.', null ) ||
                        '.'|| to_char(LIN.service_number)) LINE,
 LIN.LINE_ID                      LINE_ID,
 --DET.DELIVERY_DETAIL_ID           DET_DELIVERY_ID,
 ITM.SEGMENT1                     ITEM,
 TMP.PRIMARY_QUANTITY             PRM_Q,
 TMP.SUBINVENTORY_CODE            FROM_SUB,
 TMP.LOCATOR_ID                   FROM_LOC_ID,
 TMP.TRANSFER_SUBINVENTORY        TO_SUB,
 TMP.TRANSFER_TO_LOCATION         TO_LOC_ID,
 TMP.PROCESS_FLAG                 PROCESS,
 TMP.LOCK_FLAG                    LCK,
 TMP.TRANSACTION_MODE             TRANS_MODE,
 TMP.ERROR_CODE                   ERROR_CODE,    
 TMP.ERROR_EXPLANATION            ERROR_EXPL,
 TMP.SECONDARY_UOM_CODE           SEC_UOM_CODE,
 TMP.SECONDARY_TRANSACTION_QUANTITY SEC_TRN_QTY,
 lot.lot_number                   lot_num,
 lot.primary_quantity             lot_prm_q,
 lot.secondary_quantity           lot_sec_q,
 inv_convert.inv_um_convert(
    tmp.inventory_item_id,
    lot.lot_number,
    tmp.organization_id,
    5,
    lot.primary_quantity,
    itm.primary_uom_code,
    tmp.secondary_uom_code,
    null, null)                   lot_cal_sec_q
FROM 
    MTL_MATERIAL_TRANSACTIONS_TEMP    TMP,
--    WSH_DELIVERY_DETAILS              DET,
    OE_ORDER_LINES                    LIN,
    MTL_SYSTEM_ITEMS                  ITM, 
    mtl_transaction_lots_temp         lot
WHERE 
      TMP.DEMAND_SOURCE_LINE = LIN.LINE_ID
  and LIN.LINE_CATEGORY_CODE = 'ORDER' 
  and LIN.SHIP_FROM_ORG_ID   = ITM.ORGANIZATION_ID(+) 
  and LIN.INVENTORY_ITEM_ID  = ITM.INVENTORY_ITEM_ID(+)  
  and lot.transaction_temp_id (+)= tmp.transaction_temp_id
--  and TMP.DEMAND_SOURCE_LINE = DET.SOURCE_LINE_ID(+)   - Creates duplicates for lines with multiple Del_details.
  and LIN.HEADER_ID          = nvl('&header_id_selected',:v_header_id) 
  and NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.REFERENCE_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID)
UNION ALL
SELECT distinct 
 TMP.TRANSACTION_TEMP_ID               MTL_TRNS_ID,
 TMP.TRANSACTION_DATE                  MTL_TRNS_DATE,
 TMP.MOVE_ORDER_LINE_ID                MOVE_LINE_ID,
 --'UNPICKED'                            LINE_STATUS,
 TMP.PICK_SLIP_NUMBER                  PICK_SLIP,
 to_char(LIN.line_number) || 
          decode(LIN.shipment_number, null, null, '.' || to_char(LIN.shipment_number))|| 
          decode(LIN.option_number, null, null, '.' || to_char(LIN.option_number)) ||
          decode(LIN.component_number, null, null, 
                 decode(LIN.option_number, null, '.',null)||
                 '.'||to_char(LIN.component_number))||
          decode(LIN.service_number,null,null,
                 decode(LIN.component_number, null, '.' , null) ||
                        decode(LIN.option_number, null, '.', null ) ||
                        '.'|| to_char(LIN.service_number)) LINE,
 LIN.LINE_ID                      LINE_ID,
-- DET.DELIVERY_DETAIL_ID           DET_DELIVERY_ID,
 ITM.SEGMENT1                     ITEM,
 TMP.PRIMARY_QUANTITY             PRM_Q,
 TMP.SUBINVENTORY_CODE            FROM_SUB,
 TMP.LOCATOR_ID                   FROM_LOC_ID,
 TMP.TRANSFER_SUBINVENTORY        TO_SUB,
 TMP.TRANSFER_TO_LOCATION         TO_LOC_ID,
 TMP.PROCESS_FLAG                 PROCESS,
 TMP.LOCK_FLAG                    LCK,
 TMP.TRANSACTION_MODE             TRANS_MODE,
 TMP.ERROR_CODE                   ERROR_CODE,    
 TMP.ERROR_EXPLANATION            ERROR_EXPL,
 TMP.SECONDARY_UOM_CODE           SEC_UOM_CODE,
 TMP.SECONDARY_TRANSACTION_QUANTITY SEC_TRN_QTY,
 ' '                              lot_num,
 0                                lot_prm_q,
 0                                lot_sec_q,
 0                                lot_cal_sec_q
FROM 
    MTL_MATERIAL_TRANSACTIONS_TEMP    TMP,
--    WSH_DELIVERY_DETAILS              DET,
    OE_ORDER_LINES                    LIN,
    MTL_SYSTEM_ITEMS                  ITM 
WHERE 
      TMP.TRX_SOURCE_LINE_ID = LIN.LINE_ID
  and LIN.LINE_CATEGORY_CODE = 'RETURN' 
  and LIN.SHIP_FROM_ORG_ID   = ITM.ORGANIZATION_ID(+) 
  and LIN.INVENTORY_ITEM_ID  = ITM.INVENTORY_ITEM_ID(+)  
--  and TMP.DEMAND_SOURCE_LINE = DET.SOURCE_LINE_ID(+)
  and LIN.HEADER_ID          = nvl('&header_id_selected',:v_header_id) 
  and NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.REFERENCE_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID);

begin
 for mtt in mtl_trx_tmp
 loop
   utl_file.put_line(handle,'&sld'||n(mtt.MTL_TRNS_ID)||'&d'||n(mtt.MTL_TRNS_DATE)||'&d');
   utl_file.put_line(handle,n(mtt.MOVE_LINE_ID)||'&d');
   utl_file.put_line(handle,n(mtt.PICK_SLIP)||'&d'||n(mtt.LINE)||'&d');
   utl_file.put_line(handle,n(mtt.LINE_ID)||'&d'||n(mtt.ITEM)||'&d');
   utl_file.put_line(handle,n(mtt.PRM_Q)||'&d'||n(mtt.FROM_SUB)||'&d');
   utl_file.put_line(handle,n(mtt.FROM_LOC_ID)||'&d'||n(mtt.TO_SUB)||'&d');
   utl_file.put_line(handle,n(mtt.TO_LOC_ID)||'&d'||n(mtt.PROCESS)||'&d');
   utl_file.put_line(handle,n(mtt.LCK)||'&d'||n(mtt.TRANS_MODE)||'&d');
   utl_file.put_line(handle,n(mtt.ERROR_CODE)||'&d'||n(mtt.ERROR_EXPL)||'&d ');
   utl_file.put_line(handle,n(mtt.SEC_UOM_CODE)||'&d'||n(mtt.SEC_TRN_QTY)||'&d');
   utl_file.put_line(handle,n(mtt.LOT_NUM)||'&d'||n(mtt.LOT_PRM_Q)||'&d');
   utl_file.put_line(handle,n(mtt.LOT_SEC_Q)||'&d'||n(mtt.LOT_CAL_SEC_Q)||'&el');
 end loop;
end;
UTL_FILE.PUT_LINE(handle,'&et '); 


-- This is commented out because it runs slowly without an index 
--<do not run> CREATE INDEX MTL_MATL_TRANS_777
--<do not run>  ON INV.MTL_MATERIAL_TRANSACTIONS
--<do not run> (trx_source_line_id);

UTL_FILE.PUT_LINE(handle,'&f &f <a NAME="MTL_MATERIAL_TRANSACTIONS"> MTL_MATERIAL_TRANSACTIONS (TRN) - PICKED LINES </a>');
UTL_FILE.PUT_LINE(handle,'<a HREF="#TRN">Column Definitions</a> &f');
UTL_FILE.PUT_LINE(handle,'&std &sh WARNING &dh MTL_TRNS_ID &dh TRANSACTION DATE &dh MOVE_LINE_ID &dh TRANS_TYPE &dh ');
UTL_FILE.PUT_LINE(handle,'PICK_SLIP &dh LINE &dh LINE_ID &dh PRM_Q &dh');
UTL_FILE.PUT_LINE(handle,'FROM_SUB &dh FROM_LOC_ID &dh TO_SUB &dh TO_LOC_ID &dh ORG_ID &dh PICKING_LINE_ID &dh ');
UTL_FILE.PUT_LINE(handle,'SECONDARY UOM_CODE &dh SECONDARY TRANS_QTY &dh LOT PRM_QTY &dh LOT SEC_QTY &dh LOT_CAL SEC_QTY &eh');


declare
cursor Mtl_trans is 
SELECT
 TRN.TRANSACTION_ID                    MTL_TRNS_ID,
 TRN.TRANSACTION_DATE                  MTL_TRNS_DATE,
 TRN.MOVE_ORDER_LINE_ID                MOVE_LINE_ID,
 --to_char(TRN.TRANSACTION_TYPE_ID) || '=' || 
 -- (select TYP.TRANSACTION_TYPE_NAME 
 --   from MTL_TRANSACTION_TYPES TYP
 --   where TRN.TRANSACTION_TYPE_ID = TYP.TRANSACTION_TYPE_ID) TRANS_TYPE,
 -- See header of this script for mapping other TRANSACTION_TYPE_ID's to their meaning
 decode(TRN.TRANSACTION_TYPE_ID,
        52,'Stage Trans',
        53,'Stage Trans INT',
        33,'SO Issue',
        34,'SO Issue INT',
        15,'RMA Receipt',
        18,'PO Receipt',
        'Invalid '||to_char(TRN.TRANSACTION_TYPE_ID))       TRANS_TYPE,
 TRN.PICK_SLIP_NUMBER                  PICK_SLIP,
 TRN.TRX_SOURCE_LINE_ID                TRX_SOURCE_LINE_ID,
 TRN.TRX_SOURCE_LINE_ID                LINE_ID,
 TRN.PRIMARY_QUANTITY                  PRM_Q,
 TRN.SUBINVENTORY_CODE                 FROM_SUB,
 TRN.LOCATOR_ID                        FROM_LOC_ID,
 TRN.TRANSFER_SUBINVENTORY             TO_SUB,
 TRN.TRANSFER_LOCATOR_ID               TO_LOC_ID,
 TRN.ORGANIZATION_ID                   ORG_ID,
 TRN.PICKING_LINE_ID                   PICKING_LINE_ID,
 TRN.SECONDARY_UOM_CODE                SEC_UOM_CODE,
 TRN.SECONDARY_TRANSACTION_QUANTITY    SEC_TRN_QTY,
 lot.primary_quantity                  lot_prm_q, 
 lot.secondary_transaction_quantity    lot_sec_q,
 inv_convert.inv_um_convert(
     lot.inventory_item_id,
     lot.lot_number,
     lot.organization_id,
     5,
     lot.primary_quantity,
     itm.primary_uom_code,
     trn.secondary_uom_code,
     null, null)                       lot_cal_sec_q
FROM 
    MTL_MATERIAL_TRANSACTIONS         TRN,
    MTL_SYSTEM_ITEMS                  ITM,
    mtl_transaction_lot_numbers         lot
WHERE 
 -- klr
 (TRN.TRX_SOURCE_LINE_ID,TRN.ORGANIZATION_ID,TRN.INVENTORY_ITEM_ID) IN (SELECT DISTINCT LINE_ID,SHIP_FROM_ORG_ID,INVENTORY_ITEM_ID
                               FROM OE_ORDER_LINES         LIN1
                               WHERE LIN1.HEADER_ID = nvl('&header_id_selected',:v_header_id) 
                               and NVL('&line_id_selected',0)    in (0,LIN1.LINE_ID,
                                         LIN1.TOP_MODEL_LINE_ID,
                                         LIN1.ATO_LINE_ID,
                                         LIN1.LINK_TO_LINE_ID,
                                         LIN1.REFERENCE_LINE_ID,
                                         LIN1.SERVICE_REFERENCE_LINE_ID))
and TRN.ORGANIZATION_ID   = ITM.ORGANIZATION_ID 
and TRN.INVENTORY_ITEM_ID  = ITM.INVENTORY_ITEM_ID 
and lot.transaction_id (+) = trn.transaction_id
UNION ALL
SELECT   /* DROP SHIP */
 TRN.TRANSACTION_ID                    MTL_TRNS_ID,
 TRN.TRANSACTION_DATE                  MTL_TRNS_DATE,
 TRN.MOVE_ORDER_LINE_ID                MOVE_LINE_ID,
 decode(TRN.TRANSACTION_TYPE_ID,
        52,'Stage Trans',
        33,'SO Issue',
        15,'RMA Receipt',
        18,'PO Receipt',
        'Invalid '||to_char(TRN.TRANSACTION_TYPE_ID))       TRANS_TYPE,
 TRN.PICK_SLIP_NUMBER                  PICK_SLIP,
-- to_char(TRN.SOURCE_LINE_ID)           LINE,
 TRN.TRX_SOURCE_LINE_ID                TRX_SOURCE_LINE_ID,
 TRN.TRX_SOURCE_LINE_ID                LINE_ID,
 TRN.PRIMARY_QUANTITY                  PRM_Q,
 TRN.SUBINVENTORY_CODE                 FROM_SUB,
 TRN.LOCATOR_ID                        FROM_LOC_ID,
 TRN.TRANSFER_SUBINVENTORY             TO_SUB,
 TRN.TRANSFER_LOCATOR_ID               TO_LOC_ID,
 TRN.ORGANIZATION_ID                   ORG_ID,
 TRN.PICKING_LINE_ID                   PICKING_LINE_ID,
 TRN.SECONDARY_UOM_CODE                SEC_UOM_CODE,
 TRN.SECONDARY_TRANSACTION_QUANTITY    SEC_TRN_QTY,
 0                                     lot_prm_q, -- Only for OPM
 0                                     lot_sec_q, -- Only for OPM
 0                                     lot_cal_sec_q -- Only for OPM
FROM
    MTL_MATERIAL_TRANSACTIONS         TRN,
    MTL_SYSTEM_ITEMS                  ITM,
    OE_DROP_SHIP_SOURCES              DRP,
    PO_HEADERS_ALL                    POH
WHERE
     TRN.TRANSACTION_TYPE_ID         = 18                        -- PO Receipt
and  TRN.TRANSACTION_SOURCE_TYPE_ID  = 1
and  TRN.TRANSACTION_SOURCE_ID       = POH.PO_HEADER_ID
and  POH.PO_HEADER_ID                = DRP.PO_HEADER_ID
and  DRP.HEADER_ID                   = nvl('&header_id_selected',:v_header_id)
and  TRN.ORGANIZATION_ID             = ITM.ORGANIZATION_ID
and  TRN.INVENTORY_ITEM_ID           = ITM.INVENTORY_ITEM_ID
UNION ALL
SELECT   /* PO receipt trx for ATO BUY ITEM */
 TRN.TRANSACTION_ID                    MTL_TRNS_ID,
 TRN.TRANSACTION_DATE                  MTL_TRNS_DATE,
 TRN.MOVE_ORDER_LINE_ID                MOVE_LINE_ID,
 decode(TRN.TRANSACTION_TYPE_ID,
        52,'Stage Trans',
        33,'SO Issue',
        15,'RMA Receipt',
        18,'PO Receipt',
        'Invalid '||to_char(TRN.TRANSACTION_TYPE_ID))       TRANS_TYPE,
 TRN.PICK_SLIP_NUMBER                  PICK_SLIP,
-- to_char(RES.DEMAND_SOURCE_LINE_ID)    LINE,
 TRN.TRX_SOURCE_LINE_ID                TRX_SOURCE_LINE_ID,
 TRN.TRX_SOURCE_LINE_ID                LINE_ID,
 TRN.PRIMARY_QUANTITY                  PRM_Q,
 TRN.SUBINVENTORY_CODE                 FROM_SUB,
 TRN.LOCATOR_ID                        FROM_LOC_ID,
 TRN.TRANSFER_SUBINVENTORY             TO_SUB,
 TRN.TRANSFER_LOCATOR_ID               TO_LOC_ID,
 TRN.ORGANIZATION_ID                   ORG_ID,
 TRN.PICKING_LINE_ID                   PICKING_LINE_ID,
 TRN.SECONDARY_UOM_CODE                SEC_UOM_CODE,
 TRN.SECONDARY_TRANSACTION_QUANTITY    SEC_TRN_QTY,
 0                                     lot_prm_q, -- Only for OPM
 0                                     lot_sec_q, -- Only for OPM
 0                                     lot_cal_sec_q -- Only for OPM
FROM
    MTL_MATERIAL_TRANSACTIONS         TRN,
    MTL_SYSTEM_ITEMS                  ITM,
    MTL_RESERVATIONS                  RES,
    PO_HEADERS_ALL                    POH
WHERE
     :sales_ord_id                 = RES.DEMAND_SOURCE_HEADER_ID
and  RES.DEMAND_SOURCE_TYPE_ID     = 2                         -- SO
and  RES.SUPPLY_SOURCE_TYPE_ID     in (1,13)                   -- PO or INV
and  RES.SUPPLY_SOURCE_HEADER_ID   = POH.PO_HEADER_ID          --
and  POH.PO_HEADER_ID              = TRN.TRANSACTION_SOURCE_ID
and  TRN.ORGANIZATION_ID           = ITM.ORGANIZATION_ID
and  TRN.INVENTORY_ITEM_ID         = ITM.INVENTORY_ITEM_ID
and  TRN.TRANSACTION_TYPE_ID       = 18                        -- PO Receipt
and  TRN.TRANSACTION_SOURCE_TYPE_ID  = 1;

r_lin_no  varchar2(100);
type     per_record_typ is RECORD
               (flag    varchar2(1),
                descrip varchar2(200));
type     msg_tab is TABLE of per_record_typ INDEX by binary_integer;
msg      msg_tab;

function n(v varchar2) return varchar2 is
begin
  if v is null then
   return '&sp';
   else
   return v;
  end if;
end n;

begin
  :r_error := 0;
-- Initialize error messages even if Do Analysis not selected
-- if substr(UPPER(nvl('&do_analysis','Y')),1,1) = 'Y' then
  for i in 1..30
  loop
    msg(i).flag := '0';
    msg(i).descrip := '';
  end loop;
  msg(1).descrip   := 'x';

-- end if;

 for mt in Mtl_trans
 loop
   :r_flag := '';

   -- Order_Line number
     SELECT to_char(LIN.line_number) || 
            decode(LIN.shipment_number, null, null, '.' || to_char(LIN.shipment_number))|| 
            decode(LIN.option_number, null, null, '.' || to_char(LIN.option_number)) ||
            decode(LIN.component_number, null, null, 
                 decode(LIN.option_number, null, '.',null)||
                 '.'||to_char(LIN.component_number))||
            decode(LIN.service_number,null,null,
                   decode(LIN.component_number, null, '.' , null) ||
                   decode(LIN.option_number, null, '.', null ) ||
                        '.'|| to_char(LIN.service_number))
      into r_lin_no
      FROM OE_ORDER_LINES LIN
     WHERE mt.TRX_SOURCE_LINE_ID = LIN.LINE_ID;

if substr(UPPER(nvl('&do_analysis','Y')),1,1) = 'Y' then
  null;
   ---
   -- Basic Verification
   --
--   If  i.Ordq < i.shpq then
--     :r_flag := :r_flag || '1 ';
--   end if;
--   If  i.Ordq < i.fulq then
--     :r_flag := :r_flag || '2 ';
--   end if;
   ---
end if;

   -- Print line to Output file
   utl_file.put_line(handle,'&sld &b <a HREF="#TRNERR">'||n(:r_flag)||'</a> &eb &d');
   utl_file.put_line(handle,n(mt.MTL_TRNS_ID)||'&d'||n(mt.MTL_TRNS_DATE)||'&d');
   utl_file.put_line(handle,'<a NAME="MO'||mt.MOVE_LINE_ID||'">'||n(mt.MOVE_LINE_ID)||'</a>'||'&d');
   utl_file.put_line(handle,n(mt.TRANS_TYPE)||'&d'||n(mt.PICK_SLIP)||'&d');
   utl_file.put_line(handle,n(r_lin_no)||'&d'||n(mt.LINE_ID)||'&d');
   utl_file.put_line(handle,n(mt.PRM_Q)||'&d'||n(mt.FROM_SUB)||'&d');
   utl_file.put_line(handle,n(mt.FROM_LOC_ID)||'&d'||n(mt.TO_SUB)||'&d');
   utl_file.put_line(handle,n(mt.TO_LOC_ID)||'&d'||n(mt.ORG_ID)||'&d');
   utl_file.put_line(handle,n(mt.PICKING_LINE_ID)||'&d ');
   utl_file.put_line(handle,n(mt.SEC_UOM_CODE)||'&d'||n(mt.SEC_TRN_QTY)||'&d');
   utl_file.put_line(handle,n(mt.LOT_PRM_Q)||'&d'||n(mt.LOT_SEC_Q)||'&d');
   utl_file.put_line(handle,n(mt.LOT_CAL_SEC_Q)||'&el');

   if :r_flag is not null then
    :r_error := 1;
   end if;
 end loop;
   utl_file.put_line(handle,'&et');

 if :r_error = 1 then
   utl_file.put_line(handle,'&f &b <a NAME="TRNERR">Warning List:</a> &eb &f');
   for i in 1..30
   loop
     if msg(i).flag = '1' then
       utl_file.put_line(handle,msg(i).descrip||'&f');
     end if;
   end loop;
 end if;

end;

UTL_FILE.PUT_LINE(handle,'&et '); 

--<do not run> DROP INDEX INDEX MTL_MATL_TRANS_777;

UTL_FILE.PUT_LINE(handle,'&f &f <a NAME="MTL_SERIAL_NUMBERS"> MTL_SERIAL_NUMBERS (MSN)</a> &f');
UTL_FILE.PUT_LINE(handle,'&std &sh ERR_FLAG &dh DEL_DET_ID &dh REL_STATUS &dh LINE &dh LINE_ID &dh ITEM_ID &dh FROM_SERIAL &dh TO_SERIAL &dh ');
UTL_FILE.PUT_LINE(handle,'QUANTITY &dh SERIAL_NUMBER &dh CURR_STATUS &dh CURR_SUBINV &dh REVISION &dh LOT_NUMBER &dh ');
UTL_FILE.PUT_LINE(handle,'CURR_LOCATION &dh CURR_ORG_ID &dh GROUP_MARK &dh LINE_MARK &dh LOT_MARK &dh ORIGIN_DATE &eh');

Declare 
cursor m_serial_n is
select distinct
  DET.DELIVERY_DETAIL_ID           DEL_DET_ID,
  decode(DET.RELEASED_STATUS,
    'Y','Y=Staged',
    'R','R=Ready to Release',
    'S','S=Rel to Warhouse',
    'B','B=Backorder',
    'P','P=Pending Inv',
    'C','C=Shipped',
    'N','N=Not Ready',
    'D','D=Cancelled',
    'X','X=Not Applicable','Unknown: '||DET.RELEASED_STATUS) REL_STATUS,
  to_char(LIN.line_number) ||
          decode(LIN.shipment_number, null, null, '.' || to_char(LIN.shipment_number))||
          decode(LIN.option_number, null, null, '.' || to_char(LIN.option_number)) ||
          decode(LIN.component_number, null, null,
                 decode(LIN.option_number, null, '.',null)||
                 '.'||to_char(LIN.component_number))||
          decode(LIN.service_number,null,null,
                 decode(LIN.component_number, null, '.' , null) ||
                        decode(LIN.option_number, null, '.', null ) ||
                        '.'|| to_char(LIN.service_number)) LINE,
  DET.SOURCE_LINE_ID                LINE_ID,
  DET.INVENTORY_ITEM_ID             ITEM_ID,
  substr(wsn.fm_serial_number,1,15)  FROM_SERIAL,
  substr(wsn.to_serial_number,1,15)  TO_SERIAL,
  wsn.quantity                      WSN_QTY,
  to_char(wsn.creation_date,'DD-MON-RR_HH24:MI:SS') WSN_CRE_DATE,
  MSN.SERIAL_NUMBER                 MSN_SN,
  MSN.CURRENT_STATUS                MSN_CUR_STA,
  MSN.REVISION                      MSN_REV,
  MSN.LOT_NUMBER                    MSN_LOT,
  MSN.CURRENT_SUBINVENTORY_CODE     MSN_CUR_SUB,
  MSN.CURRENT_LOCATOR_ID            MSN_CUR_LOC,
  MSN.CURRENT_ORGANIZATION_ID       MSN_CUR_ORG,
  MSN.GROUP_MARK_ID                 MSN_GRP_MRK, 
  MSN.LINE_MARK_ID                  MSN_LIN_MRK,
  MSN.LOT_LINE_MARK_ID              MSN_LOT_MRK,
  MSN.ORIGINATION_DATE              MSN_ORI_DATE
FROM
  OE_ORDER_LINES                   LIN,
  WSH_DELIVERY_DETAILS             DET,
  WSH_SERIAL_NUMBERS               WSN,
  MTL_SERIAL_NUMBERS               MSN
where
  DET.DELIVERY_DETAIL_ID          = WSN.DELIVERY_DETAIL_ID
  and DET.SOURCE_LINE_ID              = LIN.LINE_ID
  and DET.SOURCE_CODE                 = 'OE'
  and LIN.HEADER_ID                   = nvl('&header_id_selected',:v_header_id)
  and NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.REFERENCE_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID)
  and DET.INVENTORY_ITEM_ID           = MSN.INVENTORY_ITEM_ID
  and MSN.SERIAL_NUMBER    between  WSN.FM_SERIAL_NUMBER and WSN.TO_SERIAL_NUMBER
  and (substr(UPPER(nvl('&det_cnt','Y')),1,1) = 'Y' or rownum <= 10)
order by
    LINE,MSN.SERIAL_NUMBER;
r_uom_lin        varchar2(10);
r_qshp_lin       number;
r_sta_lin        varchar2(100);
r_mtl_trn        varchar2(100);
type     per_record_typ is RECORD
               (flag    varchar2(1),
                descrip varchar2(200));
type     msg_tab is TABLE of per_record_typ INDEX by binary_integer;
msg      msg_tab;

function n(v varchar2) return varchar2 is
begin
  if v is null then
   return '&sp';
   else
   return v;
  end if;
end n;

begin
  :r_error := 0;
-- Initialize error messages even if Do Analysis not selected
-- if substr(UPPER(nvl('&do_analysis','Y')),1,1) = 'Y' then
  for i in 1..1
  loop
    msg(i).flag := '0';
    msg(i).descrip := '';
  end loop;
  msg(1).descrip   := '    1. No verifications yet.';
-- end if;

 for dd in m_serial_n
 loop
   :r_flag := '';

-- if substr(UPPER(nvl('&do_analysis','Y')),1,1) = 'Y' then
   -- Include verifications here
--   Select nvl(mtl_transactions_enabled_flag,'N')
--     into r_mtl_trn
--     from Mtl_System_items
--    where Inventory_item_Id = dd.ITEM_ID
--      and Organization_Id   = dd.WH_ID;
--   if dd.PICKABLE_FLAG <> r_mtl_trn then
--     :r_flag := :r_flag || '1 ';
--     msg(1).flag := '1';
--   end if;


-- end if; -- do_analysis
   ---

   -- Print line to Output file
   utl_file.put_line(handle,'&sld &b <a HREF="#MSNERR">'||n(:r_flag)||'</a> &eb &d ');
   utl_file.put_line(handle,n(dd.DEL_DET_ID)||' &d ');
   utl_file.put_line(handle,n(dd.REL_STATUS)||' &d '||n(dd.LINE)||' &d ');
   utl_file.put_line(handle,'<a HREF="#'||dd.LINE_ID||'">'||n(dd.LINE_ID)||'</a> &d ');
   utl_file.put_line(handle,n(dd.ITEM_ID)||' &d ');
   utl_file.put_line(handle,n(dd.FROM_SERIAL)||' &d '||n(dd.TO_SERIAL)||' &d ');
   utl_file.put_line(handle,n(dd.WSN_QTY)||' &d '||n(dd.MSN_SN)||' &d ');
   utl_file.put_line(handle,n(dd.MSN_CUR_STA)||' &d '||n(dd.MSN_CUR_SUB)||' &d ');
   utl_file.put_line(handle,n(dd.MSN_REV)||' &d '||n(dd.MSN_LOT)||' &d ');
   utl_file.put_line(handle,n(dd.MSN_CUR_LOC)||' &d '||n(dd.MSN_CUR_ORG)||' &d ');
   utl_file.put_line(handle,n(dd.MSN_GRP_MRK)||' &d '||n(dd.MSN_LIN_MRK)||' &d ');
   utl_file.put_line(handle,n(dd.MSN_LOT_MRK)||' &d '||n(dd.MSN_ORI_DATE)||' &el');

   if :r_flag is not null then
    :r_error := 1;
   end if;
 end loop;
 utl_file.put_line(handle,'&et');

 if :r_error = 1 then
   utl_file.put_line(handle,'&f &b <a NAME="MSNERR">Error List:</a> &eb &f');
   for i in 1..1
   loop
     if msg(i).flag = '1' then
       utl_file.put_line(handle,msg(i).descrip||'&f');
     end if;
   end loop;
 end if;

end;

UTL_FILE.PUT_LINE(handle,'&et '); 

UTL_FILE.PUT_LINE(handle,'&f &f MTL_UNIT_TRANSACTIONS (UNT) <a HREF="#UNT">Column Definitions</a> &f');
UTL_FILE.PUT_LINE(handle,'&std &sh TRANS_ID &dh TRANSACTION DATE &dh STATUS_ID &dh SUBINV &dh LOC_ID &dh SERIAL_NUM ');
UTL_FILE.PUT_LINE(handle,'&dh ITEM_ID &dh WH_ID &dh TRX_DATE &dh ');
UTL_FILE.PUT_LINE(handle,'TRX_SRC_ID &dh TRX_SRC_TYPE_ID &dh RECEIPT_ISSUE_TYPE &dh CUST_ID &dh SHIP_ID &eh');

Declare
cursor mtl_unt_trx is
select 
     UNT.TRANSACTION_ID                 TRANS_ID,
     UNT.TRANSACTION_DATE               MTL_TRNS_DATE,
     UNT.STATUS_ID                      STATUS_ID,
     UNT.SUBINVENTORY_CODE              SUBINV,
     UNT.LOCATOR_ID                     LOC_ID,
     UNT.SERIAL_NUMBER                  SERIAL_NUM,
     UNT.INVENTORY_ITEM_ID              ITEM_ID,
     UNT.ORGANIZATION_ID                WH_ID,
     to_char(UNT.TRANSACTION_DATE,'DD-MON-RR_HH24:MI:SS') TRX_DATE,
     UNT.TRANSACTION_SOURCE_ID          TRX_SRC_ID,
     UNT.TRANSACTION_SOURCE_TYPE_ID     TRX_SRC_TYPE_ID,
     --UNT.TRANSACTION_SOURCE_NAME        TRX_SOURCE_NAME
     UNT.RECEIPT_ISSUE_TYPE             RECEIPT_ISSUE_TYPE,
     UNT.CUSTOMER_ID                    CUST_ID,
     UNT.SHIP_ID                        SHIP_ID
    --,UNT.SERIAL_ATTRIBUTE_CATEGORY      SERIAL_ATTR_CAT
    --,UNT.ORIGINATION_DATE               ORIGIN_DATE
from MTL_UNIT_TRANSACTIONS UNT
where UNT.TRANSACTION_ID in (select TRN.TRANSACTION_ID
FROM 
    MTL_MATERIAL_TRANSACTIONS         TRN,
    --WSH_DELIVERY_DETAILS              DET,
    --OE_ORDER_LINES                    LIN,
    MTL_SYSTEM_ITEMS                  ITM 
WHERE 
 -- klr
 (TRN.TRX_SOURCE_LINE_ID,TRN.ORGANIZATION_ID,TRN.INVENTORY_ITEM_ID) IN (SELECT DISTINCT LINE_ID,SHIP_FROM_ORG_ID,INVENTORY_ITEM_ID
                               FROM OE_ORDER_LINES         LIN1
                               WHERE LIN1.HEADER_ID = nvl('&header_id_selected',:v_header_id) 
                               and NVL('&line_id_selected',0)    in (0,LIN1.LINE_ID,
                                         LIN1.TOP_MODEL_LINE_ID,
                                         LIN1.ATO_LINE_ID,
                                         LIN1.LINK_TO_LINE_ID,
                                         LIN1.REFERENCE_LINE_ID,
                                         LIN1.SERVICE_REFERENCE_LINE_ID))
and TRN.ORGANIZATION_ID   = ITM.ORGANIZATION_ID 
and TRN.INVENTORY_ITEM_ID  = ITM.INVENTORY_ITEM_ID)
and (substr(UPPER(nvl('&det_cnt','Y')),1,1) = 'Y' or rownum <= 10);

begin
 for mut in mtl_unt_trx
 loop
   utl_file.put_line(handle,'&sld'||n(mut.TRANS_ID)||' &d '||n(mut.MTL_TRNS_DATE)||' &d ');
   utl_file.put_line(handle,n(mut.STATUS_ID)||' &d ');
   utl_file.put_line(handle,n(mut.SUBINV)||' &d '||n(mut.LOC_ID)||' &d ');
   utl_file.put_line(handle,n(mut.SERIAL_NUM)||' &d '||n(mut.ITEM_ID)||' &d ');
   utl_file.put_line(handle,n(mut.WH_ID)||' &d '||n(mut.TRX_DATE)||' &d ');
   utl_file.put_line(handle,n(mut.TRX_SRC_ID)||' &d '||n(mut.TRX_SRC_TYPE_ID)||' &d ');
   utl_file.put_line(handle,n(mut.RECEIPT_ISSUE_TYPE)||' &d '||n(mut.CUST_ID)||' &d ');
   utl_file.put_line(handle,n(mut.SHIP_ID)||' &el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');


UTL_FILE.PUT_LINE(handle,'&f &f <a NAME="MTL_TXN_REQUEST_LINES_V"> MTL_TXN_REQUEST_LINES_V (MOV) - MOVE TRANSACTIONS </a> <a HREF="#MOV">Column Definitions</a> &f');
UTL_FILE.PUT_LINE(handle,'&std &sh MO_LINE_ID &dh MO_NUMBER &dh MV_HDR_ID &dh MV_LINE_NUM &dh MV_LINE_STAT &dh LINE &dh TXN_SLINE_ID &dh ');
UTL_FILE.PUT_LINE(handle,'ITEM &dh QTY &dh PRM_Q &dh DLV_Q &dh DTL_Q &dh MOVE_TYPE_NAME &dh TRNS_SRC_TYPE &dh TRNS_TYPE_NAME &dh WH_ID &dh ');
UTL_FILE.PUT_LINE(handle,'FROM_SUB &dh FROM_LOC_ID &dh TO_SUB &dh TO_LOC_ID &dh LOT_NUM &dh TRNS_HEAD_ID &dh ');
UTL_FILE.PUT_LINE(handle,'SECONDARY QTY &dh SECONDARY DETAIL_Q &dh SECONDARY DELIVER_Q &eh');

Declare
cursor mtl_trx_reql is
select distinct
 TRL.LINE_ID                         MO_LINE_ID,
 TRH.REQUEST_NUMBER                  MO_NUMBER,
 TRL.HEADER_ID                       MV_HDR_ID,
 TRL.LINE_NUMBER                     MV_LINE_NUM,
 decode(TRL.LINE_STATUS,
        1, '1=Incomplete',
        2, '2=Pend Aprvl',
        3, '3=Approved',
        4, '4=Not Apprvd',
        5, '5=Closed',
        6, '6=Canceled',
        7, '7=Pre Apprvd',
        8, '8=Part Aprvd',
        9, '9=Cncld Source') MV_LINE_STAT,
 to_char(LIN.line_number) || 
          decode(LIN.shipment_number, null, null, '.' || to_char(LIN.shipment_number))|| 
          decode(LIN.option_number, null, null, '.' || to_char(LIN.option_number)) ||
          decode(LIN.component_number, null, null, 
                 decode(LIN.option_number, null, '.',null)||
                 '.'||to_char(LIN.component_number))||
          decode(LIN.service_number,null,null,
                 decode(LIN.component_number, null, '.' , null) ||
                        decode(LIN.option_number, null, '.', null ) ||
                        '.'|| to_char(LIN.service_number)) LINE,
 TRL.TXN_SOURCE_LINE_ID              TXN_SLINE_ID,
 --DET.DELIVERY_DETAIL_ID              DEL_DET_ID,
 ITM.SEGMENT1                        ITEM,
 TRL.QUANTITY                        QTY,
 TRL.PRIMARY_QUANTITY                PRM_Q,           
 TRL.QUANTITY_DELIVERED              DLV_Q,
 TRL.QUANTITY_DETAILED               DTL_Q,
 TRL.MOVE_ORDER_TYPE_NAME            MOVE_TYPE_NAME,
 decode(TRL.TRANSACTION_SOURCE_TYPE_ID,2,'Sales Order',TRL.TRANSACTION_SOURCE_TYPE_ID) 
                                     TRNS_SRC_TYPE,  
 TRL.TRANSACTION_TYPE_NAME           TRNS_TYPE_NAME,       
 TRL.ORGANIZATION_ID                 WH_ID,
 TRL.FROM_SUBINVENTORY_CODE          FROM_SUB,
 TRL.FROM_LOCATOR_ID                 FROM_LOC_ID, 
 TRL.TO_SUBINVENTORY_CODE            TO_SUB,
 TRL.TO_LOCATOR_ID                   TO_LOC_ID,          
 TRL.LOT_NUMBER                      LOT_NUM,
 TRL.TRANSACTION_HEADER_ID           TRNS_HEAD_ID,
 trl.secondary_quantity              sec_q,
 trl.secondary_quantity_detailed     sec_dtl_q,
 trl.secondary_quantity_delivered    sec_dlv_q
from MTL_TXN_REQUEST_LINES_V   TRL,
     MTL_TXN_REQUEST_HEADERS   TRH,
     WSH_DELIVERY_DETAILS      DET,
     OE_ORDER_LINES            LIN,
     MTL_SYSTEM_ITEMS          ITM
where TRL.LINE_ID            = DET.MOVE_ORDER_LINE_ID
      --TRL.TXN_SOURCE_LINE_ID = LIN.LINE_ID
  and LIN.SHIP_FROM_ORG_ID   = ITM.ORGANIZATION_ID(+) 
  and LIN.INVENTORY_ITEM_ID  = ITM.INVENTORY_ITEM_ID(+)  
  and DET.SOURCE_LINE_ID     = LIN.LINE_ID
  and TRL.HEADER_ID          = TRH.HEADER_ID
  and LIN.HEADER_ID          = nvl('&header_id_selected',:v_header_id) 
  and NVL('&line_id_selected',0)    in (0,LIN.LINE_ID,
                                         LIN.TOP_MODEL_LINE_ID,
                                         LIN.ATO_LINE_ID,
                                         LIN.LINK_TO_LINE_ID,
                                         LIN.REFERENCE_LINE_ID,
                                         LIN.SERVICE_REFERENCE_LINE_ID);

begin
 for mtr in mtl_trx_reql
 loop
   utl_file.put_line(handle,'&sld'||n(mtr.MO_LINE_ID)||'&d'||n(mtr.MO_NUMBER)||'&d');
   utl_file.put_line(handle,n(mtr.MV_HDR_ID)||'&d'||n(mtr.MV_LINE_NUM)||'&d');
   utl_file.put_line(handle,n(mtr.MV_LINE_STAT)||'&d'||n(mtr.LINE)||'&d');
   utl_file.put_line(handle,n(mtr.TXN_SLINE_ID)||'&d'||n(mtr.ITEM)||'&d');
   utl_file.put_line(handle,n(mtr.QTY)||'&d'||n(mtr.PRM_Q)||'&d');
   utl_file.put_line(handle,n(mtr.DLV_Q)||'&d'||n(mtr.DTL_Q)||'&d');
   utl_file.put_line(handle,n(mtr.MOVE_TYPE_NAME)||'&d'||n(mtr.TRNS_SRC_TYPE)||'&d');
   utl_file.put_line(handle,n(mtr.TRNS_TYPE_NAME)||'&d'||n(mtr.WH_ID)||'&d');
   utl_file.put_line(handle,n(mtr.FROM_SUB)||'&d'||n(mtr.FROM_LOC_ID)||'&d');
   utl_file.put_line(handle,n(mtr.TO_SUB)||'&d'||n(mtr.TO_LOC_ID)||'&d');
   utl_file.put_line(handle,n(mtr.LOT_NUM)||'&d'||n(mtr.TRNS_HEAD_ID)||'&d');
   utl_file.put_line(handle,n(mtr.SEC_Q)||'&d'||n(mtr.SEC_DTL_Q)||'&d');
   utl_file.put_line(handle,n(mtr.SEC_DLV_Q)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');

end if; --prt_inv

if substr(UPPER(nvl('&prt_ar','Y')),1,1) = 'Y' then

UTL_FILE.PUT_LINE(handle,'&f &f <a NAME="RA_INTERFACE_LINES"> RA_INTERFACE_LINES (RAI) - RECEIVABLES INTERFACE LINES </a> <a HREF="#RAI">Column Definitions</a> &f');
UTL_FILE.PUT_LINE(handle,'&std &sh INTF_LINE_ID &dh BATCH_SOURCE &dh ITEM_ID &dh QTY &dh ORD_Q &dh UOM &dh PRICE &dh SO_LIN &dh AR_ID &dh IR_ID &dh');
UTL_FILE.PUT_LINE(handle,'LINE_TYPE &dh Order_Num_1 &dh Order_Type_2 &dh Delivery_3 &dh WayBill_4 &dh Line_ID_6 &dh Pick_Line_Id_7 &dh Bill_Lading_8 &dh ');
UTL_FILE.PUT_LINE(handle,'Customer_Item_9 &dh WH_ID_10 &dh PA_ID_11 &dh C_RATE &dh C_DATE &dh CURR &dh TR &dh S_TAX_ID &dh VAT_ID &dh EF &dh TERR_ID &eh');

Declare
cursor ra_int_lin is
select distinct
            RAI.INTERFACE_LINE_ID             INTF_LINE_ID, 
            RAI.BATCH_SOURCE_NAME             BATCH_SOURCE,                   
            RAI.INVENTORY_ITEM_ID             ITEM_ID,
            RAI.QUANTITY                      QTY,
            RAI.QUANTITY_ORDERED              ORD_Q,
            RAI.UOM_CODE                      UOM,
            RAI.AMOUNT                        PRICE,
            trim(RAI.SALES_ORDER_LINE)        SO_LIN,
            RAI.ACCOUNTING_RULE_ID            AR_ID,
            RAI.INVOICING_RULE_ID             IR_ID,                  
            RAI.LINE_TYPE                     LINE_TYPE,
            RAI.INTERFACE_LINE_ATTRIBUTE1     Order_Num_1,
            RAI.INTERFACE_LINE_ATTRIBUTE2     Order_Type_2,
            RAI.INTERFACE_LINE_ATTRIBUTE3     Delivery_3,
            RAI.INTERFACE_LINE_ATTRIBUTE4     WayBill_4,
            RAI.INTERFACE_LINE_ATTRIBUTE6     Line_ID_6,
            RAI.INTERFACE_LINE_ATTRIBUTE7     Pick_Line_Id_7,
            RAI.INTERFACE_LINE_ATTRIBUTE8     Bill_Lading_8,
            RAI.INTERFACE_LINE_ATTRIBUTE9     Cust_Item_9,
            RAI.INTERFACE_LINE_ATTRIBUTE10    WH_ID_10,
            RAI.INTERFACE_LINE_ATTRIBUTE11    PA_ID_11,   
            RAI.CONVERSION_RATE               C_RATE,
            to_Char(RAI.CONVERSION_DATE,'DD-MON-RR_HH24:MI:SS') C_DATE, 
            RAI.CURRENCY_CODE                 CURR,                
            RAI.TAX_RATE                      TR,
            RAI.SALES_TAX_ID                  S_TAX_ID,
            RAI.VAT_TAX_ID                    VAT_ID,
            RAI.TAX_EXEMPT_FLAG               EF,
            RAI.TERRITORY_ID                  TERR_ID
            --ENABLE_TIMESTAMP ,to_char(RAI.CREATION_DATE,'DD-MON-RR_HH24:MI:SS')    CREATE_DT 
            --ENABLE_TIMESTAMP ,to_char(RAI.LAST_UPDATE_DATE,'DD-MON-RR_HH24:MI:SS') UPDATE_DT
            --ENABLE_TIMESTAMP ,RAI.REQUEST_ID                                    REQUEST_ID        
FROM
        RA_INTERFACE_LINES            RAI,
        OE_ORDER_LINES                LIN,
        OE_ORDER_HEADERS              ORD,
        OE_TRANSACTION_TYPES_V        TYP
WHERE   
             RAI.SALES_ORDER = to_char(ORD.ORDER_NUMBER)
        and  RAI.INTERFACE_LINE_ATTRIBUTE2 = TYP.NAME
        and  TYP.TRANSACTION_TYPE_ID       = ORD.ORDER_TYPE_ID
        and  ORD.HEADER_ID                 = LIN.HEADER_ID
        and  RAI.LINE_TYPE                 = 'LINE'
        and (NVL('&line_id_selected',0) in (0,LIN.LINE_ID,
                                             LIN.TOP_MODEL_LINE_ID,
                                             LIN.ATO_LINE_ID,
                                             LIN.LINK_TO_LINE_ID,
                                             LIN.REFERENCE_LINE_ID,
                                             LIN.SERVICE_REFERENCE_LINE_ID)
        and NVL(RAI.INTERFACE_LINE_ATTRIBUTE6,0) in (0,LIN.LINE_ID,
                                             LIN.TOP_MODEL_LINE_ID,
                                             LIN.ATO_LINE_ID,
                                             LIN.LINK_TO_LINE_ID,
                                             LIN.REFERENCE_LINE_ID,
                                             LIN.SERVICE_REFERENCE_LINE_ID))
        and LIN.HEADER_ID                  = nvl('&header_id_selected',:v_header_id)
UNION
select distinct 
            RAI.INTERFACE_LINE_ID             INTF_LINE_ID, 
            RAI.BATCH_SOURCE_NAME             BATCH_SOURCE,                   
            RAI.INVENTORY_ITEM_ID             ITEM_ID,
            RAI.QUANTITY                      QTY,
            RAI.QUANTITY_ORDERED              ORD_Q,
            RAI.UOM_CODE                      UOM,
            RAI.AMOUNT                        PRICE,
            trim(RAI.SALES_ORDER_LINE)        SO_LIN,
            RAI.ACCOUNTING_RULE_ID            AR_ID,
            RAI.INVOICING_RULE_ID             IR_ID,                  
            RAI.LINE_TYPE                     LINE_TYPE,
            RAI.INTERFACE_LINE_ATTRIBUTE1     Order_Num_1,
            RAI.INTERFACE_LINE_ATTRIBUTE2     Order_Type_2,
            RAI.INTERFACE_LINE_ATTRIBUTE3     Delivery_3,
            RAI.INTERFACE_LINE_ATTRIBUTE4     WayBill_4,
            RAI.INTERFACE_LINE_ATTRIBUTE6     Line_ID_6,
            RAI.INTERFACE_LINE_ATTRIBUTE7     Pick_Line_Id_7,
            RAI.INTERFACE_LINE_ATTRIBUTE8     Bill_Lading_8,
            RAI.INTERFACE_LINE_ATTRIBUTE9     Cust_Item_9,
            RAI.INTERFACE_LINE_ATTRIBUTE10    WH_ID_10,
            RAI.INTERFACE_LINE_ATTRIBUTE11    PA_ID_11,   
            RAI.CONVERSION_RATE               C_RATE,
            to_char(RAI.CONVERSION_DATE,'DD-MON-RR_HH24:MI:SS') C_DATE, 
            RAI.CURRENCY_CODE                 CURR,                
            RAI.TAX_RATE                      TR,
            RAI.SALES_TAX_ID                  S_TAX_ID,
            RAI.VAT_TAX_ID                    VAT_ID,
            RAI.TAX_EXEMPT_FLAG               EF,
            RAI.TERRITORY_ID                  TERR_ID 
            --ENABLE_TIMESTAMP ,to_char(RAI.CREATION_DATE,'DD-MON-RR_HH24:MI:SS') CREATE_DT
            --ENABLE_TIMESTAMP ,to_char(RAI.LAST_UPDATE_DATE,'DD-MON-RR_HH24:MI:SS') UPDATE_DT
            --ENABLE_TIMESTAMP ,RAI.REQUEST_ID                REQUEST_ID        
FROM
        RA_INTERFACE_LINES            RAI,
        OE_ORDER_LINES                LIN,
        OE_ORDER_HEADERS              ORD,
        OE_PRICE_ADJUSTMENTS          ADJ,
        OE_TRANSACTION_TYPES_V        TYP
WHERE   
             RAI.SALES_ORDER = to_char(ORD.ORDER_NUMBER)
        and  RAI.INTERFACE_LINE_ATTRIBUTE2 = TYP.NAME
        and  TYP.TRANSACTION_TYPE_ID       = ORD.ORDER_TYPE_ID
        and  ORD.HEADER_ID                 = LIN.HEADER_ID
        and  RAI.LINE_TYPE                 = 'FREIGHT'
        and NVL('&line_id_selected',0) in (0,LIN.LINE_ID,
                                             LIN.TOP_MODEL_LINE_ID,
                                             LIN.ATO_LINE_ID,
                                             LIN.LINK_TO_LINE_ID,
                                             LIN.REFERENCE_LINE_ID,
                                             LIN.SERVICE_REFERENCE_LINE_ID)
        and  to_number(RAI.INTERFACE_LINE_ATTRIBUTE6) = ADJ.PRICE_ADJUSTMENT_ID
        and  ADJ.LINE_ID                    = LIN.LINE_ID
        and  ADJ.LINE_ID                    IS NOT NULL
        and  ADJ.HEADER_ID                  = nvl('&header_id_selected',:v_header_id)
        and  LIN.HEADER_ID                  = nvl('&header_id_selected',:v_header_id)
UNION
select distinct 
            RAI.INTERFACE_LINE_ID             INTF_LINE_ID, 
            RAI.BATCH_SOURCE_NAME             BATCH_SOURCE,                   
            RAI.INVENTORY_ITEM_ID             ITEM_ID,
            RAI.QUANTITY                      QTY,
            RAI.QUANTITY_ORDERED              ORD_Q,
            RAI.UOM_CODE                      UOM,
            RAI.AMOUNT                        PRICE,
            trim(RAI.SALES_ORDER_LINE)        SO_LIN,
            RAI.ACCOUNTING_RULE_ID            AR_ID,
            RAI.INVOICING_RULE_ID             IR_ID,                  
            RAI.LINE_TYPE                     LINE_TYPE,
            RAI.INTERFACE_LINE_ATTRIBUTE1     Order_Num_1,
            RAI.INTERFACE_LINE_ATTRIBUTE2     Order_Type_2,
            RAI.INTERFACE_LINE_ATTRIBUTE3     Delivery_3,
            RAI.INTERFACE_LINE_ATTRIBUTE4     WayBill_4,
            RAI.INTERFACE_LINE_ATTRIBUTE6     Line_ID_6,
            RAI.INTERFACE_LINE_ATTRIBUTE7     Pick_Line_Id_7,
            RAI.INTERFACE_LINE_ATTRIBUTE8     Bill_Lading_8,
            RAI.INTERFACE_LINE_ATTRIBUTE9     Cust_Item_9,
            RAI.INTERFACE_LINE_ATTRIBUTE10    WH_ID_10,
            RAI.INTERFACE_LINE_ATTRIBUTE11    PA_ID_11,   
            RAI.CONVERSION_RATE               C_RATE,
            to_char(RAI.CONVERSION_DATE,'DD-MON-RR_HH24:MI:SS')  C_DATE, 
            RAI.CURRENCY_CODE                 CURR,                
            RAI.TAX_RATE                      TR,
            RAI.SALES_TAX_ID                  S_TAX_ID,
            RAI.VAT_TAX_ID                    VAT_ID,
            RAI.TAX_EXEMPT_FLAG               EF,
            RAI.TERRITORY_ID                  TERR_ID 
            --ENABLE_TIMESTAMP ,to_char(RAI.CREATION_DATE,'DD-MON-RR_HH24:MI:SS')    CREATE_DT
            --ENABLE_TIMESTAMP ,to_char(RAI.LAST_UPDATE_DATE,'DD-MON-RR_HH24:MI:SS') UPDATE_DT
            --ENABLE_TIMESTAMP ,RAI.REQUEST_ID                                    REQUEST_ID        
FROM
        RA_INTERFACE_LINES            RAI,
        OE_ORDER_HEADERS              ORD,
        OE_PRICE_ADJUSTMENTS          ADJ,
        OE_TRANSACTION_TYPES_V        TYP
WHERE   
             RAI.SALES_ORDER = to_char(ORD.ORDER_NUMBER)
        and  RAI.INTERFACE_LINE_ATTRIBUTE2 = TYP.NAME
        and  TYP.TRANSACTION_TYPE_ID       = ORD.ORDER_TYPE_ID
        and  RAI.LINE_TYPE                 = 'FREIGHT'
        and  ADJ.HEADER_ID                 = nvl('&header_id_selected',:v_header_id)
        and  ADJ.LINE_ID                   IS NULL
        and  ORD.HEADER_ID                 = nvl('&header_id_selected',:v_header_id)
UNION
SELECT DISTINCT RAI.INTERFACE_LINE_ID INTF_LINE_ID,
  RAI.BATCH_SOURCE_NAME BATCH_SOURCE,
  RAI.INVENTORY_ITEM_ID ITEM_ID,
  RAI.QUANTITY QTY,
  RAI.QUANTITY_ORDERED ORD_Q,
  RAI.UOM_CODE UOM,
  RAI.AMOUNT PRICE,
  trim(RAI.SALES_ORDER_LINE) SO_LIN,
  RAI.ACCOUNTING_RULE_ID AR_ID,
  RAI.INVOICING_RULE_ID IR_ID,
  RAI.LINE_TYPE LINE_TYPE,
  RAI.INTERFACE_LINE_ATTRIBUTE1 Order_Num_1,
  RAI.INTERFACE_LINE_ATTRIBUTE2 Order_Type_2,
  RAI.INTERFACE_LINE_ATTRIBUTE3 Delivery_3,
  RAI.INTERFACE_LINE_ATTRIBUTE4 WayBill_4,
  RAI.INTERFACE_LINE_ATTRIBUTE6 Line_ID_6,
  RAI.INTERFACE_LINE_ATTRIBUTE7 Pick_Line_Id_7,
  RAI.INTERFACE_LINE_ATTRIBUTE8 Bill_Lading_8,
  RAI.INTERFACE_LINE_ATTRIBUTE9     Cust_Item_9,
  RAI.INTERFACE_LINE_ATTRIBUTE10 WH_ID_10,
  RAI.INTERFACE_LINE_ATTRIBUTE11 PA_ID_11,
  RAI.CONVERSION_RATE C_RATE,
  TO_CHAR(RAI.CONVERSION_DATE,'DD-MON-RR_HH24:MI:SS') C_DATE,
  RAI.CURRENCY_CODE CURR,
  RAI.TAX_RATE TR,
  RAI.SALES_TAX_ID S_TAX_ID,
  RAI.VAT_TAX_ID VAT_ID,
  RAI.TAX_EXEMPT_FLAG EF,
  RAI.TERRITORY_ID TERR_ID
FROM RA_INTERFACE_LINES   RAI,
     OE_ORDER_HEADERS     ORD,
     OE_PRICE_ADJUSTMENTS ADJ,
     OE_TRANSACTION_TYPES_TL TYP
WHERE RAI.SALES_ORDER             = TO_CHAR(ORD.ORDER_NUMBER)
  AND RAI.INTERFACE_LINE_ATTRIBUTE2 = TYP.NAME
  AND TYP.TRANSACTION_TYPE_ID       = ORD.ORDER_TYPE_ID
  AND RAI.LINE_TYPE                 = 'LINE'
  and ADJ.HEADER_ID                 = nvl('&header_id_selected',:v_header_id)
  AND RAI.INTERFACE_LINE_ATTRIBUTE6 = ADJ.PRICE_ADJUSTMENT_ID
  AND ADJ.CHARGE_TYPE_CODE          = 'FREIGHT'
  AND RAI.SALES_ORDER_LINE         IS NULL
  and ORD.HEADER_ID                 = nvl('&header_id_selected',:v_header_id);

begin
 for ril in ra_int_lin
 loop
   utl_file.put_line(handle,'&sld'||n(ril.INTF_LINE_ID)||'&d'||n(ril.BATCH_SOURCE)||'&d');
   utl_file.put_line(handle,n(ril.ITEM_ID)||'&d'||n(ril.QTY)||'&d');
   utl_file.put_line(handle,n(ril.ORD_Q)||'&d'||n(ril.UOM)||'&d');
   utl_file.put_line(handle,n(ril.PRICE)||'&d'||n(ril.SO_LIN)||'&d');
   utl_file.put_line(handle,n(ril.AR_ID)||'&d'||n(ril.IR_ID)||'&d');
   utl_file.put_line(handle,n(ril.LINE_TYPE)||'&d'||n(ril.ORDER_NUM_1)||'&d');
   utl_file.put_line(handle,n(ril.ORDER_TYPE_2)||'&d'||n(ril.DELIVERY_3)||'&d');
   utl_file.put_line(handle,n(ril.WAYBILL_4)||'&d'||n(ril.LINE_ID_6)||'&d');
   utl_file.put_line(handle,n(ril.PICK_LINE_ID_7)||'&d'||n(ril.BILL_LADING_8)||'&d');
   utl_file.put_line(handle,n(ril.Cust_Item_9)||'&d');
   utl_file.put_line(handle,n(ril.WH_ID_10)||'&d'||n(ril.PA_ID_11)||'&d');
   utl_file.put_line(handle,n(ril.C_RATE)||'&d'||n(ril.C_DATE)||'&d');
   utl_file.put_line(handle,n(ril.CURR)||'&d'||n(ril.TR)||'&d');
   utl_file.put_line(handle,n(ril.S_TAX_ID)||'&d'||n(ril.VAT_ID)||'&d');
   utl_file.put_line(handle,n(ril.EF)||'&d'||n(ril.TERR_ID)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');


UTL_FILE.PUT_LINE(handle,'&f &f RA_INTERFACE_ERRORS (RAE) - RECEIVABLES INTERFACE ERRORS &f');
UTL_FILE.PUT_LINE(handle,'&std &sh INTERFACE_LINE_ID &dh INTERFACE_SALESCREDIT_ID &dh MESSAGE_TEXT &eh');

Declare
cursor ra_int_err is
select distinct 
   RAE.INTERFACE_LINE_ID,
   RAE.INTERFACE_SALESCREDIT_ID,       
   RAE.MESSAGE_TEXT
FROM 
        RA_INTERFACE_LINES            RAI,
        RA_INTERFACE_ERRORS           RAE,
        OE_ORDER_LINES                LIN,
        OE_ORDER_HEADERS              ORD,
        OE_TRANSACTION_TYPES_V        TYP
WHERE   
             RAI.SALES_ORDER               = to_char(ORD.ORDER_NUMBER)
        and  RAI.INTERFACE_LINE_ID         = RAE.INTERFACE_LINE_ID
        and  RAI.INTERFACE_LINE_ATTRIBUTE2 = TYP.NAME
        and  TYP.TRANSACTION_TYPE_ID             = ORD.ORDER_TYPE_ID
        and  ORD.HEADER_ID                 = LIN.HEADER_ID
        and (NVL('&line_id_selected',0) in (0,LIN.LINE_ID,
                                             LIN.TOP_MODEL_LINE_ID,
                                             LIN.ATO_LINE_ID,
                                             LIN.LINK_TO_LINE_ID,
                                             LIN.REFERENCE_LINE_ID,
                                             LIN.SERVICE_REFERENCE_LINE_ID)
        and NVL(RAI.INTERFACE_LINE_ATTRIBUTE6,0) in (0,LIN.LINE_ID,
                                             LIN.TOP_MODEL_LINE_ID,
                                             LIN.ATO_LINE_ID,
                                             LIN.LINK_TO_LINE_ID,
                                             LIN.REFERENCE_LINE_ID,
                                             LIN.SERVICE_REFERENCE_LINE_ID))
        and LIN.HEADER_ID                  = nvl('&header_id_selected',:v_header_id)
UNION
select distinct
   RAE.INTERFACE_LINE_ID,              
   RAE.INTERFACE_SALESCREDIT_ID,       
   RAE.MESSAGE_TEXT 
FROM
        RA_INTERFACE_LINES            RAI,
        RA_INTERFACE_ERRORS           RAE,
        OE_ORDER_LINES                LIN,
        OE_ORDER_HEADERS              ORD,
        OE_PRICE_ADJUSTMENTS          ADJ,
        OE_TRANSACTION_TYPES_V        TYP
WHERE   
             RAI.SALES_ORDER = to_char(ORD.ORDER_NUMBER)
        and  RAI.INTERFACE_LINE_ATTRIBUTE2 = TYP.NAME
        and  RAI.INTERFACE_LINE_ID         = RAE.INTERFACE_LINE_ID
        and  TYP.TRANSACTION_TYPE_ID       = ORD.ORDER_TYPE_ID
        and  ORD.HEADER_ID                 = LIN.HEADER_ID
        and  RAI.LINE_TYPE                 = 'FREIGHT'
        and NVL('&line_id_selected',0) in (0,LIN.LINE_ID,
                                             LIN.TOP_MODEL_LINE_ID,
                                             LIN.ATO_LINE_ID,
                                             LIN.LINK_TO_LINE_ID,
                                             LIN.REFERENCE_LINE_ID,
                                             LIN.SERVICE_REFERENCE_LINE_ID)
        and  to_number(RAI.INTERFACE_LINE_ATTRIBUTE6) = ADJ.PRICE_ADJUSTMENT_ID
        and  ADJ.LINE_ID                    = LIN.LINE_ID
        and  ADJ.LINE_ID                    IS NOT NULL
        and  ADJ.HEADER_ID                  = nvl('&header_id_selected',:v_header_id)
        and  LIN.HEADER_ID                  = nvl('&header_id_selected',:v_header_id)
UNION
select distinct 
   RAE.INTERFACE_LINE_ID,              
   RAE.INTERFACE_SALESCREDIT_ID,       
   RAE.MESSAGE_TEXT
FROM
        RA_INTERFACE_LINES            RAI,
        RA_INTERFACE_ERRORS           RAE,
        OE_ORDER_HEADERS              ORD,
        OE_PRICE_ADJUSTMENTS          ADJ,
        OE_TRANSACTION_TYPES_V        TYP
WHERE   
             RAI.SALES_ORDER = to_char(ORD.ORDER_NUMBER)
        and  RAI.INTERFACE_LINE_ATTRIBUTE2 = TYP.NAME
        and  RAI.INTERFACE_LINE_ID         = RAE.INTERFACE_LINE_ID
        and  TYP.TRANSACTION_TYPE_ID       = ORD.ORDER_TYPE_ID
        and  RAI.LINE_TYPE                 = 'FREIGHT'
        and  ADJ.HEADER_ID                 = nvl('&header_id_selected',:v_header_id)
        and  ADJ.LINE_ID                   IS NULL
        and  ORD.HEADER_ID                 = nvl('&header_id_selected',:v_header_id);

begin
 for rie in ra_int_err
 loop
   utl_file.put_line(handle,'&sld'||n(rie.INTERFACE_LINE_ID)||'&d'||n(rie.INTERFACE_SALESCREDIT_ID)||'&d');
   utl_file.put_line(handle,n(rie.MESSAGE_TEXT)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');


UTL_FILE.PUT_LINE(handle,'&f &f <a NAME="RA_CUSTOMER_TRX"> RA_CUSTOMER_TRX (RAH) - INVOICE HEADERS </a> <a HREF="#RAH">Column Definitions</a> &f');
UTL_FILE.PUT_LINE(handle,'&std &sh CUST_TRX_ID &dh TRX_NUMBER &dh TRX_TYPE_ID &dh TRX_DATE &dh BATCH_ID &dh SOURCE_ID &dh BILL_CUST &dh BILL_SITE &dh ');
UTL_FILE.PUT_LINE(handle,'SHIP_CUST &dh SHIP_SITE &dh TERM_ID &dh SALESREP_ID &dh PO_NUMBER &dh CURR &dh AGREEMENT &dh COMP_FL &dh IR_ID &dh ');
UTL_FILE.PUT_LINE(handle,'SHIP_VIA &dh WAYBILL &dh STATUS &eh');

Declare
cursor ra_cus_trx is
select distinct 
            RAH.CUSTOMER_TRX_ID           CUST_TRX_ID,
            RAH.TRX_NUMBER                TRX_NUMBER,
            RAH.CUST_TRX_TYPE_ID          TRX_TYPE_ID,
            to_char(RAH.TRX_DATE,'DD-MON-RR_HH24:MI:SS')   TRX_DATE,
            RAH.BATCH_ID                  BATCH_ID,
            RAH.BATCH_SOURCE_ID           SOURCE_ID,
            RAH.BILL_TO_CUSTOMER_ID       BILL_CUST,
            RAH.BILL_TO_SITE_USE_ID       BILL_SITE,
            RAH.SHIP_TO_CUSTOMER_ID       SHIP_CUST,
            RAH.SHIP_TO_SITE_USE_ID       SHIP_SITE,
            RAH.TERM_ID                   TERM_ID,
            RAH.PRIMARY_SALESREP_ID       SALESREP_ID,
            RAH.PURCHASE_ORDER            PO_NUMBER,
            RAH.INVOICE_CURRENCY_CODE     CURR,
            RAH.AGREEMENT_ID              AGREEMENT,
            RAH.COMPLETE_FLAG             COMP_FL,
            RAH.INVOICING_RULE_ID         IR_ID,
            RAH.SHIP_VIA                  SHIP_VIA,
            RAH.WAYBILL_NUMBER            WAYBILL,
            RAH.STATUS_TRX                STATUS 
            --ENABLE_TIMESTAMP ,to_char(RAH.CREATION_DATE,'DD-MON-RR_HH24:MI:SS') CREATE_DT
            --ENABLE_TIMESTAMP ,to_char(RAH.LAST_UPDATE_DATE,'DD-MON-RR_HH24:MI:SS') UPDATE_DT
            --ENABLE_TIMESTAMP ,RAH.REQUEST_ID                REQUEST_ID        
FROM
        RA_CUSTOMER_TRX               RAH,
        RA_CUSTOMER_TRX_LINES         RAL,
        OE_ORDER_LINES                LIN,
        OE_ORDER_HEADERS              ORD,
        OE_TRANSACTION_TYPES_V        TYP
WHERE   
             RAH.CUSTOMER_TRX_ID           = RAL.CUSTOMER_TRX_ID
        and  RAL.SALES_ORDER               = to_char(ORD.ORDER_NUMBER)
        and  RAL.INTERFACE_LINE_ATTRIBUTE2 = TYP.NAME
        and  TYP.TRANSACTION_TYPE_ID       = ORD.ORDER_TYPE_ID
        and  ORD.HEADER_ID                 = LIN.HEADER_ID
        and  NVL('&line_id_selected',0) in (0,LIN.LINE_ID,
                                             LIN.TOP_MODEL_LINE_ID,
                                             LIN.ATO_LINE_ID,
                                             LIN.LINK_TO_LINE_ID,
                                             LIN.REFERENCE_LINE_ID,
                                             LIN.SERVICE_REFERENCE_LINE_ID)
        and NVL(RAL.INTERFACE_LINE_ATTRIBUTE6,0) in (0,LIN.LINE_ID,
                                             LIN.TOP_MODEL_LINE_ID,
                                             LIN.ATO_LINE_ID,
                                             LIN.LINK_TO_LINE_ID,
                                             LIN.REFERENCE_LINE_ID,
                                             LIN.SERVICE_REFERENCE_LINE_ID)
        and LIN.HEADER_ID                  = nvl('&header_id_selected',:v_header_id)
ORDER BY
        RAH.TRX_NUMBER;

begin
 for rct in ra_cus_trx
 loop
   utl_file.put_line(handle,'&sld'||n(rct.CUST_TRX_ID)||'&d'||n(rct.TRX_NUMBER)||'&d');
   utl_file.put_line(handle,n(rct.TRX_TYPE_ID)||'&d'||n(rct.TRX_DATE)||'&d');
   utl_file.put_line(handle,n(rct.BATCH_ID)||'&d'||n(rct.SOURCE_ID)||'&d');
   utl_file.put_line(handle,n(rct.BILL_CUST)||'&d'||n(rct.BILL_SITE)||'&d');
   utl_file.put_line(handle,n(rct.SHIP_CUST)||'&d'||n(rct.SHIP_SITE)||'&d');
   utl_file.put_line(handle,n(rct.TERM_ID)||'&d'||n(rct.SALESREP_ID)||'&d');
   utl_file.put_line(handle,n(rct.PO_NUMBER)||'&d'||n(rct.CURR)||'&d');
   utl_file.put_line(handle,n(rct.AGREEMENT)||'&d'||n(rct.COMP_FL)||'&d');
   utl_file.put_line(handle,n(rct.IR_ID)||'&d'||n(rct.SHIP_VIA)||'&d');
   utl_file.put_line(handle,n(rct.WAYBILL)||'&d'||n(rct.STATUS)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');


UTL_FILE.PUT_LINE(handle,'&f &f <a NAME="RA_CUSTOMER_TRX_LINES"> RA_CUSTOMER_TRX_LINES (RAL) - INVOICE LINES </a> <a HREF="#RAL">Column Definitions</a> &f');
UTL_FILE.PUT_LINE(handle,'&std &sh TRX_LINE_ID &dh LINK_TO_ID &dh CUST_TRX_ID &dh TRX_NUMBER &dh SOURCE &dh LINE_NUM &dh ITEM_ID &dh ORD_Q &dh ');
UTL_FILE.PUT_LINE(handle,'INV_Q &dh CRD_Q &dh UOM &dh PRICE &dh EXTD_AMT &dh REV_AMT &dh SO_LIN &dh LINE_TYPE &dh Order_Num_1 &dh Order_Type_2 &dh ');
UTL_FILE.PUT_LINE(handle,'Delivery_3 &dh WayBill_4 &dh Line_ID_6 &dh Bill_Lading_8 &dh Customer_item_9 &dh WH_ID_10 &dh PA_ID_11 &dh TF &dh TR &dh VAT_ID &dh S_TAX_ID &eh');

Declare
cursor ra_cus_trxl is
select distinct
            RAL.CUSTOMER_TRX_LINE_ID          TRX_LINE_ID,
            RAL.LINK_TO_CUST_TRX_LINE_ID      LINK_TO_ID,
            RAL.CUSTOMER_TRX_ID               CUST_TRX_ID,
            RAH.TRX_NUMBER                    TRX_NUMBER,
            RAL.SALES_ORDER_SOURCE            SOURCE,
            RAL.LINE_NUMBER                   LINE_NUM,                    
            RAL.INVENTORY_ITEM_ID             ITEM_ID,
            RAL.QUANTITY_ORDERED              ORD_Q,
            RAL.QUANTITY_INVOICED             INV_Q,
            RAL.QUANTITY_CREDITED             CRD_Q,
            RAL.UOM_CODE                      UOM,
            RAL.UNIT_SELLING_PRICE            PRICE,  
            RAL.EXTENDED_AMOUNT               EXTD_AMT,            
            RAL.REVENUE_AMOUNT                REV_AMT,          
            TRIM(RAL.SALES_ORDER_LINE)        SO_LIN,
            RAL.LINE_TYPE                     LINE_TYPE,
            RAL.INTERFACE_LINE_ATTRIBUTE1     Order_Num_1,
            RAL.INTERFACE_LINE_ATTRIBUTE2     Order_Type_2,
            RAL.INTERFACE_LINE_ATTRIBUTE3     Delivery_3,
            RAL.INTERFACE_LINE_ATTRIBUTE4     WayBill_4,
            RAL.INTERFACE_LINE_ATTRIBUTE6     Line_ID_6,
            RAL.INTERFACE_LINE_ATTRIBUTE8     Bill_Lading_8,
            RAL.INTERFACE_LINE_ATTRIBUTE9     Cust_Item_9,
            RAL.INTERFACE_LINE_ATTRIBUTE10    WH_ID_10,
            RAL.INTERFACE_LINE_ATTRIBUTE11    PA_ID_11,
            RAL.TAXABLE_FLAG                  TF,          
            RAL.TAX_RATE                      TR,
            RAL.VAT_TAX_ID                    VAT_ID,
            RAL.SALES_TAX_ID                  S_TAX_ID
            --ENABLE_TIMESTAMP ,to_char(RAL.CREATION_DATE,'DD-MON-RR_HH24:MI:SS') CREATE_DT
            --ENABLE_TIMESTAMP ,to_char(RAL.LAST_UPDATE_DATE,'DD-MON-RR_HH24:MI:SS') UPDATE_DT
            --ENABLE_TIMESTAMP ,RAL.REQUEST_ID                REQUEST_ID             
FROM
        RA_CUSTOMER_TRX               RAH,
        RA_CUSTOMER_TRX_LINES         RAL 
WHERE
        RAH.CUSTOMER_TRX_ID               = RAL.CUSTOMER_TRX_ID
    and RAL.CUSTOMER_TRX_LINE_ID in 
   (select RAL1.CUSTOMER_TRX_LINE_ID 
    FROM
            RA_CUSTOMER_TRX               RAH1,
            RA_CUSTOMER_TRX_LINES         RAL1,
            OE_ORDER_LINES                LIN,
            OE_ORDER_HEADERS              ORD,
            OE_TRANSACTION_TYPES_V        TYP
    WHERE   
             RAH1.CUSTOMER_TRX_ID           = RAL1.CUSTOMER_TRX_ID
        and  RAL1.SALES_ORDER               = to_char(ORD.ORDER_NUMBER)
        and  RAL1.INTERFACE_LINE_ATTRIBUTE2 = TYP.NAME
        and  TYP.TRANSACTION_TYPE_ID       = ORD.ORDER_TYPE_ID
        and  ORD.HEADER_ID                 = LIN.HEADER_ID
        and  NVL('&line_id_selected',0) in (0,LIN.LINE_ID,
                                             LIN.TOP_MODEL_LINE_ID,
                                             LIN.ATO_LINE_ID,
                                             LIN.LINK_TO_LINE_ID,
                                             LIN.REFERENCE_LINE_ID,
                                             LIN.SERVICE_REFERENCE_LINE_ID)
        and NVL(RAL1.INTERFACE_LINE_ATTRIBUTE6,0) in (0,LIN.LINE_ID,
                                             LIN.TOP_MODEL_LINE_ID,
                                             LIN.ATO_LINE_ID,
                                             LIN.LINK_TO_LINE_ID,
                                             LIN.REFERENCE_LINE_ID,
                                             LIN.SERVICE_REFERENCE_LINE_ID)
        and LIN.HEADER_ID                  = nvl('&header_id_selected',:v_header_id))
UNION
select /* LINKED LINES */
        distinct 
            RAL.CUSTOMER_TRX_LINE_ID          TRX_LINE_ID,
            RAL.LINK_TO_CUST_TRX_LINE_ID      LINK_TO_ID,
            RAL.CUSTOMER_TRX_ID               CUST_TRX_ID,
            RAH.TRX_NUMBER                    TRX_NUMBER,
            RAL.SALES_ORDER_SOURCE            SOURCE,
            RAL.LINE_NUMBER                   LINE_NUM,                    
            RAL.INVENTORY_ITEM_ID             ITEM_ID,
            RAL.QUANTITY_ORDERED              ORD_Q,
            RAL.QUANTITY_INVOICED             INV_Q,
            RAL.QUANTITY_CREDITED             CRD_Q,
            RAL.UOM_CODE                      UOM,
            RAL.UNIT_SELLING_PRICE            PRICE,  
            RAL.EXTENDED_AMOUNT               EXTD_AMT,            
            RAL.REVENUE_AMOUNT                REV_AMT,          
            TRIM(RAL.SALES_ORDER_LINE)        SO_LIN,
            RAL.LINE_TYPE                     LINE_TYPE,
            RAL.INTERFACE_LINE_ATTRIBUTE1     Order_Num_1,
            RAL.INTERFACE_LINE_ATTRIBUTE2     Order_Type_2,
            RAL.INTERFACE_LINE_ATTRIBUTE3     Delivery_3,
            RAL.INTERFACE_LINE_ATTRIBUTE4     WayBill_4,
            RAL.INTERFACE_LINE_ATTRIBUTE6     Line_ID_6,
            RAL.INTERFACE_LINE_ATTRIBUTE8     Bill_Lading_8,
            RAL.INTERFACE_LINE_ATTRIBUTE9     Cust_Item_9,
            RAL.INTERFACE_LINE_ATTRIBUTE10    WH_ID_10,
            RAL.INTERFACE_LINE_ATTRIBUTE11    PA_ID_11,
            RAL.TAXABLE_FLAG                  TF,          
            RAL.TAX_RATE                      TR,
            RAL.VAT_TAX_ID                    VAT_ID,
            RAL.SALES_TAX_ID                  S_TAX_ID 
            --ENABLE_TIMESTAMP ,to_char(RAL.CREATION_DATE,'DD-MON-RR_HH24:MI:SS')    CREATE_DT
            --ENABLE_TIMESTAMP ,to_char(RAL.LAST_UPDATE_DATE,'DD-MON-RR_HH24:MI:SS') UPDATE_DT
            --ENABLE_TIMESTAMP ,RAL.REQUEST_ID                                    REQUEST_ID             
FROM
        RA_CUSTOMER_TRX               RAH,
        RA_CUSTOMER_TRX_LINES         RAL
WHERE
     RAH.CUSTOMER_TRX_ID               = RAL.CUSTOMER_TRX_ID
 and RAL.LINK_TO_CUST_TRX_LINE_ID in 
   (select RAL1.CUSTOMER_TRX_LINE_ID 
    FROM
            RA_CUSTOMER_TRX               RAH1,
            RA_CUSTOMER_TRX_LINES         RAL1,
            OE_ORDER_LINES                LIN,
            OE_ORDER_HEADERS              ORD,
            OE_TRANSACTION_TYPES_V        TYP
    WHERE   
             RAH1.CUSTOMER_TRX_ID           = RAL1.CUSTOMER_TRX_ID
        and  RAL1.SALES_ORDER               = to_char(ORD.ORDER_NUMBER)
        and  RAL1.INTERFACE_LINE_ATTRIBUTE2 = TYP.NAME
        and  TYP.TRANSACTION_TYPE_ID        = ORD.ORDER_TYPE_ID
        and  ORD.HEADER_ID                  = LIN.HEADER_ID
        and  NVL('&line_id_selected',0) in (0,LIN.LINE_ID,
                                             LIN.TOP_MODEL_LINE_ID,
                                             LIN.ATO_LINE_ID,
                                             LIN.LINK_TO_LINE_ID,
                                             LIN.REFERENCE_LINE_ID,
                                             LIN.SERVICE_REFERENCE_LINE_ID)
        and NVL(RAL1.INTERFACE_LINE_ATTRIBUTE6,0) in (0,LIN.LINE_ID,
                                             LIN.TOP_MODEL_LINE_ID,
                                             LIN.ATO_LINE_ID,
                                             LIN.LINK_TO_LINE_ID,
                                             LIN.REFERENCE_LINE_ID,
                                             LIN.SERVICE_REFERENCE_LINE_ID)
        and LIN.HEADER_ID                   = nvl('&header_id_selected',:v_header_id))
order by 
        TRX_NUMBER,
        TRX_LINE_ID;

begin
 for rctl in ra_cus_trxl
 loop
   utl_file.put_line(handle,'&sld'||n(rctl.TRX_LINE_ID)||'&d'||n(rctl.LINK_TO_ID)||'&d');
   utl_file.put_line(handle,n(rctl.CUST_TRX_ID)||'&d'||n(rctl.TRX_NUMBER)||'&d');
   utl_file.put_line(handle,n(rctl.SOURCE)||'&d'||n(rctl.LINE_NUM)||'&d');
   utl_file.put_line(handle,n(rctl.ITEM_ID)||'&d'||n(rctl.ORD_Q)||'&d');
   utl_file.put_line(handle,n(rctl.INV_Q)||'&d'||n(rctl.CRD_Q)||'&d');
   utl_file.put_line(handle,n(rctl.UOM)||'&d'||n(rctl.PRICE)||'&d');
   utl_file.put_line(handle,n(rctl.EXTD_AMT)||'&d'||n(rctl.REV_AMT)||'&d');
   utl_file.put_line(handle,n(rctl.SO_LIN)||'&d'||n(rctl.LINE_TYPE)||'&d');
   utl_file.put_line(handle,n(rctl.ORDER_NUM_1)||'&d'||n(rctl.ORDER_TYPE_2)||'&d');
   utl_file.put_line(handle,n(rctl.DELIVERY_3)||'&d'||n(rctl.WAYBILL_4)||'&d');
   utl_file.put_line(handle,n(rctl.LINE_ID_6)||'&d'||n(rctl.BILL_LADING_8)||'&d');
   utl_file.put_line(handle,n(rctl.Cust_Item_9)||'&d');
   utl_file.put_line(handle,n(rctl.WH_ID_10)||'&d'||n(rctl.PA_ID_11)||'&d');
   utl_file.put_line(handle,n(rctl.TF)||'&d'||n(rctl.TR)||'&d');
   utl_file.put_line(handle,n(rctl.VAT_ID)||'&d'||n(rctl.S_TAX_ID)||'&el');
 end loop;
end;

UTL_FILE.PUT_LINE(handle,'&et');

end if; --prt_ar

end if; -- :v_head_only

-- Column descriptions for each table
UTL_FILE.PUT_LINE(handle,'&f &f');

UTL_FILE.PUT_LINE(handle,'&std &sh <CENTER> COLUMN DEFINITIONS </CENTER> &eh &et &f');

UTL_FILE.PUT_LINE(handle,'&std &sh <a NAME="OOH">OE_ORDER_HEADERS (ORD)</a> &dh &sp &dh &sp &eh');
UTL_FILE.PUT_LINE(handle,'&sld OP  => OPEN FLAG      &d  BK  => BOOKED FLAG          &d SP  => PARTIAL SHIPMENTS ALLOWED &el ');
UTL_FILE.PUT_LINE(handle,'&sld CN  => CANCEL FLAG    &d  STA => SHIP TOLERANCE ABOVE &d STB => SHIP TOLERANCE BELOW &el &et');

UTL_FILE.PUT_LINE(handle,'&f &f');

UTL_FILE.PUT_LINE(handle,'&std &sh <a NAME="OOL">OE_ORDER_LINES (LIN)</a> &dh &sp &dh &sp &eh');
UTL_FILE.PUT_LINE(handle,'&sld  SHN_Q => SHIPPING QUANTITY    &d  OP  => OPEN FLAG            &d  ORG => ORGANIZATION CODE &el');
UTL_FILE.PUT_LINE(handle,'&sld  SHP_Q => SHIPPED QUANTITY     &d  BK  => BOOKED FLAG          &d  SI  => Shipping Interfaced Flag &el');
UTL_FILE.PUT_LINE(handle,'&sld  FUL_Q => FULFILLED QUANTITY   &d  SH  => SHIPPABLE FLAG       &d  II  => Inventory Interfaced Flag &el');
UTL_FILE.PUT_LINE(handle,'&sld  CAN_Q => CANCELLED QUANTITY   &d  CN  => CANCELLED FLAG       &d  ATO => ATO FLAG  &el');
UTL_FILE.PUT_LINE(handle,'&sld  INC_Q => INVOICED QUANTITY    &d  VD  => VISIBLE DEMAND FLAG  &d  OPT => OPTION FLAG &el');
UTL_FILE.PUT_LINE(handle,'&sld  RES_Q => RESERVATION QUANTITY &d  SMC => SHIP MODEL COMPLETE  &d  FF  => FULFILLED_FLAG &el');
UTL_FILE.PUT_LINE(handle,'&sld  LCN_Q => LAST CANCELLED QTY   &d  CAL_PR => CALCULATE PRICE   &d &sp &el');
UTL_FILE.PUT_LINE(handle,'&sld  STA   => SHIP TOLERANCE ABOVE &d  STB => SHIP TOLERANCE BELOW &d &sp &el &et');

UTL_FILE.PUT_LINE(handle,'&f &f');

if substr(UPPER(nvl('&prt_price','Y')),1,1) = 'Y' then

UTL_FILE.PUT_LINE(handle,'&std &sh <a NAME="ADJC">APPLIED and UN-APPLIED HEADER PRICE ADJUSTMENTS (ADJ)</a> &dh &sp &dh &sp &eh');
UTL_FILE.PUT_LINE(handle,'&sld  MOD_LVL   => MODIFIER_LEVEL    &d IF  => INVOICED FLAG    &d  LK  => LOCK CONTROL  &el');
UTL_FILE.PUT_LINE(handle,'&sld  CHG_TY_CD => CHARGE_TYPE_CODE  &d EF  => ESTIMATED FLAG   &d  PP  => PRICING PHASE &el');
UTL_FILE.PUT_LINE(handle,'&sld  CD        => CREDIT_OR_DEBIT   &d UA  => UPDATE ALLOWED   &d  PI  => PRINT ON INVOICE &el');
UTL_FILE.PUT_LINE(handle,'&sld  AF        => AUTOMATIC FLAG    &d UF  => UPDATED FLAG     &d  AP  => APPLIED FLAG     &el &et');

UTL_FILE.PUT_LINE(handle,'&f &f');

UTL_FILE.PUT_LINE(handle,'&std &sh <a NAME="ALA">APPLIED AND UNAPPLIED LINE PRICE ADJUSTMENTS</a> &dh &sp &dh &sp &eh');
UTL_FILE.PUT_LINE(handle,'&sld MOD_LVL    => MODIFIER_LEVEL_CODE       &d CD => CREDIT_OR_CHARGE_FLAG &d UA => UPDATE_ALLOWED &el');
UTL_FILE.PUT_LINE(handle,'&sld CHG_TY_CD  => CHARGE_TYPE_CODE          &d AF => AUTOMATIC_FLAG        &d UF => UPDATED_FLAG   &el');
UTL_FILE.PUT_LINE(handle,'&sld ARITH_OP   => ARITHMETIC_OPERATOR       &d PI => PRINT_ON_INVOICE_FLAG &d AP => APPLIED_FLAG   &el');
UTL_FILE.PUT_LINE(handle,'&sld OP_PER_QTY => OPERAND_PER_PQTY          &d AC => ACCRUAL_FLAG          &d LK => LOCK_CONTROL   &el');
UTL_FILE.PUT_LINE(handle,'&sld ADJ_AMT_PO => ADJUSTED_AMOUNT_PER_PQTY  &d EF => ESTIMATED_FLAG        &d PERC => PERCENT      &el');
UTL_FILE.PUT_LINE(handle,'&sld ADJ_AMT    => ADJUSTED_AMOUNT           &d PP => PRICING_PHASE_ID      &d &sp &el &et');

UTL_FILE.PUT_LINE(handle,'&f &f');

end if; -- prt_price

UTL_FILE.PUT_LINE(handle,'&std &sh <a NAME="WDD">WSH_DELIVERY_DETAILS (DET)</a> &dh &sp &eh');
UTL_FILE.PUT_LINE(handle,'&sld  MO_LINE_ID => MOVE ORDER LINE ID          &d  DLV_Q      => DELIVERED QUANTITY &el');
UTL_FILE.PUT_LINE(handle,'&sld  DELIV_ID   => DELIVERY ID                 &d  CAN_Q      => CANCELLED QUANTITY &el');
UTL_FILE.PUT_LINE(handle,'&sld  SRQ_Q      => SOURCE REQUESTED QUANTITY   &d  SRQ_U      => SOURCE REQUESTED QUANTITY UOM &el');
UTL_FILE.PUT_LINE(handle,'&sld  REQ_Q      => REQUESTED QUANTITY          &d  REQ_U      => REQUESTED QUANTITY UOM &el');
UTL_FILE.PUT_LINE(handle,'&sld  OMI        => ORDER MANAGEMENT INTERFACED &d  INI        => INVENTORY INTERFACED &el');
UTL_FILE.PUT_LINE(handle,'&sld  PIKF       => PICKABLE_FLAG               &d  PIK_Q      => PICKED_QUANTITY &el');
UTL_FILE.PUT_LINE(handle,'&sld  SHP_Q      => SHIPPED QUANTITY            &d  ORG_SUB    => ORIGINAL SUBINVENTORY   &el');
UTL_FILE.PUT_LINE(handle,'&sld  CUR_SUB    => CURRENT SUBINVENTORY        &d  CF         => CONTAINER FLAG &el');
UTL_FILE.PUT_LINE(handle,'&sld  FL_PERC    => FILL PERCENTAGE             &d  SHP_SET    => SHIP_SET_ID  &el');
UTL_FILE.PUT_LINE(handle,'&sld  STA        => SHIP TOLERANCE ABOVE        &d  STB        => SHIP TOLERANCE BELOW &el');
UTL_FILE.PUT_LINE(handle,'&sld  TRX_TEMP_ID=> TRANSACTION_TEMP_ID         &d  TOP_MO_LIN => TOP_MODEL_LINE_ID &el');
UTL_FILE.PUT_LINE(handle,'&sld  SHP_MC     => SHIP_MODEL_COMPLETE_FLAG    &d  SPL_DEL_DET_ID ==> SPLIT FROM DEL.DETAIL ID &el &et');

UTL_FILE.PUT_LINE(handle,'&f &f');

UTL_FILE.PUT_LINE(handle,'&std &sh <a NAME="WSN">WSH_SERIAL_NUMBERS (WSN)</a> &dh &sp &eh');
UTL_FILE.PUT_LINE(handle,'&sld  DET_DET_ID => DELIVERY DETAIL ID          &d  DLV_Q      => DELIVERED QUANTITY &el');
UTL_FILE.PUT_LINE(handle,'&sld  REL_STATUS => RELEASED STATUS             &d  CAN_Q      => CANCELLED QUANTITY &el');
UTL_FILE.PUT_LINE(handle,'&sld  SRQ_Q      => SOURCE REQUESTED QUANTITY   &d  SRQ_U      => SOURCE REQUESTED QUANTITY UOM &el');
UTL_FILE.PUT_LINE(handle,'&sld  REQ_Q      => REQUESTED QUANTITY          &d  REQ_U      => REQUESTED QUANTITY UOM &el');
UTL_FILE.PUT_LINE(handle,'&sld  OMI        => ORDER MANAGEMENT INTERFACED &d  INI        => INVENTORY INTERFACED &el');
UTL_FILE.PUT_LINE(handle,'&sld  PIKF       => PICKABLE_FLAG               &d  PIK_Q      => PICKED_QUANTITY &el');
UTL_FILE.PUT_LINE(handle,'&sld  SHP_Q      => SHIPPED QUANTITY            &d  ORG_SUB    => ORIGINAL SUBINVENTORY   &el');
UTL_FILE.PUT_LINE(handle,'&sld  CUR_SUB    => CURRENT SUBINVENTORY        &d  CF         => CONTAINER FLAG &el');
UTL_FILE.PUT_LINE(handle,'&sld  FL_PERC    => FILL PERCENTAGE             &d  SHP_SET    => SHIP_SET_ID  &el');
UTL_FILE.PUT_LINE(handle,'&sld  STA        => SHIP TOLERANCE ABOVE        &d  STB        => SHIP TOLERANCE BELOW &el');
UTL_FILE.PUT_LINE(handle,'&sld  SHP_MC     => SHIP_MODEL_COMPLETE_FLAG    &d  SPL_DEL_DET_ID ==> SPLIT FROM DEL.DETAIL ID &el &et');

UTL_FILE.PUT_LINE(handle,'&f &f');

if substr(UPPER(nvl('&prt_inv','Y')),1,1) = 'Y' then

UTL_FILE.PUT_LINE(handle,'&std &sh <a NAME="MR">MTL_RESERVATIONS (RES)</a> &dh &sp &eh');
UTL_FILE.PUT_LINE(handle,'&sld  RES_Q      => PRIMARY RESERVATION QUANTITY &d  DET_Q      => DETAILED QUANTITY   &el');
UTL_FILE.PUT_LINE(handle,'&sld  DS_TYPE    => DEMAND SOURCE TYPE           &d  WH_ID      => ORGANIZATION ID    &el');
UTL_FILE.PUT_LINE(handle,'&sld  SS_TYPE_ID => SUPPLY SOURCE TYPE ID        &d &sp &el &et');

UTL_FILE.PUT_LINE(handle,'&f &f');

UTL_FILE.PUT_LINE(handle,'&std &sh <a NAME="MTI">MTL_TRANSACTIONS_INTERFACE (MTI)</a> &dh &sp &eh');
UTL_FILE.PUT_LINE(handle,'&sld  TRX_INT_ID  => TRANSACTION_INTERFACE_ID &d PRM_Q      => PRIMARY QUANTITY &el');
UTL_FILE.PUT_LINE(handle,'&sld  LCK         => LOCK CODE                &d FROM_SUB   => FROM SUBINVENTORY &el');
UTL_FILE.PUT_LINE(handle,'&sld  FROM_LOC_ID => FROM LOCATOR ID          &d TRANS_MODE => TRANSACTION MODE &el &et');

UTL_FILE.PUT_LINE(handle,'&f &f');

UTL_FILE.PUT_LINE(handle,'&std &sh <a NAME="TMP">MTL_MATERIAL_TRANSACTIONS_TEMP (TMP) - UNPICKED LINES</a> &dh &sp &eh');
UTL_FILE.PUT_LINE(handle,'&sld  MTL_TRNS_ID => MTL TRANSACTION ID          &d  PICK_SLIP   => PICK SLIP NUMBER &el');
UTL_FILE.PUT_LINE(handle,'&sld  PRM_Q       => PRIMARY QUANTITY            &d  LCK         => LOCK CODE &el');
UTL_FILE.PUT_LINE(handle,'&sld  FROM_SUB    => FROM SUBINVENTORY           &d  FROM_LOC_ID => FROM LOCATOR ID &el');
UTL_FILE.PUT_LINE(handle,'&sld  TRANS_MODE  => TRANSACTION MODE            &d  &sp &el &et');

UTL_FILE.PUT_LINE(handle,'&f &f');

UTL_FILE.PUT_LINE(handle,'&std &sh <a NAME="TRN">MTL_MATERIAL_TRANSACTIONS (TRN) - PICKED LINES</a> &dh &sp &eh');
UTL_FILE.PUT_LINE(handle,'&sld  MTL_TRNS_ID => MTL TRANSACTION ID          &d  TRANS_TYPE  => TRANSACTION TYPE &el');
UTL_FILE.PUT_LINE(handle,'&sld  PRM_Q       => PRIMARY QUANTITY            &d  LCK         => LOCK CODE &el');
UTL_FILE.PUT_LINE(handle,'&sld  FROM_SUB    => FROM SUBINVENTORY           &d  FROM_LOC_ID => FROM LOCATOR ID &el');
UTL_FILE.PUT_LINE(handle,'&sld  TO_SUB      => TO SUBINVENTORY             &d  TO_LOC_ID   => TO LOCATOR ID &el');
UTL_FILE.PUT_LINE(handle,'&sld  TRANS_MODE  => TRANSACTION MODE            &d  &sp &el &et');

UTL_FILE.PUT_LINE(handle,'&f &f');

UTL_FILE.PUT_LINE(handle,'&std &sh <a NAME="UNT">MTL_UNIT_TRANSACTIONS (UNT)</a> &dh &sp &eh');
UTL_FILE.PUT_LINE(handle,'&sld  TRANS_ID    => TRANSACTION ID              &d  SUBINV          => SUBINVENTRY CODE &el');
UTL_FILE.PUT_LINE(handle,'&sld  LOC_ID      => LOCATOR ID                  &d  SERIAL_NUM      => SERIAL NUMBER &el');
UTL_FILE.PUT_LINE(handle,'&sld  WH_ID       => WAREHOUSE ID                &d  TRX_DATE        => TRANSACTION DATE &el');
UTL_FILE.PUT_LINE(handle,'&sld  TRX_SRC_ID  => TRANSACTION SOURCE ID       &d  TRX_SRC_TYPE_ID => TRANSACTION SOURCE TYPE ID &el');
UTL_FILE.PUT_LINE(handle,'&sld  CUST_ID     => CUSTOMER ID                 &d  SHIP_ID         => SHIPMENT ID &el &et');

UTL_FILE.PUT_LINE(handle,'&f &f');

UTL_FILE.PUT_LINE(handle,'&std &sh <a NAME="MOV">MTL_TXN_REQUEST_LINES_V (MOV) - MOVE TRANSACTIONS</a> &dh &sp &eh');
UTL_FILE.PUT_LINE(handle,'&sld  MO_LINE_ID   => MOVE ORDER LINE ID          &d  MO_NUMBER   => MOVE ORDER NUMBER &el');
UTL_FILE.PUT_LINE(handle,'&sld  MV_HDR_ID    => MOVE ORDER HEADER ID        &d  MV_LINE_NUM => MOVE ORDER LINE NUMBER &el');
UTL_FILE.PUT_LINE(handle,'&sld  MV_LINE_STAT => MOVE ORDER LINE STATUS      &d  TXN_SLINE_ID=> TRANSACTION SOURCE LINE ID &el');
UTL_FILE.PUT_LINE(handle,'&sld  PRM_Q        => PRIMARY QUANTITY            &d  DLV_Q       => DELIVERED QUANTITY &el');
UTL_FILE.PUT_LINE(handle,'&sld  DTL_Q        => DETAILED QUANTITY           &d  WH_ID       => WAREHOUSE ID &el');
UTL_FILE.PUT_LINE(handle,'&sld  FROM_SUB     => FROM SUBINVENTORY           &d  FROM_LOC_ID => FROM LOCATOR ID &el');
UTL_FILE.PUT_LINE(handle,'&sld  TO_SUB       => TO SUBINVENTORY             &d  TO_LOC_ID   => TO LOCATOR ID &el');
UTL_FILE.PUT_LINE(handle,'&sld  LOT_NUM      => LOT NUMBER                  &d  TRNS_HEAD_ID=> TRANSACTION HEADER ID &el &et');

UTL_FILE.PUT_LINE(handle,'&f &f');

end if; -- prt_inv

if substr(UPPER(nvl('&prt_ar','Y')),1,1) = 'Y' then

UTL_FILE.PUT_LINE(handle,'&std &sh <a NAME="RAI">RA_INTERFACE_LINES (RAI)</a>  &dh &sp &dh &sp &eh');
UTL_FILE.PUT_LINE(handle,'&sld  QTY    => QUANTITY             &d   IR_ID    => INVOICING RULE ID    &d  TR       => TAX RATE  &el');
UTL_FILE.PUT_LINE(handle,'&sld  ORD_Q  => ORDERED QUANTITY     &d   PA_ID_11 => PRICE ADJUSTMENT ID  &d  EF       => TAX EXEMPT FLAG &el');
UTL_FILE.PUT_LINE(handle,'&sld  SO_LIN => SALES ORDER LINE ID  &d   C_RATE   => CONVERSION RATE      &d  S_TAX_ID => SALES TAX ID &el');
UTL_FILE.PUT_LINE(handle,'&sld  AR_ID  => ACCOUNTING RULE ID   &d   C_DATE   => CONVERSION DATE      &d  CURR     => CURRENCY CODE &el');
UTL_FILE.PUT_LINE(handle,'&sld  WH_ID  => WAREHOUSE ID         &d   VAT_ID   => VALUE ADDED TAX ID   &d  EF       => TAX EXEMPT FLAG &el');
UTL_FILE.PUT_LINE(handle,'&sld  TERR_ID=> TERRITORY ID         &d   &sp      &d  &sp  &el  &et');

UTL_FILE.PUT_LINE(handle,'&f &f');

UTL_FILE.PUT_LINE(handle,'&std &sh <a NAME="RAH">RA_CUSTOMER_TRX (RAH) - INVOICE HEADERS</a>  &dh &sp &dh &sp &eh');
UTL_FILE.PUT_LINE(handle,'&sld  CUST_TRX_ID => CUSTOMER TRANSACTION ID  &d TRX_NUMBER  => TRANSACTION NUMBER  &d TRX_DATE   => TRANSACTION DATE  &el');
UTL_FILE.PUT_LINE(handle,'&sld  CURR        => CURRENCY                 &d COMP_FL     => COMPLETE FLAG       &d IR_ID      => INVOICING RULE ID &el &et');

UTL_FILE.PUT_LINE(handle,'&f &f');

UTL_FILE.PUT_LINE(handle,'&std &sh <a NAME="RAL">RA_CUSTOMER_TRX_LINES (RAL) - INVOICE LINES</a>  &dh &sp &dh &sp &eh');
UTL_FILE.PUT_LINE(handle,'&sld  TRX_LINE_ID => TRANSACTION LINE ID  &d CUST_TRX_ID => CUSTOMER TRANSACTION ID  &d TRX_NUMBER => TRANSACTION NUMBER &el');
UTL_FILE.PUT_LINE(handle,'&sld  ORD_Q       => ORDERED QUANTITY     &d INV_Q       => INVOICED QUANTITY        &d CRD_Q      => CREDITED QUANTITY &el');
UTL_FILE.PUT_LINE(handle,'&sld  EXTD_AMT    => EXTENDED AMOUNT      &d REV_AMT     => REVISED AMOUNT           &d PA_ID_11   => PRICE ADJUSTMENT ID &el');
UTL_FILE.PUT_LINE(handle,'&sld  TF          => TAXABLE FLAG         &d TR          => TAX RATE                 &d VAT_ID     => VALUE ADDED TAX ID &el  &et');

end if; -- prt_ar

UTL_FILE.PUT_LINE(handle,'&f &f');

-- Closing Time
select '&std &sh Script completion time: &eh &sld '||to_char(sysdate,'DD-MON-RR_HH24:MI:SS')||' &el &et &f &f'
       into text
  from dual;
UTL_FILE.PUT_LINE(handle,text);

UTL_FILE.PUT_LINE(handle,'</HTML>');

-- Close output file

   UTL_FILE.FCLOSE(handle);
END;
/

