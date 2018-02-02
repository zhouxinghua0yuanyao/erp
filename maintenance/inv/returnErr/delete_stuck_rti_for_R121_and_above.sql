/*
 * Generic script to delete RTI records which are not appearing in Transaction Status Summary form for deletion.
 * Please refer to bug 9919054.
 *
 * Input for the script :-
 * please replace &interface_transaction_ids with all interface transaction ids separated by comma. 
 *
 * Important Note :
 * Please ensure the scripts are ran on TEST instance first and tested for data 
 * correctness thoroughly. After the scripts are ran, please check the data and 
 * only the correct records are updated before committing.
 * If all goes well, the script can be promoted to the PRODUCTION instance.
 *
 */


--back up date in rti, rli, mtli,mtlt, rsi, msni and msnt
create table rti_bak as 
select * from rcv_transactions_interface
where interface_transaction_id in (&interface_transaction_ids)
and (processing_status_code in ('ERROR','PENDING','RUNNING') or (processing_status_code = 'COMPLETED' and transaction_status_code = 'ERROR'))
and lcm_shipment_line_id is null   /* LCM shipment receipt */
and unit_landed_cost is null          /* LCM shipment receipt */
and (header_interface_id is NULL OR mobile_txn = 'Y');     


create table rli_bak as
select * from rcv_lots_interface 
where  interface_transaction_id in (select interface_transaction_id
                                    from rti_bak);
create table mtli_bak as 
select * from mtl_transaction_lots_interface
where  product_code = 'RCV' 
and    product_transaction_id in  (select interface_transaction_id
                                    from rti_bak);

create table mtlt_bak as 
select * from mtl_transaction_lots_temp
where  product_code = 'RCV' 
and    product_transaction_id in  (select interface_transaction_id
                                    from rti_bak);

create table rsi_bak as
select * from rcv_serials_interface
where  interface_transaction_id  in  (select interface_transaction_id
                                     from rti_bak);
create table msni_bak as
select * from mtl_serial_numbers_interface
where  product_code = 'RCV'
and    product_transaction_id in  (select interface_transaction_id
                                    from rti_bak);

create table msnt_bak as
select * from mtl_serial_numbers_temp
where  product_code = 'RCV'
and    product_transaction_id in  (select interface_transaction_id
                                    from rti_bak);

--delete data in rti, rli, mtli,mtlt, rsi, msni and msnt
delete rcv_transactions_interface 
where interface_transaction_id in  (select interface_transaction_id
                                    from rti_bak);

delete rcv_lots_interface 
where  interface_transaction_id in (select interface_transaction_id
                                    from rti_bak);

delete mtl_transaction_lots_interface
where  product_code = 'RCV'
and    product_transaction_id in (select interface_transaction_id 
                               from rti_bak);

delete mtl_transaction_lots_temp
where  product_code = 'RCV' 
and    product_transaction_id in  (select interface_transaction_id
                                    from rti_bak);

delete rcv_serials_interface
where  interface_transaction_id  in  (select interface_transaction_id
                                     from rti_bak);

delete mtl_serial_numbers_interface
where  product_code = 'RCV'
and    product_transaction_id in  (select interface_transaction_id
                                    from rti_bak);

delete mtl_serial_numbers_temp
where  product_code = 'RCV'
and    product_transaction_id in  (select interface_transaction_id
                                    from rti_bak);