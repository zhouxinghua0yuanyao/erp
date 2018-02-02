DECLARE
  --c1.batch_id NUMBER := 779;
  CURSOR cur_error IS
    SELECT pie.error_message,
           pie.error_message_name,
           pie.batch_id
      FROM po_interface_errors pie
    /*WHERE pie.BATCH_ID >100*/
    ;
BEGIN

  FOR c1 IN cur_error
  LOOP
  
    dbms_output.put_line(c1.batch_id || '--' || c1.error_message_name || '--' || c1.error_message);
  
    --delete rcv_lots_interface
    DELETE FROM rcv_lots_interface t
     WHERE EXISTS (SELECT 1
              FROM rcv_transactions_interface w
             WHERE t.interface_transaction_id = w.interface_transaction_id
               AND w.group_id = c1.batch_id);
    --mtl_transaction_lots_interface
    DELETE FROM mtl_transaction_lots_interface t
     WHERE EXISTS (SELECT 1
              FROM rcv_transactions_interface w
             WHERE t.product_transaction_id = w.interface_transaction_id
               AND w.group_id = c1.batch_id);
    --  delete mtl_transaction_lots_temp
    DELETE FROM mtl_transaction_lots_temp t
     WHERE EXISTS (SELECT 1
              FROM rcv_transactions_interface w
             WHERE t.transaction_temp_id = w.interface_transaction_id
               AND w.group_id = c1.batch_id);
    --  delete mtl_serial_numbers_temp
    DELETE FROM mtl_serial_numbers_temp t
     WHERE EXISTS (SELECT 1
              FROM rcv_transactions_interface w
             WHERE t.transaction_temp_id = w.interface_transaction_id
               AND w.group_id = c1.batch_id);
    -- delete rcv_headers_interface
    DELETE FROM rcv_headers_interface t
     WHERE t.group_id = c1.batch_id;
    -- delete rcv_transactions_interface
    DELETE FROM rcv_transactions_interface t
     WHERE t.group_id = c1.batch_id;
  END LOOP;
END ;
