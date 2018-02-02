REM  ===============================================================
REM   This script aims to check backup table:
REM   1. RTI_BAK
REM   2. RLI_BAK
REM   3. MTLI_BAK
REM   4. MTLT_BAK
REM   5. RSI_BAK
REM   6. MSNI_BAK
REM   7. MSNT_BAK
REM   No inputs are required for this script.
REM   ==============================================================

set serveroutput on size 1000000;
declare
  l_tbl_exists number := 0;

  cursor cur_bkup_tbl is
    select table_name
      from dba_tables
     where table_name in ('RTI_BAK',
                          'RLI_BAK',
                          'MTLI_BAK',
                          'MTLT_BAK',
                          'RSI_BAK',
                          'MSNI_BAK',
                          'MSNT_BAK');

begin

  dbms_output.put_line('Checking backup table..');
  for tbl_rec in cur_bkup_tbl loop
    l_tbl_exists := 1;
    dbms_output.put_line('Table ' || tbl_rec.table_name ||
                         ' already exists.');
  end loop;

  if l_tbl_exists = 1 then
    dbms_output.put_line('-------------------------------------------------------------------------');
    dbms_output.put_line('For the tables listed above, please verify their data, drop/rename them and then re-run this script');
    dbms_output.put_line('-------------------------------------------------------------------------');
    return;
  else
      dbms_output.put_line('Pass! Please continue to run subsequent script(s)!');  
  end if;

exception
  when others then
    dbms_output.put_line('Encount error:' || sqlerrm);
end;
/
