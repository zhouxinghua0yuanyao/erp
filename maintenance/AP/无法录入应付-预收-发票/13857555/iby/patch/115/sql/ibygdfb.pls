REM +=================================================================+
REM |                Copyright (c) 2014, 2014 Oracle Corporation      |
REM |                   Redwood Shores, California, USA               |
REM |                        All rights reserved.                     |
REM +=================================================================+
REM |  Name
REM |    ibygdfb.pls
REM |
REM |  Description - Package Body IBY_DATA_FIX_UTILITY_PKG
REM |                This package is a storage of common  procedures
REM |                that can be called from the (generic) datafix
REM |                scripts.
REM |
REM |
REM |  History
REM |    Created By:  vkarlapu (8th Sep,2010)
REM +==================================================================+
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.1.12010000.26=120.29):~PROD:~PATH:~FILE

SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK
WHENEVER OSERROR EXIT FAILURE ROLLBACK

CREATE OR REPLACE PACKAGE BODY IBY_DATA_FIX_UTILITY_PKG AS
/* $Header: ibygdfb.pls 120.0.12020000.2 2014/08/21 10:47:15 asarada noship $ */

G_CURRENT_RUNTIME_LEVEL     NUMBER       := FND_LOG.G_CURRENT_RUNTIME_LEVEL;
G_LEVEL_UNEXPECTED CONSTANT NUMBER       := FND_LOG.LEVEL_UNEXPECTED;
G_LEVEL_ERROR      CONSTANT NUMBER       := FND_LOG.LEVEL_ERROR;
G_LEVEL_EXCEPTION  CONSTANT NUMBER       := FND_LOG.LEVEL_EXCEPTION;
G_LEVEL_EVENT      CONSTANT NUMBER       := FND_LOG.LEVEL_EVENT;
G_LEVEL_PROCEDURE  CONSTANT NUMBER       := FND_LOG.LEVEL_PROCEDURE;
G_LEVEL_STATEMENT  CONSTANT NUMBER       := FND_LOG.LEVEL_STATEMENT;
G_MODULE_NAME      CONSTANT VARCHAR2(50) :='IBY.PLSQL.IBY_DATA_FIX_UTILITY_PKG.';


log_file_handler UTL_FILE.FILE_TYPE;
  /* Procedure to open the log files on the instance where the datafix
     script is being run. The log file contains the log messages
     and the report outputs written by the data fix scripts.
     The file location is the environment's 'utl_file_dir' parameter. */

  PROCEDURE Open_Log_File
       (P_Bug_Number             IN      varchar2,
        P_File_Location          OUT NOCOPY VARCHAR2) IS

    l_log_file         VARCHAR2(30);
    L_OUT_FILE         varchar2(30);    
    l_file_location    v$parameter.value%type;
    No_Utl_Dir         EXCEPTION;
    l_date             VARCHAR2(30);
    L_MESSAGE          VARCHAR2(500);
    l_module_name   varchar2(100) := G_MODULE_NAME||'Open_Log_File';

  BEGIN
     SELECT TO_CHAR(SYSDATE, '-HH24:MI:SS')
     INTO   l_date
     FROM   DUAL;

    l_log_file := p_bug_number||l_date||'.html';
    l_out_file := null;

    SELECT decode(instr(value,','),0,value,
                   SUBSTR (value,1,instr(value,',') - 1))
    INTO   l_file_location
    FROM   v$parameter
    WHERE  name = 'utl_file_dir';

    IF l_file_location IS NULL THEN
      RAISE No_Utl_Dir;
    END IF;

    P_FILE_LOCATION:=L_FILE_LOCATION||'/'||L_LOG_FILE;
    
   /*FND_FILE.PUT_NAMES(l_log_file,
                       l_out_file,
                       L_FILE_LOCATION);*/
                       
    log_file_handler:= UTL_FILE.FOPEN(L_FILE_LOCATION,L_LOG_FILE,'A');                       
             
                          
  EXCEPTION
    WHEN OTHERS THEN
        l_message := l_module_name||'-Exception :: '||SQLERRM||'<p>';
       APP_EXCEPTION.RAISE_EXCEPTION;
  END Open_Log_File;



  /* Procedure to close the log files on the instance once all the log
     messages are written to it. */

  PROCEDURE Close_Log_Out_Files IS
  BEGIN
    UTL_FILE.FCLOSE(log_file_handler);
  END Close_Log_Out_Files;



 /* Procedure to print messages in the Log file */
  PROCEDURE Print
      (p_message                 IN       VARCHAR2) IS
    l_message          varchar2(500);
    L_CALLING_SEQUENCE VARCHAR2(500);
    l_module_name   varchar2(100) := G_MODULE_NAME||'Print';    
  BEGIN
     l_calling_sequence:=l_module_name;

     UTL_FILE.PUT_line(log_file_handler,p_message||'<p>');

  Exception
    WHEN OTHERS THEN
        l_message := l_calling_sequence||'-Exception :: '||SQLERRM||'<p>';
        UTL_FILE.PUT_line(log_file_handler,l_message);
      APP_EXCEPTION.RAISE_EXCEPTION;
  End Print;


/* Procedure to print the values in the table and column list
   passed as parameters, in HTML table format, into the Log file. */

Procedure Print_Html_Table
    (p_select_list       in VARCHAR2,
     p_table_in          in VARCHAR2,
     p_where_in          in VARCHAR2,
     P_calling_sequence  in VARCHAR2) IS

  l_calling_sequence varchar2(500);
   select_list1 varchar2(2000):=P_SELECT_LIST;

   TYPE string_tab IS TABLE OF VARCHAR2(100)
      INDEX BY BINARY_INTEGER;

   TYPE integer_tab IS TABLE OF NUMBER
      INDEX BY BINARY_INTEGER;

   colname string_tab;
   coltype string_tab;
   collen integer_tab;

   owner_nm VARCHAR2(100) := USER;
   table_nm VARCHAR2(100) := UPPER (p_table_in);
   where_clause VARCHAR2(1000) := LTRIM (UPPER (p_where_in));

   cur INTEGER := DBMS_SQL.OPEN_CURSOR;
   fdbk INTEGER := 0;

   string_value VARCHAR2(2000);
   number_value NUMBER;
   date_value DATE;

   dot_loc INTEGER;
   cur_pos INTEGER:=1;

   col_count INTEGER := 0;
   col_line LONG;
   col_list VARCHAR2(2000);
   L_MESSAGE varchar2(2000):='<table border="5"><tr>';
   l_module_name   varchar2(100) := G_MODULE_NAME||'Print_Html_Table';

begin
     l_calling_sequence:=l_module_name||'::'||p_calling_sequence;
   dot_loc := INSTR (table_nm, '.');
   IF dot_loc > 0
   THEN
      owner_nm := SUBSTR (table_nm, 1, dot_loc-1);
      table_nm := SUBSTR (table_nm, dot_loc+1);
   END IF;
   loop
   dot_loc := INSTR(select_list1,',');

   IF (DOT_LOC<=0) THEN
    col_list := col_list || ', ' || select_list1;
    col_count := col_count + 1;
    colname (col_count) := select_list1;
    l_message:=l_message||'<th>'||colname (col_count)||'</th></tr>';
   ELSE
    col_list := col_list || ', ' || SUBSTR (select_list1, 1, dot_loc-1);
    col_count := col_count + 1;
    colname (col_count) := SUBSTR (select_list1, 1, dot_loc-1);
    cur_pos:=dot_loc+1;
      select_list1:=SUBSTR (select_list1, dot_loc+1);

    l_message:=l_message||'<th>'||colname (col_count)||'</th>';
   end if;

      SELECT data_type,DATA_LENGTH
        INTO coltype (col_count) ,collen(col_count)
        FROM all_tab_columns
       WHERE owner = owner_nm
         AND table_name = table_nm
         AND column_name=colname (col_count);

     EXIT WHEN (DOT_LOC<=0);

   end loop;
    col_list := RTRIM (LTRIM (col_list, ', '), ', ');

       print(l_message);
 

   IF where_clause IS NOT NULL
   THEN
      IF (where_clause NOT LIKE 'GROUP BY%' AND
          where_clause NOT LIKE 'ORDER BY%')
      THEN
         where_clause :=
            'WHERE ' || LTRIM (where_clause, 'WHERE');
      END IF;
   END IF;

   DBMS_SQL.PARSE
      (cur,
       'SELECT ' || col_list ||
       '  FROM ' || p_table_in || ' ' || where_clause,
       1);

   FOR col_ind IN 1 .. col_count
   LOOP
      IF (coltype(col_ind) IN ('CHAR', 'VARCHAR2'))
      THEN
         DBMS_SQL.DEFINE_COLUMN
            (cur, col_ind, string_value, collen (col_ind));
      ELSIF (coltype(col_ind) = 'NUMBER')
      THEN
         DBMS_SQL.DEFINE_COLUMN (cur, col_ind, number_value);

      ELSIF (coltype(col_ind) = 'DATE')
      THEN
         DBMS_SQL.DEFINE_COLUMN (cur, col_ind, date_value);
      END IF;
   END LOOP;

   fdbk := DBMS_SQL.EXECUTE (cur);
   LOOP
      fdbk := DBMS_SQL.FETCH_ROWS (cur);
      EXIT WHEN fdbk = 0;

      col_line := NULL;
     l_message:='<tr>';
      FOR col_ind IN 1 .. col_count
      LOOP
         IF (coltype(col_ind) IN ('CHAR', 'VARCHAR2'))
         THEN

            DBMS_SQL.COLUMN_VALUE
               (cur, col_ind, string_value);

         ELSIF (coltype(col_ind) = 'NUMBER')
         THEN

            DBMS_SQL.COLUMN_VALUE
               (cur, col_ind, number_value);
            string_value := TO_CHAR (number_value);

         ELSIF (coltype(col_ind) = 'DATE')
         THEN

            DBMS_SQL.COLUMN_VALUE
               (cur, col_ind, date_value);
            string_value := date_value;
         END IF;

         col_line :=
            col_line || ' ' ||
            RPAD (NVL (string_value, ' '), collen (col_ind));
            l_message:=l_message||'<td>'||NVL (string_value, ' ')||'</td>';

      END LOOP;
      l_message:=l_message||'</tr>';


         Print(l_message);


   END LOOP;

         print('</table>');

  Exception
    WHEN OTHERS THEN
        l_message := 'SELECT ' || col_list ||
               '  FROM ' || P_TABLE_IN || ' ' || WHERE_CLAUSE||'<p>';
        print(l_message);

        l_message := l_calling_sequence||'-Exception :: '||SQLERRM||'<p>';
        print(l_message);

      APP_EXCEPTION.RAISE_EXCEPTION;
END Print_Html_Table;


PROCEDURE apps_initialize
      (p_user_name          IN           FND_USER.USER_NAME%TYPE,
       p_resp_name          IN           FND_RESPONSIBILITY_TL.RESPONSIBILITY_NAME%TYPE,
       p_calling_sequence   IN           VARCHAR2) IS

  l_user_id                              NUMBER;
  l_resp_id                              NUMBER;
  l_application_id                       NUMBER := 200;
  l_debug_info                           VARCHAR2(4000);
  l_error_log                            LONG;
  L_CALLING_SEQUENCE                     VARCHAR2(4000);
  l_module_name   varchar2(100) := G_MODULE_NAME||'Open_Log_File';  
BEGIN

  l_calling_sequence := l_module_name||'::'||p_calling_sequence;

  l_debug_info := 'Before fetching the User Details ';
  BEGIN
    SELECT fu.user_id
      INTO l_user_id
      FROM fnd_user fu
     WHERE fu.user_name = p_user_name;

  EXCEPTION
    WHEN OTHERS THEN
      print('User '||p_user_name||' Not Found');
      APP_EXCEPTION.RAISE_EXCEPTION();
  END;

  l_debug_info := 'Before fetching the responsibility details';
  BEGIN
    SELECT fr.responsibility_id
      INTO l_resp_id
      FROM fnd_responsibility_tl fr
     WHERE fr.responsibility_name = p_resp_name
       AND rownum = 1;

  EXCEPTION
    WHEN OTHERS THEN
      print('Responsibility '||p_resp_name||' Not Found');
      APP_EXCEPTION.RAISE_EXCEPTION();
  END;

  l_debug_info := 'Before Initializing the Application';
  FND_GLOBAL.apps_initialize
    (l_user_id,
     l_resp_id,
     l_application_id);

EXCEPTION
  WHEN OTHERS THEN
    IF SQLCODE <> -20001 THEN
      l_error_log := ' Encountered an Unhandled Exception, '||SQLCODE||'-'||SQLERRM||
                     ' in '||l_calling_sequence||' while performing '||l_debug_info;
      Print(l_error_log);
    END IF;
    APP_EXCEPTION.RAISE_EXCEPTION();
END;


END IBY_DATA_FIX_UTILITY_PKG;
/
COMMIT;
EXIT;
--sho err;
