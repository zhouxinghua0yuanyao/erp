REM +=================================================================+
REM |                Copyright (c) 2014, 2014 Oracle Corporation      |
REM |                   Redwood Shores, California, USA               |
REM |                        All rights reserved.                     |
REM +=================================================================+
REM |  Name
REM |    ibygdfs.pls
REM |
REM |  Description - Package Body IBY_DATA_FIX_UTILITY_PKG
REM |                This package is a storage of common  procedures
REM |                that can be called from the (generic) datafix
REM |                scripts.
REM |
REM |
REM |
REM |  History
REM |    Created By:  vkarlapu (8th Sep,2010)
REM +==================================================================+
REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=pls \
REM dbdrv: checkfile(120.0.12010000.2=120.1)(120.1.12010000.26=120.29):~PROD:~PATH:~FILE

SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK
WHENEVER OSERROR EXIT FAILURE ROLLBACK

CREATE OR REPLACE PACKAGE IBY_DATA_FIX_UTILITY_PKG AS
/* $Header: ibygdfs.pls 120.1.12020000.2 2014/08/21 10:46:34 asarada noship $ */



  /* Procedure to open the log files on the instance where the datafix
     script is being run. The log file contains the log messages
     and the report outputs written by the data fix scripts.
     The file location is the environment's 'utl_file_dir' parameter. */

  PROCEDURE Open_Log_File
       (P_Bug_Number             IN      varchar2,
        P_File_Location          OUT NOCOPY VARCHAR2);



  /* Procedure to close the log files on the instance once all the log
     messages are written to it. */

  PROCEDURE Close_Log_Out_Files; 


 /* Procedure to print messages in the Log file */
  PROCEDURE Print
      (P_MESSAGE                 IN       VARCHAR2);
       


/* Procedure to print the values in the table and column list
   passed as parameters, in HTML table format, into the Log file. */
  Procedure Print_Html_Table
      (p_select_list       in VARCHAR2,
       P_TABLE_IN          IN VARCHAR2,
       P_WHERE_IN          IN VARCHAR2,
       P_calling_sequence  in VARCHAR2) ;       
 
 
 
  PROCEDURE apps_initialize
      (p_user_name          IN           FND_USER.USER_NAME%TYPE,
       P_RESP_NAME          IN           FND_RESPONSIBILITY_TL.RESPONSIBILITY_NAME%TYPE,
       p_calling_sequence   IN           VARCHAR2);
       
       
END IBY_DATA_FIX_UTILITY_PKG;
/
COMMIT;
EXIT;
--sho err;
