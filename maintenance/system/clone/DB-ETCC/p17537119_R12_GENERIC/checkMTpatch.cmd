@ECHO OFF
Setlocal EnableDelayedExpansion
REM Main Start

set CMDDIR=%~dp0
set s_filename=%0%

:: Generate random number
SET _rand=
for /f "skip=1" %%x in ('wmic os get localdatetime') do if not defined _rand SET _rand=%%x
for /f "tokens=1 delims=." %%y in ('echo %_rand%') do set _rand=%%y
set _rand=%_rand:~6%

:: Defined the log file
SET LOGFILE=%CMDDIR%checkMTpatch_%_rand%.log

call :prnt "\n"
call :prnt " +===============================================================+"
call :prnt " |       Copyright (c) 2005, 2017 Oracle and/or its affiliates   |"
call :prnt " |                     All rights reserved                       |"
call :prnt " |              Oracle E-Business Suite Release 12.2             |"
call :prnt " |         Application Tier Technology Codelevel Checker         |"
call :prnt " +===============================================================+"
call :prnt "\n"
REM $Header: checkMTpatch.cmd 120.0.12020000.12 2017/01/11 14:26:10 paholman noship $

REM This script checks that the prerequisite patches have been applied to 
REM the application tier ORACLE_HOME, and should be run before an upgrade.
REM It uses the context from the existing applications environment.
REM If this is not set, it prompts for a context file.

SET commasep=""
SET TCCresult=MISSING

REM Missing fix or fix to be rolled back
SET MissingBugFix=""   

REM ER 17588765
SET s_filename=%0%
SET doubleline===================================================================
SET singleline=------------------------------------------------------------------


REM  Messages
SET promptenterctxfile=Enter full path to application tier context file:  
SET msgctxloc=Verify location and rerun this script.
SET msgctxcheck=Verify context file is valid and then rerun this script.
SET msgctxid1=Unable to read the value of
SET msgctxid2=from the context file
SET msglocate=Unable to locate
SET msgresultbugsmissing=One or more products have bugfixes missing.
SET msgapplymissing=Apply the missing patches and rerun this script.
SET msgfsincomplete=File system appears to be incomplete.
SET msgunabletosave=Unable to store results in the database.
SET msgensuredbup=Ensure the database is accessible, then rerun this script.
SET msgsaved=These results have been stored in the database.
SET msgdbnotavail=Database not available. Results could not be stored.
SET msgresultsnotstored=Results could not be stored.
SET msgtableexist=Table to store results already exists in database.
SET msgtablenotexist=Table to store results does not exist in database.
SET msgcreatedtable=Created table to store results.
SET msgfailtocreatetable=Failed to create table to store results.
SET msgresultallapplied=All required one-offs are confirmed as present.
SET msgbugfix=bugfixes
SET msgcustomnote=
SET msgbugtitle=Bugfix
SET msgwlsmissing=The above list shows missing patches for Oracle WebLogic Server. If you have applied other Oracle WebLogic Server patches, they may have included the bugfixes needed. Contact Oracle Support if you require assistance in determining whether this is the case.
SET msgmosid=See Doc ID 1594274.1 for the patches that provide the bugfixes. The footnotes for patches also apply to corresponding overlay patches.
SET contextfile=%CONTEXT_FILE%
SET ctxfilesource=currently set applications environment:

:: Get Context file

  SET contextfile=""
  
  IF "a%1" == "a" (
    IF DEFINED CONTEXT_FILE (
      SET contextfile=%CONTEXT_FILE%
      SET ctxfilesourcemsg=currently set applications environment:
      GOTO :checkcon
    ) else (
      GOTO :getcontextfile
    )
  )
  
  IF NOT "%1" == "contextfile" (
    ECHO Wrong Usage..
    ECHO Please run ETCC in either of the 2 ways:
    ECHO 1. %0 contextfile=APPS_CONTEXT_FILE
    ECHO 2. %0
    exit /b 1;
  ) else (
    IF [%2] == [] (
      GOTO :getcontextfile
    ) else (
      FOR /F "tokens=* delims==" %%A in ('ECHO %2') DO SET contextfile=%%A
      SET CONTEXT_FILE=%contextfile%
      SET ctxfilesourcemsg=command line argument:
    )
  )

  GOTO :checkcon
  
:: Get the context file as user input

:getcontextfile

  ECHO.
  ECHO Applications environment not set, so context file must be specified.
  set /P contextfile="Enter the full path to Application Tier context file : "
  SET CONTEXT_FILE=%contextfile%
  SET ctxfilesourcemsg=user input:


:checkcon

:: Check the context file
  IF NOT EXIST %contextfile% (
    call :prnt "Unable to identify the context file"
    call :prnt "%msgctxloc%"
    call :prnt "\n"
    EXIT /b 1;
  )
  
:: Display the context file being used
  call :prnt "Using context file from %ctxfilesourcemsg%"
  call :prnt "%contextfile%"
  call :prnt "\n"
  
REM Extract required values from contextfile

REM 1. Extract Tools Oracle Home

FINDSTR s_tools_oh %contextfile% >NUL 2>&1

IF NOT "%ERRORLEVEL%" == "0" (
  call :prnt "Unable to identify Tools home from context file."
  call :prnt "%msgctxid1% s_tools_oh %msgctxid%"
  call :prnt "%msgctxloc%"
  EXIT /b 1;
)

FOR /F "tokens=2 delims=>" %%A in ('FINDSTR s_tools_oh %contextfile%') do SET HOME_1="%%A"
FOR /F "tokens=1 delims=<" %%B in ('echo %HOME_1%') do SET HOME_2="%%B"
SET TOOLS_ORACLE_HOME=%HOME_2:"=%


REM 2. Extract Tools Oracle Home

FINDSTR s_file_edition_type %contextfile% >NUL 2>&1

IF NOT "%ERRORLEVEL%" == "0" (
  call :prnt "%msgctxid1% s_file_edition_type %msgctxid%"
  call :prnt "%msgctxloc%"
  EXIT /b 1;
)

FOR /F "tokens=2 delims=>" %%A in ('FINDSTR s_file_edition_type %contextfile%') do SET EDN_1="%%A"
FOR /F "tokens=1 delims=<" %%B in ('echo %EDN_1%') do SET EDN_2="%%B"
SET fileedition=%EDN_2:"=%



REM 3. Extract Current Base

FINDSTR s_current_base %contextfile% >NUL 2>&1

IF NOT "%ERRORLEVEL%" == "0" (
  call :prnt "%msgctxid1% s_current_base %msgctxid%"
  call :prnt "%msgctxloc%"
  EXIT /b 1;
)

FOR /F "tokens=2 delims=>" %%A in ('FINDSTR s_current_base %contextfile%') do SET bas_1="%%A"
FOR /F "tokens=1 delims=<" %%B in ('echo %bas_1%') do SET bas_2="%%B"
SET currentbase=%bas_2:"=%



REM 4. Extract  iasoraclehome

FOR /F "tokens=2 delims=>" %%A in ('FINDSTR s_weboh_oh %contextfile%') do SET web_1="%%A"
FOR /F "tokens=1 delims=<" %%B in ('echo %web_1%') do SET web_2="%%B"
SET iasoraclehome=%web_2:"=%


IF NOT EXIST %iasoraclehome% (
  call :prnt "Directory %iasoraclehome% could not be found."
  call :prnt "This %fileedition% %msgfsincomplete%"
  call :prnt "Exiting."

  EXIT /b 1;
)


REM 4. Fusion Middleware home

FINDSTR s_fmw_home %contextfile% >NUL 2>&1

IF NOT "%ERRORLEVEL%" == "0" (
  call :prnt "%msgctxid1% s_fmw_home %msgctxid%"
  call :prnt "%msgctxloc%%"
  EXIT /b 1;
)

FOR /F "tokens=2 delims=>" %%A in ('FINDSTR s_fmw_home %contextfile%') do SET fmw_1="%%A"
FOR /F "tokens=1 delims=<" %%B in ('echo %fmw_1%') do SET fmw_2="%%B"
SET fmwhome=%fmw_2:"=%


REM 5. TNS_ADMIN

FINDSTR s_tools_tnsadmin %contextfile% >NUL 2>&1

IF NOT "%ERRORLEVEL%" == "0" (
  call :prnt "%msgctxid1% s_tools_tnsadmin %msgctxid%"
  call :prnt "%msgctxloc%"
  EXIT /b 1;
)

FOR /F "tokens=2 delims=>" %%A in ('FINDSTR s_tools_tnsadmin %contextfile%') do SET tns_1="%%A"
FOR /F "tokens=1 delims=<" %%B in ('echo %tns_1%') do SET tns_2="%%B"
SET TNS_ADMIN=%tns_2:"=%



REM 5. Extract Environment File


FOR /F "tokens=2 delims=>" %%A in ('FINDSTR s_appsora_file %contextfile%') do SET efl_1="%%A"
FOR /F "tokens=1 delims=<" %%B in ('echo %efl_1%') do SET efl_2="%%B"
SET ebsenvfile=%efl_2:"=%


IF NOT EXIST %ebsenvfile%% (
  call :prnt "%msglocate% the environment file %ebsenvfile%"
  call :prnt "This %fileedition% %msgfsincomplete%"
  call :prnt "Exiting."
  EXIT /b 1
)


REM 6. Extract Two_Task

FINDSTR s_twotask %contextfile% >NUL 2>&1

IF NOT "%ERRORLEVEL%" == "0" (
  call :prnt "%msgctxid1% s_twotask %msgctxid%"
  call :prnt "%msgctxloc%"
  call :prnt "Exiting."
  EXIT /b 1;
)

FOR /F "tokens=2 delims=>" %%A in ('FINDSTR s_twotask %contextfile%') do SET tsk_1="%%A"
FOR /F "tokens=1 delims=<" %%B in ('echo %tsk_1%') do SET tsk_2="%%B"
SET twotask=%tsk_2:"=%


REM 6. Extract APPS and APPLSYS usernames from contextfile

FINDSTR s_apps_user %contextfile% >NUL 2>&1

IF NOT "%ERRORLEVEL%" == "0" (
  call :prnt "%msgctxid1% s_applsys_user %msgctxid%"
  call :prnt "%msgctxloc%"
  EXIT /b 1;
)

FOR /F "tokens=2 delims=>" %%A in ('FINDSTR s_apps_user %contextfile%') do SET apu_1="%%A"
FOR /F "tokens=1 delims=<" %%B in ('echo %apu_1%') do SET apu_2="%%B"
SET appsuser=%apu_2:"=%



FINDSTR s_applsys_user %contextfile% >NUL 2>&1

IF NOT "%ERRORLEVEL%" == "0" (
  call :prnt "Unable to identify APPLSYS user from context file."
  call :prnt "%msgctxloc%"
  EXIT /b 1;
)

FOR /F "tokens=2 delims=>" %%A in ('FINDSTR s_applsys_user %contextfile%') do SET asu_1="%%A"
FOR /F "tokens=1 delims=<" %%B in ('echo %asu_1%') do SET asu_2="%%B"
SET applsysuser=%asu_2:"=%



REM 7. Creating TEMP Folder

SET LOCAL_TEMP=%CMDDIR%tempmt
IF NOT EXIST %LOCAL_TEMP% (
  mkdir %LOCAL_TEMP%
  IF "%ERRORLEVEL%" == "0" GOTO :TEMP_OK
  call :prnt "Unable to create temp directory %LOCAL_TEMP%"
  EXIT /b 1;
)

:TEMP_OK

REM Get the list of one-offs to be applied
  SET BUGFIXES_APPLIED=%LOCAL_TEMP%\bugfixesApplied.lst
  
  SET FORMS_INVENTORY=%LOCAL_TEMP%\FormsInv.out
  SET COMMON_INVENTORY=%LOCAL_TEMP%\CommonInv.out
  SET HTTP_INVENTORY=%LOCAL_TEMP%\HTTPInv.out
  SET WLS_INVENTORY=%LOCAL_TEMP%\WLSInv.out

  SET FORMS_PATCHES_LIST=%LOCAL_TEMP%\FormsPatches.lst
  SET FORMSRSF_PATCHES_LIST=%LOCAL_TEMP%\FormsRsfPatches.lst
  SET COMMON_PATCHES_LIST=%LOCAL_TEMP%\CommonPatches.lst
  SET HTTP_PATCHES_LIST=%LOCAL_TEMP%\HttpPatches.lst
  SET HTTPRSF_PATCHES_LIST=%LOCAL_TEMP%\HttpRsfPatches.lst
  SET WLS_PATCHES_LIST=%LOCAL_TEMP%\WlsPatches.lst
  SET FINAL_LIST=%LOCAL_TEMP%\FinalList.lst
  
  SET missingbuglist=%LOCAL_TEMP%\missingbuglist.tmp
  SET CAPTURED_TAG_FILE=%PTCH_TMP%\tag.xml

  SET CAPTURED_TAG_FILE=%LOCAL_TEMP%\target.tag
  SET PATCHES_LIST=%LOCAL_TEMP%\patches.lst
  SET DISABLE_MAPPING=F

REM Extract Hostname

FOR /F %%i IN ('hostname') DO SET s_host=%%i
FOR %%Y IN (a b c d e f g h i j k l m n o p q r s t u v w x y z) DO SET s_host=!s_host:%%Y=%%Y!

ECHO %s_host% >%LOCAL_TEMP%\host_temp.txt
FINDSTR \. %LOCAL_TEMP%\host_temp.txt  >NUL 2>&1
IF %ERRORLEVEL%==0 GOTO :NEXT
GOTO :NEXT_OK

:NEXT
FOR /F "tokens=1 delims=." %%A in ('echo %s_host%') do (
  SET s_host=%%A
)

:NEXT_OK

  del %LOCAL_TEMP%\host_temp.txt

  SET v_count=1
  FOR /F "tokens=4 delims= " %%A in ('FINDSTR Header: !s_filename!') do (
    IF "!v_count!" == "1" (
      SET progver=%%A
    )
    SET /A v_count=!v_count! +1
  )
  
  call :prnt "\n"
  call :prnt "Starting Application Tier Technology Codelevel Checker, Version: %progver%"
  call :prnt "%date%%time%"
  call :prnt "Log file for this session: %LOGFILE%"

REM Set Bug Fix XML file name and verify it exists

  SET BUG_LIST_FILE=%CMDDIR%txk_R1220_MT_base_bugs.xml
  SET MAPPING_FILE=%CMDDIR%txk_R1220_MT_mappings.xml
  
  IF NOT EXIST %BUG_LIST_FILE% (
    call :prnt "%BUG_LIST_FILE% does not exist."
    call :prnt "Extract it from patch zip file to this script execution directory and then rerun MT-ETCC."
    call :prnt "\n"
    EXIT /b 1;
  )

REM Check version of script

  FOR /F "tokens=4,5,6 delims= " %%A in ('findstr Header: %BUG_LIST_FILE%') do  (
    SET xmlver=%%A
    SET xmlfiledate=%%B %%C
    call :prnt "\n"
    call :prnt "Bugfix XML file version: !xmlver!"
    call :prnt "This file will be used for identifying missing bugfixes."
  )

REM Check version of mapping file

  IF NOT EXIST %MAPPING_FILE% (
    call :prnt "%MAPPING_FILE% does not exist."
    call :prnt "Extract it from patch zip file to this script execution directory and then rerun MT-ETCC."
    call :prnt "\n"
    EXIT /b 1;
  )
  
  FOR /F "tokens=4,5,6 delims= " %%A in ('findstr Header: %MAPPING_FILE%') do  (
    SET mapver=%%A
    call :prnt "\n"
    call :prnt "Mapping XML file version: !mapver!"
    call :prnt "This file will be used for mapping bugfixes to patches."
  )

REM Get Consolidated Patch details

  FOR /F tokens^=2-4^ delims^=^" %%A in  ('findstr consolidated_patch %MAPPING_FILE%') do (
    SET consolpatchid=%%A
    SET consolpatchdesc=%%C
  )

REM Check FILE EDITION

  FOR /F "tokens=1" %%A in ('%iasoraclehome%\perl\bin\perl -e "print time"') do SET curdate=%%A
  call :prnt "\n"
  call :prnt "Checking for prerequisite bugfixes in File Edition: %fileedition%"

REM Set DB connect string

REM DB up-down flag
REM assuming DB is up

  SET dbup=1  
  call :prnt "\n"
  CALL :checkDB
  IF NOT "%dbup%" == "0" (
    SET etcctable=0
    SET storeresults=false
    CALL :checkADCodeLevel
    IF "!sqlresult!" == "STORE" CALL :checkForEtccTable
  )
  
REM Setting the default exit code

  SET patchesmissing="false"

REM Collect inventory details and run Comparison

:: Checking for Oracle Forms and Reports

  SET tshome=Oracle Forms and Reports
  SET tshead=%tshome%
  SET TECHNAME=TOOLS_HOME
  SET invreport=%FORMS_INVENTORY%
  SET TS_HOME=%ORACLE_HOME%
  SET TOOLS_HOME=%ORACLE_HOME%
  SET PATCHES_LIST=%FORMS_PATCHES_LIST%
  SET obs=F
  
  REM Check if OPatch exists in the Oracle Home

  IF NOT EXIST %ORACLE_HOME%\OPatch\opatch (
    call :prnt " %msglocate% %ORACLE_HOME%\OPatch\opatch."
    call :prnt "This %fileedition% %msgfsincomplete%"
    call :prnt "Exiting.."
    exit /b 1;
  )
  
  CALL :runInventoryReport

  REM Getting Tools home version
  
  CALL %ORACLE_HOME%\OPatch\opatch lsinventory -detail >%TEMP%\tool_invn.txt
  FINDSTR /C:"Oracle Application Server and Developer Suite 10g Rel 2 Software Update" %TEMP%\tool_invn.txt >%TEMP%\tool_ver.txt

  for /f  "tokens=11 delims= " %%A in ('type %TEMP%\tool_ver.txt') do (
    SET toolver=%%A
  )
  SET tsvers=%toolver:Update=%

  call :prnt "Oracle Home = %TS_HOME%"
  call :prnt "Product version = %tsvers%"

  IF EXIST %TEMP%\tool_invn.txt DEL %TEMP%\tool_invn.txt
  IF EXIST %TEMP%\tool_ver.txt DEL %TEMP%\tool_ver.txt

  CALL :runIdentifyBugList
  
  REM  Tools RSF
  
  SET tshome=RSF within Forms
  SET tshead=%tshome%
  SET TECHNAME=TOOLS_RSF
  SET invreport=%FORMS_INVENTORY%
  SET TS_HOME=%TOOLS_HOME%
  SET tsvers=10.1.0.5.0
  SET PATCHES_LIST=%FORMSRSF_PATCHES_LIST%
  SET obs=F
  
  CALL :runIdentifyBugList
  
  
:: Checking for Oracle Fusion Middleware (FMW) - Web Tier

  SET tshead=Oracle Fusion Middleware (FMW) - Web Tier
  SET tshome=FMW - webtier
  SET TECHNAME=FMW_WEB
  SET invreport=%HTTP_INVENTORY%
  SET TS_HOME=%iasoraclehome%
  SET PATCHES_LIST=%HTTP_PATCHES_LIST%
  SET obs=F
  
  CALL :runInventoryReport

  REM Getting Webtier home version
  CALL %ORACLE_HOME%\OPatch\opatch lsinventory -detail >%TEMP%\tool_invn.txt
  FINDSTR /C:"Oracle WebTier and Utilities CD" %TEMP%\tool_invn.txt >%TEMP%\tool_ver.txt
  for /f  "tokens=6 delims= " %%A in ('type %TEMP%\tool_ver.txt') do (
    SET tsvers=%%A
  )
  IF EXIST %TEMP%\tool_invn.txt DEL %TEMP%\tool_invn.txt
  IF EXIST %TEMP%\tool_ver.txt DEL %TEMP%\tool_ver.txt

  REM  Oracle Common version would match Web tier version, so set it here.
  SET commonvers=%tsvers%
  SET fmwversion=%tsvers%
  call :prnt "Oracle Home = %TS_HOME%"
  call :prnt "Product Version = %tsvers%"
  call :prnt "\n"
  
  :: Check for older versions
  if "%fmwversion%"  == "11.1.1.6.0" (
    call :prnt "+----------------------------------------------------------------+"
    call :warningForObsolete "!tshead! !tsvers!"
    call :prnt "+----------------------------------------------------------------+"
    call :prnt "\n"
    SET obs=T
  )
  
  CALL :runIdentifyBugList

  REM  Web RSF
  SET tshome=RSF within FMW Web tier
  SET tshead=%tshome%
  SET TECHNAME=WEB_RSF
  SET invreport=%HTTP_INVENTORY%
  SET TS_HOME=%iasoraclehome%
  SET tsvers=11.1.0.7.0
  SET PATCHES_LIST=%HTTPRSF_PATCHES_LIST%
  SET obs=F
  
  CALL :runIdentifyBugList

  
:: Checking for Web Tier FMW - oracle_common

  SET tshead=Oracle Fusion Middleware (FMW) - Oracle_Common
  SET tshome=FMW - oracle common
  SET TECHNAME=FMW_COMMON
  SET invreport=%COMMON_INVENTORY%
  SET TS_HOME=%fmwhome%\oracle_common
  SET PATCHES_LIST=%COMMON_PATCHES_LIST%
  SET obs=F
  
  CALL :runInventoryReport
  
  SET tsvers=%commonvers%
  call :prnt "Oracle Home = %TS_HOME%"
  call :prnt "Product Version = %tsvers%"
  call :prnt "\n"
  
  :: Check for older versions
  if "%commonvers%"  == "11.1.1.6.0" (
    call :prnt "+----------------------------------------------------------------+"
    call :warningForObsolete "%tshead% %tsvers%"
    call :prnt "+----------------------------------------------------------------+"
    call :prnt "\n"
    SET obs=T
  )
  
  CALL :runIdentifyBugList
  
  
:: Checking for WLS

  SET tshome=Oracle WebLogic Server (WLS)
  SET tshead=Oracle WebLogic Server
  SET TECHNAME=WLS_HOME
  SET invreport=%WLS_INVENTORY%
  SET TS_HOME=%fmwhome%\wlserver_10.3
  SET msgcustomnote=Note that for Oracle WebLogic Server, patches rather than bugfixes are verified.
  SET PATCHES_LIST=%WLS_PATCHES_LIST%
  SET obs=F
  
  REM Check for presence of comps.xml.
  IF NOT EXIST %fmwhome%\wlserver_10.3\inventory\ContentsXML\comps.xml (
    call :prnt "%msglocate% %fmwhome%\wlserver_10.3\inventory\ContentsXML\comps.xml"
    call :prnt "This %fileedition% %msgfsincomplet%"
    call :prnt "Exiting."
    IF EXIST %PTCH_TMP%\patchesToApply.lst DEL %PTCH_TMP%\patchesToApply.lst
    EXIT /b 1
  )

  CALL %fmwhome%\wlserver_10.3\server\bin\setWLSEnv.cmd >NUL
  java weblogic.version -verbose > %TEMP%\wls_ser_rpt.txt
  FINDSTR PSU %TEMP%\wls_ser_rpt.txt > %TEMP%\wls_ser.txt
  FOR /F "tokens=3 delims= " %%A in ('type %TEMP%\wls_ser.txt') do SET tsvers=%%A


  IF EXIST %TEMP%\wls_ser_rpt.txt DEL %TEMP%\wls_ser_rpt.txt
  IF EXIST %TEMP%\wls_ser.txt DEL %TEMP%\wls_ser.txt

  CALL :runSmartUpdate
  
  SET msgbugtitle=Patch ID
  SET msgbugfix=patches
  call :prnt "Oracle Home = %TS_HOME%"
  call :prnt "Product Version = %tsvers%"
  call :prnt "\n"
  call :prnt "%msgcustomnote%"
  
  CALL :runIdentifyBugList

  IF NOT %patchesmissing% == "false" (
  
    call :prnt "%doubleline%"
    call :prnt "\n"
    call :prnt "Generating Patch Recommendation Summary ..."
    call :prnt "\n"
    call :prnt "%doubleline%"
    call :prnt "PATCH RECOMMENDATION SUMMARY"
    call :prnt "%doubleline%"
    call :prnt "One or more products have bugfixes missing."
    call :prnt "The default patch recommendations to install these missing bugfixes are:"
    call :prnt "\n"
    
    SET MISSING_PSU=""
   REM pause
    
    REM From each of the filenames saved, display the patch recommendations
    FOR /f "tokens=1 delims=^#" %%X IN ('type %FINAL_LIST%') DO (
      if NOT [%%X] == [] (
        FOR /F "tokens=1,2,3 delims=~" %%B IN ("%%X") DO (
          call :prnt "!singleline!"
          call :prnt "%%B"
          call :prnt "!singleline!"
          
          FOR /f "tokens=1 delims=^#" %%P IN ('type %%C') DO (
            FOR /f "tokens=1-4 delims=~" %%K IN ("%%P") DO (
              if "%%K" == "U" (
                call :prnt "  Upgrade Required"
              ) else (
                if "%%L" == "0" (
                  call :prnt "  Bugfix %%K (No Patch mapping data found)"
                  call :prnt "    - Use Doc ID 1594274.1 to find the corresponding patch that delivers this bugfix."
                ) else (
                  if "%%N" == "0" (
                    call :prnt "  Patch %%L"
                  ) else (
                    call :prnt "  Patch %%L (%%N)"
                  
                    SET _tmp=%%N
                    if NOT "!_tmp:PSU=!" == "!_tmp!" (
                      SET MISSING_PSU=Patch %%L !_tmp!
                    )
                  )
                  call :prnt "    - Filename: %%M"
                )
              call :prnt "\n"
              )
            )
          )

          if "%%C" == "T" (
            call :prnt "\n"
            call :warningForObsolete "%%B"
          )
          call :prnt "\n"
        )
      )
    )
    call :prnt "%msgapplymissing%"
    call :prnt "\n"
    if not x"!consolpatchid!" == x (
      call :prnt "+----------------------------------------------------------------+"
      call :prnt "A consolidated zip file with the required applicaton tier patches"
      call :prnt "is available on My Oracle Support via:"
      call :prnt "\n"
      call :prnt "   Patch !consolpatchid!"
      call :prnt "     - !consolpatchdesc!"
      call :prnt "\n"
      call :prnt "+----------------------------------------------------------------+"
    )
    
REM    If DEFINED MISSING_PSU (
  If NOT !MISSING_PSU! == "" (
      call :prnt "[WARNING] !MISSING_PSU! is missing."
      call :prnt "This is the recommended minimum WLS patch set."
      call :prnt "You should install it now, and then rerun this script to check for any"
      call :prnt "further fixes available."
      call :prnt "+----------------------------------------------------------------+"
      call :prnt "\n"
    )
    
    call :prnt "See Doc ID 1594274.1 for any special instructions regarding these patches."
    call :prnt "Footnotes in Doc ID 1594274.1 also apply to corresponding overlay patches."
  ) else (

    call :prnt "%msgresultallapplied%"
    SET TCCresult=OK
  )
  
  FOR /F "delims=" %%d IN ('date /t') DO set today=%%d
  FOR /F "delims=" %%t IN ('time /t') DO set now=%%t

  call :prnt "\n"
  call :prnt "Finished checking prerequisite patches for File Edition: %fileedition%"
  call :prnt "%today% %now%"
  call :prnt "\n"
  call :prnt "Log file for this session: %LOGFILE%"
  call :prnt "\n"
  call :prnt "%doubleline%"
  rd /S /Q %LOCAL_TEMP%
  EXIT /b 0

REM END MAIN


:: ---------------------------------------------------------------
:: Print the output to both the console as well as to the log file
:: ---------------------------------------------------------------

:prnt
  
  FOR /F "delims=" %%1 IN ('echo %1') DO set _ln=%%1
  if !_ln! == "\n" (
    echo.
    if defined LOGFILE echo. >> %LOGFILE%
  ) else (
    :: Remove quotes from beginning and end of parameter
    set _ln=!_ln:~1!
    set _ln=!_ln:~0,-1!
    echo !_ln!
    if defined LOGFILE echo !_ln! >> %LOGFILE%
  )
  
  GOTO:EOF
  
:: ---------------------------------------------------------------
:: Display Warning for error correction support
:: ---------------------------------------------------------------

:warningForObsolete

  call :prnt "[WARNING] ETCC cannot list required fixes:"
  call :prnt %1
  call :prnt "is no longer covered by Error Correction Support or compatible with ETCC."
  call :prnt "For ETCC to be able to provide a list of missing bug fixes and patch mappings"
  call :prnt "upgrade to an Oracle Fusion Middleware version listed in My Oracle Support"
  call :prnt "Document 1594274.1."
  
  GOTO:EOF

  
:checkDB

REM Check database connectivity.
REM Get APPS password.
set /P appspass="Enter the password for the %appsuser% user: "
call :prnt "\n"

SET ORACLE_HOME=%TOOLS_ORACLE_HOME%
SET TNS_ADMIN=%TNS_ADMIN%
SET LOCAL=%twotask%

REM Check if sqlplus exists in the Oracle Home.

IF NOT EXIST %ORACLE_HOME%\bin\sqlplus.exe (
  call :prnt "Binary %ORACLE_HOME%\bin\sqlplus.exe could not be found."
  call :prnt "This %fileedition% %msgfsincomplete%"
  call :prnt "The directory %ORACLE_HOME% is not the Oracle Home."
  call :prnt "Exiting."

  REM  Remove the temporary file with the list of patches to be applied.
  EXIT /b 1;
)

SET TEMP_LOGOFF=%TEMP%\check.sql
ECHO spool %TEMP%\check.log > %TEMP_LOGOFF%
ECHO connect %appsuser%/%appspass% >> %TEMP_LOGOFF%

ECHO spool off; >> %TEMP_LOGOFF%
ECHO exit;      >> %TEMP_LOGOFF%

%ORACLE_HOME%\bin\sqlplus /nolog @%TEMP_LOGOFF% >NUL
SET dbup=0
FOR /F "tokens=1 delims=^#"   %%A in ('type %TEMP%\check.log') do (
    IF "!dbup!" == "0" (
      FOR /f "tokens=1" %%H in ("%%A") do (
        if "%%H" == "Connected." (
        call :prnt "Database connection successful."
        call :prnt "\n"
        SET dbup=1
        ) else (
          call :prnt "Database connection failed."
          call :prnt "\n"
          SET dbup=0             
        )
      )
    )
  )

  
IF EXIST "%TEMP_LOGOFF%" DEL %TEMP_LOGOFF%
IF EXIST "%TEMP%\check.log" DEL %TEMP%\check.log


goto :EOF

:checkForEtccTable
IF NOT "%dbup%" =="1" (
  call :prnt "\n"
  call :prnt "%msgdbnotavail%"
  call :prnt "\n"
  GOTO:EOF
)


SET etcctable=0
SET tblExists=0


SET TEMP_LOGOFF=%TEMP%\checktable.sql


ECHO spool %TEMP%\checktable.log > %TEMP_LOGOFF%
ECHO connect %appsuser%/%appspass%   >> %TEMP_LOGOFF%

ECHO SET HEAD OFF FEEDBACK OFF VERIFY OFF PAGESIZE 0  >> %TEMP_LOGOFF%
ECHO select 1 from all_tables where table_name='TXK_TCC_RESULTS' and owner='%applsysuser%'; >> %TEMP_LOGOFF%


ECHO spool off; >> %TEMP_LOGOFF%
ECHO exit;      >> %TEMP_LOGOFF%

%ORACLE_HOME%\bin\sqlplus /nolog @%TEMP_LOGOFF% > NUL

FOR /F "tokens=1 delims=^#"   %%A in ('type %TEMP%\checktable.log') do (
    FOR /F "tokens=1" %%H in ("%%A") do (
        IF  "%%H" == "1" (
          SET tblExists=1     
          call :prnt "Table to store Technology Codelevel Checker results exists in the database"

        )
      
    )
)
call :prnt "\n"
call :prnt "\n"


IF EXIST %TEMP_LOGOFF% DEL %TEMP_LOGOFF%
IF EXIST %TEMP%\checktable.log DEL %TEMP%\checktable.log


IF "!tblExists!" == "0" (

  SET  s_ctable="CREATE TABLE %applsysuser%.TXK_TCC_RESULTS ( tcc_version VARCHAR2(20) NOT NULL,bugfix_xml_version VARCHAR2(20) NOT NULL,node_name  VARCHAR2(100) NOT NULL,database_name VARCHAR2(64) NOT NULL,component_name VARCHAR2(10) NOT NULL,component_version VARCHAR2(20) NOT NULL,component_home VARCHAR2(600),check_date DATE,check_result VARCHAR2(10) NOT NULL,check_message VARCHAR2(4000));"
  SET TEMP_LOGOFF=%TEMP%\createtable.sql
  ECHO spool %TEMP%\createtable.log > !TEMP_LOGOFF!
  ECHO connect %applsysuser%/%appspass%  >> !TEMP_LOGOFF!
  ECHO SET HEAD OFF FEEDBACK OFF VERIFY OFF PAGESIZE 0  >> !TEMP_LOGOFF!
  SET s_ctable=!s_ctable:"=!
  ECHO !s_ctable! >> !TEMP_LOGOFF!
  ECHO GRANT select on %applsysuser%.TXK_TCC_RESULTS to %appsuser%;    >> !TEMP_LOGOFF!
  ECHO spool off; >> !TEMP_LOGOFF!
  ECHO exit;      >> !TEMP_LOGOFF!

  %ORACLE_HOME%\bin\sqlplus /nolog @!TEMP_LOGOFF! > NUL

  IF %ERRORLEVEL% == 0 GOTO :CREATE_TABLE_OK
  call :prnt "Failed to create the table."
  call :prnt "Results of MT Technology Codelevel Checker will not be stored."
  call :prnt "Make sure the database is running and connectivity exists, and re-run the MT Technology Codelevel Checker."
  GOTO :EOF  

  :CREATE_TABLE_OK
  SET tblExists=1
  call :prnt "Created the table to store MT Technology Codelevel Checker results."

)


IF EXIST %TEMP_LOGOFF% DEL %TEMP_LOGOFF%
IF EXIST %TEMP%\createtable.log DEL %TEMP%\createtable.log




echo CONNECT %appsuser%/%appspass%  > !TEMP_LOGOFF!
ECHO CREATE OR REPLACE SYNONYM %appsuser%.TXK_TCC_RESULTS FOR %applsysuser%.TXK_TCC_RESULTS;  >> !TEMP_LOGOFF!
ECHO spool off; >> !TEMP_LOGOFF!
ECHO exit;      >> !TEMP_LOGOFF!

%ORACLE_HOME%\bin\sqlplus /nolog @!TEMP_LOGOFF! > NUL

IF EXIST %TEMP_LOGOFF% DEL %TEMP_LOGOFF%

FOR /F "tokens=3" %%A IN ('REG QUERY "HKCU\Control Panel\International" /v sDate ') DO SET sDate=%%A
FOR /F "tokens=3" %%A IN ('REG QUERY "HKCU\Control Panel\International" /v iDate ') DO SET iDate=%%A


FOR /F "tokens=1-4 delims=/ " %%I IN ('DATE /t') DO SET mydate=%%J/%%K/%%L
CALL :ParseDate  %mydate%

set GYear1=%GYear%
set GMonth1=%GMonth%
set GDay1=%GDay%

set s_time=%TIME:~0,2%:%TIME:~3,2%:%TIME:~6,2%
set s_ttime=%GDay1%-%GMonth1%-%GYear1% %s_time%


CALL :JDate %GYear1% %GMonth1% %GDay1%
set JDate1=%JDate%
for /f "tokens=1-5 delims=/"    %%N in ("!xmlfiledate!") do (
  set GYear=%%N
  set GMonth=%%O
  set GDay=%%P
)
CALL :JDate %GYear% %GMonth% %GDay%
IF %JDate% GTR %JDate1% (
    SET /A DateDiff = %JDate% - %JDate1%
) ELSE (
    SET /A DateDiff = %JDate1% - %JDate%
)

:: Format output for singular or plural
SET Days=days
IF %DateDiff% EQU 1 SET Days=day

:: Prefix value with a minus sign if negative
IF %JDate% GTR %JDate1% SET DateDiff=-%DateDiff%
if %DateDiff% GTR 30 (
  call :prnt "Bugfix XML file [txk_R1220_MT_base_bugs.xml] is more than 30 days old."
  call :prnt "Please check if a newer version is available in patch."
  call :prnt "\n"
  call :prnt "\n"
)


goto :EOF

:updateResInDB

IF NOT "%dbup%" =="1" (
call :prnt "\n"
call :prnt "%msgdbnotavail%"
call :prnt "\n"
GOTO:EOF

)


REM ECHO  Update the results in database.
SET ORACLE_HOME=%TOOLS_ORACLE_HOME%
SET TNS_ADMIN=%TNS_ADMIN%
SET LOCAL=%twotask%

SET InsCmd="INSERT INTO %applsysuser%.TXK_TCC_RESULTS (tcc_version,bugfix_xml_version,node_name, database_name,component_name,component_version,component_home,check_date,check_result, check_message) values"



SET tccversion=%progver%
SET tccexectime=%execdate%
SET tccresult=%TCCresult%
SET xmlversion=%xmlver%
SET server=!s_host!
SET compver=%tsvers%
SET compname=%TECHNAME%
SET comphome=%TS_HOME%
SET chkmesg=!MissingBugFix!
call :prnt "\n"
IF "%storeresults%" == "false" GOTO :endupdateResInDB

IF  NOT "%dbup%" == "1" GOTO :dbup_1
SET TEMP_DBNAME=%TEMP%\checkname.sql
ECHO spool %TEMP%\checkname.log >> %TEMP_DBNAME%
ECHO connect %appsuser%/%appspass%  >> %TEMP_DBNAME%
ECHO set head off feedback off verify off pagesize 0  >> %TEMP_DBNAME%
ECHO select NAME from V$DATABASE; >> %TEMP_DBNAME%
ECHO spool off; >> %TEMP_DBNAME%
ECHO exit;         >> %TEMP_DBNAME%

CALL %ORACLE_HOME%\bin\sqlplus /nolog @%TEMP_DBNAME% > NUL
FOR /F %%A in ('type %TEMP%\checkname.log') do (
              SET s_dbname=%%A
)

IF EXIST %TEMP_DBNAME% DEL %TEMP_DBNAME%
IF EXIST %TEMP%\checkname.sql DEL %TEMP%\checkname.sql
IF EXIST %TEMP%\checkname.log DEL %TEMP%\checkname.log

SET InsCmd="%InsCmd% ('%tccversion%', '%xmlversion%', '%server%', '!s_dbname!', '%compname%', '%compver%', '%comphome%',to_date('!s_ttime!','dd-mm-yyyy HH24:MI:SS'), '%tccresult%', '%chkmesg%');"
SET DelCmd="DELETE from %applsysuser%.TXK_TCC_RESULTS where node_name='%server%' and component_name='%compname%' and component_home like '!currentbase!%%';"

SET DelCmd=!DelCmd:"=!
SET InsCmd=!InsCmd:"=!
SET TEMP_DBEXEC=%TEMP%\execute.sql
ECHO spool %TEMP%\checkexec.log > %TEMP_DBEXEC%
ECHO connect %appsuser%/%appspass%   >> %TEMP_DBEXEC%
ECHO set head off feedback off verify off pagesize 0  >> %TEMP_DBEXEC%
ECHO !DelCmd! >> %TEMP_DBEXEC%
ECHO !InsCmd! >> %TEMP_DBEXEC%
ECHO spool off; >> %TEMP_DBEXEC%
ECHO exit;         >> %TEMP_DBEXEC%

CALL %ORACLE_HOME%\bin\sqlplus /nolog @%TEMP_DBEXEC% >NUL
IF NOT "%ERRORLEVEL%" == "0" (
   call :prnt "%msgunabletosave%"
   call :prnt "\n"
   call :prnt "%msgensuredbup%"
) else (
	call :prnt "%msgsaved%"
    	)
call :prnt "\n"	
:dbup_1
IF NOT "%dbup%" =="1" (
call :prnt "%msgdbnotavail%"
)

IF EXIST %TEMP%\checkexec.log DEL %TEMP%\checkexec.log
IF EXIST %TEMP_DBEXEC% DEL %TEMP_DBEXEC%

:endupdateResInDB
GOTO:EOF

:runInventoryReport
  
  call :prnt "%doubleline%"
  call :prnt "!tshead!"
  call :prnt "%doubleline%"

  REM First, check if opatch exists in the Oracle Home.
  SET OPATCH=%ORACLE_HOME%\OPatch\opatch
  IF EXIST %OPATCH% GOTO :YOOPATCH
  
  :NOOPATCH
    call :prnt "The opatch program could not be found in %tshome%"
    call :prnt "Cannot obtain list of %msgbugfix% for this ORACLE_HOME."
    call :prnt "Exiting."

    REM Replace temporary file with the list of patches to be applied.
    IF EXIST %PTCH_TMP%\patchesToApply.lst DEL %PTCH_TMP%\patchesToApply.lst
    EXIT /b 1;

  :YOOPATCH
    call :prnt "Now examining product %tshead%"
    call :prnt "\n"
    
    SET ORACLE_HOME=%TS_HOME%
    CALL %TS_HOME%\OPatch\opatch lsinventory -detail > %invreport%

    IF NOT "%ERRORLEVEL%" == "0" (
      call :prnt "Error checking inventory.  Error code : %ERRORLEVEL%"
      call :prnt "Ensure %tshome% home is registered in oraInventory."
      rd /S /Q %LOCAL_TEMP%
      call :prnt "Exiting..."
      EXIT /b 1;
    )
    
  GOTO:EOF

:runSmartUpdate

  REM Record current directory

  SET oldpwd=%cd%

  REM Change directory to run bsu.
  cd /d  %fmwhome%\utils\bsu
  SET bsucmd=bsu.cmd -view -status=applied -prod_dir=%fmwhome%\wlserver_10.3
  call :prnt "%doubleline%"
  call :prnt "%tshome%"
  call :prnt "%doubleline%"
  call :prnt "Now examining product %tshead%"
  call :prnt "\n"
  CALL %bsucmd% > %invreport%
  IF NOT "%ERRORLEVEL%" == "0" (
    call :prnt "Error running Smart Update.  Error code : %ERRORLEVEL%"
    call :prnt "inside"
    rd /S /Q %LOCAL_TEMP%
    call :prnt "Exiting..."
    EXIT /b 1;
  )

  REM ECHO Change back to previous directory
  cd %oldpwd%

  GOTO :EOF

:runIdentifyBugList

  SET PLAT=NT
  SET counter=1
  SET Wcounter=1

REM Check for End of Correction Support

  REM IF "%tsvers%" == "11.1.1.6.0" (
    REM call :prnt "%singleline%"
    REM call :prnt "[WARNING]"
    REM call :prnt "!tshead! !tsvers! is no longer"
    REM call :prnt "recommended for use with Oracle E-Business Suite Release 12.2."
    REM call :prnt "Details of required bugfixes and patches are no longer"
    REM call :prnt "maintained for this version, so ETCC output may be out of date ""
    REM call :prnt "and inapplicable."
    REM call :prnt "\n"
    REM call :prnt "For information on recommended versions, refer to My Oracle Support Knowledge"
    REM call :prnt "Document 1594274.1, Oracle E-Business Suite Release 12.2: Consolidated List"
    REM call :prnt "of Patches and Technology Bug Fixes"
    REM call :prnt "%singleline%"
  REM )
  
REM End check

  for /f  "tokens=1,* delims= " %%A in ('type %BUG_LIST_FILE%') do (
    SET /A counter=!counter! + 1
    SET /A Wcounter=!Wcounter! + 1
    IF "%%A" == "<Technology" (
      ECHO %%A %%B>%TEMP%\tech_name.txt
      FINDSTR %TECHNAME% %TEMP%\tech_name.txt  >%TEMP%\tech_name_check.txt
      IF "!ERRORLEVEL!" == "0" (
        FINDSTR %tsvers% %TEMP%\tech_name_check.txt >NUL 2>&1
        IF "!ERRORLEVEL!" == "0" (
          SET counter=3000
        )
      )
    )
    ECHO  %%A %%B >%TEMP%\counter_3000.txt
    IF !counter! GTR 3000 (
      FINDSTR %PLAT% %TEMP%\counter_3000.txt >NUL 2>&1
      IF "!ERRORLEVEL!" == "0" (
        SET Wcounter=5000
      )
    )
    
    IF !Wcounter! GTR 5000 (
      FINDSTR base_bugs %TEMP%\counter_3000.txt >NUL 2>&1
      IF !ERRORLEVEL! == 0 (
        GOTO  :NEXT_INV
      )
    )
  )
  
  :NEXT_INV
  
  for /f  "tokens=2 delims=<" %%A in ('type %TEMP%\counter_3000.txt') do (
    ECHO %%A >%TEMP%\listba.txt
  )
  for /f  "tokens=2 delims=>" %%M in ('type %TEMP%\listba.txt') do SET newbuglist=%%M

  IF EXIST %TEMP%\tech_name.txt DEL %TEMP%\tech_name.txt
  IF EXIST %TEMP%\counter_3000.txt DEL %TEMP%\counter_3000.txt
  IF EXIST %TEMP%\listba.txt DEL %TEMP%\listba.txt
  IF EXIST %missingbuglist% DEL %missingbuglist%


  IF EXIST %PATCHES_LIST% DEL %PATCHES_LIST%
  TYPE nul > %PATCHES_LIST%

  SET newbuglist=!newbuglist: =!
  IF "!newbuglist!" == "None" (
    call :prnt "There are no mandatory one-offs"
    SET TCCresult=OK
    SET MissingBugFix=
    CALL :updateResInDB
  ) else (
    IF "!newbuglist!x" =="x" (

      REM: Obsolete versions will not have any bugfixes in XML so we need to store result

      IF "!obs!" =="T" (
        SET TCCresult=MISSING
        SET MissingBugFix=UPGRADE REQUIRED
        ECHO U~0~0~0 >> !PATCHES_LIST!
        IF NOT EXIST !FINAL_LIST! TYPE nul > !FINAL_LIST!
        ECHO !tshead! !tsvers!~!PATCHES_LIST!~!obs! >>!FINAL_LIST!

        CALL :updateResInDB
      ) else (
        call :prnt "Failed to extract the list of bugfixes."
        call :prnt "Verify the xml file is correct and retry."
        rd /S /Q %LOCAL_TEMP%
        exit /b 1;
        GOTO:EOF
      )
    ) else (
      CALL :runPatchComparison
    )
  )
  
  GOTO:EOF

:runPatchComparison

  SET MissingBugFix=x
  SET MissingBugFix_for_print=x
  SET pmiss=
  
  REM ECHO %msgcustomnote%
  call :prnt "Checking required %msgbugfix% for %tshome% %tsvers%"
  
  :: Touch the %missingbuglist% file
  TYPE nul > %missingbuglist%
  
  FOR /F "tokens=* delims=," %%A in ('echo !newbuglist!') DO FOR %%B in (%%A) do (
    set pmiss=0
    FINDSTR %%B %invreport% >NUL 2>&1
    SET pmiss=!ERRORLEVEL!
    REM echo %%B : !pmiss! and !ERRORLEVEL!
    REM Corner case for WLS patch 13964737 which is specific to FMW 11.1.1.9
    IF "%%B" == "13964737" (
      REM If FMW is not 11.1.1.9 then do not display as missing / required
      IF NOT "!fmwversion!" == "11.1.1.9.0" (
        SET pmiss=0
      )
    )
    REM echo miss = !pmiss!
    IF NOT "!pmiss!" == "0" (
      SET MissingBugFix=!MissingBugFix!%%B,
      SET MissingBugFix_for_print=%%B
      
      IF NOT "!MissingBugFix!x" == "x" ECHO !MissingBugFix_for_print!>> %missingbuglist%
    )
  )
  
  SET MissingBugFix=!MissingBugFix:~1,-1!
  IF NOT "!MissingBugFix!x" == "x" (
    SET patchesmissing=true
    SET TCCresult=MISSING
    
  :: Capture the correct technology tag from mapping file
    CALL :CAPTURE_TAG
    
  :: Display the applicable patch for each missing bugfix
    CALL :DisplayPatchForBugfix
    
    :: Save the filename in FINAL_LIST for displaying the final patches list
    IF NOT EXIST !FINAL_LIST! TYPE nul> !FINAL_LIST!
    ECHO !tshead! !tsvers!~!PATCHES_LIST!~!obs!>>!FINAL_LIST!
    
    IF "%TECHNAME%" == "WLS_HOME" (
        REM ECHO %msgwlsmissing%
        call :prnt "The above list shows missing patches for Oracle WebLogic Server."
        call :prnt "If you have applied other Oracle WebLogic Server patches, they may have included the bugfixes needed."
        call :prnt "Contact Oracle Support if you require assistance in determining whether this is the case."
    ) else (
        call :prnt "The above list shows missing %msgbugfix% for !tshead!"
    )
    CALL :updateResInDB
    GOTO :EOF
  )

  IF "!MissingBugFix!x" == "x" (
    call :prnt "All required %msgbugfix% are present for %tshome%"
    SET TCCresult=OK
    SET MissingBugFix=""
    CALL :updateResInDB
  )

  GOTO:EOF

:: Extract the tag to a temporary file
:: Capture only the matching <technology> tag

:CAPTURE_TAG
  
  :: Recreate the tag file
  IF EXIST %CAPTURED_TAG_FILE% DEL %CAPTURED_TAG_FILE%
  TYPE nul > %CAPTURED_TAG_FILE%
  
  :: Parse the mapping file and capture the tag
  SET _begin_parse=0
  SET _begin_platform=0
  FOR /f "tokens=1 delims=^#" %%P IN ('type %MAPPING_FILE%') DO (
  
    IF "!_begin_parse!" == "1" (
      SET _tmp=
      FOR /f "tokens=1-3" %%Q IN ("%%P") DO (
        
        if "%%Q" == "<base_bugs" (
          ECHO %%P>> %CAPTURED_TAG_FILE%
        ) else if "%%Q" == "<platform" (
          IF "%%S" == "name="!PLAT!">" (
            set _begin_platform=1
            ECHO %%P>> %CAPTURED_TAG_FILE%
          )
        ) else if "!_begin_platform!" == "1" (
          ECHO %%P>> %CAPTURED_TAG_FILE%
          if "%%Q" == "</platform>" (
            set _begin_platform=0
          )
        ) else if "%%Q" == "</base_bugs>" (
          ECHO %%P>> %CAPTURED_TAG_FILE%
        ) else if "%%Q" == "</Technology>" (
          :: ECHO Done
          ECHO %%P>> %CAPTURED_TAG_FILE%
          goto :DONE_CAPTURE_TAG
        )
      )
    ) else (
      FOR /f "tokens=1-4 delims= " %%T IN ("%%P") DO (
        IF "%%U" == "name="!TECHNAME!"" IF "%%V" == "version="!tsvers!">" (
          SET _begin_parse=1
          ECHO %%P> %CAPTURED_TAG_FILE%
        )
      )
    )
  )

  :: Return from CAPTURE_TAG
  
  :DONE_CAPTURE_TAG
  GOTO:EOF
  
  
:: List out the bugfix and its relted patch
:: If patch mapping is disabled, then just list out the bugs

:DisplayPatchForBugfix
  
  :: Find the patches for the missing bugfixes

  REM ECHO Found patch records in the inventory.
  REM ECHO.
  
  FOR /f "tokens=1 delims=^#" %%P IN ('type %missingbuglist%') DO (
    
    IF NOT [%%P] == [] (
    
      call :GET_PATCH %%P
      
      REM echo !BUGFIX_PATCH! - !BUGFIX_PATCH_FILE! - !BUGFIX_PATCH_NOTES!
      SET _no_data=F
      if "!BUGFIX_PATCH!" == "0" SET _no_data=T
      if "!BUGFIX_PATCH_FILE!" == "0" SET _no_data=T
      
      REM Capture all the patches into PATCHES_LIST
      REM Also capture bugfixes that do not have patches
      if "!_no_data!" == "T" (
        call :prnt "  Missing !msgbugtitle!: %%P  ->  Patch data missing for bugfix %%P"
        ECHO %%P~0~0~0>> !PATCHES_LIST!
      ) else (
        if "%TECHNAME%" == "WLS_HOME" (
          call :prnt "  Missing !msgbugtitle!: %%P"
        ) else (
          call :prnt "  Missing !msgbugtitle!: %%P  ->  Patch !BUGFIX_PATCH!"
        )
        :: Ensure not to put any duplicates
        FINDSTR /M "!BUGFIX_PATCH!" !PATCHES_LIST! 1>Nul
        if !ERRORLEVEL! == 1 (
          ECHO %%P~!BUGFIX_PATCH!~!BUGFIX_PATCH_FILE!~!BUGFIX_PATCH_NOTES!>> !PATCHES_LIST!
        )
      )
    )
  )
  
  GOTO:EOF
  

:: Get the patch information from the 'CAPTURED_TAG_FILE'
:: Ensure to take the platform into consideration

:GET_PATCH

  SET BUG_TO_FIND=%1
  SET BUGFIX_PATCH=0
  SET BUGFIX_PATCH_FILE=0
  SET BUGFIX_PATCH_NOTES=0
  SET _tmp=
  
  SET _begin_base_bugs=0
  SET _begin_parse=0
  FOR /f "tokens=1 delims=^#" %%P IN ('type %CAPTURED_TAG_FILE%') DO (
  
    if "!_begin_base_bugs!" == "0" (
      
      for /f "tokens=1,2 delims=>=" %%i in ("%%P") do (
        set _tmp=%%j
        
        :: if _tmp not empty and the bug exists in it then begin parsing base_bug tag
        if not [!_tmp!] == [] if not "!_tmp:%BUG_TO_FIND%=!" == "!_tmp!" (
          SET _begin_base_bugs=1
        )
      )
    ) else if "!_begin_parse!" == "0" (
      FOR /f "tokens=1-3 delims= " %%R IN ("%%P") DO (
        IF NOT [%%T] == [] IF "%%T" == "name="!PLAT!">" (
          SET _begin_parse=1
        )
      )
    ) else if "!_begin_parse!" == "1" (
      for /f "tokens=2,3,4 delims=<>" %%i in ("%%P") do (
        if "%%i" == "patch_id" (
          if not x"%%j" == x SET BUGFIX_PATCH=%%j
        ) else if "%%i" == "file_name" (
          if not "%%j" == "/file_name" SET BUGFIX_PATCH_FILE=%%j
        ) else if "%%i" == "notes" (
          if not "%%j" == "/notes" SET BUGFIX_PATCH_NOTES=%%j
        ) else if "%%i" == "/platform" (
          SET _begin_parse=0
          GOTO :DONE_GET_PATCH
        )
      )
    )
  )
  
  :DONE_GET_PATCH
  GOTO:EOF
  

:ParseDate
:: Parse (Gregorian) date depending on registry's date format settings
:: Argument : Gregorian date in local date format,
:: Requires : sDate (local date separator), iDate (local date format number)
:: Returns  : GYear (4-digit year), GMonth (2-digit month), GDay (2-digit day)
  
  IF %iDate%==0 FOR /F "TOKENS=1-3 DELIMS=%sDate%" %%A IN ("%*") DO (
    SET GYear=%%C
    SET GMonth=%%A
    SET GDay=%%B
  )
  IF %iDate%==1 FOR /F "TOKENS=1-3 DELIMS=%sDate%" %%A IN ("%*") DO (
    SET GYear=%%C
    SET GMonth=%%B
    SET GDay=%%A
  )
  IF %iDate%==2 FOR /F "TOKENS=1-3 DELIMS=%sDate%" %%A IN ("%*") DO (
    SET GYear=%%A
    SET GMonth=%%B
    SET GDay=%%C
  )

  GOTO:EOF

:JDate
:: Convert date to Julian
:: Arguments : YYYY MM DD
:: Returns   : Julian date
::
:: First strip leading zeroes; a logical error in this
:: routine was corrected with help from Alexander Shapiro
  SET MM=%2

  SET DD=%3
  IF 1%MM% LSS 110 SET MM=%MM:~1%
  IF 1%DD% LSS 110 SET DD=%DD:~1%
  SET /A Month1 = ( %MM% - 14 ) / 12
  SET /A Year1  = %1 + 4800
  SET /A JDate  = 1461 * ( %Year1% + %Month1% ) / 4 + 367 * ( %MM% - 2 -12 * %Month1% ) / 12 - ( 3 * ( ( %Year1% + %Month1% + 100 ) / 100 ) ) / 4 + %DD% - 32075
  FOR %%A IN (Month1 Year1) DO SET %%A=
  GOTO:EOF

:checkADCodeLevel

  IF NOT "%dbup%" =="1" (
    call :prnt "\n"
    call :prnt "%msgdbnotavail%"
    call :prnt "\n"
    GOTO:EOF
  )

  SET ORACLE_HOME=%TOOLS_ORACLE_HOME%
  SET TNS_ADMIN=%TNS_ADMIN%
  SET LOCAL=%twotask%

  REM Identify AD codelevel

  SET TEMP_CODE=%TEMP%\code.sql
  ECHO spool %TEMP%\code.log > %TEMP_CODE%
  ECHO connect %appsuser%/%appspass% >> %TEMP_CODE%
  ECHO SET HEAD OFF FEEDBACK OFF VERIFY OFF PAGESIZE 0 >> %TEMP_CODE%
  ECHO select codelevel from ad_trackable_entities where abbreviation ='ad'; >> %TEMP_CODE%
  ECHO spool off; >> %TEMP_CODE%
  ECHO exit;      >> %TEMP_CODE%

  %ORACLE_HOME%\bin\sqlplus /nolog @%TEMP_CODE% >NUL

  FOR /F "tokens=* delims="   %%A in ('type %TEMP%\code.log') do SET deltalevel=%%A
  SET deltalevel=!deltalevel: =!

  IF "%deltalevel%x" == "x" (
    call :prnt "Unable to identify AD codelevel"
    SET deltalevel=AD.!deltalevel!
  ) else (
    SET deltalevel=AD.!deltalevel!
  )


  IF EXIST %TEMP_CODE% DEL %TEMP_CODE% 
  IF EXIST %TEMP%\code.log DEL %TEMP%\code.log


  REM Only store checker results in DB if AD.C Delta 7 or higher.

  SET TEMP_STORE=%TEMP%\store.sql
  ECHO spool %TEMP%\store.log > %TEMP_STORE%
  ECHO connect %appsuser%/%appspass% >> %TEMP_STORE%
  ECHO SET HEAD OFF FEEDBACK OFF VERIFY OFF PAGESIZE 0 >> %TEMP_STORE%
  ECHO select 'STORE' from ad_trackable_entities where (abbreviation ='ad' and substr(codelevel,1,1) = 'C' and to_number (substr(codelevel,3)) ^>= 7) or (abbreviation ='ad' and baseline = 'D'); >> %TEMP_STORE%
  ECHO spool off; >> %TEMP_STORE%
  ECHO exit;      >> %TEMP_STORE%

  %ORACLE_HOME%\bin\sqlplus /nolog @%TEMP_STORE% >NUL
  SET Scounter=1
  FOR /F "tokens=* delims="   %%A in ('type %TEMP%\store.log') do SET sqlresult=%%A
  SET sqlresult=!sqlresult: =!

  IF NOT "!sqlresult!" == "STORE" (
    SET storeresults=false
    call :prnt "The installed !deltalevel! codelevel does not support storing the results in the database."
    call :prnt "\n"
  ) else (
    SET storeresults=true
    call :prnt "The installed !deltalevel! codelevel supports storing the results in the database."
    call :prnt "\n"
  )
  IF EXIST %TEMP_STORE% DEL %TEMP_STORE%
  IF EXIST %TEMP%\store.log DEL %TEMP%\store.log
  GOTO:EOF

call :prnt "END"
