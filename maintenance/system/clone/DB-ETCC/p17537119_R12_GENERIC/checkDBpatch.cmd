@echo OFF
REM
REM +======================================================================+
REM |    Copyright (c) 2005, 2017 Oracle and/or its affiliates.            |
REM |                         All rights reserved.                         |
REM |                           Version 12.0.0                             |
REM +======================================================================+
REM # $Header: checkDBpatch.cmd 120.23 2017/08/10 11:33:54 paholman noship $
REM $AutoConfig$
REM

Setlocal EnableDelayedExpansion

SET EC=ERRORCODE
SET CMDDIR=%~dp0
SET s_filename=%0%

:: Generate random number
SET _rand=
for /f "skip=1" %%x in ('wmic os get localdatetime') do if not defined _rand SET _rand=%%x
for /f "tokens=1 delims=." %%y in ('echo %_rand%') do set _rand=%%y
set _rand=%_rand:~6%

:: Defined the log file
SET LOGFILE=%CMDDIR%checkDBpatch_%_rand%.log

call :prnt "\n"
call :prnt " +===============================================================+"
call :prnt " |       Copyright (c) 2005, 2017 Oracle and/or its affiliates   |"
call :prnt " |                     All rights reserved                       |"
call :prnt " |              Oracle E-Business Suite Release 12.2             |"
call :prnt " |           Database EBS Technology Codelevel Checker           |"
call :prnt " +===============================================================+"
call :prnt "\n"

SET TCCresult="MISSING"
SET MissRlbkFix=""
SET _fnd=0
SET row_exst=0

SET ctxfilesourcemsg=""

SET BUG_LIST_FILE=%CMDDIR%txk_R1220_DB_base_bugs.xml
SET MAPPING_FILE=%CMDDIR%txk_R1220_DB_mappings.xml

:: Get the version of the cmd script
  for /f "tokens=*" %%A in ('findstr Header: %s_filename%') do (
    if "!_fnd!" == "0" (
      for /f "tokens=5"  %%H in ("%%A") do (
        set cmd_filever=%%H
        set _fnd=1
      )
    )
  )
  echo.
  echo Starting Database EBS Technology Codelevel Checker, Version %cmd_filever%
  echo.

:: Get Context file
  SET contextfile=""

  IF "a%1" == "a" (
    IF DEFINED CONTEXT_FILE (
      SET contextfile=%CONTEXT_FILE%
      SET ctxfilesourcemsg=currently set database environment:
      GOTO :beginetcc
    ) else (
      GOTO :getcontextfile
    )
  )

  IF NOT "%1" == "contextfile" (
    ECHO Wrong Usage..
    ECHO Please run ETCC in either of the 2 ways:
    ECHO 1. %0 contextfile=DB_CONTEXT_FILE
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

  GOTO :beginetcc

:: Get the context file as user input

:getcontextfile

  ECHO Database environment not set, so context file must be specified.
  set /P contextfile="Enter the full path to Database context file : "
  SET CONTEXT_FILE=%contextfile%
  SET ctxfilesourcemsg=user input:

:beginetcc

:: Check the context file

  IF NOT EXIST %contextfile% (
    call :prnt "Unable to locate context file %contextfile%."
    call :prnt "Verify location and then rerun DB-ETCC."
    EXIT /b 1;
  )

:: And other necessary variables

  set LOCAL_TEMP=%CMDDIR%temp
  set CAPTURED_TAG_FILE=%LOCAL_TEMP%\target.tag
  set PATCHES_LIST=%LOCAL_TEMP%\patches.lst
  set DISABLE_MAPPING=F

:: Display the context file being used
  call :prnt "\n"
  call :prnt "Using context file from %ctxfilesourcemsg%"
  call :prnt "%contextfile%"
  call :prnt "\n"

:: Check the XML file version
  IF NOT EXIST %BUG_LIST_FILE% (
    call :prnt "\n"
    call :prnt "%BUG_LIST_FILE% does not exist."
    call :prnt "Extract it from patch zip file to this script execution directory and then rerun DB-ETCC."
    GOTO :EOF
  )

  for /f "tokens=4,5,6 delims= " %%A in ('findstr Header: %BUG_LIST_FILE%') do (
    SET xmlver=%%A
    SET filedate=%%B %%C
  )

  call :prnt "Bugfix XML file version: %xmlver%"
  call :prnt "This file will be used as the reference for identifying missing bugfixes."
  call :prnt "\n"

:: Check how old the XML file is
  FOR /F "tokens=3" %%A IN ('REG QUERY "HKCU\Control Panel\International" /v sDate ') DO (
    SET sDate=%%A
  )
  FOR /F "tokens=3" %%A IN ('REG QUERY "HKCU\Control Panel\International" /v iDate ') DO (
    SET iDate=%%A
  )
:: Use WMIC to retrieve date and time
for /f "skip=1 tokens=1-6" %%a in ('wmic path Win32_LocalTime Get Day^,Hour^,Minute^,Month^,Second^,Year /Format:table') do (
	if not "%%~f"=="" (
		set /A FormattedDate=10000 * %%f + 100 * %%d + %%a
                set GYear1=!FormattedDate:~0,4!
                set GMonth1=!FormattedDate:~4,2!
                set GDay1=!FormattedDate:~6,2!

 
		set /A FormattedTime=10000 * %%b + 100 * %%c + %%e
		set FormattedTime=0000000!FormattedTime!
		set FormattedTime=!FormattedTime:~-6,2!:!FormattedTime:~-4,2!:!FormattedTime:~-2,2!
	)
)
 
REM  FOR /F "tokens=1-4 delims=/ " %%I IN ('DATE /t') DO SET mydate=%%J/%%K/%%L
REM echo the daye is %mydate%
REM  CALL :ParseDate  %FormattedDate%

  REM set GYear1=%GYear%
REM   set GMonth1=%GMonth%
  REM set GDay1=%GDay%



  set s_time=%TIME:~0,2%:%TIME:~3,2%:%TIME:~6,2%

  set s_ttime=%GDay1%-%GMonth1%-%GYear1% %s_time%

  CALL :JDate %GYear1% %GMonth1% %GDay1%
  set JDate1=%JDate%

  for /f "tokens=1-5 delims=/"    %%N in ("!filedate!") do (
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
    call :prnt "\n"
    call :prnt "[WARNING] DB-ETCC: Bugfix XML file (txk_R1220_DB_base_bugs.xml) in current directory is more than 30 days old."
    call :prnt "Please check if a newer version is available in patch 17537119."
    call :prnt "\n"
    call :prnt "\n"
  )

:: Check the mapping file version
:: Check for mapping file

  IF NOT EXIST %MAPPING_FILE% (
    call :prnt "%MAPPING_FILE% does not exist."
    call :prnt "Extract it from patch zip file to this script execution directory and then rerun DB-ETCC."
    call :prnt "\n"
    call :prnt "Patch mapping disabled."
    call :prnt "\n"
    SET DISABLE_MAPPING=T
  ) else (
    for /f "tokens=4,5,6 delims= " %%A in ('findstr Header: %MAPPING_FILE%') do (
      SET mapver=%%A
    )
    call :prnt "Mapping XML file version: !mapver!"
    call :prnt "This file will be used for mapping bugfixes to patches."
    call :prnt "\n"

    REM Get Consolidated Patch details
    REM ------------------------------
    FOR /F tokens^=2-4^ delims^=^" %%A in  ('findstr consolidated_patch %MAPPING_FILE%') do (
      SET consolpatchid=%%A
      SET consolpatchdesc=%%C
    )
  )

  REM Get the Oracle Home value
  REM -------------------------
  REM Extracting ORACLE_HOME
  REM ECHO Executing Technology Codelevel Checker version..

  FOR /F "tokens=2 delims=>" %%A in ('FINDSTR s_db_oh %contextfile%') do SET HOME_1="%%A"
  FOR /F "tokens=1 delims=<" %%B in ('echo %HOME_1%') do SET HOME_2="%%B"
  SET s_db_oh=%HOME_2:"=%

  REM Get the path to the perl.exe (s_adperlprg)
  REM ------------------------------------------
  FOR /F "tokens=2 delims=>" %%A in ('FINDSTR s_adperlprg %contextfile%') do SET PERL_1="%%A"
  FOR /F "tokens=1 delims=<" %%B in ('echo %PERL_1%') do SET PERL_2="%%B"
  SET PERLPRG=%PERL_2:"=%

  REM Get the PERL5LIB PATH and set it in the environment (s_perl5lib)
  REM ----------------------------------------------------------------
  FOR /F "tokens=2 delims=>" %%A in ('FINDSTR s_perl5lib %contextfile%') do SET PERL_1="%%A"
  FOR /F "tokens=1 delims=<" %%B in ('echo %PERL_1%') do SET PERL_2="%%B"
  SET PERL5LIB=%PERL_2:"=%

REM echo Perl is :%PERLPRG%
REM echo perl5lib is :%PERL5LIB%

  IF EXIST %s_db_oh%\bin\sqlplus.exe GOTO :OH_EXISTS
  call :prnt "The ORACLE_HOME directory %s_db_oh% does not exist"
  goto :EOF

:OH_EXISTS

  if not exist %s_db_oh%\temp mkdir %s_db_oh%\temp


:GET_ORACLE_SID

  REM Get the ORACLE_SID value
  REM -------------------------
  FOR /F "tokens=2 delims=>" %%C in ('FINDSTR s_instName %contextfile%') do SET SID_1="%%C"
  FOR /F "tokens=1 delims=<" %%D in ('echo %SID_1%') do SET SID_2="%%D"
  SET s_db_oh=%HOME_2:"=%
  SET s_instName=%SID_2:"=%


:CHECK_DB

:: Get the release of the DB

  call :prnt "Identifying database release."

  FOR /F "tokens=1-3 delims= " %%T IN ('%s_db_oh%\bin\sqlplus -v') DO SET s_dbVersion=%%V
  FOR /F "tokens=1-5 delims=." %%A IN ('echo %s_dbVersion%') DO (
    SET s_dbVersion=%%A.%%B.%%C.%%D
    SET s_dbVersion_psu=%%E
  )
  call :prnt "Setting database release to %s_dbVersion%."
  :: ECHO Setting PSU=%s_dbVersion_psu%
  call :prnt "\n"

:: Check the version of the DB

  SET obsoleted=F

  if "%s_dbVersion%" == "11.2.0.3" SET obsoleted=T
  if "%s_dbVersion%" == "12.1.0.1" SET obsoleted=T
  FOR /F "tokens=1-5 delims=." %%A IN ('echo %s_dbVersion%') DO (
    if %%A LEQ 10 SET obsoleted=T
  )

:: Check the status of the DB

  SET TEMP_LOGOFF=%s_db_oh%\check.sql
  ECHO spool %s_db_oh%\check.log >> %TEMP_LOGOFF%
  ECHO connect / as sysdba; >> %TEMP_LOGOFF%
  ECHO spool off; >> %TEMP_LOGOFF%
  ECHO exit; >> %TEMP_LOGOFF%

  SET ORACLE_HOME=%s_db_oh%
  SET ORACLE_SID=%s_instName%
  SET PATH=%s_db_oh%;%PATH%

  REM CHECKING DB_MODE
  REM ----------------
  SET TEMP_DBMODE=%s_db_oh%\dbmode.sql
  ECHO spool %s_db_oh%\dbmode.log > %TEMP_DBMODE%
  ECHO connect / as sysdba  >> %TEMP_DBMODE%
  REM ECHO SET HEAD OFF FEEDBACK OFF VERIFY OFF 0>> %TEMP_DBNAME%
  ECHO SELECT OPEN_MODE FROM V$DATABASE; >> %TEMP_DBMODE%
  ECHO SPOOL OFF; >> %TEMP_DBMODE%
  ECHO EXIT;      >> %TEMP_DBMODE%
  %s_db_oh%\bin\sqlplus /nolog @%TEMP_DBMODE% > NUL

  FINDSTR /C:"READ WRITE" %s_db_oh%\dbmode.log > NUL
  IF NOT %ERRORLEVEL% == 0 (
    SET DBmode=0
    REM ECHO "Database is not open in READ WRITE mode."
    REM  EXIT /b 1;
  ) else (
    SET DBmode=1
    REM ECHO "Database open in READ WRITE mode."
  )

  IF EXIST %TEMP_DBMODE% DEL %TEMP_DBMODE%
  IF EXIST %s_db_oh%\dbmode.log DEL %s_db_oh%\dbmode.log

  REM ECHO Executing command: %s_db_oh%\bin\sqlplus.exe /nolog @%TEMP_LOGOFF%
  %s_db_oh%\bin\sqlplus /nolog @%TEMP_LOGOFF% > NUL
  set db_up=0

  for /f "tokens=1 delims=^#" %%A in ('type %s_db_oh%\check.log') do (
    if "!db_up!" == "0" (
      for /f "tokens=1" %%H in ("%%A") do (
        if "%%H" == "Connected." (
          call :prnt "\n"
          call :prnt "Database connection successful."
          call :prnt "\n"
          set db_up=1
          call :identifyAPPS
        ) else (
          call :prnt "+------------------------------------------------------------------------------+"
          call :prnt "[WARNING] DB-ETCC: Could not connect to database, so unable to check:           "
          call :prnt "  - Whether database is in READ-WRITE mode.                                     "
          call :prnt "  - Existence of table needed to store DB-ETCC results.                         "
          call :prnt "Resolve the database connectivity issue, and then rerun DB-ETCC.                "
          call :prnt "+------------------------------------------------------------------------------+"
          call :prnt "\n"
          set db_up=0
          goto :runProcess_1
        )
      )
    )
  )


:runProcess_1

  REM ECHO  echo runProcess_1

:: Begin the execution
  FOR /F "delims=" %%d IN ('date /t') DO set today=%%d
  FOR /F "delims=" %%t IN ('time /t') DO set now=%%t

  call :prnt "Started prerequisite patch testing : %today% %now%"
  call :prnt "\n"
  call :prnt "Log file for this session : %LOGFILE%"
  call :prnt "\n"

:: Check the local temp

  IF NOT EXIST %LOCAL_TEMP% (
    mkdir %LOCAL_TEMP%
    if !errorlevel! == 0 goto :LTEMP_OK
    call :prnt "Unable to create temp directory %LOCAL_TEMP%."
    call :prnt "%EC% = !errorlevel!   %EC%_END"
    goto :EOF
  )


:LTEMP_OK


:CFG_OK

  set PTCH_TMP=%LOCAL_TEMP%
  SET PATCHES_TOAPPLY=%PTCH_TMP%\PAPPLY.lst
  SET PATCHES_TOROLLBACK=%PTCH_TMP%\PRLBK.lst
  SET PATCH_NAME=%PTCH_TMP%\PNAME.lst

  TYPE nul > %PATCHES_TOAPPLY%
  TYPE nul > %PATCHES_TOROLLBACK%

  REM EXTRACTING HOSTNAME
  REM Later, convert the hostname to lower case letter
  REM ------------------------------------------------
  FOR /F "tokens=2 delims=>" %%E in ('FINDSTR s_dbhost %contextfile%') do SET host_1="%%E"
  FOR /F "tokens=1 delims=<" %%F in ('echo %host_1%') do SET host_2="%%F"
  SET s_host=%host_2:"=%
  SET s_host=!s_host:A=a!
  SET s_host=!s_host:B=b!
  SET s_host=!s_host:C=c!
  SET s_host=!s_host:D=d!
  SET s_host=!s_host:E=e!
  SET s_host=!s_host:F=f!
  SET s_host=!s_host:G=g!
  SET s_host=!s_host:H=h!
  SET s_host=!s_host:I=i!
  SET s_host=!s_host:J=j!
  SET s_host=!s_host:K=k!
  SET s_host=!s_host:L=l!
  SET s_host=!s_host:M=m!
  SET s_host=!s_host:N=n!
  SET s_host=!s_host:O=o!
  SET s_host=!s_host:P=p!
  SET s_host=!s_host:Q=q!
  SET s_host=!s_host:R=r!
  SET s_host=!s_host:S=s!
  SET s_host=!s_host:T=t!
  SET s_host=!s_host:U=u!
  SET s_host=!s_host:V=v!
  SET s_host=!s_host:W=w!
  SET s_host=!s_host:X=x!
  SET s_host=!s_host:Y=y!
  SET s_host=!s_host:Z=z!

  SET OPATCH=%s_db_oh%\OPatch\opatch.bat
  IF EXIST %OPATCH% GOTO :OPATCHBIN_EXIST
  call :prnt "Could not find opatch utility in Database ORACLE_HOME."
  call :prnt "Therefore, list of bugfixes applied cannot be obtained."
  call :prnt "\n"
  call :prnt "Exiting."
  call :prnt "\n"

  REM remove the temporary file with the list of patches to be applied
  REM IF EXIST %PTCH_TMP%\patchesToApply.lst (

  IF EXIST %PTCH_TMP% (
    REM CALL :REMOVE_TEMP_FILES
    REM rmdir %PTCH_TMP%
    rd /s /q  %PTCH_TMP%
    REM GOTO :SCRIPTEND
    goto :EOF
  )


:OPATCHBIN_EXIST

  set optchstr=""
  set _fnd=0
  set opatchinv=%PTCH_TMP%\opatchinv.lst

  call %s_db_oh%\Opatch\opatch lsinventory > %opatchinv%

  for /f "tokens=1 delims=^#" %%A in ('type %opatchinv%') do (
    if "!_fnd!" == "0" (
     	for /f "tokens=1-4" %%H in ("%%A") do (
        if "%%H" == "Oracle" (
          if "%%I" == "Database" (
            set optchver=%%K
            set _fnd=1
          )
        )
      )
    )
  )

:: Parse the Buglist

  set db_flag=0

  for /f "tokens=1 delims=^#" %%A in ('type %BUG_LIST_FILE%') do (
    if "!_x!"=="1" (
      REM echo Line must be base bugs or rollback %%A
      for /f "tokens=2,3 delims=<|>" %%H in ("%%A") do (
        if "%%H" == "base_bugs" (
          set BUG_LIST=%%I
        ) else if "%%H" == "bugs_rollback/" (
          set ROLLBACK_LIST=0
          goto :oneoffchecks
        ) else if "%%H" == "base_bugs/" (
          set BUG_LIST=0
        ) else if "%%H" == "bugs_rollback" (
          set ROLLBACK_LIST=%%I
          goto :oneoffchecks
        )
      )
    )

    for /f "tokens=1-4 delims= " %%U in ("%%A") do (
      if "!db_flag!" == "0" (
        if "%%W" == "version="%s_dbVersion%">" (
          set z=%%W
          set db_flag=1
        )
      )
      if "%%W" == "name="NT">" (
        if "!db_flag!" == "1" (
          set _x=1
        )
      )
    )
  )

REM if by chance the script comes here
goto :oneoffchecks

:warningForObsolete

  call :prnt "[ERROR] ETCC cannot list required fixes:"
  call :prnt "Oracle Database %s_dbVersion% is no longer covered by Error Correction Support"
  call :prnt "or compatible with ETCC. For ETCC to be able to provide a list of missing bug"
  call :prnt "fixes and patch mappings, upgrade to an Oracle Database version listed in"
  call :prnt "My Oracle Support Document 1594274.1."

  goto :EOF

:oneoffchecks

  if NOT "%obsoleted%" == "T" (
    call :prnt "Obtained list of bugfixes to be applied and the list to be rolled back."
    call :prnt "Now checking Database ORACLE_HOME."
    call :prnt "\n"
  )

  if "%BUG_LIST%" NEQ "0" echo %BUG_LIST%  >> %PATCHES_TOAPPLY%
  if "%ROLLBACK_LIST%" NEQ "0" echo %ROLLBACK_LIST%  >> %PATCHES_TOROLLBACK%

  REM Check if opatch exists in the Oracle Database Home
  REM --------------------------------------------------
  SET OPATCH=%s_db_oh%\OPatch\opatch.bat
  SET ORACLE_HOME=%s_db_oh%

  IF EXIST %OPATCH% GOTO :OPATCH_EXIST

  call :prnt "opatch binary missing in Oracle Database Home."
  call :prnt "Therefore, list of bugfixes applied cannot be obtained."

  REM remove the temporary file with the list of patches to be applied
  REM IF EXIST %PTCH_TMP%\patchesToApply.lst (

  IF EXIST %PTCH_TMP% (
    REM     CALL :REMOVE_TEMP_FILES
    REM rmdir %PTCH_TMP%
    rd /s /q  %PTCH_TMP%
    REM     GOTO :SCRIPTEND
    goto :EOF
  )


:OPATCH_EXIST

  set optchstr=""
  set opatchver=%PTCH_TMP%\opatchver.lst
  call %ORACLE_HOME%\Opatch\opatch version > %opatchver%
  for /F "delims=: tokens=1,2" %%A in ('findstr Version %opatchver%') do ( set optchstr=%%B)
  call :prnt "The opatch utility is version: %optchstr%."
  for /f  "tokens=1-6 delims=." %%H in ("%optchstr%") do (
    set r=%%H
    set s=%%I
    set t=%%J
    set u=%%K
  )
  if %r% LSS 11 (
    call :prnt "To use DB-ETCC, you must obtain a newer version of opatch (Patch 6880880 from My Oracle Support) and then rerun this script."
    call :prnt "\n"
    goto :EOF
  )
  if %r% GTR 11 (
    call :prnt "DB-ETCC is compatible with this opatch version."
    call :prnt "\n"
  ) else (
    if %s% LSS 2 (
      call :prnt "To use DB-ETCC, you must obtain a newer version of opatch (Patch 6880880 from My Oracle Support) and then rerun this script."
      call :prnt "\n"
      goto :EOF
    )
    if %s% GTR 2 (
      call :prnt "DB-ETCC is compatible with this opatch version."
      call :prnt "\n"
    ) else (
      if %t% GTR 0 (
        call :prnt "DB-ETCC is compatible with this opatch version."
        call :prnt "\n"
      )
      if %t% == 0 (
        if %u% LSS 3 (
          call :prnt "To use DB-ETCC, you must obtain a newer version of opatch (Patch 6880880 from My Oracle Support) and then rerun this script."
          call :prnt "\n"
          goto :EOF
        ) else (
          call :prnt "DB-ETCC is compatible with this opatch version."
          call :prnt "\n"
        )
      )
    )
  )

  if "%obsoleted%" == "T" (
    call :prnt "==================================================================================="
    call :warningForObsolete
    call :prnt "==================================================================================="
    call :prnt "\n"
  )


  REM Get the list of one-offs already applied to the Oracle Database Home
  set _patches_applied=%PTCH_TMP%\patchesApplied.lst
  set _tmp_patches_applied=%PTCH_TMP%\tmp_patchesApplied.lst
  set _lsinventory=%PTCH_TMP%\ls_inventory.lst
  TYPE NUL > %_patches_applied%
  TYPE NUL > %_tmp_patches_applied%

  call %OPATCH% lsinventory > %_lsinventory%
  %PERLPRG% -pi.bak -e "s/\)//g" %_lsinventory%
  for /f  "tokens=1 delims=^#" %%A in ('type %_lsinventory%') do (
    call :parse_line %%A
  )

  %SystemRoot%\system32\sort.exe %_tmp_patches_applied% > %_patches_applied%

  REM List of patches that are missing in existing Oracle Home
  REM --------------------------------------------------------

  REM echo The BUG LIST here is !BUG_LIST!
  REM if  %BUG_LIST% == "" (

  SET OHOME_PATCHED=%PTCH_TMP%\ohome_patched.lst
  TYPE nul > %OHOME_PATCHED%

  if "%BUG_LIST%" == "0" goto :CHECK_OHOME_RBK

  FOR /d %%a in (%BUG_LIST%) do (
    CALL :GET_PATCH_MISSING_IN_OHOME %%a
  )

  FOR /f "DELIMS=" %%F in ('ECHO %OHOME_PATCHED%') DO SET OHOME_PATCHED_SIZE=%%~zF
  IF %OHOME_PATCHED_SIZE% EQU 0 GOTO :OHOME_PATCHED_OK


:: ER Bug 22385226

:: Disable mapping for unsupported versions of DB

:: For 11.2.0.3 mapping disabled

  if "%s_dbVersion%"=="11.2.0.3" SET DISABLE_MAPPING=T

:: For 11.2.0.4, PSU 1 to 5 are not mapped

  if "%s_dbVersion%"=="11.2.0.4" if %s_dbVersion_psu% GTR 0 if %s_dbVersion_psu% LEQ 5 SET DISABLE_MAPPING=T

:: For 12.1.0.2, PSU 1 to 3 are not mapped

  if "%s_dbVersion%"=="12.1.0.2" if %s_dbVersion_psu% GTR 0 if %s_dbVersion_psu% LEQ 3 SET DISABLE_MAPPING=T

:: Print the buglist if mapping is disabled

  IF "%DISABLE_MAPPING%" == "T" (
    call :prnt "Some required one-off bugfixes are missing from the Database ORACLE_HOME."
    call :prnt "The missing bugfixes are:"

    TYPE %OHOME_PATCHED%

    call :prnt "\n"
    call :prnt "** Please refer to MOS Doc ID 1594274.1:Oracle Ebusiness Suite Release 12.2 Consolidated List of Patches and Technology Bug Fixes **"
    call :prnt "Stored Technology Codelevel Checker results in the database successfully."
    call :prnt "\n"
    call :prnt "Apply the missing bugfixes and then rerun the script."
    call :prnt "\n"
    GOTO :DONE_LISTING
  )

:: Extract the tag to a temporary file
:: Capture only the matching <technology> tag

  IF EXIST %CAPTURED_TAG_FILE% DEL %CAPTURED_TAG_FILE%
  TYPE nul > %CAPTURED_TAG_FILE%

  FOR /f "tokens=1 delims=^#" %%P IN ('type %MAPPING_FILE%') DO (

    IF "!_begin_parse!" == "1" (
      FOR /f "tokens=1 delims= " %%Q IN ("%%P") DO (
        ECHO %%P>> %CAPTURED_TAG_FILE%
        if "%%Q" == "</Technology>" (
          :: ECHO Done
          goto :DONE_CAPTURE_TAG
        )
      )
    )

    FOR /f "tokens=1-4 delims= " %%U IN ("%%P") DO (
      IF "%%W" == "version="!s_dbVersion!_NT">" (
        SET _begin_parse=1
        ECHO %%P> %CAPTURED_TAG_FILE%
      )
    )
  )

:DONE_CAPTURE_TAG

:: Find the patches for the missing bugfixes

  call :prnt "Found patch records in the inventory."
  call :prnt "\n"

  :: TYPE %OHOME_PATCHED%

  IF EXIST %PATCHES_LIST% DEL %PATCHES_LIST%
  TYPE nul > %PATCHES_LIST%

  FOR /f "tokens=1 delims=^#" %%P IN ('type %OHOME_PATCHED%') DO (
	SET _no_data=""
    IF NOT [%%P] == [] call :GET_PATCH %%P

    if "!BUGFIX_PATCH!" == "0" SET _no_data=T
    if "!BUGFIX_PATCH_FILE!" == "0" SET _no_data=T

    REM Capture all the patches into PATCHES_LIST
    REM Also capture bugfixes that do not have patches

    if "!_no_data!" == "T" (
      call :prnt "  Missing Bugfix: %%P  ->  Patch data missing for bugfix %%P"
      ECHO %%P~0~0~0>> !PATCHES_LIST!
    ) else (
      call :prnt "  Missing Bugfix: %%P  ->  Patch !BUGFIX_PATCH!"
      :: Ensure not to put any duplicates
      FINDSTR /M "!BUGFIX_PATCH!" !PATCHES_LIST! 1>Nul
      if !ERRORLEVEL! == 1 (
        ECHO %%P~!BUGFIX_PATCH!~!BUGFIX_PATCH_FILE!~!BUGFIX_PATCH_NOTES!>> !PATCHES_LIST!
      )
    )
  )

  call :prnt "\n"
  call :prnt "Generating Patch Recommendation Summary ..."
  call :prnt "\n"

:: Print all the patch recommendations

  call :prnt "=================================================================="
  call :prnt "PATCH RECOMMENDATION SUMMARY"
  call :prnt "=================================================================="
  call :prnt "The default patch recommendations to install these missing bugfixes are as follows:"
  call :prnt "------------------------------------------------------------------"
  if %s_dbVersion_psu% EQU 0 (
    call :prnt "Oracle Database Release %s_dbVersion%  (No PSU applied)"
  ) else (
    call :prnt "Oracle Database Release %s_dbVersion%  (PATCHSET UPDATE %s_dbVersion%.%s_dbVersion_psu%)"
  )
  call :prnt "------------------------------------------------------------------"

  FOR /f "tokens=1 delims=^#" %%P IN ('type %PATCHES_LIST%') DO (
    FOR /f "tokens=1-4 delims=~" %%K IN ("%%P") DO (
      if "%%L" == "0" (
        call :prnt "  Bugfix %%K (No Patch mapping data found)"
        call :prnt "    - Use Doc ID 1594274.1 to find the corresponding patch that delivers this bugfix."
      ) else (
        if "%%N" == "0" (
          call :prnt "  Patch %%L"
        ) else (
          call :prnt "  Patch %%L (%%N)"
        )
        call :prnt "    - Filename: %%M"
        call :prnt "\n"
      )
    )
  )

:: Check for End of Correction Support

  if "%obsoleted%" == "T" (
    call :warningForObsolete
    call :prnt "+------------------------------------------------------------------+"
  )

  call :prnt "\n"
  call :prnt "Apply the required patches and rerun this script."
  call :prnt "You should check the patch READMEs for minimum opatch version requirements."
  call :prnt "The latest opatch is available from My Oracle Support via Patch 6880880."
  call :prnt "\n"
  if not x"!consolpatchid!" == x (
    call :prnt "+----------------------------------------------------------------+"
    call :prnt "A consolidated zip file with all the required patches for Database"
    call :prnt "Release %s_dbVersion%.0 is available on My Oracle Support."
    call :prnt "\n"
    call :prnt "   Patch !consolpatchid! [%s_dbVersion%.0 version]"
    call :prnt "     - !consolpatchdesc!"
    call :prnt "\n"
    call :prnt "+----------------------------------------------------------------+"
    call :prnt "\n"
  )
  call :prnt "See Doc ID 1594274.1 for any special instructions regarding patches."
  call :prnt "The footnotes for patches also apply to corresponding overlay patches."
  call :prnt "\n"

:DONE_LISTING

  for /f  "tokens=* " %%A in ('type %OHOME_PATCHED%') do (
    set MissRlbkFix=!MissRlbkFix! %%A
  )

  SET XIT=Y
  if "!db_up!" == "1" (
    call :updateResInDB
  )

  GOTO :CHECK_OHOME_RBK

:: Get the patch information from the 'CAPTURED_TAG_FILE'

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
        IF NOT [%%T] == [] IF "%%T" == "name="NT">" (
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
  GOTO :EOF

:OHOME_PATCHED_OK

  if "%obsoleted%" == "T" (
    SET TCCresult=MISSING
    SET MissRlbkFix=UPGRADE REQUIRED
  ) else (
    call :prnt "All the required one-off bugfixes are present in Database ORACLE_HOME."
    set TCCresult="OK"
  )

  if "!db_up!" == "1" (
    call :updateResInDB
  )



:CHECK_OHOME_RBK

  SET OHOME_RBK=%PTCH_TMP%\ohome_rbk.lst
  TYPE nul > %OHOME_RBK%

  if  "%ROLLBACK_LIST%" == "0" goto :TEMP_FILES

  FOR /d  %%a in (%ROLLBACK_LIST%) do (
    CALL :GET_PATCH_2BEROLLBACK_IN_OHOME %%a
  )

  FOR /f "DELIMS=" %%F in ('ECHO %OHOME_RBK%') DO SET OHOME_RBK_SIZE=%%~zF
  IF %OHOME_RBK_SIZE% NEQ 0 (
    call :prnt "Roll back the following bug fixes from Database ORACLE_HOME:"
    TYPE %OHOME_RBK%

    for /f  "tokens=* " %%A in ('type %OHOME_RBK%') do (
      set MissRlbkFix=!MissRlbkFix! %%A
    )

    REM set MissRlbkFix=%OHOME_RBK%
    SET XIT=Y
    set TCCresult="ROLLBACK"
    if "!db_up!" == "1" (
      call :updateResInDB
    )
  )


:TEMP_FILES

  REM clean up of temp files
  CALL :REMOVE_TEMP_FILES
  REM bugfix 12900661  --end

  FOR /F "delims=" %%d IN ('date /t') DO set today=%%d
  FOR /F "delims=" %%t IN ('time /t') DO set now=%%t

  :: Print the time
  call :prnt "\n"
  call :prnt "Finished prerequisite patch testing : %today% %now%"
  call :prnt "\n"
  call :prnt "Log file for this session : %LOGFILE%"
  call :prnt "\n"
  call :prnt "========================================================="
  call :prnt "\n"

  IF NOT "%XIT%" == "Y"   GOTO :SCRIPTEND

  REM set errorlevel=99
  :: echo Apply the missing bugfixes and then rerun the script.
  REM echo %EC% = !errorlevel!   %EC%_END

  GOTO :SCRIPTEND
  REM bugfix 18251611 --end


:parse_bugs

  for /f  "delims=, tokens=1-20"  %%G in ("%*")  do (
    if "%%G" NEQ "" echo %%G >> %_tmp_patches_applied%
    if "%%H" NEQ "" echo %%H >> %_tmp_patches_applied%
    if "%%I" NEQ "" echo %%I >> %_tmp_patches_applied%
    if "%%J" NEQ "" echo %%J >> %_tmp_patches_applied%
    if "%%K" NEQ "" echo %%K >> %_tmp_patches_applied%
    if "%%L" NEQ "" echo %%L >> %_tmp_patches_applied%
    if "%%M" NEQ "" echo %%M >> %_tmp_patches_applied%
    if "%%N" NEQ "" echo %%N >> %_tmp_patches_applied%
    if "%%O" NEQ "" echo %%O >> %_tmp_patches_applied%
    if "%%P" NEQ "" echo %%P >> %_tmp_patches_applied%
    if "%%Q" NEQ "" echo %%Q >> %_tmp_patches_applied%
    if "%%R" NEQ "" echo %%R >> %_tmp_patches_applied%
    if "%%S" NEQ "" echo %%S >> %_tmp_patches_applied%
    if "%%T" NEQ "" echo %%T >> %_tmp_patches_applied%
  )
  goto :EOF


:parse_line

  for /f "tokens=1-8 delims=: " %%U in ("%*") do (
    if "%%U%%V" == "Patchdescription" (
            set _x=0
            ) else (
    if "%%U%%W" == "Patchapplied" (
            echo %%V >> %_tmp_patches_applied%
            set _x=0
            ) else (
    IF "%%U%%V" == "Bugsfixed" (
            set _x=1
            ) else (
    if "!_x!"=="1" (
    REM echo the line passed is %%*
            call :parse_bugs %*
    ))))
  )
  goto :EOF


:REMOVE_TEMP_FILES

  DEL %_patches_applied%
  DEL %PATCHES_TOAPPLY%
  DEL %PATCHES_TOROLLBACK%

  IF EXIST "%OHOME_PATCHED%"   DEL %OHOME_PATCHED%
  IF EXIST "%OHOME_RBK%" DEL %OHOME_RBK%

  IF EXIST %s_db_oh%\check.log DEL %s_db_oh%\check.log
  IF EXIST %s_db_oh%\check.sql DEL %s_db_oh%\check.sql
  IF EXIST %s_db_oh%\createtable.log DEL %s_db_oh%\createtable.log
  IF EXIST %s_db_oh%\createtable.sql DEL %s_db_oh%\createtable.sql
  IF EXIST %s_db_oh%\checktable.log DEL %s_db_oh%\checktable.log
  IF EXIST %s_db_oh%\createsyn.sql DEL %s_db_oh%\createsyn.sql
  IF EXIST %s_db_oh%\createsyn.log DEL %s_db_oh%\createsyn.log  
  IF EXIST %s_db_oh%\checkname.sql DEL %s_db_oh%\checkname.sql
  IF EXIST %s_db_oh%\checkname.log DEL %s_db_oh%\checkname.log
  IF EXIST %s_db_oh%\checkapps.sql DEL %s_db_oh%\checkapps.sql
  IF EXIST %s_db_oh%\checkapps.log DEL %s_db_oh%\checkapps.log
  IF EXIST %s_db_oh%\checkapplsys.sql DEL %s_db_oh%\checkapplsys.sql
  IF EXIST %s_db_oh%\checkapplsys.log DEL %s_db_oh%\checkapplsys.log  
  IF EXIST %s_db_oh%\execute.sql DEL %s_db_oh%\execute.sql
  IF EXIST %s_db_oh%\checkexec.log  DEL %s_db_oh%\checkexec.log
  IF EXIST %s_db_oh%\checktable.sql DEL %s_db_oh%\checktable.sql

  IF EXIST %PTCH_TMP% (
    REM rmdir %PTCH_TMP%
    rd /s /q  %PTCH_TMP%
  )

  GOTO :EOF

  REM
  REM  This function checks the patch is to be rolled back or to be applied.
  REM  It also creates patches_toapply and patches_torollback files.
  REM


:GET_PATCHES_TO_ROLLBACK_APPLY

  SET TMP_RBK=0
  SET TMP_APPLY=0

  REM Check if the patch is to be rolled back
  FOR /F "DELIMS=" %%a IN ('FINDSTR /i opatch_patch_action %2 ^| FINDSTR /i mode^=\"rollback\"') DO (
    SET TMP_RBK=1
  )

  REM
  REM  If TMP_RBK is 1, the patch is to be rolled back.  Write this patch to
  REM  PATCHES_TOROLLBACK
  REM
  IF %TMP_RBK% EQU 1 (
    ECHO %1 >> %PATCHES_TOROLLBACK%
    GOTO :END_GET_PATCHES_TO_ROLLBACK_APPLY
  )

  REM
  REM  Check if the patch has an opatch action to apply or rehost
  REM
  FOR /F "DELIMS=" %%a IN ('FINDSTR /i opatch_patch_action %2') DO (
    SET TMP_APPLY=1
  )

  REM
  REM  If TMP_APPLY is 1, the patch is to be applied.  Write it to PATCHES_TOAPPLY
  REM
  IF %TMP_APPLY% EQU 1 (
    ECHO %1 >> %PATCHES_TOAPPLY%
  )


:END_GET_PATCHES_TO_ROLLBACK_APPLY

  GOTO :EOF


:GET_PATCH_MISSING_IN_OHOME

  REM Get patches missing in the DB ORACLE_HOME
  REM -----------------------------------------

  SET TMP_PATCH=0
  REM Check if the patch is to be applied

  FOR /F "DELIMS=" %%a IN ('FINDSTR /i %* %PTCH_TMP%\patchesApplied.lst') DO (
    SET TMP_PATCH=1
  )

  REM
  REM If TMP_PATCH is 0, the patch is missing in DB ORACLE_HOME.
  REM Write it to %OHOME_PATCHED%
  REM
  IF %TMP_PATCH% EQU 0 (
   ECHO %1>> %OHOME_PATCHED%
  )
  GOTO :EOF

  REM
  REM Check patch to be rolled back from the existing Oracle Home
  REM If it is in the existing Oracle Home, write it to the file %OHOME_RBK%
  REM
  REM :GET_PATCH_2BEROLLBACK_IN_OHOME
  REM  SET TMP_RBK=0

  REM Check if the patch is to be rolled back
  REM FOR /F "DELIMS=" %%a IN ('FINDSTR /i %%1 REM
  REM Check patch to be rolled back from the existing Oracle Home
  REM If it is in the existing Oracle Home, write it to the file %OHOME_RBK%
  REM


:GET_PATCH_2BEROLLBACK_IN_OHOME
  SET TMP_RBK=0

  REM Check if the patch is to be rolled back
  FOR /F "DELIMS=" %%a IN ('FINDSTR /i %1 %PTCH_TMP%\patchesApplied.lst') DO (
    SET TMP_RBK=1
  )

  REM
  REM If TMP_RBK is 1, patch is to be rolled back in the existing Oracle Home
  REM Write it to the file %OHOME_RBK%
  REM
  IF %TMP_RBK% EQU 1 (
    ECHO %1 >> %OHOME_RBK%
  )

  GOTO :EOF


:ParseDate
  :: Parse (Gregorian) date depending on registry's date format settings
  :: Argument : Gregorian date in local date format,
  :: Requires : sDate (local date separator), iDate (local date format number)
  :: Returns  : GYear (4-digit year), GMonth (2-digit month), GDay (2-digit day)
  ::
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


:identifyAPPS

  set _conn=0
  set appsuser=""

  call :prnt "Identifying APPS and APPLSYS schema names." 
  SET CHECKAPPS=%s_db_oh%\checkapps.sql
  ECHO spool %s_db_oh%\checkapps.log >> %CHECKAPPS%
  ECHO connect / as sysdba;   >> %CHECKAPPS%

  echo set head off feedback off verify off pagesize 0  >> %CHECKAPPS%
  echo select oracle_username from system.fnd_oracle_userid where read_only_flag ='U'; >> %CHECKAPPS%
  ECHO spool off; >> %CHECKAPPS%
  ECHO exit; >> %CHECKAPPS%

  REM ECHO Executing command: %s_db_oh%\bin\sqlplus /nolog @%CHECKAPPS%
  %s_db_oh%\bin\sqlplus /nolog @%CHECKAPPS% > NUL

  FINDSTR /C:"ORA-" %s_db_oh%\checkapps.log > NUL
    IF "%ERRORLEVEL%" == "0" (
      call :prnt "Failed to identify APPS schema."
      set /P appsuser="Enter the Applications 'APPS' user : "
    )
    IF NOT "%ERRORLEVEL%" == "0" (
      for /f %%A in ('type %s_db_oh%\checkapps.log') do (
        if "!_conn!" == "1" (
          set appsuser=%%A
        )
        if  "%%A" == "Connected." (
          set _conn=1
        )
      )
    )

  call :prnt " - APPS schema : !appsuser!"


:identifyAPPLSYS

  set _conn=0
  set applsysuser=""

  SET CHECKAPPLSYS=%s_db_oh%\checkapplsys.sql
  ECHO spool %s_db_oh%\checkapplsys.log >> %CHECKAPPLSYS%
  ECHO connect / as sysdba;   >> %CHECKAPPLSYS%

  echo set head off feedback off verify off pagesize 0  >> %CHECKAPPLSYS%
  echo select oracle_username from system.fnd_oracle_userid where read_only_flag ='E'; >> %CHECKAPPLSYS%
  ECHO spool off; >> %CHECKAPPLSYS%
  ECHO exit; >> %CHECKAPPLSYS%

  REM ECHO Executing command: %s_db_oh%\bin\sqlplus /nolog @%CHECKAPPLSYS%
  %s_db_oh%\bin\sqlplus /nolog @%CHECKAPPLSYS% > NUL

  FINDSTR /C:"ORA-" %s_db_oh%\checkapplsys.log > NUL
    IF "%ERRORLEVEL%" == "0" (
      call :prnt "Failed to identify APPLSYS schema."
      set /P applsysuser="Enter the Applications 'APPLSYS' user : "
    )
    IF NOT "%ERRORLEVEL%" == "0" (
      for /f %%A in ('type %s_db_oh%\checkapplsys.log') do (
        if "!_conn!" == "1" (
          set applsysuser=%%A
        )
        if  "%%A" == "Connected." (
          set _conn=1
        )
      )
    )

  call :prnt " - APPLSYS schema : !applsysuser!"
  call :prnt "\n"


:checkForEtccTable

  set etcctable=0

  SET TEMP_LOGOFF=%s_db_oh%\checktable.sql
  ECHO spool %s_db_oh%\checktable.log >> %TEMP_LOGOFF%
  ECHO connect / as sysdba;   >> %TEMP_LOGOFF%

  ECHO set head off feedback off verify off pagesize 0  >> %TEMP_LOGOFF%
  ECHO select 1 from all_tables where table_name='TXK_TCC_RESULTS' and owner='%applsysuser%'; >> %TEMP_LOGOFF%
  ECHO spool off; >> %TEMP_LOGOFF%
  ECHO exit; >> %TEMP_LOGOFF%

  REM ECHO Executing command: %s_db_oh%\bin\sqlplus /nolog @%TEMP_LOGOFF%
  %s_db_oh%\bin\sqlplus /nolog @%TEMP_LOGOFF% > NUL

  for /f "tokens=1 delims=^#"  %%A in ('type %s_db_oh%\checktable.log') do (
    for /f "tokens=1" %%H in ("%%A") do (
      if  "%%H" == "1" (
        SET row_exst=1
        call :prnt "Table to store DB-ETCC results already exists in the database."
        call :prnt "\n"
      )
    )
  )

  if "!row_exst!" == "0" (
    if "%DBmode%" == "0" (
      call :prnt "\n"
      call :prnt "[WARNING] DB-ETCC: "
      call :prnt "   The database is not open in READ WRITE mode."
      call :prnt "   Table to store the execution results could not be created."
      GOTO :here
    )
    SET  s_ctable="CREATE TABLE %applsysuser%.TXK_TCC_RESULTS ( tcc_version   VARCHAR2(20) NOT NULL,   bugfix_xml_version VARCHAR2(20) NOT NULL,   node_name  VARCHAR2(100) NOT NULL,  database_name  VARCHAR2(64) NOT  NULL,  component_name  VARCHAR2(10) NOT NULL,  component_version VARCHAR2(20) NOT NULL,  component_home   VARCHAR2(600), check_date      DATE,check_result  VARCHAR2(10) NOT NULL,check_message  VARCHAR2(4000)    );"

    SET TEMP_LOGOFF=%s_db_oh%\createtable.sql

    ECHO spool %s_db_oh%\createtable.log >> !TEMP_LOGOFF!
    ECHO connect / as sysdba;   >> !TEMP_LOGOFF!

    ECHO set head off feedback off verify off pagesize 0  >> !TEMP_LOGOFF!

    SET s_ctable=!s_ctable:"=!
    ECHO !s_ctable! >>  !TEMP_LOGOFF!

    ECHO GRANT select on %applsysuser%.TXK_TCC_RESULTS to %appsuser%;    >> !TEMP_LOGOFF!
    ECHO spool off; >> !TEMP_LOGOFF!
    ECHO exit; >> !TEMP_LOGOFF!

    REM ECHO Executing command: %s_db_oh%\bin\sqlplus /nolog @!TEMP_LOGOFF!
    %s_db_oh%\bin\sqlplus /nolog @!TEMP_LOGOFF! > NUL


    if %errorlevel% == 0 GOTO :CREATE_TABLE_OK
    call :prnt "\n"
    call :prnt "[WARNING]"
    call :prnt "   Could not create database table to store DB-ETCC results."
    call :prnt "   Hence the results of DB-ETCC will not be stored."
    call :prnt "   Ensure the database is available, and then rerun DB-ETCC."
    goto :EOF


:CREATE_TABLE_OK
    SET row_exst=1
    call :prnt "Created the table to store DB-ETCC results."


:here
    call :prnt "\n"

  )
  :: Bug 24007644 - Create synonym
  SET CREATESYN=%s_db_oh%\createsyn.sql
  ECHO spool %s_db_oh%\createsyn.log >> %CREATESYN%
  ECHO connect / as sysdba;   >> !CREATESYN!
  ECHO CREATE OR REPLACE SYNONYM %appsuser%.TXK_TCC_RESULTS FOR %applsysuser%.TXK_TCC_RESULTS;  >> !CREATESYN!
  ECHO spool off; >> !CREATESYN!
  ECHO exit;      >> !CREATESYN!

  %s_db_oh%\bin\sqlplus /nolog @!CREATESYN! > NUL

:SCRIPTEND

  goto :EOF


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

  goto :EOF


:updateResInDB

  set _conn=0
  set s_intable="INSERT INTO %applsysuser%.TXK_TCC_RESULTS (tcc_version,bugfix_xml_version,node_name, database_name,component_name,component_version,component_home,check_date,check_result, check_message) values"
  set compname="RDBMS"

  SET TEMP_DBNAME=%s_db_oh%\checkname.sql
  ECHO spool %s_db_oh%\checkname.log >> %TEMP_DBNAME%
  ECHO connect / as sysdba;   >> %TEMP_DBNAME%

  echo set head off feedback off verify off pagesize 0  >> %TEMP_DBNAME%
  echo select NAME from V$DATABASE; >> %TEMP_DBNAME%
  ECHO spool off; >> %TEMP_DBNAME%
  ECHO exit; >> %TEMP_DBNAME%

  REM ECHO Executing command: %s_db_oh%\bin\sqlplus /nolog @%TEMP_DBNAME%
  %s_db_oh%\bin\sqlplus /nolog @%TEMP_DBNAME% > NUL

  for /f %%A in ('type %s_db_oh%\checkname.log') do (
    if "!_conn!" == "1" (
      set s_dbname=%%A
      REM echo db name is !s_dbname!
    )
    if  "%%A" == "Connected." (
      set _conn=1
      REM echo Table to store Technology Codelevel Checker results exists in the database
    )
  )

  set s_values="('!cmd_filever!','!xmlver!','!s_host!','!s_dbname!','%compname%','!s_dbVersion!','%s_db_oh%',to_date('!s_ttime!','dd-mm-yyyy HH24:MI:SS'), '!TCCresult!', '!MissRlbkFix!'); "
  set delcmd="DELETE from %applsysuser%.TXK_TCC_RESULTS where node_name='!s_host!' and component_name='%compname%';"
  set delcmd=!delcmd:"=!


  set s_insert=%s_intable%%s_values%
  set s_insert=!s_insert:"=!
  SET TEMP_DBEXEC=%s_db_oh%\execute.sql
  ECHO spool %s_db_oh%\checkexec.log >> %TEMP_DBEXEC%
  ECHO connect / as sysdba;   >> %TEMP_DBEXEC%
  echo set head off feedback off verify off pagesize 0  >> %TEMP_DBEXEC%

  echo %delcmd% >> %TEMP_DBEXEC%
  echo %s_insert% >>  %TEMP_DBEXEC%

  ECHO spool off; >> %TEMP_DBEXEC%
  ECHO exit; >> %TEMP_DBEXEC%

  REM ECHO Executing command: %s_db_oh%\bin\sqlplus /nolog @%TEMP_DBEXEC%

  IF "%DBmode%" == "0" GOTO :DBMODE_0
  IF NOT "%DBmode%" == "0" GOTO :DBMODE_1


:DBMODE_0


  IF NOT "!row_exst!" == "1" GOTO :NOT_ROW

  call :prnt "   WARNING"
  call :prnt "   To record the results of Technology Codelevel Checker results, open the database in READ WRITE mode and execute the following SQL statements using SQLPLUS in the same order."
  call :prnt "   1. %delcmd% "
  call :prnt "   2. %s_insert%"
  call :prnt "   3. commit;"
  call :prnt "   4. exit;"
  call :prnt "Otherwise the results will not be recorded in the database."
  GOTO:EOF


:NOT_ROW

  call :prnt "Open the Database in READ WRITE mode and re-run the utility. "
  call :prnt "Otherwise the results will not be recorded in the database. "
  GOTO:EOF


:DBMODE_1

  %s_db_oh%\bin\sqlplus /nolog @%TEMP_DBEXEC% > NUL

  if %ERRORLEVEL% == 0 GOTO :INSERT_TABLE_OK
  call :prnt "WARNING"
  call :prnt "Unable to insert Technology Codelevel Checker result in the database"
  call :prnt "Results of Technology Codelevel Checker will not be stored"
  call :prnt "Make sure the database is available, and re-run the Technology Codelevel Checker"
  GOTO:EOF


  :INSERT_TABLE_OK
   call :prnt "Stored Technology Codelevel Checker results in the database successfully."
  GOTO:EOF
