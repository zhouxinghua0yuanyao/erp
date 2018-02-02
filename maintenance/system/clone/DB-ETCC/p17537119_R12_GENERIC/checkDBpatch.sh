#!/bin/sh
#
# $Header: checkDBpatch.sh 120.55 2017/11/02 09:53:58 paholman noship $

#
# This script verifies if all the required pre-requisite patches
# exist in an existing ORACLE_HOME. The script is intended for use 
# before starting an Upgrade flow.
#

runProcess_1() {

if test "${sqlpatchesonly}" = "y" ; then
  list_already_applied_patches
else
  printf "\nChecking Bugfix XML file for ${DB_VERSION}\n" | tee -a $LOGFILE
  # Check for versions out of Error Correction Support
  checkECS
  BUG_APP_RBK=`${AWK} -F'[<|>]' '/Technology/{t=$2}/platform/{p=$2}/base_bugs/{bb=$3}/bugs_rollback/{br=$3; printf "%s!%s:%s#%s\n",t,p,bb,br}' ${BUG_LIST_FILE}  | grep ${DB_VERSION}\" | grep ${PLAT}` 
  if test "${isRacEnv}" = "y" ; then
    RAC_BUGS_XML=`${AWK} -F'[<|>]' '/Technology/{t=$2}/platform/{p=$2}/rac_bugs/{rb=$3; printf "%s!%s:%s\n",t,p,rb}' ${BUG_LIST_FILE}  | grep ${DB_VERSION}\" | grep ${PLAT}` 
  fi

  if test "${bundleapplied}" = "y" -o "${psuapplied}" = "y" ; then
    #printf "\nDEBUG: Checking for ${ojvmbundle} fix in ${BUG_LIST_FILE}\n" | tee -a $LOGFILE
    OJVM_BUGS_XML=`${AWK} -v jvm="${ojvmbundle}" -F '[<|>]' '/Technology/{t=$2}/platform/{p=$2}$0~jvm{j=$3; printf "%s!%s:%s\n",t,p,j}' ${BUG_LIST_FILE}  | grep ${DB_VERSION}\" | grep ${PLAT}` 
    #printf "\nDEBUG: Extracted JVM related metadata ${OJVM_BUGS_XML}\n" | tee -a $LOGFILE
  fi

    #DIFF_LIST=`${AWK} -F'[<|>]' '/Technology/{t=$2}/platform/{p=$2}/base_bugs/{bb=$2}/patch_id/{pi=$3; printf "%s#%s:%s,%s\n",t,p,bb,pi}' ${MAP_LIST_FILE} | grep ${DB_VERSION}\" | grep ${PLAT} |${AWK} -F":" {'print $NF'} |${AWK} -F"=" {'print $NF'}  | tr -d '"' | tr -d ' '`
  if test "${listdiffs}" = "y" ; then
    DIFF_LIST=`${AWK} -F'[<|>]' '/Technology/{t=$2}/platform/{p=$2}/base_bugs/{bb=$2}/merges/{mp=$3}/patch_id/{pi=$3; printf "%s#%s:%s,%s,%s\n",t,p,bb,pi,mp}' ${MAP_LIST_FILE} | grep ${DB_VERSION}\" | grep ${PLAT} |${AWK} -F":" {'print $NF'} |${AWK} -F"=" {'print $NF'}  | tr -d '"' | tr -d ' '`
  fi
    #printf "\n[DEBUG] DIFF_LIST is :\n${DIFF_LIST}\n" | tee -a $LOGFILE
  appsuser=""
    if test "${BUG_APP_RBK}x" = "x" ; then
      if test "${obsolete}" = "y" ; then
            TCCresult="MISSING"
            MissRlbkFix="UPGRADE REQUIRED"
            if test "${contextfile}" != "none" ; then
               updateResInDB
            fi
            exit_script
      else
        printf "\nCould not get list of bugfixes from ${BUG_LIST_FILE} for ${hometype} version ${DB_VERSION}." | tee -a $LOGFILE
        printf "\nVerify correct XML file is being used and then retry.\n" | tee -a $LOGFILE
        rm -rf ${PTCH_TMP}
        exit 1
      fi
   else
      printf "\nObtained list of bugfixes to be applied and the list to be rolled back." | tee -a $LOGFILE
      printf "\nNow checking ${hometype} ORACLE_HOME." | tee -a $LOGFILE
   fi
   
   BUG_LIST=`echo ${BUG_APP_RBK}| ${AWK} -F: '{ print $2}' | ${AWK} -F# '{ print $1}'|sed 's/^[ \t]*//;s/[ \t]*$//'`
   BUG_RBK=`echo ${BUG_APP_RBK}| ${AWK} -F# '{ print $2}' | sed 's/^[ \t]*//;s/[ \t]*$//'`
   JVM_BUG=`echo ${OJVM_BUGS_XML}| ${AWK} -F: '{ print $2}' |sed 's/^[ \t]*//;s/[ \t]*$//'` 
   
   if test "${BUG_LIST}x" != "x" ; then 
	    echo ${BUG_LIST} | tr "," "\012" > ${PATCHES_TOAPPLY}

   fi

   if test "${JVM_BUG}x" != "x" ; then
      echo ${JVM_BUG} | tr "," "\012" >> ${PATCHES_TOAPPLY}
      #printf "\nDEBUG: Added ${JVM_BUG} to ${PATCHES_TOAPPLY}\n" | tee -a $LOGFILE
   fi 

   if test "${BUG_RBK}x" != "x" ; then 
	    echo ${BUG_RBK} | tr "," "\012" > ${PATCHES_TOROLLBACK}
   fi

   if test "${RAC_BUGS_XML}x" != "x" ; then
      #printf "\nDEBUG: RACBUGSML is ${RAC_BUGS_XML}"
      RAC_BUG_LIST=`echo ${RAC_BUGS_XML}| ${AWK} -F: '{ print $2}' |sed 's/^[ \t]*//;s/[ \t]*$//'`     
      #printf "\nDEBUG: RACBUGLIST is ${RAC_BUG_LIST}"
      echo ${RAC_BUG_LIST} | tr "," "\012" > ${RAC_PATCHES_TOAPPLY}
      checkRACfixes="y"
   fi

 

   # Get the list of one-offs already applied to the Oracle Database Home
   list_already_applied_patches

   if test "${listdiffs}" = "y" ; then
      DIFF_LIST2=`echo ${DIFF_LIST} | sed 's/,/ /g'`
          #printf "\n[DEBUG] DIFF_LIST2 is :\n${DIFF_LIST2}\n" | tee -a $LOGFILE
      echo ${DIFF_LIST2} | tr " " "\012" > ${FIXES_FROMXML}1
      sort ${FIXES_FROMXML}1 | uniq > ${FIXES_FROMXML}
      runDiffsCheck # Run opatch again to a diff patch output
   fi
   
   # Patch mapping changes 1
   rm -f ${LOCAL_TEMP}/${TECHNAME}.map
   rm -f ${LOCAL_TEMP}/summary.txt
   touch ${LOCAL_TEMP}/${TECHNAME}.map
   touch ${LOCAL_TEMP}/summary.txt
   
   # Check if mapping data available in mappings XML file
   if test "${isPatchMap}" = "y" ; then
      printf "\nChecking Mapping XML file for ${mapvers}\n" | tee -a $LOGFILE
      isMapData=`grep "\"${mapvers}\"" ${MAP_LIST_FILE}`
      #printf "\n DEBUG: isMapData = ${isMapData}"
      if test "${isMapData}x" = "x" ; then
         # Disable patch mapping
         isPatchMap="n"
         printf "\nPatch mapping not available." | tee -a $LOGFILE
         if test "${listdiffs}" = "y" ; then
            # Disable patch diffs as no mapping data available
            listdiffs="n"
            printf "\nDisabling 'listdiffs' option." | tee -a $LOGFILE
         fi
         # Display correct info according to DB type or options enabled
         if test "${isDBIM}" = "y" ; then
            printf "\nPatch mapping data is not available for ${tsvers} ${IMheader} ${DOTheader}" | tee -a $LOGFILE
            if test "${cloud}" = "y" ; then
              printf "\n" # Bug 25432094
              #printf "\nYou now need to find the recommended bundle patch on the Cloud service console.\n" | tee -a $LOGFILE
            else
              printf "\nRefer to Doc ID 1594274.1 for the recommended Bundle Patch versions.\n" | tee -a $LOGFILE
            fi  
         else
            if test "${psuversion}x" != "x" ; then
               printf "\nPatch mapping data is not available for ${tsvers} ${DOTheader}" | tee -a $LOGFILE
               printf "\nRefer to Doc ID 1147107.1 for the required overlay patches for PSUs and Oracle E-Business Suite\n" | tee -a $LOGFILE
            else
               printf "\nPatch mapping data is not available for ${tsvers} ${patchmapID}" | tee -a $LOGFILE
            fi
         fi
      fi
   # End mapping data check
   fi
 if test "${BUG_LIST}x" != "x" ; then
   # List of patches that are missing in existing Oracle Home
   if test "${PLATFORM}" != "HP_IA" -a "${PLATFORM}" != "Linux_x64" -a "${PLATFORM}" != "IBM_AIX" ; then   #bug14759449
        OHOME_PATCHED=`fgrep -vf ${PATCHES_APPLIED} ${PATCHES_TOAPPLY}`
        if test "${checkRACfixes}" = "y" ; then
          RACHOME_PATCHED=`fgrep -vf ${PATCHES_APPLIED} ${RAC_PATCHES_TOAPPLY}`
        fi
        if test "${listdiffs}" = "y" ; then
          OHOME_EXTRAS=`fgrep -vf ${FIXES_FROMXML} ${DIFF_PATCHES_APPLIED}` # Additional Applied Patch info 
        fi
   else
        OHOME_PATCHED=`grep -Fxvf ${PATCHES_APPLIED} ${PATCHES_TOAPPLY}`
        if test "${checkRACfixes}" = "y" ; then
          RACHOME_PATCHED=`grep -Fxvf ${PATCHES_APPLIED} ${RAC_PATCHES_TOAPPLY}`
        fi
        if test "${listdiffs}" = "y" ; then
          OHOME_EXTRAS=`grep -Fxvf ${FIXES_FROMXML} ${DIFF_PATCHES_APPLIED}` # Additional Applied Patch info
        fi
   fi
   MissRlbkFix=${OHOME_PATCHED}
   if test "${listdiffs}" = "y" ; then
      OHOME_PATCHED="" # Skip missing bugfix check for this mode
      RACHOME_PATCHED="" # Skip missing bugfix check for this mode
   fi    
   if test "${OHOME_PATCHED}x" != "x" ; then
       if test "${isPatchMap}" != "y" ; then
          printf "\nSome required one-off bugfixes are missing from the ${hometype} ORACLE_HOME."| tee -a $LOGFILE
          printf "\nThe missing bugfixes are:"| tee -a $LOGFILE
          printf "\n${OHOME_PATCHED}\n"| tee -a $LOGFILE
          if test "${cloud}" = "y" ; then
            printf "\n" # Bug 25432094
            #printf "\n** Patch mapping is available for missing bugfixes for the latest quarterly \npatchset and bundle patches only.  You can find the recommended bundle patch on \nthe Cloud service console. ** \n"| tee -a $LOGFILE
          else
            printf "\n** ${mosnote} ** \n"| tee -a $LOGFILE
            printf "\nApply the missing bugfixes and then rerun the script. \n"| tee -a $LOGFILE
          fi
          xit="y"
       else
         # Patch mapping enabled

          echo "${OHOME_PATCHED}" | while read bugfixtoapply;
            do              
                # Call patch mapping function
                MapBugfixIDtoPatchID  
            done

            # Update summary temp file
            printf "\n-------------------------------------------------------------------------------">> ${LOCAL_TEMP}/summary.txt
            printf "\nOracle Database Release ${tsvers} ${IMheader} ${DOTheader}">> ${LOCAL_TEMP}/summary.txt
            printf "\n-------------------------------------------------------------------------------\n">> ${LOCAL_TEMP}/summary.txt
            cat ${LOCAL_TEMP}/${TECHNAME}.map >> ${LOCAL_TEMP}/summary.txt
            if test -f ${LOCAL_TEMP}/${TECHNAME}.ecs ; then
               cat ${LOCAL_TEMP}/${TECHNAME}.ecs >> ${LOCAL_TEMP}/summary.txt
               printf "+-----------------------------------------------------------------------------+\n">> ${LOCAL_TEMP}/summary.txt
            fi
       
            # Print Summary
            printf "\n\nGenerating Patch Recommendation Summary." | tee -a $LOGFILE
            printf "\n\n===============================================================================" | tee -a $LOGFILE
            printf "\nPATCH RECOMMENDATION SUMMARY\n" | tee -a $LOGFILE
            printf "===============================================================================" | tee -a $LOGFILE
            printf "\nThe default patch recommendations to install these missing bugfixes are:"| tee -a $LOGFILE
            cat ${LOCAL_TEMP}/summary.txt | tee -a $LOGFILE
            if [ $dbup != 0 ] ; then
               printf "\nApply the required patches and rerun this script" | tee -a $LOGFILE
               printf "\n\nYou should check the patch READMEs for minimum opatch version requirements.\nThe latest opatch is available from My Oracle Support via Patch 6880880.\n\n" | tee -a $LOGFILE
               
              if test "${cloud}" != "y" ; then
                  if test "${bundleapplied}" = "y" ; then
                      CheckBundleMissing
                  else 
                      # check for missing PSU
                      CheckPSUMissing
                  fi
              fi

               # if DBCS + RAC and 12c check for missing bundle
              if test "${cloud}" = "y" ; then
                  if test "${isRacEnv}" = "y" -a "${DB_REL}" = "12" ; then
                     CheckBundleMissing
                  else
                     # check for missing PSU
                     CheckPSUMissing
                  fi
              fi

               printf "\nSee Doc ID ${mosID} for any special instructions for these patches."| tee -a $LOGFILE
               printf "\nNote: Footnotes in Doc ID 1594274.1 also apply to corresponding overlay patches.\n" | tee -a $LOGFILE
            else
               printDBdownWarning
            fi
           # End Print Summary
       # End Patch Mapping 
       fi
       if test "${contextfile}" != "none" ; then
          updateResInDB
       fi
   else
       if test "${listdiffs}" != "y" ; then
          printf "\nAll the required one-off bugfixes are present in ${hometype} ORACLE_HOME.\n"| tee -a $LOGFILE
          TCCresult="OK"
          if test "${contextfile}" != "none" ; then
             updateResInDB
          fi
       fi
       rm -f ${LOCAL_TEMP}/${TECHNAME}.map
   fi

   # Additional Applied Patch info   
   if test "${OHOME_EXTRAS}x" = "x" ; then
     if test "${listdiffs}" = "y" ; then
        printf "\nAll patches applied to this ${hometype} ORACLE_HOME are listed in the XML file.\n"| tee -a $LOGFILE
        listdiffs="n"
     else
        listdiffs="n"
     fi
   fi

   # Additional Bugfix Header
   if test "${RACHOME_PATCHED}x" != "x" -o "${listdiffs}" = "y" ; then
      printf "\n-------------------------------------------------------------------------------"| tee -a $LOGFILE
      printf "\nADDITIONAL BUGFIX INFORMATION FOR ORACLE DATABASE ${DB_VERSION}"| tee -a $LOGFILE
      printf "\n-------------------------------------------------------------------------------"| tee -a $LOGFILE
   fi

   # RAC fixes
   if test "${RACHOME_PATCHED}x" != "x" ; then
          printf "\nOracle Grid related bugfixes are missing from the ${hometype} ORACLE_HOME."| tee -a $LOGFILE
          printf "\nThe missing bugfixes are:"| tee -a $LOGFILE
          printf "\n${RACHOME_PATCHED}\n"| tee -a $LOGFILE
          printf "\n** Refer to Appendix B of Doc ID 1594274.1 for further information ** \n"| tee -a $LOGFILE
          #printf "\nApply the missing bugfixes and then rerun the script. \n"| tee -a $LOGFILE   
   fi

   if test "${listdiffs}" = "y" ; then

      printf "\nThere are bugfixes applied to this ${hometype} ORACLE_HOME that are not listed in\nthe XML file:" | tee -a $LOGFILE

      if test "${listdiffs}" = "y" ; then
         echo ${OHOME_EXTRAS} | tr " " "\012:" > ${xlist}
         findDiffsDetails
      fi   
      printf "\n"| tee -a $LOGFILE
   fi
   # End Additional Applied Patch info  

 fi
 if test "${BUG_RBK}x" != "x" ; then
   # List of patches that are to be rolled back from the existing Oracle Home
   if test "${PLATFORM}" != "HP_IA" ; then         #bug14759449
       OHOME_RBK=`fgrep -f ${PATCHES_APPLIED} ${PATCHES_TOROLLBACK}`
   else
       OHOME_RBK=`grep -Fxf ${PATCHES_APPLIED} ${PATCHES_TOROLLBACK}`
   fi
 
   if test "${OHOME_RBK}x" != "x" ; then
       printf "\nRoll back the following bug fixes from ${hometype} ORACLE_HOME: "| tee -a $LOGFILE
       printf "\n${OHOME_RBK}\n"| tee -a $LOGFILE
       xit="y";
       TCCresult="ROLLBACK"
       MissRlbkFix=${OHOME_RBK}
       if test "${contextfile}" != "none" ; then
          updateResInDB
       fi
   fi
 fi
fi # end SQLpatch check
   # clean up of temp files
   rm -rf ${PTCH_TMP} 
 

 
 
}
 
 list_already_applied_patches() {
 oplist="${PTCH_TMP}/opatch_$$.lst"
 plist="${PTCH_TMP}/patch_$$.lst"
 dlist="${PTCH_TMP}/dpatch_$$.lst"
 #xlist="${PTCH_TMP}/xpatch_$$.lst"
 xlist="/tmp/xpatch.lst"


# Ensure opatch version is at 11.2.0.3 or higher
 OPVER1=`${OPATCH} version | grep "Version:" | ${AWK} -F: '{ print $2 }'`
 MREL1=`echo $OPVER1 | ${AWK} -F\. '{ print $1 }'`
 MREL2=`echo $OPVER1 | ${AWK} -F\. '{ print $2 }'`
 MREL3=`echo $OPVER1 | ${AWK} -F\. '{ print $3 }'`
 MREL4=`echo $OPVER1 | ${AWK} -F\. '{ print $4 }'`
 printf "\n\nThe opatch utility is version${OPVER1}."
 if test $MREL1 -lt 11; then
    printf "\nTo use DB-ETCC, you must obtain a newer version of opatch (Patch 6880880 from \nMy Oracle Support) and then rerun this script. Exiting.\n"| tee -a $LOGFILE
    exit 1
 fi
 if test $MREL1 -gt 11 ; then
    printf "\nDB-ETCC is compatible with this opatch version.\n"| tee -a $LOGFILE
 else
   if test $MREL2 -lt 2; then
      printf "\nTo use DB-ETCC, you must obtain a newer version of opatch (Patch 6880880 from \nMy Oracle Support) and then rerun this script. Exiting.\n"| tee -a $LOGFILE
      exit 1
   fi
   if test $MREL2 -gt 2; then
      printf "\nDB-ETCC is compatible with this opatch version.\n"| tee -a $LOGFILE
   else
      if test $MREL3 -gt 0; then
        printf "\nDB-ETCC is compatible with this opatch version.\n"| tee -a $LOGFILE
      fi
      if test $MREL3 -eq 0; then
         if test $MREL4 -lt 3; then
            printf "\nTo use DB-ETCC, you must obtain a newer version of opatch (Patch 6880880 from \nMy Oracle Support) and then rerun this script. Exiting.\n"| tee -a $LOGFILE
            exit 1
         else
            printf "\nDB-ETCC is compatible with this opatch version.\n"| tee -a $LOGFILE
         fi
      fi
   fi
 fi

 # get the list of patches
 if test "${sqlpatchesonly}" = "y" ; then
   $OPATCH lsinventory -detail |grep sqlpatch > ${oplist}
   exit_code=$?
 else
   $OPATCH lspatches -bugs > ${oplist}
   exit_code=$?
 fi
 
 if test "$exit_code" != "0" ; then
     printf "\nError running opatch. Error code : ${exit_code}"| tee -a $LOGFILE
     printf "\nCannot confirm bugfixes applied to the ${hometype} ORACLE_HOME."| tee -a $LOGFILE
     rm -rf ${PTCH_TMP}  
     printf "\nExiting.\n"| tee -a $LOGFILE
     exit 1
 fi

 opOut=`grep "no Interim patches" ${oplist}`

 if test "${opOut}x" = "x" ; then
   # Patches have been applied
   printf "\nFound patch records in the inventory.\n" | tee -a $LOGFILE
   if test "${sqlpatchesonly}" = "y" ; then
      ListSQLPatches
   else
     #remove all blank spaces and empty lines
     sed '/^$/d;s/ //g' ${oplist} > ${oplist}1  
     mv ${oplist}1 ${oplist}
     for line in `cat ${oplist}`
     do
       patchnum=`echo ${line} | cut -d";" -f1 `
       bugsnum=`echo ${line} | cut -d";" -f3 `

       if test "${bugsnum}x" = "x" ; then
     bugsnum=`echo ${line} | cut -d";" -f2 `
       fi
       if test "${bugsnum}x" = "x" ; then
           printf "\nError reading opatch utility output.\n"
     exit 1
       fi
       if test "$patchnum" != "$bugsnum" ; then
           echo "${patchnum}" >>${plist}
     bugsnum=`echo ${bugsnum} | tr "," "\012" `
           echo "${bugsnum}" >> ${plist}
       else
            echo "${patchnum}" >> ${plist}
       fi
     done
    fi     
 else
     # No patches applied to this OH
     printf "\nNo patch records found in the inventory.\n"| tee -a $LOGFILE
     echo "" >> ${plist}
     listdiffs="n"
 fi

 # sort the file with the list of applied patches/bug fixes
 sed '/^$/d' ${plist} >> ${plist}1
 mv ${plist}1 ${plist}

 sort ${plist} | uniq > ${PATCHES_APPLIED}

 IFS=$oIFS # Solaris compatibility
 }

runDiffsCheck(){

  # Read existing opatchlist but remove "Opatchsucceeded." line
  `cat ${oplist} |grep -v OPatch >> ${oplist}1`
  mv ${oplist}1 ${oplist}
  for line in `cat ${oplist}`
   do
       patchnum=`echo ${line} | cut -d";" -f1 `
       bugsnum=$patchnum

       if test "${bugsnum}x" = "x" ; then
          bugsnum=`echo ${line} | cut -d";" -f2 `
       fi
       if test "${bugsnum}x" = "x" ; then
          printf "\nError reading opatch utility output. Log a bug against ETCC.\n"
          exit 1
       fi     
       if test "$patchnum" != "$bugsnum" ; then
          echo "${patchnum}" >>${dlist}
          bugsnum=`echo ${bugsnum} | tr "," "\012" `
          echo "${bugsnum}" >> ${dlist}
       else
          echo "${patchnum}" >> ${dlist}
       fi
   done

  # sort the file with the list of applied patches/bug fixes
  sed '/^$/d' ${dlist} >> ${dlist}1
  mv ${dlist}1 ${dlist}
  sort ${dlist} | uniq > ${DIFF_PATCHES_APPLIED}
  IFS=$oIFS # Solaris compatibility
}

findDiffsDetails(){
  printf '%s\n' "$OHOME_EXTRAS" | while IFS= read -r line
  do
       patchnum=`echo ${line}`
       printf "\n\nPatch ${patchnum} details" | tee -a $LOGFILE
       printf "\n------------------------" | tee -a $LOGFILE
       patchdetails1=`$OPATCH lspatches -id ${patchnum}`
       patchdetails=`echo ${patchdetails1} | sed 's/OPatch succeeded.//g'`
       printf "\n${patchdetails}" | tee -a $LOGFILE
       patchsqldetails=`$OPATCH lsinventory -detail |grep sqlpatch |grep ${patchnum}`
       if test "${patchsqldetails}x" != "x" ; then
          printf "\nsql files:\n${patchsqldetails}" | tee -a $LOGFILE
       fi
  done
  printf "\n-------------------------------------------------------------------------------"| tee -a $LOGFILE

}

setSQLplusenv(){

ORACLE_HOME=$ORACLE_HOME
export ORACLE_HOME
ORACLE_SID=$ORACLE_SID
export ORACLE_SID
# Check for multitenant and set temp env vars accordingly
# The following was removed for Bug 26940382
#if test "${isMultiTenant}" = "true" ; then
   #printf "\nMulti-tenant identified."
#   oracle_sid_org=${ORACLE_SID}  
#   ORACLE_SID=${ORACLE_CDB_SID}
#   export ORACLE_SID
   #printf "\nMultitenant: Temporarily setting ORACLE_SID=${ORACLE_SID}."
#fi
}

checkDB() {
# check DB connectivity
setSQLplusenv

printf "\nConnecting to database. "  | tee -a $LOGFILE
dbconn=`$ORACLE_HOME/bin/sqlplus / as sysdba <<EOS
exit
EOS
`

#need the : at the end else idle instance connection will return success
dbstat=`echo $dbconn | grep "Connected to:"`
result=$?

if [ $result -eq 0 ]
then
   printf "\nDatabase connection successful. \n" | tee -a $LOGFILE
   dbup=1
   storeresults="y"
else
   dbup=0
   storeresults="n"
fi

}

# Bug 21073382
identifyAPPS(){
setSQLplusenv
printf "\nIdentifying APPS and APPLSYS schema names. " | tee -a $LOGFILE
schemaname=`$ORACLE_HOME/bin/sqlplus -s "/ as sysdba" <<EOS1
set head off feedback off verify off pagesize 0
${altersession}
select oracle_username from system.fnd_oracle_userid where read_only_flag ='U';
exit;
EOS1
`

sqlerror=`echo ${schemaname} | grep "ORA-"`
result=$?

if [ $result -eq 0 ] # If result contains ORA- then check failed
  then
  #printf "\n[DEBUG] SQL output is :\n${schemaname}\n" | tee -a $LOGFILE
  appsuser=""
else
  appsuser=`echo ${schemaname} | sed 's/^$//g'`
fi

if test "${appsuser}x" = "x" ; then
  printf "\nFailed to identify APPS schema." | tee -a $LOGFILE
  printf "\nEnter the Applications 'APPS' user : "
  read appssuser
fi

printf "\n - APPS schema : ${appsuser}" | tee -a $LOGFILE
identifyAPPLSYS


}
# Bug 21073382
identifyAPPLSYS(){
setSQLplusenv
schemaname=`$ORACLE_HOME/bin/sqlplus -s "/ as sysdba" <<EOS1
set head off feedback off verify off pagesize 0
${altersession}
select oracle_username from system.fnd_oracle_userid where read_only_flag ='E';
exit;
EOS1
`
sqlerror=`echo ${schemaname} | grep "ORA-"`
result=$?

if [ $result -eq 0 ] # If result contains ORA- then check failed
  then
  #printf "\n[DEBUG] SQL output is :\n${schemaname}\n" | tee -a $LOGFILE
  applsysuser=""
else
  applsysuser=`echo ${schemaname} | sed 's/^$//g'`
fi


if test "${applsysuser}x" = "x" ; then
  printf "\nFailed to identify APPLSYS schema." | tee -a $LOGFILE
  printf "\nEnter the Applications 'APPLSYS' user : "
  read applsysuser
fi

printf "\n - APPLSYS schema : ${applsysuser}\n" | tee -a $LOGFILE


}

checkForEtccTable(){
setSQLplusenv
printf "\nChecking for DB-ETCC results table. " | tee -a $LOGFILE
# check for TXK_TCC_RESULTS table existence

# Check for multitenant and set temp env vars accordingly
# The following was removed for Bug 26940382
#if test "${isMultiTenant}" = "true" ; then
   #printf "\nMulti-tenant identified."
#   oracle_sid_org=${ORACLE_SID}  
#   ORACLE_SID=${ORACLE_CDB_SID}
#   export ORACLE_SID
   #printf "\nMultitenant: Temporarily setting ORACLE_SID=${ORACLE_CDB_SID}."
#fi

tblExists=`$ORACLE_HOME/bin/sqlplus -s "/ as sysdba" <<EOS1
set head off feedback off verify off pagesize 0
${altersession}
select 1 from all_tables where table_name='TXK_TCC_RESULTS' and owner='${applsysuser}';
exit;
EOS1
`

tblExists=`echo ${tblExists} | sed 's/^$//g'`
if test "${tblExists}x" = "x" ; then
  tblExists=0
fi

if test "${tblExists}" != "1" ; then
	if test "${DBmode}" != "0" ; then
       printf "\nCreating DB-ETCC results table. " | tee -a $LOGFILE
	   $ORACLE_HOME/bin/sqlplus -s "/ as sysdba" <<EOS2
    	  set head off feedback off verify off pagesize 0
        ${altersession}
	      CREATE TABLE ${applsysuser}.TXK_TCC_RESULTS
	      ( tcc_version   VARCHAR2(20) NOT NULL,
    	    bugfix_xml_version VARCHAR2(20) NOT NULL,
	       node_name  VARCHAR2(100) NOT NULL,
		database_name  VARCHAR2(64) NOT NULL,
	        component_name  VARCHAR2(10) NOT NULL,
    	    component_version VARCHAR2(20) NOT NULL,
	        component_home   VARCHAR2(600),
		check_date	DATE,
		check_result  VARCHAR2(10) NOT NULL,
    	  	check_message VARCHAR2(4000)
	      );
	      GRANT select on ${applsysuser}.TXK_TCC_RESULTS to ${appsuser};
	    exit;
EOS2
	    exit_code=$?
    	if test "$exit_code" != "0" ; then
        	printf "\n[WARNING]\n Could not create database table to store DB-ETCC results." | tee -a $LOGFILE
    		printf "\nUnable to store results in the database.\n" | tee -a $LOGFILE
	        printf "\nEnsure the database is available, and then rerun DB-ETCC.\n" | tee -a $LOGFILE
    	 else
			tblExists="1"
	        printf "\nCreated the table to store DB-ETCC results.\n" | tee -a $LOGFILE
    	 fi
	else
		printf "\n[WARNING] DB-ETCC: \n"| tee -a $LOGFILE
		printf "  The database is not open in READ-WRITE mode.\n" | tee -a $LOGFILE
		printf "  Could not create table to store DB-ETCC results.\n" | tee -a $LOGFILE
	fi
else
     printf "\nTable to store DB-ETCC results already exists in the database.\n" | tee -a $LOGFILE
fi
# Bug 24007644 - Create synonym
$ORACLE_HOME/bin/sqlplus -s "/ as sysdba" <<EOS3
set head off feedback off verify off pagesize 0
${altersession}
CREATE OR REPLACE SYNONYM ${appsuser}.TXK_TCC_RESULTS FOR ${applsysuser}.TXK_TCC_RESULTS;
exit;
EOS3
}

checkGrid() {

  invreport=${LOCAL_TEMP}/lsinventory.txt

  if test "${ORACLE_HOME}x" = "x" ; then
    printf "\nCannot determine ORACLE_HOME from environment."
    printf "\nEnter the ORACLE_HOME: "
    read ORACLE_HOME
  fi

  $ORACLE_HOME/OPatch/opatch lsinventory > ${invreport}
  exit_code=$?

  if test "$exit_code" != "0" ; then
    printf "\nError running opatch. Error code : ${exit_code}"| tee -a $LOGFILE
    printf "\nCannot confirm bugfixes applied to the ${hometype} ORACLE_HOME."| tee -a $LOGFILE
    rm -rf ${PTCH_TMP}
    printf "\nExiting.\n"| tee -a $LOGFILE
    exit 1
  fi

  gridvers="`grep 'Oracle Grid Infrastructure' ${invreport} |${AWK} NR==1{'print $5'}`"
  #printf "\nDEBUG: gridvers is ${gridvers}."| tee -a $LOGFILE

  if test "${gridvers}x" != "x" ; then
    GridHome="y"
    contextfile="none"
    racornot="false" # RAC DB ETCC code not relevant for GRID
    isPatchMap="n" # Patch mapping currently not supported for GRID
    hometype="Grid Infrastructure"
    DB_VERSION=`echo ${gridvers} | ${AWK} -F\. '{
    if ($3 == "") t="0";
       else t=$3;
    if ($4 == "") f="0";
       else f=$4;
    printf "%s.%s.%s.%s",$1,$2,t,f }'`
    tsvers=$DB_VERSION
    printf "\nOracle Grid Infrastructure ${tsvers} identified.\n" 
    mapvers=${DB_VERSION}
    TECHNAME="GRID_HOME"
  else
    GridHome="n"
    hometype="Database"
    printf "\nOracle Grid Infrastructure not identified.\n" 
  fi 
} # end checkGrid

checkInMemory() {
printf "\nChecking if InMemory option is enabled. " | tee -a $LOGFILE
inmemsize=`$ORACLE_HOME/bin/sqlplus -s "/ as sysdba" <<EOS3
set head off verify off feedback off pagesize 0
${altersession}
select value from v\\\$parameter where name = 'inmemory_size';
exit;
EOS3
`

if test "${inmemsize}x" != "x" -a "${inmemsize}" != "0" ; then
    isDBIM="y"
    printf "\nInMemory option is enabled in the database.\n"| tee -a $LOGFILE	
    #printf "InMemory size: ${inmemsize} \n"
    IMheader="- InMemory"
else
    isDBIM="n"
    printf "\nInMemory option is not enabled in the database.\n"| tee -a $LOGFILE

fi
}


checkASM(){
setSQLplusenv
asmdisks=`$ORACLE_HOME/bin/sqlplus -s "/ as sysdba" <<EOS1
set head off feedback off verify off pagesize 0
${altersession}
select count(*) from v\\\$asm_disk;
exit;
EOS1
`
sqlerror=`echo ${askdisks} | grep "ORA-"`
result=$?

#printf "\n[DEBUG] SQL output is :\n${asmdisks}\n" | tee -a $LOGFILE
#
if [ $result -eq 0 ] # If result contains ORA- then check failed
  then
  #printf "\n[DEBUG] SQL output is :\n${asmdisks}\n" | tee -a $LOGFILE
  asmdisk=""
else
  askdisks=`echo ${asmdisks} | sed 's/^$//g'`
fi

#printf "\n[DEBUG] ASMDISKS value is now :\n${asmdisks}\n" | tee -a $LOGFILE


if test "${asmdisks}x" = "x" ; then
  printf "\n[ERROR] Failed to identify if ASM is in use" | tee -a $LOGFILE
  printf "\nMake sure the database is up and connectivity exists, and re-run the Technology Codelevel Checker." | tee -a $LOGFILE
else
  if [ $asmdisks -eq 0 ] 
  then
    isASMenabled="n"
  else
    printf "\nASM storage identified.\n" | tee -a $LOGFILE
    isASMenabled="y"
  fi
fi

printf "\n - APPLSYS schema : ${applsysuser}\n" | tee -a $LOGFILE

}

checkOPC() {
  if test -f /usr/bin/dbaascli -o -f /opt/oracle/dcs/client/bin/raccli ; then
    printf "\nOracle Database Cloud Service utilities found.\n"| tee -a $LOGFILE
    cloud="y"
    isDBCS="y"
    cloudtag="DBCS"
  fi

  if test -f /opt/oracle/dcs/bin/odacli -o -f /opt/oracle/dcs/bin/dbcli ; then
    printf "\nOracle Bare Metal Cloud Service utilities found.\n"| tee -a $LOGFILE
    isBMCS="y"
  fi

  if test "${isBMCS}" = "y" ; then
    printf "\nRunning on Oracle Bare Metal Cloud Service.\n"| tee -a $LOGFILE
    cloud="y"
    isBMCS="y"
    cloudtag="BMCS"
  fi

}

checkExaData() {
    if test -d /etc/oracle/cell -o -f /opt/oracle.cellos/ORACLE_CELL_OS_IS_SETUP ; then
          printf "\nRunning on an Engineered System.\n"| tee -a $LOGFILE
           if test "${DB_REL}" = "12" ; then
               isDBIM="y"
               IMheader="- Engineered Systems"
           else
               DB_VERSION="${tsvers}_ES"
           fi
          isExa="y"
          mosID="1392527.1"
          mosnote="Refer to MOS Doc ID \"1392527.1:Database Patches Required by Oracle E-Business Suite on Oracle Engineered Systems: Exadata Database Machines and SuperClusters\" to find the recommended combination of patches.\n"

    fi
	}
updateResInDB(){

# Update the results into DB
InsCmd="INSERT INTO ${applsysuser}.TXK_TCC_RESULTS (tcc_version,bugfix_xml_version,node_name, database_name,component_name,component_version,component_home,check_date,check_result, check_message) values"
tccversion=$progver
tccexectime=$execdate
tccresult=$TCCresult
xmlversion=$xmlver
server=`grep s_dbhost ${contextfile} | sed 's/^.*s_dbhost[^>.]*>[ ]*\([^<]*\)<.*/\1/g; s/ *$//g'`
physserver=`grep s_hostname ${contextfile} | sed 's/^.*s_hostname[^>.]*>[ ]*\([^<]*\)<.*/\1/g; s/ *$//g'`
dbname=$DBNAME
compver=$DB_VERSION
compname="RDBMS"
comphome=$ORACLE_HOME
chkmesg1=`echo ${MissRlbkFix}| tr "\012" "," `
chkmesg=`echo ${chkmesg1}| sed 's/.$//'`

if [ $dbup -eq 1 ] 
then
   dbname=`$ORACLE_HOME/bin/sqlplus -s "/ as sysdba" <<EOSR
    set head off feedback off verify off pagesize 0
    ${altersession}
    select SYS_CONTEXT('USERENV','DB_NAME') from dual;
    exit;
EOSR`

DelCmd="DELETE from ${applsysuser}.TXK_TCC_RESULTS where component_name='$compname' and (lower(node_name)=lower('$server') or lower(node_name)=lower('$physserver'));"
  
  # Bugs 23023865 / 24301355 / 20795308 - Use Physical Server server name for Engineered Systems
  if test "${isRacEnv}" = "y" -a "${isExa}" = "y" ; then
   InsCmd="${InsCmd} ('$tccversion', '$xmlversion', '$physserver', '$dbname', '$compname', '$compver', '$comphome', to_date('$tccexectime','dd-mm-yyyy HH24:MI:SS'), '$tccresult', '$chkmesg'); "
  else
   InsCmd="${InsCmd} ('$tccversion', '$xmlversion', '$server', '$dbname', '$compname', '$compver', '$comphome', to_date('$tccexectime','dd-mm-yyyy HH24:MI:SS'), '$tccresult', '$chkmesg'); "
  fi

	if test "${DBmode}" != "0" ; then
		$ORACLE_HOME/bin/sqlplus -s "/ as sysdba" <<EOS3
	     set head off feedback off verify off pagesize 0
       ${altersession}
	     ${DelCmd}
	     ${InsCmd}
	     commit;
	     exit;
EOS3

		 exit_code=$?
		 if test "$exit_code" != "0" ; then
		      printf "\nUnable to insert Technology Codelevel Checker result in the database." | tee -a $LOGFILE
		      printf "\nMake sure the database is up and connectivity exists, and re-run the Technology Codelevel Checker." | tee -a $LOGFILE
		 else
		      printf "\nStored Technology Codelevel Checker results in the database ${dbname} successfully.\n" | tee -a $LOGFILE
		 fi
	else
		printf "\n[WARNING] Database mode is ${DBmode}:\n" | tee -a $LOGFILE
		if test "${tblExists}" = "1" ; then
			printf "To store the results of DB-ETCC, open the database in READ-WRITE mode and run the following SQL statements using SQL*Plus in the order shown.\n" | tee -a $LOGFILE
    	    printf "\n1. ${DelCmd}\n" | tee -a $LOGFILE
			printf "2. ${InsCmd}\n" | tee -a $LOGFILE
    		printf "3. commit;\n" | tee -a $LOGFILE
			printf "4. exit;\n\n" | tee -a $LOGFILE
		else
			printf " Open the database in READ-WRITE mode and then rerun DB-ETCC." | tee -a $LOGFILE
			printf " Otherwise the results will not be stored in the database.\n" | tee -a $LOGFILE
		fi
	fi
else
   printf "\n[WARNING] Database not available. DB-ETCC results cannot be stored.\n" | tee -a $LOGFILE
fi
}

checkRAC() {
DBNAME=`$ORACLE_HOME/bin/sqlplus  -s "/ as sysdba" <<EOSR
    set head off feedback off verify off pagesize 0
    ${altersession}
    select SYS_CONTEXT('USERENV','DB_NAME') from dual;
    exit;
EOSR`
export DBNAME
}

checkDBmode(){
# check if DB is open in READ WRITE mode

if test "${isMultiTenant}" = "true" ; then
  dbmode=`$ORACLE_HOME/bin/sqlplus  -s "/ as sysdba" <<EOSM
  set head off feedback off verify off pagesize 0
  SELECT open_mode FROM v\\\$pdbs where name = upper('${oracle_pdb}');
  exit;
EOSM
`
else
  dbmode=`$ORACLE_HOME/bin/sqlplus  -s "/ as sysdba" <<EOSM
  set head off feedback off verify off pagesize 0
  select open_mode from v\\\$database;
  exit;
EOSM
`
fi


printf "\nDatabase ${DBNAME} is in ${dbmode} mode.\n";
if test "${dbmode}x" != "x" -a "${dbmode}" = "READ WRITE" ; then
	DBmode="1"
else
	DBmode="0"
    export DBmode
fi

}

checkECS() {

# Check for Error Correction Support
rm -f ${LOCAL_TEMP}/${TECHNAME}.ecs
if test "${tsvers}" = "11.2.0.3" -o  "${tsvers}" = "12.1.0.1" ; then
   obsolete="y"
   touch ${LOCAL_TEMP}/${TECHNAME}.ecs
   # Store part of the warning in a temp ECS file for the patch recommendation summary
   printf "\n+-----------------------------------------------------------------------------+"| tee -a $LOGFILE
   printf "\n[ERROR] ETCC cannot list required fixes:\n${hometype} ${DB_VERSION} is no longer covered by Error Correction Support
or compatible with ETCC. For ETCC to be able to provide a list of missing bug
fixes and patch mappings, upgrade to an Oracle database version listed in
My Oracle Support Document 1594274.1."
   printf "\n+-----------------------------------------------------------------------------+\n"| tee -a $LOGFILE  

   printf "  Upgrade Required\n" >> ${LOCAL_TEMP}/${TECHNAME}.ecs   
fi
# End check

}

checkBundleversion() {

bundleapplied="n"
if test "${mapvers}" = "12.1.0.2" -o "${mapvers}" = "12.2.0.1" ; then
   bundlevers="`$OPATCH lspatches|grep -i 'DATABASE BUNDLE PATCH' |${AWK} {'print $4'}`"
   if test "${bundlevers}" = ":" ; then
      bundlevers="`$OPATCH lspatches|grep -i 'DATABASE BUNDLE PATCH' |${AWK} {'print $5'}`"
   fi
   bpversion=`echo ${bundlevers} | ${AWK} -F\. '{
    t=$5;
        printf t}'`
   bpheader="BUNDLE PATCH"
   patchmapID="ProactiveBP"
   patchheader=" Proactive BP"
fi
if test "${mapvers}" = "11.2.0.4" ; then
   bundlevers="`$OPATCH lspatches|grep 'DATABASE PATCH FOR EXADATA' |${AWK} {'print $7'} | sed 's/)//g'`"
   if test "${bundlevers}" = "-" ; then
      bundlevers="`$OPATCH lspatches|grep 'DATABASE PATCH FOR EXADATA' |${AWK} {'print $8'} | sed 's/)//g'`"
   fi

   bpversion=`echo ${bundlevers} | ${AWK} -F\. '{
    t=$5;
        printf t}'`
   bpheader="EXADATA BUNDLE PATCH"
   if test "${cloud}" != "y" ; then
    patchmapID="ExadataDatabase"
   fi
   patchheader=" ExadataDatabase"
fi

if test "${bpversion}x" != "x" ; then
   bundleapplied="y"
   DOTheader="(${bpheader} ${tsvers}.${bpversion})"
   patchrelease="${tsvers}.${bpversion}${patchheader}"
   if test "${mapvers}" = "11.2.0.4" ; then
      TAGID="ES"
   fi
else
    DOTheader="(No Bundle Patch applied)"
    patchrelease="${tsvers}.0"
fi
if test "${bundleapplied}" = "y" ; then
  mapvers="${mapvers}.${bpversion}${patchmapID}"
  #printf "\nDEBUG mapvers = ${mapvers}"
  ojvmbundle="ojvm_${bpversion}"
  #printf "\nDEBUG ojvmbundle = ${ojvmbundle}"
fi   
}

checkPSUversion () {
   psuapplied="n"
   psuvers="`$OPATCH lspatches|grep 'Database Patch Set Update' |${AWK} {'print $6'}`"
   psuversion=`echo ${psuvers} | ${AWK} -F\. '{
    t=$5;
        printf t}'`
    
   if test "${psuversion}x" != "x" ; then
     psuapplied="y"
     mapvers="${mapvers}.${psuversion}"
     patchrelease=${mapvers}
     DOTheader="(PATCHSET UPDATE ${tsvers}.${psuversion})"
     TAGID="PSU"
     mosID="1147107.1"
     mosnote="Refer to MOS Doc ID \"1147107.1:Database Patch Set Update Overlay Patches Required for Use with PSUs and Oracle E-Business Suite\" to find the corresponding overlay patch that delivers the bug fix."
     ojvmbundle="ojvm_${psuversion}"
     #printf "\nDEBUG ojvmbundle = ${ojvmbundle}"
   else
     DOTheader="(No PSU applied)"
     psuapplied="n"
     patchrelease="${tsvers}.0"
   fi 
}

MapBugfixIDtoPatchID() {
	# Patch Mappings
	# To avoid Bug 21557632 - 3,000 bytes AWK limit on HP, using sed as alternative for all platforms 
	sed -n "/EBS release/,/EBS> / p" ${MAP_LIST_FILE} > ${LOCAL_TEMP}/mappings.out
	cat ${LOCAL_TEMP}/mappings.out | sed -n "/${TECHNAME}/,/<\/Technology>/p" > ${LOCAL_TEMP}/mappings0.out
	cat ${LOCAL_TEMP}/mappings0.out | sed -n -e "/\"${mapvers}\"/,/<\/Technology>/p" > ${LOCAL_TEMP}/mappings1.out
	cat ${LOCAL_TEMP}/mappings1.out | sed -n -e "/${bugfixtoapply}/,/<\/base_bugs>/p" > ${LOCAL_TEMP}/mappings2.out
	cat ${LOCAL_TEMP}/mappings2.out | sed -n -e "/${PLAT}/,/<\/platform>/p" > ${LOCAL_TEMP}/mappings3.out
	cat ${LOCAL_TEMP}/mappings3.out | sed -n 's/^.*<\(patch_id\)>\([^<]*\)<\/.*$/\2/p' |sed 's/^[ \t]*//;s/[ \t]*$//'| tr ',' '\n' > ${LOCAL_TEMP}/missingpatch.out
  cat ${LOCAL_TEMP}/mappings3.out | sed -n 's/^.*<\(notes\)>\([^<]*\)<\/.*$/\2/p' |sed 's/^[ \t]*//;s/[ \t]*$//'| tr ',' '\n' > ${LOCAL_TEMP}/missingpatchnotes.out
  cat ${LOCAL_TEMP}/mappings3.out | sed -n 's/^.*<\(file_name\)>\([^<]*\)<\/.*$/\2/p' |sed 's/^[ \t]*//;s/[ \t]*$//'| tr ',' '\n' > ${LOCAL_TEMP}/missingpatchfile.out

    mappedpatchid="`cat ${LOCAL_TEMP}/missingpatch.out`"
    mappedpatchnotes="`cat ${LOCAL_TEMP}/missingpatchnotes.out`"
    mappedpatchfilename="`cat ${LOCAL_TEMP}/missingpatchfile.out`"
    if test "${mappedpatchnotes}x" != "x" ; then
       mappedpatchnotes=" [${mappedpatchnotes}]"
    fi
    if test "${mappedpatchfilename}x" != "x" ; then
       mappedpatchfiletitle="    - Filename: ${mappedpatchfilename}"
    fi
    if test "${mappedpatchid}x" = "x" ; then
       printf "  Bugfix ${bugfixtoapply} [Warning: No patch mapping data found.]\n    - Use Doc ID ${mosID} to find the patch that delivers this bugfix.\n\n" >> ${LOCAL_TEMP}/${TECHNAME}.map
       mappedpatchid="data missing for bugfix ${bugfixtoapply}"
    else
        # Store mapped patch IDs in a temp file so we can print them as a summary
        checkforduplicate=`grep "${mappedpatchid}" ${LOCAL_TEMP}/${TECHNAME}.map`
        if test "${checkforduplicate}x" = "x" ; then
           printf "  Patch ${mappedpatchid}${mappedpatchnotes}\n${mappedpatchfiletitle}\n\n" >> ${LOCAL_TEMP}/${TECHNAME}.map
           printf "${mappedpatchfilename}\n" >> ${LOCAL_TEMP}/${TECHNAME}.files
           if test "${PLATFORM}" != "Solaris" ; then   #bug 26641323
              patchcount=`echo $(($patchcount + 1))`
           else
              patchcount=`expr $patchcount + 1`
           fi
        fi
    fi
    printf "\n  Missing Bugfix: $bugfixtoapply" | tee -a $LOGFILE
    printf "  ->  Patch ${mappedpatchid}" | tee -a $LOGFILE
    
    echo $patchcount > ${PATCHCOUNTER}
    # End Patch Mappings
}

printDBdownWarning() {

    printf "\n+-----------------------------------------------------------------------------+"| tee -a $LOGFILE
    printf "\n[WARNING] DB-ETCC: Could not connect to database, so unable to check:"| tee -a $LOGFILE
    printf "\n  - Whether database is in READ-WRITE mode. " | tee -a $LOGFILE 
    printf "\n  - Existence of table needed to store DB-ETCC results. " | tee -a $LOGFILE 
    if test "${DB_REL}" = "12" ; then
        printf "\n  - Enablement of database In-Memory option. \n    If this feature is enabled, additional fixes need to be verified."|tee -a $LOGFILE 
        IMheader="- InMemory status unknown"
    fi
    if test "${isBMCS}" = "y" -a "${isExa}" != "y" -a "isRAC" != "y" ; then
        printf "\n  - ASM storage\n    If ASM is in use, additional fixes need to be verified."|tee -a $LOGFILE 
    fi
    printf "\n\nResolve the database connectivity issue, and then rerun DB-ETCC." | tee -a $LOGFILE  
    printf "\n+-----------------------------------------------------------------------------+\n"| tee -a $LOGFILE 
}

CheckBundleMissing() {

BundleMissing=`grep "${bpheader}" ${LOCAL_TEMP}/${TECHNAME}.map`
if test "${BundleMissing}x" != "x" ; then
    MissingPatchTitle="${BundleMissing}"
    MissingPatchType="bundle patch"
    PrintConsolPatchMessage
    PrintMissingPatchWarning
else
    PrintConsolPatchMessage

fi
}

CheckPSUMissing() {

PSUMissing=`grep "PSU" ${LOCAL_TEMP}/${TECHNAME}.map`
if test "${PSUMissing}x" != "x" ; then
    MissingPatchTitle="${PSUMissing}"
    MissingPatchType="patch set"
    PrintConsolPatchMessage
    PrintMissingPatchWarning
else
    PrintConsolPatchMessage
fi
}

PrintMissingPatchWarning() {
    #printf "\n+-----------------------------------------------------------------------------+"| tee -a $LOGFILE
    printf "[WARNING]${MissingPatchTitle} is missing. "| tee -a $LOGFILE
    printf "\nThis is the recommended database ${MissingPatchType}. You should install it now"| tee -a $LOGFILE
    printf "\nand then rerun this script to check for any further fixes available."| tee -a $LOGFILE
    printf "\nAlternatively, you can remain on your existing ${MissingPatchType}, download the"| tee -a $LOGFILE
    printf "\nconsolidated zip file listed above, and apply all the other missing patches."| tee -a $LOGFILE
    printf "\n+-----------------------------------------------------------------------------+\n"| tee -a $LOGFILE

}

PrintConsolPatchMessage() {

  # Don't print Consolidated Zip availabilty if we do not have a Consolidated Patch ID or if number of missing patches if less than 2
  missingpatchcount=`cat ${PATCHCOUNTER}`
  #printf "\nDEBUG: Number of missing patches : ${missingpatchcount}\n" | tee -a $LOGFILE

  if test "${consolpatchid}x" != "x" -a "${missingpatchcount}" -gt 1 ; then
    printf "\n+-----------------------------------------------------------------------------+"| tee -a $LOGFILE
    printf "\nA consolidated zip file with the required patches for Database release"| tee -a $LOGFILE
    printf "\n${patchrelease} is available on My Oracle Support via:\n" | tee -a $LOGFILE
    printf "\n  Patch ${consolpatchid} [${patchrelease} version]" | tee -a $LOGFILE
    printf "\n    - ${consolpatchdesc}\n" | tee -a $LOGFILE
    printf "\nNote: This zip does not include any database bundle patches or PSUs." | tee -a $LOGFILE    
    if test "${cloud}" = "y" ; then
      printf "\n\nOnce this zip is downloaded and extracted, the above patches can be found under\nthe ${mapvers} directory.\n" | tee -a $LOGFILE
    fi
    printf "\n+-----------------------------------------------------------------------------+\n"| tee -a $LOGFILE
  fi

}


ListSQLPatches () {

    splist="${PTCH_TMP}/spatch_$$.lst"
    touch ${splist}
    touch ${splist}1

    printf "\n-------------------------------------------------------------------------------"| tee -a $LOGFILE
    printf "\nPATCH INFORMATION FOR ORACLE DATABASE ${DB_VERSION}"| tee -a $LOGFILE
    printf "\n-------------------------------------------------------------------------------"| tee -a $LOGFILE
    printf "\nThere are patches applied to this Database ORACLE_HOME that include SQL files:" | tee -a $LOGFILE

    #remove all blank spaces and empty lines
    sed '/^$/d' ${oplist} > ${oplist}1 
    sort ${oplist}1  | uniq > ${oplist}
    for line in `cat ${oplist}`
    do
       patchnum=`echo ${line} | sed -n 's:.*ORACLE_HOME\/sqlpatch\/\(.*\)\/.*:\1:p' | cut -d"/" -f1`
       if test "${patchnum}x" != "x" ; then
          printf "\n${patchnum}" >> ${splist}1 
       fi
    done

    sort ${splist}1 | uniq > ${splist}

    for line in `cat ${splist}`
    do
      patchnum=`echo ${line}`
      if test "${patchnum}x" != "x" ; then
         sqldetails=`cat ${oplist} | grep ${patchnum}`
         printf "\n\nPatch ${patchnum}:\n---------------\n${sqldetails}" | tee -a $LOGFILE
      fi
    done
    printf "\n" | tee -a $LOGFILE
    # clean up of temp files
    rm -rf ${PTCH_TMP} 
    exit_script
}

printUsage() {
      printf "Usage : \n"
      printf "\tcheckDBpatch.sh [help] contextfile=<file> \n\n"
      printf "Valid arguments for checkDBpatch.sh:\n\n"
      printf "\thelp             : get usage information\n"
      printf "\tcontextfile      : provide database context file name\n"
      printf "\tcloud=[y/n]      : for use by Oracle Public Cloud\n"
      printf "\tbaremetal=[y/n]  : for use by Oracle Bare Metal Cloud\n";
      exit 0;
}

exit_script() {
unset PLATFORM
unset PLATFORM_LOWER
unset CMDDIR

printf "\nFinished checking fixes for Oracle ${hometype}: " | tee -a $LOGFILE
date | tee -a $LOGFILE
printf "\nLog file for this session: ${LOGFILE}\n" | tee -a $LOGFILE
printf "\n===============================================================================\n" | tee -a $LOGFILE

exit 0
}

# begin main here

prgdir=`dirname $0`
CMDDIR=${prgdir}
export CMDDIR

isPatchMap="y"
patchcount=0
execdate=`date '+%d-%m-%Y %H:%M:%S'`
TCCresult="MISSING"
TECHNAME="DB_HOME"
obsolete="n"
MissRlbkFix=""   # missing fix or fix to be rolled back
#ER 17588765
prog=`basename $0`

LOGFILE="${prgdir}/checkDBpatch_$$.log"

printf "\n +===============================================================+ " | tee -a $LOGFILE
printf "\n |    Copyright (c) 2005, 2017 Oracle and/or its affiliates.     | " | tee -a $LOGFILE
printf "\n |                     All rights reserved.                      | " | tee -a $LOGFILE
printf "\n |             Oracle E-Business Suite Release 12.2              | " | tee -a $LOGFILE
printf "\n |          Database EBS Technology Codelevel Checker            | " | tee -a $LOGFILE
printf "\n +===============================================================+ \n" | tee -a $LOGFILE

LOCAL_TEMP=$CMDDIR/temp_checkDBpatch_$$
   
  if test ! -d "$LOCAL_TEMP" ; then
     mkdir -p $LOCAL_TEMP
     exit_code=$?
     if test "$exit_code" != "0" ; then
         printf "\nUnable to create temp directory $LOCAL_TEMP.\n" | tee -a $LOGFILE
         exit $exit_code
     fi
  fi

UNAME=`uname -s`
case $UNAME in
  "HP-UX")  OSTYPE=`uname -m`
            if [ $OSTYPE = "ia64" ] ; then
              PLATFORM=HP_IA
            else
              PLATFORM=HP_UX
            fi
            ;;
  "AIX")    PLATFORM=IBM_AIX
            PLATFORM_LOWER=aix
	    ;;
  "OSF1")   PLATFORM=UNIX_Alpha
            PLATFORM_LOWER=decunix
	    ;;
  "SunOS")  OSTYPE=`uname -m`
	    if [ $OSTYPE = "sun4u" ] ; then
  	      PLATFORM=Solaris
              PLATFORM_LOWER=solaris
            elif [ $OSTYPE = "sun4us" ] ; then 
                  PLATFORM=Solaris 
                  PLATFORM_LOWER=solaris
	    elif [ $OSTYPE = "i86pc" ] ; then
		  PLATFORM=Solaris_x86-64
	  	  PLATFORM_LOWER=solaris_x86-64
	    fi
            PTYPE=`uname -p`
	    if [ $PTYPE = "sparc" ] ; then
	      PLATFORM=Solaris
              PLATFORM_LOWER=solaris
	    fi
	    unset PTYPE
	    unset OSTYPE
	    ;;
  "Linux")  OSTYPE=`uname -m` 
            if [ $OSTYPE = "x86_64" ] ; then
              PLATFORM=Linux_x64
              PLATFORM_LOWER=linux_x64
            elif [ $OSTYPE = "s390x" ] ; then
              PLATFORM=LINUX_ZSER
              PLATFORM_LOWER=linux_zser
            else 
              PLATFORM=Linux
              PLATFORM_LOWER=linux
            fi
            unset OSTYPE
            break;;
esac
unset UNAME

UNZIP=$CMDDIR/unzip/$PLATFORM/unzip

case $PLATFORM in
  "HP_UX")         if [ x${PATH} != x ] ; then
                     PATH=/usr/bin:${PATH}
                   else
                     PATH=/usr/bin
                   fi
                   export PATH
                   ORATAB_PATH=/etc
		   PLAT="HP_UX_IA64"
            	   AWK="awk"
                   ;;
  "HP_IA")         if [ x${PATH} != x ] ; then
                     PATH=/usr/bin:${PATH}
                   else
                     PATH=/usr/bin
                   fi
                   export PATH 
		   PLAT="HP_UX_IA64"
	           AWK="awk"
		   ;;
  "IBM_AIX")       if [ x${PATH} != x ] ; then
                     PATH=/usr/bin:${PATH}
                   else
                     PATH=/usr/bin
                   fi
                   export PATH
                   ORATAB_PATH=/etc
	           PLAT="IBM_AIX"
		   AWK="awk"
		   ;;
  "UNIX_Alpha")    if [ x${PATH} != x ] ; then
                     PATH=/usr/ccs/bin:${PATH}
                   else
                     PATH=/usr/bin:/usr/ccs/bin
                   fi
                   export PATH
                   ORATAB_PATH=/etc
                   ;;
  "Solaris")       if [ x${PATH} != x ] ; then
                     PATH=/usr/ccs/bin:${PATH}
                   else
                     PATH=/usr/bin:/usr/ccs/bin
                   fi
                   export PATH
                   ORATAB_PATH=/var/opt/oracle
		   PLAT="Solaris_sparc"
                   AWK="nawk"
                   ;;
  "Solaris_x86-64") if [ x${PATH} != x ] ; then
                     PATH=/usr/ccs/bin:${PATH}
                   else
                     PATH=/usr/bin:/usr/ccs/bin
                   fi
                   export PATH
                   ORATAB_PATH=/var/opt/oracle
                   PLAT="Solaris_x86-64"
                   AWK="nawk"
                   ;;

  "Linux")         if [ x${PATH} != x ] ; then
                     PATH=/usr/bin:/bin:${PATH}
                   else
                     PATH=/usr/bin:/bin
                   fi
                   export PATH
                   ORATAB_PATH=/etc
		   PLAT="Linux_32bit"
	           AWK="awk"
                   ;;
  "Linux_x64")    if [ x${PATH} != x ] ; then
                     PATH=/usr/bin:/bin:${PATH}
                   else
                     PATH=/usr/bin:/bin
                   fi
                   export PATH
                   ORATAB_PATH=/etc
		   PLAT="LINUX_X86-64"
	           AWK="awk"
                   ;;
  "LINUX_ZSER")   if [ x${PATH} != x ] ; then
                      PATH=/usr/bin:/bin:${PATH}
                    else
                      PATH=/usr/bin:/bin
                    fi
                    ORATAB_PATH=/etc
		    AWK="awk"
		    PLAT="LINUX_ZSER"
esac

#Set PATH for uzip executable
PATH=$PATH:$CMDDIR/unzip/$PLATFORM
export PATH

ctxfilesource="currently set database environment:"
contextfile="${CONTEXT_FILE}"
listdiffs="n"
hometype="Database"

for myarg in $*
do
    arg=`echo $myarg| sed 's/^-//'`
    case $arg in
        contextfile=*)
                contextfile=`echo $arg | sed 's/contextfile=//g'`
                shift
                ;;               
        cloud=*)
                cloud=`echo $arg | sed 's/cloud=//g'`
                shift
                ;;  
        baremetal=*)
                isBMCS=`echo $arg | sed 's/baremetal=//g'`
                shift
                ;;                            
        listdiffs)
                listdiffs="y"
                ;;                 
        listsqlpatches)
                listdiffs="n"
                sqlpatchesonly="y"
                isPatchMap="n"
                ;;
        help)
                printUsage
                exit 0;
                ;;                               
    esac
    CONTEXT_FILE=$contextfile
    export CONTEXT_FILE
    ctxfilesource="command line argument:"
done



# If cloud=n then we don't want to check for DBCS / ExaCS
if test "${cloud}" != "n" ; then
    checkOPC # Identify Cloud Services
fi

if test "${contextfile}x" = "x" ; then

  checkGrid

  if test "${GridHome}" != "y" ; then
    printf "\nDatabase environment not set, so context file must be specified.\n"
    printf "Enter full path to database context file: "
    read contextfile
    CONTEXT_FILE=$contextfile
    export CONTEXT_FILE
    ctxfilesource="user input:"
  fi
fi

if test "${contextfile}" != "none" ; then
  if [ ! -f $contextfile ]; then
    printf "Unable to locate context file $contextfile."
    printf "Verify location and then rerun DB-ETCC.\n"
    exit 1;
  else
    printf "\nUsing context file from $ctxfilesource" | tee -a $LOGFILE
    printf "\n${CONTEXT_FILE}" | tee -a $LOGFILE
  fi
else
  if test "${GridHome}" != "y" ; then
     printf "\nUser requested processing without reference to context file." | tee -a $LOGFILE
  fi
  printf "\nWith this option, results will not be stored in the database." | tee -a $LOGFILE

fi

if test "${cloud}" = "y" ; then
  if test "${isBMCS}" = "y" ; then # BMCS
    printf "\n\nRunning on Oracle Bare Metal Cloud Services." | tee -a $LOGFILE
    BUG_LIST_FILE="${CMDDIR}/txk_R1220_BMC_base_bugs.xml"
    MAP_LIST_FILE="${CMDDIR}/txk_R1220_BMC_mappings.xml"  
    if test "${GridHome}" = "y" ; then
      BUG_LIST_FILE="${CMDDIR}/txk_R1220_BMC_GRID_base_bugs.xml"
      MAP_LIST_FILE="${CMDDIR}/txk_R1220_BMC_GRID_mappings.xml" 
    fi
  else # DBCS
    printf "\n\nRunning on Oracle Database Cloud Service." | tee -a $LOGFILE
    BUG_LIST_FILE="${CMDDIR}/txk_R1220_OPC_base_bugs.xml"
    MAP_LIST_FILE="${CMDDIR}/txk_R1220_OPC_mappings.xml"  
    if test "${GridHome}" = "y" ; then
      BUG_LIST_FILE="${CMDDIR}/txk_R1220_OPC_GRID_base_bugs.xml"
      MAP_LIST_FILE="${CMDDIR}/txk_R1220_OPC_GRID_mappings.xml" 
    fi
  fi
else # OnPrem
  BUG_LIST_FILE="${CMDDIR}/txk_R1220_DB_base_bugs.xml"
  MAP_LIST_FILE="${CMDDIR}/txk_R1220_DB_mappings.xml" 
  if test "${GridHome}" = "y" ; then
    BUG_LIST_FILE="${CMDDIR}/txk_R1220_GRID_base_bugs.xml"
    MAP_LIST_FILE="${CMDDIR}/txk_R1220_GRID_mappings.xml" 
  fi  
fi

progver=`grep Header ${prgdir}/${prog} | grep ${prog} |  ${AWK} '{ print $4 }'`

printf "\n\nStarting Database EBS Technology Codelevel Checker, Version ${progver} \n" |tee -a $LOGFILE
date | tee -a $LOGFILE

printf "Log file for this session : ${LOGFILE}\n" | tee -a $LOGFILE

if test ! -f ${BUG_LIST_FILE} ; then
	printf "\n${BUG_LIST_FILE} does not exist. \nExtract it from patch zip file " | tee -a $LOGFILE
	printf "to this script execution directory and then rerun DB-ETCC.\n" | tee -a $LOGFILE
	exit 1
else
    xmlver=`grep Header ${BUG_LIST_FILE} | ${AWK} '{ print $4 }'`
    printf "\nBugfix XML file version: ${xmlver}" | tee -a $LOGFILE
    printf "\nThis file will be used for identifying missing bugfixes. \n"| tee -a $LOGFILE
fi


if test ! -f ${MAP_LIST_FILE} ; then
    if test "${GridHome}" = "y" ; then 
      printf "\nPatch mapping disabled for Oracle Grid Infrastructure.\n" | tee -a $LOGFILE 
    else
      printf "\nPatch mapping disabled because ${MAP_LIST_FILE} is missing.\n" | tee -a $LOGFILE  
    fi
    isPatchMap="n"
else
    mapxmlver=`grep Header ${MAP_LIST_FILE} | ${AWK} '{ print $4 }'`
    printf "\nMapping XML file version: ${mapxmlver}\n" | tee -a $LOGFILE
    printf "This file will be used for mapping bugfixes to patches.\n" | tee -a $LOGFILE
    consolpatchid=`grep consolidated_patch ${MAP_LIST_FILE} | sed 's/^.*id=[^".]*"[ ]*\([^"]*\)".*/\1/g; s/ *$//g'`
    consolpatchdesc=`grep consolidated_patch ${MAP_LIST_FILE} | sed 's/^.*name=[^".]*"[ ]*\([^"]*\)".*/\1/g; s/ *$//g'`
fi

if test "${contextfile}" != "none" ; then
    ORACLE_HOME=`grep s_db_oh ${contextfile} | sed 's/^.*s_db_oh[^>.]*>[ ]*\([^<]*\)<.*/\1/g; s/ *$//g'`
fi

if test "${ORACLE_HOME}x" = "x" ; then
    printf "\nCannot determine ORACLE_HOME from database environment."
    printf "\nEnter the ORACLE_HOME: "
    read ORACLE_HOME
fi

if test ! -d "$ORACLE_HOME" ; then
   printf "\nThe directory ${ORACLE_HOME} does not exist. \n" | tee -a $LOGFILE
   exit 1
fi

if test "${contextfile}" != "none" ; then
    racornot=`grep "s_dbCluster\"" ${contextfile} | sed 's/^.*s_dbCluster[^>.]*>[ ]*\([^<]*\)<.*/\1/g; s/ *$//g'`
    perlprg=`grep "s_adperlprg\"" ${contextfile} | sed 's/^.*s_adperlprg[^>.]*>[ ]*\([^<]*\)<.*/\1/g; s/ *$//g'`
    export perlprg

    perlpath=`grep "s_perl5lib\"" ${contextfile} | sed 's/^.*s_perl5lib[^>.]*>[ ]*\([^<]*\)<.*/\1/g; s/ *$//g'`

    PERL5LIB=$perlpath
    export PERL5LIB
else
    perlprg=${ORACLE_HOME}/perl/bin/perl
fi

if test "${racornot}x" = "x" ; then
  while true; do
      printf "\nIs this an Oracle RAC environment [y/n]: " 
      read yn
      case $yn in
          [Yy] ) isRacEnv="y"; break;;
          [Nn] ) isRacEnv="n"; break;;
          * ) echo "Answer Y or N.";;
      esac
  done
else
  if test "${racornot}" = "true" -o "${racornot}" = "TRUE"; then
    isRacEnv="y";
    printf "\nNOTE: This is a Real Application Cluster (RAC) database." | tee -a $LOGFILE
    printf "\n   - run this tool on all RAC nodes. "
  else
    isRacEnv="n";
  fi
fi

xmlfiledate=`grep Header ${BUG_LIST_FILE} | ${AWK} '{ printf "%s %s \n", $5,$6 }'`
epochxfdate=`echo $xmlfiledate | ${perlprg} -MPOSIX -pwe 's{(\d{4})/(\d{2})/(\d{2}) (\d{2}):(\d{2}):(\d{2})}{mktime($6,$5,$4,$3,$2-1,$1-1900)}e;'`

curdate=`${perlprg} -e 'print time'`

xmldays=`expr  $curdate - $epochxfdate `
xmldays=`expr $xmldays / 86400`

if test $xmldays -gt 30 ; then
   printf "\n\n[WARNING] DB-ETCC: Bugfix XML file (${BUG_LIST_FILE}) in current directory is more than 30 days old.\n " |tee -a $LOGFILE
   printf "Check if a newer version is available in patch 17537119.\n"|tee -a $LOGFILE
fi

if test "${GridHome}" != "y" ; then
  # Get DB version
  printf "\nIdentifying database release. " | tee -a $LOGFILE

  if test -f "${ORACLE_HOME}/bin/oracle" ; then
      dbver=`strings -a ${ORACLE_HOME}/bin/oracle | grep NLSRTL | ${AWK} '!f{ print $3; f=1 }'`
      DB_REL=`echo ${dbver} | cut -d"." -f1`
      DB_VERSION=`echo ${dbver} | ${AWK} -F\. '{
        if ($3 == "") t="0";
                    else t=$3;
        if ($4 == "") f="0";
                    else f=$4;
        printf "%s.%s.%s.%s",$1,$2,t,f }'`
      tsvers=$DB_VERSION
      mapvers=${DB_VERSION}
      printf "\nDatabase release set to ${DB_VERSION}.\n" | tee -a $LOGFILE
      # Patch Mappings
      rm -f ${LOCAL_TEMP}/${TECHNAME}.ecs
      # End Patch Mappings
  else
      printf "\nThe 'oracle' binary could not be found in \${ORACLE_HOME}. " | tee -a $LOGFILE
      printf "Provide the correct \$ORACLE_HOME location.\n\n" | tee -a $LOGFILE
      exit 1
  fi
fi
unset JAVA_HOME

altersession=""


# DBCS and DB 12c set MT to true
if test "${cloud}" = "y" -a "${DB_REL}" = "12" -a "${GridHome}" != "y" ; then
  isMultiTenant="true"
else
  if test "${cloud}" = "y" -a "${contextfile}" = "none" -a "${DB_REL}" = "12" ; then
    while true; do
      printf "\nIs this a Multitenant environment [y/n]: " 
      read yn
      case $yn in
          [Yy] ) isMultiTenant="true"; break;;
          [Nn] ) isMultiTenant="false"; break;;
          * ) echo "Answer Y or N.";;
      esac
    done
  fi
fi

# Multitenant supported for DBCS only
if test "${isMultiTenant}" = "true" ; then
  printf "\nMultitenant identified.\n"

  # Retrieve values from ctx file
  if test "${contextfile}" != "none" ; then
    oracle_cdb=`grep s_cdb_name ${contextfile} | sed 's/^.*s_cdb_name[^>.]*>[ ]*\([^<]*\)<.*/\1/g; s/ *$//g'`    
    oracle_pdb=`grep s_pdb_name ${contextfile} | sed 's/^.*s_pdb_name[^>.]*>[ ]*\([^<]*\)<.*/\1/g; s/ *$//g'`
  fi
 
  # Prompt for value if not set
  if test "${oracle_cdb}x" = "x" ; then
    #printf "\nCannot identify name of container database (CDB)."
    printf " - Enter the name of the container database (CDB): "
    read oracle_cdb
  else
    printf " - Container database (CDB) identified via s_cdb_name is ${oracle_cdb}\n"
  fi 

  # Prompt for value if not set
  if test "${oracle_pdb}x" = "x" ; then
    #printf "\nCannot identify name of pluggable database (PDB)."
      printf " - Enter the name of the pluggable database (PDB): "
      read oracle_pdb
  else
    printf " - Pluggable database (PDB) identified via s_pdb_name is ${oracle_pdb}\n"
  fi
  
  if test "${isRacEnv}" = "n" ; then
      ORACLE_SID=${oracle_cdb}
  fi
  altersession="alter session set container = ${oracle_pdb};"
   
fi

if test "${contextfile}" != "none" ; then
    ORACLE_SID=`grep s_instName ${contextfile} | sed 's/^.*s_instName[^>.]*>[ ]*\([^<]*\)<.*/\1/g; s/ *$//g'`

    # Multitenant supported for DBCS only
    if test "${cloud}" = "y" ; then
      isMultiTenant=`grep s_pluggable_database ${contextfile} | sed 's/^.*s_pluggable_database[^>.]*>[ ]*\([^<]*\)<.*/\1/g; s/ *$//g'`
      # convert to lowercase
      isMultiTenant=`echo "${isMultiTenant}" | sed 's/./\L&/g'`
    fi

else
  if test "${isRacEnv}" = "y" ; then
      printf "\nThis is a Real Applications Clusters environment.\n"
      if test "${isMultiTenant}" = "true" ; then
         printf "\nDB-ETCC must establish a connection to the container database."
         printf "\n - Enter ORACLE_SID of the RAC CDB Instance [eg: ${oracle_cdb}1]: "
         read ORACLE_SID
      else
         printf "\n - Enter ORACLE_SID of this RAC Instance [eg: PROD1]: "
         read ORACLE_SID
      fi
  else
    if test "${GridHome}" != "y" ; then
      if test "${ORACLE_SID}x" = "x" ; then
        printf "\nCannot determine ORACLE_SID from database environment.\n"
        printf " - Enter the ORACLE_SID: "
        read ORACLE_SID
      fi
    fi
  fi
fi

bpheader="BUNDLE PATCH"
mosID="1594274.1"
mosnote="Refer to MOS Doc ID \"1594274.1:Oracle E-Business Suite Release 12.2: Consolidated List of Patches and Technology Bug Fixes\" to find the corresponding patch that delivers the bug fix. If an overlay patch is needed for any particular patch listed, the footnote for that patch also applies to the overlay patch."


#DB up-down flag
dbup=1  # assuming DB is up
if test "${GridHome}" != "y" ; then
  checkDB
  isDBIM=""
  DBmode="1" # read write 
else
  dbup=0
fi

if [ $dbup != 0 ] ; then
    etcctable=0
    checkRAC
    checkDBmode
    if test "${contextfile}" != "none" ; then
      # Bug 21073382
      identifyAPPS
      checkForEtccTable
    fi
    
    checkInMemory
    
else
    if test "${GridHome}" != "y" ; then
       printf "\n"
       printDBdownWarning
    fi
fi

# Check for Exadata / Engineered Systems
checkExaData

# create a temp dir
   PTCH_TMP=${LOCAL_TEMP}
 
   PATCHES_TOAPPLY="${PTCH_TMP}/PAPPLY.lst"
   PATCHES_TOROLLBACK="${PTCH_TMP}/PRLBK.lst"
   PATCHES_APPLIED="${PTCH_TMP}/patchesApplied.lst"
   DIFF_PATCHES_APPLIED="${PTCH_TMP}/diffpatchesApplied.lst"
   FIXES_FROMXML="${PTCH_TMP}/patchesFromXML.lst" 
   RAC_PATCHES_TOAPPLY="${PTCH_TMP}/RACPAPPLY.lst"
   PATCHCOUNTER="${PTCH_TMP}/PATCHCOUNTER.lst"
   # BUG_LIST_FILE="${CMDDIR}/txk_R1220_DB_base_bugs.xml"

   touch ${PATCHES_TOAPPLY}
   touch ${PATCHES_TOROLLBACK}
   touch ${PATCHES_APPLIED}
   touch ${DIFF_PATCHES_APPLIED}
   touch ${FIXES_FROMXML}
   touch ${RAC_PATCHES_TOAPPLY}
   touch ${PATCHCOUNTER}

   # Check if opatch exists in the Oracle Database Home
   OPATCH=${ORACLE_HOME}/OPatch/opatch
   if test ! -f ${OPATCH} ; then
       printf "\nCould not find opatch utility in database ORACLE_HOME. \nCannot confirm bugfixes applied."| tee -a $LOGFILE
       printf "\nExiting.\n"| tee -a $LOGFILE
       # remove the temporary file with the list of patches to be applied
       if test -f ${PTCH_TMP}/patchesToApply.lst ; then
             rm -rf ${PTCH_TMP}  
       fi
       exit 1
   fi

if test "${isDBIM}" = "y" ; then
    DB_VERSION="${tsvers}_BP"
fi

if test "${cloud}" != "y" ; then

  if test "${GridHome}" != "y" ; then
    # if InMemory enabled or Exadata check for Bundle Patch
    if test "${isDBIM}" = "y" -o "${isExa}" = "y" ; then
       checkBundleversion
    else 
      # Customers on 12c may have DBBP applied, so we need to check for Bundle
      if test "${DB_REL}" = "12" ; then
        checkBundleversion

        # If no DBBP is applied, then check for PSU
        if test "${bundleapplied}" = "y" ; then
          DB_VERSION="${tsvers}_BP" # Use same bug list as InMemory & Exadata
        else  
          checkPSUversion  
        fi

      else
        # check for a PSU version
        checkPSUversion  
      fi
    fi
  fi # if not Grid

else # Cloud 
  # ExaCS check for Bundle
  if test "${isExa}" = "y" ; then
      checkBundleversion
  else # Non=EXA
    if test "${DB_REL}" = "12" ; then
      if test "${isRacEnv}" != "y" ; then
        if test "${isBMCS}" = "y" ; then
          checkBundleversion   # BMCS non-RAC is DBBP
        else
          checkPSUversion # OPC on-RAC is PSU
        fi
      else  # RAC for 12c on BMC and OPC is DBBP
        checkBundleversion
      fi
    else # 11g non-Exa or non-RAC is PSU
       checkPSUversion   
    fi 
  fi # End ExaCS check
fi # End Bundle or PSU patch checks

 
# End Bundle or PSU patch checks

if test "${TAGID}x" != "x" ; then
   DB_VERSION="${tsvers}_${TAGID}"  # <version>_PSU or <version>_ES (11g Exadata)
fi
      
# DBCS
if test "${cloud}" = "y" -a "${GridHome}" != "y" ; then
    consolpatchid=`grep consolidated_cloud_patch ${MAP_LIST_FILE} | sed 's/^.*id=[^".]*"[ ]*\([^"]*\)".*/\1/g; s/ *$//g'`
    consolpatchdesc=`grep consolidated_cloud_patch ${MAP_LIST_FILE} | sed 's/^.*name=[^".]*"[ ]*\([^"]*\)".*/\1/g; s/ *$//g'`
    
    DB_VERSION="${tsvers}_${cloudtag}"
    cloudvers="${mapvers}_${cloudtag}"  # e.g 11.2.0.4.160419_DBCS

    if test "${isRacEnv}" = "y" ; then
       DB_VERSION="${tsvers}_${cloudtag}_RAC" # e.g 11.2.0.4_DBCS_RAC
       cloudvers="${mapvers}_${cloudtag}_RAC" # e.g 11.2.0.4.160419_DBCS_RAC
    fi

    if test "${isExa}" = "y" ; then
      if test "${isBMCS}" = "y" ; then
        DB_VERSION="${tsvers}_BMCS_ECS"
        cloudvers="${mapvers}_BMCS_ECS" # e.g 11.2.0.4.160419_BMCS_ECS
      else
        DB_VERSION="${tsvers}_ECS"
        cloudvers="${mapvers}_ECS" # e.g 11.2.0.4.160419_ECS
      fi
    fi

    mapvers=$cloudvers
fi

# ASM check for BMCS
if test "${isBMCS}" = "y" -a "${isExa}" != "y" -a "isRAC" != "y" ; then
  checkASM
fi

if test "${isASMenabled}" = "y" ; then
    DB_VERSION="${DB_VERSION}_ASM"
    mapvers="${mapvers}_ASM"
fi

#
#Setting the default exit code
#
exit_code=0;


#Call the patch checking routine
#listdiffs="y" # Additional Applied Patch info 
runProcess_1
exit_script



