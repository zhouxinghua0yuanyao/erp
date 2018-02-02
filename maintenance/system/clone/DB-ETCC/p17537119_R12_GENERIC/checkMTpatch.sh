#!/bin/sh

# $Header: checkMTpatch.sh 120.0.12020000.32 2017/01/30 15:36:08 paholman noship $

# This script checks that the prerequisite bugfixes have been applied to 
# the application tier ORACLE_HOME, and should be run before an upgrade.
# It uses the context from the existing applications environment.
# If this is not set, it prompts for a context file.

runInventoryReport() {

# First, check if opatch exists in the Oracle Home.
OPATCH=${ORACLE_HOME}/OPatch/opatch
  if test ! -f ${OPATCH} ; then
  printf "\nThe opatch program could not be found in $tshome." | tee -a $LOGFILE
  printf "\nCannot obtain list of ${msgbugfix} for this ORACLE_HOME."| tee -a $LOGFILE
  printf "\nExiting.\n"| tee -a $LOGFILE

# Replace temporary file with the list of patches to be applied.
  if test -f ${PTCH_TMP}/patchesToApply.lst ; then
    rm -rf ${PTCH_TMP}  
  fi
    exit 1
  else
    printf "\n${doubleline}" | tee -a $LOGFILE
    printf "\n$tshead" | tee -a $LOGFILE
    printf "\n${doubleline}" | tee -a $LOGFILE
    printf "\nNow examining product $tshead." | tee -a $LOGFILE
    printf "\n${tscustomnote}" | tee -a $LOGFILE

    ORACLE_HOME=${TS_HOME}
    if test ! -f ${ORACLE_HOME}/oraInst.loc ; then
       printf "\nCannot open the file: ${ORACLE_HOME}/oraInst.loc"| tee -a $LOGFILE
       exit_code="100"
    else
       # Use -invPtrLoc to cover both local and global inventories.
       ${TS_HOME}/OPatch/opatch lsinventory -invPtrLoc ${TS_HOME}/oraInst.loc -detail > ${invreport}
       exit_code=$?
    fi
    if test "$exit_code" != "0" ; then
        printf "\nCannot confirm bugfixes applied to ${ORACLE_HOME}\n"| tee -a $LOGFILE
        printf "\nInventory could not be checked. Error code: ${exit_code}.\n"| tee -a $LOGFILE
        rm -rf ${LOCAL_TEMP}
        printf "\nExiting.\n"| tee -a $LOGFILE
        exit 1
    fi
   fi
}

runSmartUpdate () {
    # Record current directory. 
    oldpwd="`pwd`" 
    # Change directory to run bsu.
    cd ${fmwhome}/utils/bsu
    bsucmd="./bsu.sh -view -status=applied -prod_dir=${fmwhome}/wlserver_10.3"
    printf "\n${doubleline}" | tee -a $LOGFILE
#   printf "\nExamining component $tshome." | tee -a $LOGFILE
    printf "\n$tshome" | tee -a $LOGFILE
    printf "\n${doubleline}" | tee -a $LOGFILE
    printf "\nNow examining product $tshead." | tee -a $LOGFILE
    $bsucmd > ${invreport}
    exit_code=$?
    if test "$exit_code" != "0" ; then
        printf "\nError running Smart Update. Error code: ${exit_code}.\n"| tee -a $oldpwd/$LOGFILE
        rm -rf ${LOCAL_TEMP}
        printf "\nExiting.\n"| tee -a $oldpwd/$LOGFILE
        exit 1
    fi
    # Change back to previous directory.
    cd ${oldpwd}
}

runIdentifyBugList () {

  checkECS
    
    #printf "\n Reading list of mandatory one-off fixes." | tee -a $LOGFILE

    # Bug 21557632 - 3,000 bytes AWK limit on HP, use sed as alternaive for HP only 
    if [ "$PLAT" = "HP_UX_IA64" ] ; then 
       sed -n "/EBS release/,/EBS> / p" ${BUG_LIST_FILE} > ${LOCAL_TEMP}/buglist_HP.out
       cat ${LOCAL_TEMP}/buglist_HP.out | sed -n "/${TECHNAME}/,/<\/Technology>/p" > ${LOCAL_TEMP}/buglist.out
       cat ${LOCAL_TEMP}/buglist.out | sed -n -e "/${tsvers}/,/<\/Technology>/p" > ${LOCAL_TEMP}/buglist0.out
       cat ${LOCAL_TEMP}/buglist0.out | sed -n -e "/${PLAT}/,/<bugs_mandatory/ p" > ${LOCAL_TEMP}/buglist1.out
       cat ${LOCAL_TEMP}/buglist1.out | sed -n 's/^.*<\(base_bugs\)>\([^<]*\)<\/.*$/\2/p' |sed 's/^[ \t]*//;s/[ \t]*$//'| tr ',' '\n' > ${LOCAL_TEMP}/newbuglist.out
       newbuglist=`cat ${LOCAL_TEMP}/newbuglist.out`
    else
       buglist=`${AWK} -F'[<|>]' '/Technology/{t=$2}/platform/{p=$2}/base_bugs/{bb=$3}/bugs_rollback/{br=$3; printf "%s!%s:%s#%s\n",t,p,bb,br}' ${BUG_LIST_FILE}  | grep ${tsvers}\" | grep ${PLAT} | grep ${TECHNAME}`
       newbuglist=`echo ${buglist}| ${AWK} -F: '{ print $2}' | ${AWK} -F# '{ print $1}'|sed 's/^[ \t]*//;s/[ \t]*$//'| tr ',' '\n'`
    fi

    #printf "\nDEBUG: Bug list is: ${buglist}" 
    #printf "\nDEBUG: New Bug list is: ${newbuglist}" 

    if test "${newbuglist}" = "None" ; then
        printf "\nThere are no mandatory one-offs for ${tshome} ${tsvers}.\n"| tee -a $LOGFILE
        TCCresult="OK"
        MissingBugFix=""
        updateResInDB

    elif test "${newbuglist}x" = "x" ; then      
      if test "${obsolete}" != "y" ; then
        printf "\nUnable to extract list of ${msgbugfix} from ${tshome} for version ${tsvers}.\n" | tee -a $LOGFILE
        printf "\nVerify XML file is correct and retry.\n" | tee -a $LOGFILE
        rm -rf ${LOCAL_TEMP}
        exit 1
      else
        # Unsupported version won't exist in XML file
        TCCresult="MISSING"
        MissingBugFix="UPGRADE REQUIRED"
        touch ${LOCAL_TEMP}/${TECHNAME}.map
        updateResInDB
        PatchRecommendationSummary
      fi
    else
    #    printf "\n Extracted list of mandatory one-off fixes." | tee -a $LOGFILE
        runPatchComparison
    fi
}

runPatchComparison() {
   printf "${msgcustomnote}" | tee -a $LOGFILE
   if test "${obsolete}" != "y" ; then
      printf "\nChecking required ${msgbugfix} for ${tshome} ${tsvers}." | tee -a $LOGFILE

        rm -f ${missingbuglist}
        touch ${missingbuglist}
        touch ${LOCAL_TEMP}/${TECHNAME}.map
        touch ${LOCAL_TEMP}/summary.txt
        mapvers=${tsvers}
        # Enable mapping for component if Patch Mapping is enabled
        TSPatchMap="${isPatchMap}"
        # if mapping enabled, check if mapping data available in mappings XML file
        if test "${isPatchMap}" = "y" ; then
          isMapData=`grep "\"${TECHNAME}\"" ${MAP_LIST_FILE} | grep "\"${mapvers}\""`
          #printf "\n DEBUG: isMapData = ${isMapData}"
          
          # Disable mapping if mapping data not found
          #if test "${TECHNAME}" != "WLS_HOME" ; then
             if test "${isMapData}x" = "x" ; then
                TSPatchMap="n"
                printf "\nPatch mapping disabled as no data available for ${TECHNAME} ${mapvers}." | tee -a $LOGFILE
             fi
          #fi

        fi
        # End check for mapping
    
        echo "${newbuglist}" | while read bugfixtoapply;
        do
          grep $bugfixtoapply ${invreport} > /dev/null 2>&1
          errorcode="$?"
           # Corner case for WLS patch 13964737 which is specific to FMW 11.1.1.9
           if test "${bugfixtoapply}" = "13964737" ; then
              # If FMW is not 11.1.1.9 then do not display as missing / required
              if test "$fmwversion" != "11.1.1.9.0" ; then
                errorcode="0"
              fi
           fi
           
           # Check if bugfixes are those specific to JDK 1.7 (bug 21778534)		   
       
           if test "${bugfixtoapply}" = "8551790" -o "${bugfixtoapply}" = "16234436" -o "${bugfixtoapply}" = "16271876" -o "${bugfixtoapply}" = "17645157" -o "${bugfixtoapply}" = "17653437" -o "${bugfixtoapply}" = "17594101" ; then
              # If JDK is not 1.7 then do not display as missing / required
              if test "${jdkvers}" != "17" ; then
                errorcode="0"
              fi
           fi
           # End JDK specific bugfix check
       
           if test "$errorcode" != "0" ; then
           
              # Two WLS BSU IDs have no Patch ID stored so replacing manually so Patch ID will be displayed
              
              if test "${bugfixtoapply}" = "32I2" ; then
                 bugfixtoapply="13642485"
              fi
              if test "${bugfixtoapply}" = "YHJK" ; then
                 bugfixtoapply="13845626"              
              fi
              
              # End of WLS BSU substitution
              
              # if Patch mapping enabled for this component then call Patch Mappings function
              if test "${TSPatchMap}" = "y" ; then
                 MapBugfixIDtoPatchID
              fi
              #  End Patch Mappings
  

              # if patch mapping disabled, then print bugfixes only
              if test "${TSPatchMap}" != "y" ; then
                 printf "\n  Missing $msgbugtitle: $bugfixtoapply" | tee -a $LOGFILE
                 printf "  ${msgbugtitle} ${bugfixtoapply}\n" >> ${LOCAL_TEMP}/${TECHNAME}.map
              #else
              # patch mapping enabled so print bugfix and patch id 
              #  printf "\n  Missing $msgbugtitle: $bugfixtoapply    -> Patch ${mappedpatchid}" | tee -a $LOGFILE
              fi
              
              if test "`cat ${missingbuglist}`x" != "x" ; then
                   commasep=","
              else
                   commasep=""
              fi
              printf "$commasep$bugfixtoapply" >> ${missingbuglist}
              
              
              if test "${TSPatchMap}" = "y" ; then
                 # Store mapped patch IDs in a temp file so we can print them as a summary
                 # We don't want to display multiple rows of the same patch in the summary
                 checkforduplicate=`grep "${mappedpatchid}" ${LOCAL_TEMP}/${TECHNAME}.map`
                 #printf "\n DEBUG: checkforduplicate=${checkforduplicate}\n"
                 if test "${checkforduplicate}x" = "x" ; then
                    printf "  Patch ${mappedpatchid}\n" >> ${LOCAL_TEMP}/${TECHNAME}.map
                 fi
              fi
           fi
        done
        missingpatch=`cat ${missingbuglist}`        
        if test "${missingpatch}x" != "x" ; then
           if test "${TECHNAME}" = "WLS_HOME" ; then
              printf "\n${msgwlsmissing}\n"| tee -a $LOGFILE
           else
              printf "\nThe above list shows missing ${msgbugfix} for ${tshome}.\n"| tee -a $LOGFILE
           fi
           
           patchesmissing="true"
           MissingBugFix=${missingpatch}
           TCCresult="MISSING"
           updateResInDB
           PatchRecommendationSummary

        else
           printf "\nAll required ${msgbugfix} are present for ${tshome}.\n"| tee -a $LOGFILE
           TCCresult="OK"
           MissingBugFix=""
           updateResInDB
           rm -f ${LOCAL_TEMP}/${TECHNAME}.map
        fi
   fi        
}

checkDB() {
    # Check database connectivity.
    # Get APPS password.
    stty -echo
    printf "\nEnter the password for the ${appsuser} user: "
    read appspass
    stty echo

    ORACLE_HOME=$TOOLS_ORACLE_HOME
    export ORACLE_HOME
    TNS_ADMIN=$TNS_ADMIN
    export TNS_ADMIN
    TWO_TASK=$twotask
    export TWO_TASK

    # Check if sqlplus exists in the Oracle Home.

    ohbinsqlplus="${ORACLE_HOME}/bin/sqlplus"

    if test ! -f ${ohbinsqlplus} ; then
       printf "\nBinary ${ohbinsqlplus} could not be found.  \nThis ${fileedition} $msgfsincomplete"| tee -a $LOGFILE
       printf "\nExiting.\n"| tee -a $LOGFILE
       # Replace the temporary file with the list of patches to be applied.
       if test -f ${PTCH_TMP}/patchesToApply.lst ; then
          rm -rf ${PTCH_TMP}  
       fi
       exit 1
    else
       printf "\nConnecting to database. "  | tee -a $LOGFILE
       dbconn=`$TOOLS_ORACLE_HOME/bin/sqlplus ${appsuser}/$appspass <<EOS
       exit
EOS
       `
       #need the : at the end else idle instance connection will return success
       dbstat=`echo $dbconn | grep "Connected to:"`
       result=$?

       if [ $result -eq 0 ]
       then
          printf "\nDatabase connection successful." | tee -a $LOGFILE
          dbup=1
       else
          printf "\nDatabase connection failed.\n" | tee -a $LOGFILE
          dbup=0
       fi
    fi

}

checkForEtccTable(){

    ORACLE_HOME=$TOOLS_ORACLE_HOME
    export ORACLE_HOME
    TNS_ADMIN=$TNS_ADMIN
    export TNS_ADMIN
    TWO_TASK=$twotask
    export TWO_TASK

# Check for existence of TXK_TCC_RESULTS table.
printf "\nChecking for ETCC results table. " | tee -a $LOGFILE
tblExists=`$TOOLS_ORACLE_HOME/bin/sqlplus -s ${appsuser}/$appspass <<EOS1
set head off feedback off verify off pagesize 0
select 1 from all_tables where table_name='TXK_TCC_RESULTS' and owner='${applsysuser}';
exit;
EOS1
`

tblExists=`echo ${tblExists} | sed 's/^$//g'`
if test "${tblExists}x" = "x" ; then
  # Table missing
  tblExists=0
fi

if test "${tblExists}" != "1" ; then
      printf "\n$msgtablenotexist" | tee -a $LOGFILE
      printf "\nCreating table." | tee -a $LOGFILE
      $TOOLS_ORACLE_HOME/bin/sqlplus -s ${applsysuser}/${appspass} <<EOS2
      set head off feedback off verify off pagesize 0
      CREATE TABLE ${applsysuser}.TXK_TCC_RESULTS
      ( tcc_version   VARCHAR2(20) NOT NULL,
        bugfix_xml_version VARCHAR2(20) NOT NULL,
        node_name  VARCHAR2(100) NOT NULL,
        database_name  VARCHAR2(64) NOT NULL,
        component_name  VARCHAR2(10) NOT NULL,
        component_version VARCHAR2(20) NOT NULL,
        component_home   VARCHAR2(600),
        check_date      DATE,
        check_result  VARCHAR2(10) NOT NULL,
        check_message VARCHAR2(4000)
      );
      GRANT select on ${applsysuser}.TXK_TCC_RESULTS to ${appsuser};
    exit;
EOS2
    exit_code=$?
     if test "$exit_code" != "0" ; then
        printf "\n$msgfailtocreatetable" | tee -a $LOGFILE
        printf "\n$msgresultsnotstored \n" | tee -a $LOGFILE
        printf "\n$msgensuredbup \n" | tee -a $LOGFILE
     else
        printf "\n$msgcreatedtable \n" | tee -a $LOGFILE
     fi
else
     printf "\n$msgtableexist \n" | tee -a $LOGFILE
fi
# Create synonym
$TOOLS_ORACLE_HOME/bin/sqlplus -s ${appsuser}/${appspass} <<EOS3
set head off feedback off verify off pagesize 0
CREATE OR REPLACE SYNONYM ${appsuser}.TXK_TCC_RESULTS FOR ${applsysuser}.TXK_TCC_RESULTS;
exit;
EOS3
}

checkADCodeLevel(){

    ORACLE_HOME=$TOOLS_ORACLE_HOME
    export ORACLE_HOME
    TNS_ADMIN=$TNS_ADMIN
    export TNS_ADMIN
    TWO_TASK=$twotask
    export TWO_TASK


# Identify AD codelevel

deltalevel=`$TOOLS_ORACLE_HOME/bin/sqlplus -s ${appsuser}/$appspass <<EOS1
set head off feedback off verify off pagesize 0
select codelevel from ad_trackable_entities where abbreviation ='ad';
exit;
EOS1
`
if test "${deltalevel}x" = "x" ; then
   printf "\nUnable to identify AD codelevel" | tee -a $LOGFILE
   deltalevel="AD.${deltalevel}"
else
   deltalevel="AD.${deltalevel}"
fi

# Only store checker results in DB if AD.C Delta 7 or higher.

sqlresult=`$TOOLS_ORACLE_HOME/bin/sqlplus -s ${appsuser}/$appspass <<EOS1
set head off feedback off verify off pagesize 0
select 'STORE' from ad_trackable_entities where (abbreviation ='ad' and substr(codelevel,1,1) = 'C' and to_number(substr(codelevel,3)) >= 7) or (abbreviation ='ad' and baseline = 'D');
exit;
EOS1
`

sqlresult=`echo ${sqlresult} | sed 's/^$//g'`

if test "${sqlresult}" != "STORE" ; then
   storeresults="false"
   printf "\nThe installed ${deltalevel} codelevel does not support storing the results in the database.\n" | tee -a $LOGFILE
else
   storeresults="true"
   printf "\nThe installed ${deltalevel} codelevel supports storing the results in the database.\n" | tee -a $LOGFILE
fi

}

updateResInDB(){
# Update the results in database.

    ORACLE_HOME=$TOOLS_ORACLE_HOME
    export ORACLE_HOME
    TNS_ADMIN=$TNS_ADMIN
    export TNS_ADMIN
    TWO_TASK=$twotask
    export TWO_TASK

InsCmd="INSERT INTO ${applsysuser}.TXK_TCC_RESULTS (tcc_version,bugfix_xml_version,node_name, database_name,component_name,component_version,component_home,check_date,check_result, check_message) values"
tccversion=$progver
tccexectime=$execdate
tccresult=$TCCresult
xmlversion=$xmlver
server=`uname -n`
server=`echo ${server} | ${AWK} -F. '{ print $1 }'`
compver=$tsvers
compname=$TECHNAME
comphome=$TS_HOME
chkmesg1=`echo ${MissingBugFix}| tr "\012" "," `
chkmesg=`echo ${chkmesg1}| sed 's/.$//'`

if test "${storeresults}" != "false" ; then
 if [ $dbup -eq 1 ] 
 then
   dbname=`$TOOLS_ORACLE_HOME/bin/sqlplus -s ${appsuser}/$appspass <<\EOSR
    set head off feedback off verify off pagesize 0
    select NAME from V$DATABASE;
    exit;
EOSR`

InsCmd="${InsCmd} ('$tccversion', '$xmlversion', '$server', '$dbname', '$compname', '$compver', '$comphome', to_date('$tccexectime','dd-mm-yyyy HH24:MI:SS'), '$tccresult', '$chkmesg'); "

DelCmd="DELETE from ${applsysuser}.TXK_TCC_RESULTS where node_name='$server' and component_name='$compname' and component_home like '$currentbase%';"
# Comment the following to avoid database update.
   $TOOLS_ORACLE_HOME/bin/sqlplus -s ${appsuser}/$appspass <<EOS3
     set head off feedback off verify off pagesize 0
     ${DelCmd}
     ${InsCmd}
     commit;
     exit;
EOS3

# Print the updates (as opposed to executing them).
#     printf "\nDEBUG: Table delete will be: ${DelCmd}" | tee -a $LOGFILE
#     printf "\nDEBUG: Table insert will be: ${InsCmd}" | tee -a $LOGFILE
#     exit_code=0
 exit_code=$?
   if test "$exit_code" != "0" ; then
      printf "$msgunabletosave" | tee -a $LOGFILE
      printf "$msgensuredbup" | tee -a $LOGFILE
   else
      printf "$msgsaved\n" | tee -a $LOGFILE
   fi
  else
   # Database not available
   printf "\n$msgdbnotavail \n" | tee -a $LOGFILE
  fi
fi
}

checkECS() {

# Check for Error Correction Support
rm -f ${LOCAL_TEMP}/${TECHNAME}.ecs
if test "${tsvers}" = "11.1.1.6.0"  ; then
   obsolete="y"
   touch ${LOCAL_TEMP}/${TECHNAME}.ecs
   # Store part of the warning in a temp ECS file for the patch recommendation summary
   printf "\n\n+-----------------------------------------------------------------------------+"| tee -a $LOGFILE
   printf "\n[WARNING] ETCC cannot list required fixes:\n${tshead} ${tsvers} is no longer 
covered by Error Correction Support or compatible with ETCC.
For ETCC to be able to provide a list of missing bug fixes and patch mappings
upgrade to an Oracle Fusion Middleware version listed in My Oracle Support
Document 1594274.1.\n"| tee -a $LOGFILE 
   printf "+-----------------------------------------------------------------------------+\n"| tee -a $LOGFILE

   printf "  Upgrade Required\n" >> ${LOCAL_TEMP}/${TECHNAME}.ecs   
fi
# End check

}

MapBugfixIDtoPatchID() {

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
       printf "  Bugfix ${bugfixtoapply} [Warning: No patch mapping data found.]\n    - Use Doc ID 1594274.1 to find the patch that delivers this bugfix.\n\n" >> ${LOCAL_TEMP}/${TECHNAME}.map
       mappedpatchid="data missing for bugfix ${bugfixtoapply}"
    else
        # Store mapped patch IDs in a temp file so we can print them as a summary
        checkforduplicate=`grep "${mappedpatchid}" ${LOCAL_TEMP}/${TECHNAME}.map`
        if test "${checkforduplicate}x" = "x" ; then
           printf "  Patch ${mappedpatchid}${mappedpatchnotes}\n${mappedpatchfiletitle}\n\n" >> ${LOCAL_TEMP}/${TECHNAME}.map
           printf "${mappedpatchfilename}\n" >> ${LOCAL_TEMP}/${TECHNAME}.files
        fi
    fi
    printf "\n  Missing ${msgbugtitle}: $bugfixtoapply" | tee -a $LOGFILE
    if test "${TECHNAME}" != "WLS_HOME" ; then
       printf "  ->  Patch ${mappedpatchid}" | tee -a $LOGFILE
    fi
    
    # End Patch Mappings
}
CheckPSUMissing() {

    if test -f ${LOCAL_TEMP}/WLS_HOME.map ; then
	#PSUMissing=`grep "PSU" ${LOCAL_TEMP}/${TECHNAME}.map`
	PSUMissing=`grep "PSU" ${LOCAL_TEMP}/WLS_HOME.map`
	if test "${PSUMissing}x" != "x" ; then
	    MissingPSUPatch="${PSUMissing}"
	    #printf "\n+-----------------------------------------------------------------------------+" | tee -a $LOGFILE
	    printf "\n[WARNING]${MissingPSUPatch} is missing. \n" | tee -a $LOGFILE
	    printf "This is the recommended minimum WLS patch set.\n" | tee -a $LOGFILE
	    printf "You should install it now, and then rerun this script to check for any"| tee -a $LOGFILE
	    printf "\nfurther fixes available.\n"| tee -a $LOGFILE
	    printf "+-----------------------------------------------------------------------------+"| tee -a $LOGFILE
	fi
    fi
}
PatchRecommendationSummary(){		   
           # Update summary temp file
           printf "\n-------------------------------------------------------------------------------">> ${LOCAL_TEMP}/summary.txt
           printf "\n${tshead} ${tsvers}" >> ${LOCAL_TEMP}/summary.txt
           printf "\n-------------------------------------------------------------------------------\n">> ${LOCAL_TEMP}/summary.txt
           if test "${TSPatchMap}" != "y" ; then
              printf "\nPatch mapping data not available for ${TECHNAME} ${mapvers}." >> ${LOCAL_TEMP}/summary.txt
              printf "\nDisplaying bugfixes instead." >> ${LOCAL_TEMP}/summary.txt
              printf "\nRefer to Doc ID 1594274.1 for the patches that provide these bugfixes.\n" >> ${LOCAL_TEMP}/summary.txt
           fi
           cat ${LOCAL_TEMP}/${TECHNAME}.map >> ${LOCAL_TEMP}/summary.txt

           if test -f ${LOCAL_TEMP}/${TECHNAME}.ecs ; then
              cat ${LOCAL_TEMP}/${TECHNAME}.ecs >> ${LOCAL_TEMP}/summary.txt
           fi
}
PrintConsolPatchMessage() {

  if test "${consolpatchid}x" != "x" ; then 
    printf "\n+-----------------------------------------------------------------------------+"| tee -a $LOGFILE
    printf "\nA consolidated zip file with the required application tier patches is"| tee -a $LOGFILE
    printf "\navailable on My Oracle Support via:\n" | tee -a $LOGFILE
    printf "\n  Patch ${consolpatchid}" | tee -a $LOGFILE
    printf "\n    - ${consolpatchdesc}\n" | tee -a $LOGFILE
    printf "\n+-----------------------------------------------------------------------------+"| tee -a $LOGFILE
  fi
}
# Main

prgdir=`dirname $0`
CMDDIR=${prgdir}
export CMDDIR
LOGDIR=`pwd`
export LOGDIR

LOGFILE="${LOGDIR}/checkMTpatch_$$.log"

isPatchMap="y"

printf "\n +===============================================================+ " | tee -a $LOGFILE
printf "\n |    Copyright (c) 2005, 2016 Oracle and/or its affiliates.     | " | tee -a $LOGFILE
printf "\n |                     All rights reserved.                      | " | tee -a $LOGFILE
printf "\n |             Oracle E-Business Suite Release 12.2              | " | tee -a $LOGFILE
printf "\n |        Applicatione Tier Technology Codelevel Checker         | " | tee -a $LOGFILE
printf "\n +===============================================================+ \n" | tee -a $LOGFILE

commasep=""
execdate=`date '+%d-%m-%Y %H:%M:%S'`
TCCresult="MISSING"
MissingBugFix=""   # A fix was found to be missing, or had to be rolled back.
#ER 17588765
prog=`basename $0`
doubleline="==============================================================================="

# Messages
promptenterctxfile="Enter full path to application tier context file: "
msgctxloc="Verify location and rerun this script."
msgctxcheck="Verify context file is valid and then rerun this script."
msgctxid1="Unable to read the value of"
msgctxid2="from the context file"
msglocate="Unable to locate"
msgresultbugsmissing="One or more products have bugfixes missing."
msgapplymissing="Apply the required bugfixes and rerun this script. "
msgfsincomplete="File system appears to be incomplete."
msgunabletosave="Unable to store results in the database."
msgensuredbup="Ensure the database is accessible, then rerun this script."
msgsaved="These results have been stored in the database."
msgdbnotavail="Database not available. Results could not be stored."
msgresultsnotstored="Results could not be stored."
msgtableexist="Table to store results already exists in database."
msgtablenotexist="Table to store results does not exist in database."
msgcreatedtable="Created table to store results."
msgfailtocreatetable="Failed to create table to store results."
msgresultallapplied="All required one-offs are confirmed as present."
msgbugfix="bugfixes"
msgcustomnote=""
msgbugtitle="Bugfix"
msgwlsmissing="The above list shows missing patches for Oracle WebLogic Server. 
If you have applied other Oracle WebLogic Server patches, they may have included the bugfixes needed. 
Contact Oracle Support if you require assistance in determining whether this is the case." 
msgmosid="See Doc ID 1594274.1 for the patches that provide the bugfixes.
Footnotes in Doc ID 1594274.1 also apply to corresponding overlay patches."
contextfile="${CONTEXT_FILE}"
ctxfilesource="currently set applications environment:"
msgsummaryheader="PATCH RECOMMENDATION SUMMARY"
msgdefaultpatchtext="The default patch recommendations to install these missing bugfixes are:"
msgmosidmissing="See Doc ID 1594274.1 for any special instructions regarding these patches.
Footnotes in Doc ID 1594274.1 also apply to corresponding overlay patches."
msgapplymissingpatches="Apply the required patches and rerun this script. "
for myarg in $*
do
    arg=`echo $myarg| sed 's/^-//'`
    case $arg in
        contextfile=*)
                contextfile=`echo $arg | sed 's/contextfile=//g'`
                shift
                ;;
    esac
    CONTEXT_FILE=$contextfile
    export CONTEXT_FILE
    ctxfilesource="command line argument:"
done

if test "${contextfile}x" = "x" ; then
    printf "\nApplications environment not set, so context file must be specified.\n"
    printf "$promptenterctxfile"
    read contextfile
    CONTEXT_FILE=$contextfile
    export CONTEXT_FILE
    ctxfilesource="user input:"
fi

if test "${contextfile}x" = "x" ; then
        printf "\n$msgctxcheck\n\n" | tee -a $LOGFILE
    exit 1;
fi

if [ ! -f $contextfile ] ; then
        printf "$msglocate context file." | tee -a $LOGFILE
        printf "\n$msgctxloc\n\n" | tee -a $LOGFILE
    exit 1;
fi


printf "\nUsing context file from $ctxfilesource" | tee -a $LOGFILE
printf "\n${CONTEXT_FILE}" | tee -a $LOGFILE

# Extract required values from context file.,

ORACLE_HOME="`grep s_tools_oh ${contextfile} | sed 's/^.*s_tools_oh[^>.]*>[ ]*\([^<]*\)<.*/\1/g; s/ *$//g'`"

if test "${ORACLE_HOME}x" = "x" ; then
        printf "\n$msgctxid1 s_tools_oh $msgctxid2" | tee -a $LOGFILE
        printf "\n$msgctxloc\n" | tee -a $LOGFILE
    exit 1;
else
    TOOLS_ORACLE_HOME=${ORACLE_HOME}
fi

MYJAVA="`grep s_adjvaprg ${contextfile} | sed 's/^.*s_adjvaprg[^>.]*>[ ]*\([^<]*\)<.*/\1/g; s/ *$//g'`"

if test "${MYJAVA}x" = "x" ; then
        printf "\n$msgctxid1 s_adjvaprg $msgctxid2" | tee -a $LOGFILE
        printf "\n$msgctxloc\n" | tee -a $LOGFILE
    exit 1;
fi

jdkvers="`${MYJAVA} -version 2>&1 | sed 's/java version "\(.*\)\.\(.*\)\..*"/\1\2/; 1q'`"

fileedition="`grep s_file_edition_type ${contextfile} | sed 's/^.*s_file_edition_type[^>.]*>[ ]*\([^<]*\)<.*/\1/g; s/ *$//g'`"

if test "${fileedition}x" = "x" ; then
        printf "\n$msgctxid1 s_file_edition_type $msgctxid2" | tee -a $LOGFILE
        printf "\n$msgctxloc\n" | tee -a $LOGFILE
    exit 1;
fi

currentbase="`grep s_current_base ${contextfile} | sed 's/^.*s_current_base[^>.]*>[ ]*\([^<]*\)<.*/\1/g; s/ *$//g'`"

if test "${currentbase}x" = "x" ; then
        printf "\n$msgctxid1 s_current_base $msgctxid2" |
tee -a $LOGFILE
        printf "\n$msgctxloc\n" | tee -a $LOGFILE
    exit 1;
fi

iasoraclehome="`grep s_weboh_oh ${contextfile} | sed 's/^.*s_weboh_oh[^>.]*>[ ]*\([^<]*\)<.*/\1/g; s/ *$//g'`"

if [ ! -d ${iasoraclehome} ] ; then
        printf "\nDirectory ${iasoraclehome} could not be found.\nThis ${fileedition} $msgfsincomplete"| tee -a $LOGFILE
        printf "\nExiting.\n"| tee -a $LOGFILE
    exit 1;
fi

fmwhome="`grep s_fmw_home ${contextfile} | sed 's/^.*s_fmw_home[^>.]*>[ ]*\([^<]*\)<.*/\1/g; s/ *$//g'`"

if test "${fmwhome}x" = "x" ; then
        printf "\n$msgctxid1 s_fmw_home $msgctxid2" | tee -a $LOGFILE
        printf "\n$msgctxloc\n" | tee -a $LOGFILE
    exit 1;
fi

TNS_ADMIN="`grep s_tools_tnsadmin ${contextfile} | sed 's/^.*s_tools_tnsadmin[^>.]*>[ ]*\([^<]*\)<.*/\1/g; s/ *$//g'`"

if test "${ORACLE_HOME}x" = "x" ; then
        printf "\n$msgctxid1 s_tools_tnsadmin $msgctxid2" | tee -a $LOGFILE
        printf "\n$msgctxloc\n" | tee -a $LOGFILE
    exit 1;
else
    TOOLS_ORACLE_HOME=${ORACLE_HOME}
fi

ebsenvfile="`grep s_appsora_file ${contextfile} | sed 's/^.*s_appsora_file[^>.]*>[ ]*\([^<]*\)<.*/\1/g; s/ *$//g'`"

if test ! -f ${ebsenvfile} ; then
        printf "\n$msglocate the environment file ${ebsenvfile}" | tee -a $LOGFILE
        printf "\nThis ${fileedition} $msgfsincomplete" | tee -a $LOGFILE
        printf "\nExiting.\n"| tee -a $LOGFILE
    exit 1;
fi

twotask="`grep s_twotask ${contextfile} | sed 's/^.*s_twotask[^>.]*>[ ]*\([^<]*\)<.*/\1/g; s/ *$//g'`"

if test "${twotask}x" = "x" ; then
        printf "\n$msgctxid1 s_twotask $msgctxid2" | tee -a $LOGFILE
        printf "\nThis ${fileedition} $msgfsincomplete" | tee -a $LOGFILE
        printf "\nExiting.\n"| tee -a $LOGFILE
    exit 1;
fi


# Retrieve APPS and APPLSYS usernames from contextfile

appsuser="`grep s_apps_user ${contextfile} | sed 's/^.*s_apps_user[^>.]*>[ ]*\([^<]*\)<.*/\1/g; s/ *$//g'`"

if test "${appsuser}x" = "x" ; then
        printf "\n$msgctxid1 s_apps_user $msgctxid2" | tee -a $LOGFILE
        printf "\n$msgctxloc\n" | tee -a $LOGFILE
        exit 1
fi

applsysuser="`grep s_applsys_user ${contextfile} | sed 's/^.*s_applsys_user[^>.]*>[ ]*\([^<]*\)<.*/\1/g; s/ *$//g'`"

if test "${applsysuser}x" = "x" ; then
        printf "\n$msgctxid1 s_applsys_user $msgctxid2" | tee -a $LOGFILE
        printf "\n$msgctxloc\n" | tee -a $LOGFILE
        exit 1
fi

  LOCAL_TEMP=$LOGDIR/temp_checkMTpatch_$$

  if test ! -d "$LOCAL_TEMP" ; then
     mkdir -p $LOCAL_TEMP
     exit_code=$?
     if test "$exit_code" != "0" ; then
         printf "\nUnable to create temp directory $LOCAL_TEMP.\n" | tee -a $LOGFILE
         exit $exit_code
     fi
  fi

# Get the list of one-offs to be applied
   BUGFIXES_APPLIED="${LOCAL_TEMP}/bugfixesApplied.lst"
   BUG_LIST_FILE="${CMDDIR}/txk_R1220_MT_base_bugs.xml"
   MAP_LIST_FILE="${CMDDIR}/txk_R1220_MT_mappings.xml"   
   FORMS_INVENTORY="${LOCAL_TEMP}/FormsInv.out"
   COMMON_INVENTORY="${LOCAL_TEMP}/CommonInv.out"
   HTTP_INVENTORY="${LOCAL_TEMP}/HTTPInv.out"
   WLS_INVENTORY="${LOCAL_TEMP}/WLSInv.out"
   missingbuglist=${LOCAL_TEMP}/$$.tmp

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

progver=`grep Header ${prgdir}/${prog} | grep ${prog} |  ${AWK} '{ print $4 }'`

printf "\n\nStarting Application Tier Technology Codelevel Checker \nVersion: ${progver}.\n" | tee -a $LOGFILE
date | tee -a $LOGFILE
printf "Log file for this session: ${LOGFILE}\n" | tee -a $LOGFILE

# Set Bug Fix XML file name and verify it exists.
BUG_LIST_FILE="${CMDDIR}/txk_R1220_MT_base_bugs.xml"

if test ! -f ${BUG_LIST_FILE} ; then
        printf "\n${BUG_LIST_FILE} does not exist. \nExtract this file from patch" | tee -a $LOGFILE
        printf " zip file to this script execution directory and rerun the script.\n" | tee -a $LOGFILE
        exit 1
else
    xmlver=`grep Header ${BUG_LIST_FILE} | ${AWK} '{ print $4 }'`
    printf "\nBugfix XML file version: ${xmlver}" | tee -a $LOGFILE
    printf "\nThis file will be used for identifying missing bugfixes. \n"| tee -a $LOGFILE
fi

MAP_LIST_FILE="${CMDDIR}/txk_R1220_MT_mappings.xml"  

if test ! -f ${MAP_LIST_FILE} ; then
        printf "\n${MAP_LIST_FILE} does not exist. \nExtract this file from patch" | tee -a $LOGFILE
        printf " zip file to this script execution directory and rerun the script.\n" | tee -a $LOGFILE
        #exit 1
        isPatchMap="n"
        printf "Patch mapping not available.\n" | tee -a $LOGFILE
else
    mapxmlver=`grep Header ${MAP_LIST_FILE} | ${AWK} '{ print $4 }'`
    printf "\nMapping XML file version: ${mapxmlver}\n" | tee -a $LOGFILE
    printf "This file will be used for mapping bugfixes to patches.\n" | tee -a $LOGFILE
    consolpatchid=`grep consolidated_patch ${MAP_LIST_FILE} | sed 's/^.*id=[^".]*"[ ]*\([^"]*\)".*/\1/g; s/ *$//g'`
    consolpatchdesc=`grep consolidated_patch ${MAP_LIST_FILE} | sed 's/^.*name=[^".]*"[ ]*\([^"]*\)".*/\1/g; s/ *$//g'`

fi

xmlfiledate=`grep Header ${BUG_LIST_FILE} | ${AWK} '{ printf "%s %s \n", $5,$6 }'`
epochxfdate=`echo $xmlfiledate | ${iasoraclehome}/perl/bin/perl -MPOSIX -pwe 's{(\d{4})/(\d{2})/(\d{2}) (\d{2}):(\d{2}):(\d{2})}{mktime($6,$5,$4,$3,$2-1,$1-1900)}e;'`
    exit_code=$?
    if test "$exit_code" != "0" ; then
        printf "\nError validating Bugfix XML file version. Error code: ${exit_code}\n"| tee -a $LOGFILE
        rm -rf ${LOCAL_TEMP}
        printf "\nExiting.\n"| tee -a $LOGFILE
        exit 1
    fi

curdate=`${iasoraclehome}/perl/bin/perl -e 'print time'`

printf "\nChecking for prerequisite bugfixes in File Edition: ${fileedition}\n" | tee -a $LOGFILE

# Set database connect string.
#dbconnectstring=". ${ebsenvfile} ; $TOOLS_ORACLE_HOME/bin/sqlplus -s ${appsuser}/$appspass"

# Database up-down flag
dbup=1  # assuming datbase is up
checkDB
if [ $dbup != 0 ] ; then
    etcctable=0
    storeresults="false"
    checkADCodeLevel
    if test "${sqlresult}" = "STORE" ; then
       checkForEtccTable
    fi
fi

# Set default exit code.
#
exit_code=0;

patchesmissing="false"

# Collect inventory details and run comparison.

# Forms
tshome="Oracle Forms and Reports"
tshead=${tshome}
TECHNAME="TOOLS_HOME"
invreport="$FORMS_INVENTORY"
TS_HOME="${ORACLE_HOME}"
TOOLS_HOME="${ORACLE_HOME}"

# Check if frmcmp_batch exists in the Oracle Home.

if test ! -f ${ORACLE_HOME}/bin/frmcmp_batch ; then
   printf "\n$msglocate ${ORACLE_HOME}/bin/frmcmp_batch.  \nThis ${fileedition} $msgfsincomplete."| tee -a $LOGFILE
   printf "\nExiting.\n"| tee -a $LOGFILE
   # Remove the temporary file with the list of patches to be applied.
   if test -f ${PTCH_TMP}/patchesToApply.lst ; then
         rm -rf ${PTCH_TMP}  
   fi
   exit 1
fi

runInventoryReport
tsvers="`. ${ebsenvfile} ; ${ORACLE_HOME}/bin/frmcmp_batch help=y |grep 'Forms 10.1 (Form Compiler) Version' |${AWK} {'print $6'}`"
fmwversion=""
printf "Oracle Home = ${TS_HOME}." | tee -a $LOGFILE
printf "\nProduct version = ${tsvers}." | tee -a $LOGFILE
obsolete="n"
runIdentifyBugList

# Tools RSF
tshome="RSF within Forms"
tshead=${tshome}
TECHNAME="TOOLS_RSF"
invreport="$FORMS_INVENTORY"
TS_HOME="${TOOLS_HOME}"
tsvers="10.1.0.5.0"
obsolete="n"
runIdentifyBugList

# HTTP
tshome="FMW - Web Tier"
tshead="Oracle Fusion Middleware (FMW) - Web Tier"
TECHNAME="FMW_WEB"
invreport="$HTTP_INVENTORY"
TS_HOME="${iasoraclehome}"
runInventoryReport
tsvers="`grep 'Oracle WebTier and Utilities CD' ${invreport} |${AWK} NR==1{'print $6'}`"
# Oracle Common version would match Web tier version, so set it here.
commonvers=${tsvers}
fmwversion=${tsvers}
printf "\nOracle Home = ${TS_HOME}." | tee -a $LOGFILE
printf "\nProduct Version = ${tsvers}" | tee -a $LOGFILE
obsolete="n"
runIdentifyBugList

# Web RSF
tshome="RSF within FMW Web tier"
tshead=${tshome}
TECHNAME="WEB_RSF"
invreport="$HTTP_INVENTORY"
TS_HOME="${iasoraclehome}"
tsvers="11.1.0.7.0"
obsolete="n"
runIdentifyBugList

# FMW - oracle_common
tshome="FMW - oracle common"
tshead="Oracle Fusion Middleware (FMW) - oracle_common"
TECHNAME="FMW_COMMON"
invreport="$COMMON_INVENTORY"
TS_HOME="${fmwhome}/oracle_common"
runInventoryReport
tsvers=${commonvers}
printf "\nOracle Home = ${TS_HOME}." | tee -a $LOGFILE
printf "\nProduct Version = ${tsvers}" | tee -a $LOGFILE
obsolete="n"
runIdentifyBugList

# WLS
tshome="Oracle WebLogic Server (WLS)"
tshead="${tshome}"
TECHNAME="WLS_HOME"
invreport=$WLS_INVENTORY
TS_HOME="${fmwhome}/wlserver_10.3"
msgcustomnote="\n\nNote that for Oracle WebLogic Server, patches rather than bugfixes are verified.\n"

# Check for presence of comps.xml.
if test ! -f ${fmwhome}/wlserver_10.3/inventory/ContentsXML/comps.xml ; then
   printf "\n$msglocate $fmwhome/wlserver_10.3/inventory/ContentsXML/comps.xml  \nThis ${fileedition} $msgfsincomplet"| tee -a $LOGFILE
   printf "\nExiting.\n"| tee -a $LOGFILE
   # Remove the temporary file with the list of patches to be applied.
   if test -f ${PTCH_TMP}/patchesToApply.lst ; then
         rm -rf ${PTCH_TMP}
   fi
   exit 1
fi
wlsvers="`${MYJAVA} -cp $fmwhome/patch_wls1036/profiles/default/sys_manifest_classpath/weblogic_patch.jar:$fmwhome/wlserver_10.3/server/lib/weblogic.jar weblogic.version |grep PSU |${AWK} {'print $3'}`"
if test "${wlsvers}x" = "x" ; then
   wlsvers="10.3.6.0"
fi
runSmartUpdate
tsvers=$wlsvers
msgbugtitle="Patch ID"
msgbugfix="patches"
printf "\n\nOracle Home = ${TS_HOME}." | tee -a $LOGFILE
printf "\nProduct Version = ${tsvers}" | tee -a $LOGFILE
obsolete="n"
runIdentifyBugList

unset PLATFORM
unset PLATFORM_LOWER
unset CMDDIR

printf "\n${doubleline}\n" | tee -a $LOGFILE
if test "${patchesmissing}" != "false" ; then

   if test "${isPatchMap}" = "y" ; then
       # Print Summary
       printf "\nGenerating Patch Recommendation Summary." | tee -a $LOGFILE
       printf "\n\n${doubleline}" | tee -a $LOGFILE
       printf "\n${msgsummaryheader}\n" | tee -a $LOGFILE
       printf "${doubleline}" | tee -a $LOGFILE
       printf "\n$msgresultbugsmissing"| tee -a $LOGFILE
       printf "\n$msgdefaultpatchtext\n" | tee -a $LOGFILE
       cat ${LOCAL_TEMP}/summary.txt | tee -a $LOGFILE
       
       printf "\n$msgapplymissingpatches\n"| tee -a $LOGFILE
       PrintConsolPatchMessage
       if test "${TECHNAME}" = "WLS_HOME" ; then
           CheckPSUMissing
       fi
       printf "\n\n$msgmosidmissing\n"| tee -a $LOGFILE
   else
       printf "\n$msgapplymissing\n"| tee -a $LOGFILE
       printf "\n$msgmosidmissing\n"| tee -a $LOGFILE
   fi
else
       printf "\n$msgresultallapplied \n"| tee -a $LOGFILE
       TCCresult="OK"
fi

printf "\nFinished checking prerequisite patches for File Edition: ${fileedition}.\n" | tee -a $LOGFILE
date | tee -a $LOGFILE
printf "\nLog file for this session: ${LOGFILE}\n" | tee -a $LOGFILE
printf "\n${doubleline}\n" | tee -a $LOGFILE
rm -rf ${LOCAL_TEMP}

exit 0

# end Main
