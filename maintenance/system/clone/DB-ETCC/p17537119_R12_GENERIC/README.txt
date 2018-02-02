# $Header: README.txt 120.12 2017/10/03 05:37:54 paholman noship $
#
      EBS Technology Codelevel Checker (ETCC)
      =======================================

This patch provides two scripts, together referred to as ETCC, that help ensure you have the required database and middle tier bugfixes 
installed for your Oracle E-Business Suite Release 12.2 system.

The scripts are:
- checkDBpatch.sh (checkDBpatch.cmd on Windows). This is the Database EBS Technology Codelevel Checker (DB-ETCC), which determines if all the needed bugfixes exist in the specified database ORACLE_HOME.
- checkMTpatch.sh (checkMTpatch.cmd on Windows). This is the Middle Tier EBS Technology Codelevel Checker (MT-ETCC), which determines if all the needed bugfixes exist in the middle tier file system. 

Usage Scenarios
===============

- If you are upgrading to Release 12.2 with an existing Oracle Database Release 11.2.0.3, 11.2.0.4, 12.1.0.1 or 12.1.0.2, before you run Rapid Install to create the upgrade file system you must run DB-ETCC to ensure you have installed all the required one-off database bugfixes.

- If you are applying the latest AD-TXK release update packs, you must run DB-ETCC and MT-ETCC (in that order) to ensure you have applied all the required database and middle tier technology one-off bugfixes. (Note that for Oracle WebLogic Server, MT-ETCC verifies patches rather than bugfixes.)

Patch Mapping
=============

ETCC maps missing bugfixes to corresponding patches for the latest and latest but one quarterly bundles supported by Oracle E-Business Suite Release 12.2. Refer to Doc ID 1594274.1 "Section 3: Database Patches and Bug Numbers" for the Oracle database releases and bundle documentation.


Database Cloud Service & Exadata Cloud Service on Oracle Public Cloud
=====================================================================

Environments running on Database Cloud Service & Exadata Cloud Service require a specific set of database bugfixes.  When running DB-ETCC on Exadata Cloud Service, use the "cloud=y" command line option. This is not required on Database Cloud Service.

Exadata Cloud Service on Oracle Cloud Infratructure (OCI / Bare Metal Cloud
Services)
=====================================================================

Environments running on Oracle Cloud Infrastructure (OCI) also require a specific set of database bugfixes.  When running DB-ETCC on Exadata Cloud Service on OCI, use the "baremetal=y" command line option. 

Oracle Grid Infrastructure
==========================

DB-ETCC can now be run on the Grid Oracle Home to identify missing bugfixes only.
Ensure the ORACLE_HOME environment variable is set to the Grid Infrastructure home and CONTEXT_FILE is unset.

Additional Prerequisites For DB-ETCC
====================================

Ensure you have the appropriate version of the opatch utility for the Database ORACLE_HOME being validated. For example, for a Release 11.2.0.3 database the opatch utility should be Version 11.2.0.3 or above. Or, for a Release 11.2.0.4 database, opatch should be Version 11.2.0.4 or above.

In a multi-node Oracle RAC environment, you need to run DB-ETCC:
- On all non-shared ORACLE_HOMEs.
- On any one of the shared ORACLE_HOMEs.

MT-ETCC Restriction
===================

For AD-TXK codelevel Delta 6 and lower, MT-ETCC does not support storing the results in the database.

Patch Contents
==============

Unzip the patch to a directory of your choice and confirm all the following files are present:

Readme:
- README.txt

DB-ETCC files:
- checkDBpatch.sh
- checkDBpatch.cmd
- txk_R1220_DB_base_bugs.xml
- txk_R1220_DB_mappings.xml
- txk_R1220_GRID_base_bugs.xml (for Oracle Grid Infrastructure use only)
- txk_R1220_OPC_base_bugs.xml (for Database Cloud Service & Exadata Cloud Service use only)
- txk_R1220_OPC_mappings.xml (for Database Cloud Service & Exadata Cloud Service use only)
- txk_R1220_BMC_base_bugs.xml (for Oracle Cloud Infratructure use only)
- txk_R1220_BMC_mappings.xml (for Oracle Cloud Infratructure use only)

MT-ETCC files:
- checkMTpatch.sh
- checkMTpatch.cmd
- txk_R1220_MT_base_bugs.xml
- txk_R1220_MT_mappings.xml

===============================================
Using DB Technology Codelevel Checker (DB-ETCC)
===============================================

Steps:

1. If it does not already exist, create the <RDBMS_ORACLE_HOME>/appsutil/etcc directory (on UNIX) or <RDBMS_ORACLE_HOME>\appsutil\etcc directory (on Windows).
2. Copy the DB-ETCC files listed above from the unzip location to the directory created in Step 1 (overwriting any existing files).
3. From the etcc directory created in Step 1, run DB-ETCC as shown in the following examples:


  UNIX:

    Interactive mode:
      $ ./checkDBpatch.sh

    Exadata Cloud Service use on Oracle Public Cloud only:
      $ ./checkDBpatch.sh cloud=y

    Oracle Cloud Infrastructure use only:
      $ ./checkDBpatch.sh baremetal=y

    Non-interactive mode:
      $ ./checkDBpatch.sh contextfile=<full path to database context file>

  Windows:

    Interactive mode:
      C:\oracle\ORCL\11.2.0\appsutil\etcc>checkDBpatch.cmd      

    Non-interactive mode:

      C:\oracle\ORCL\11.2.0\appsutil\etcc>checkDBpatch.cmd contextfile=<full path to database context file>

Examples of DB-ETCC execution are shown below, in examples 1 and 2.

===============================================
Using MT Technology Codelevel Checker (MT-ETCC)
===============================================

Scenarios for running MT-ETCC:

- During an install or upgrade, run MT-ETCC on the run edition file system and apply any missing middle tier bugfixes to the run edition. The upgrade process will eventually require you to run fs_clone, which will synchronize the technology codelevel to the patch edition file system.

- During a patching cycle, run MT-ETCC on the patch edition file system and apply any missing middle tier bugfixes to the patch edition. After completing the patching cycle, run fs_clone to synchronize the technology codelevel to the other file system. 

Steps:

  1. Source the correct environment for the applicable scenario (install/upgrade, or online patching cycle).

  2. Change directory to the location where the ETCC files were unzipped.

  3. Run the MT-ETCC script.

        UNIX:

        To run in interactive mode:

            $ ./checkMTpatch.sh

        To run in non-interactive mode:

            $ (echo <appspass>) |./checkMTpatch.sh contextfile=<full path to middle tier context file>

        Windows:

        To run in interactive mode:

            C:\etcc>checkMTpatch.cmd

        To run in non-interactive mode:

            C:\etcc>(echo <appspass>)| checkMTpatch.cmd contextfile=<full path to middle tier context file>

  4.  Take the appropriate actions based on what the script reports about any missing bugfixes.

Examples of MT-ETCC execution are shown below, in examples 3 and 4.

Additional Requirements For MT-ETCC
===================================

  In a multi-node Oracle E-Business Suite environment, you need to run MT-ETCC:
  - On all non-shared application (middle) tier nodes.
  - On any one of the shared application (middle) tier nodes.

======================================
Usage Examples for DB-ETCC and MT-ETCC
======================================
 
    Example 1: DB Technology Codelevel Checker (DB-ETCC) - Missing bugfixes have been identified.
    Example 2: DB Technology Codelevel Checker (DB-ETCC) - All required bugfixes have been applied.
    Example 3: MT Technology Codelevel Checker (MT-ETCC) - Missing bugfixes have been identified.
    Example 4: MT Technology Codelevel Checker (MT-ETCC) - All required bugfixes have been applied.

    Note: All the bugfixes shown are examples only.


Example 1: DB Technology Codelevel Checker (DB-ETCC) - Missing bugfixes have been identified.
=============================================================================================

  $ pwd
  /u01/oracle/ORCL/11.2.0/appsutil/etcc

  $ ./checkDBpatch.sh contextfile=/u01/oracle/ORCL/11.2.0/appsutil/ORCL_dbserver1.xml

   +===============================================================+
   |    Copyright (c) 2005, 2016 Oracle and/or its affiliates.     |
   |                     All rights reserved.                      |
   |             Oracle E-Business Suite Release 12.2              |
   |          Database EBS Technology Codelevel Checker            |
   +===============================================================+

  Using context file from command line argument:
  /u01/oracle/ORCL/11.2.0/appsutil/ORCL_dbserver1.xml

  Starting Database EBS Technology Codelevel Checker, Version 120.31
  Fri Jan  8 12:48:37 GMT 2016
  Log file for this session : ./checkDBpatch_21875.log
  
  Bugfix XML file version: 120.0.12020000.28
  This file will be used for identifying missing bugfixes.

  Mapping XML file version: 120.0.12020000.1
  This file will be used for mapping bugfixes to patches.

  Identifying database release.
  Database release set to 12.1.0.2.

  Connecting to database.
  Database connection successful.

  Checking for DB-ETCC results table.
  Table to store DB-ETCC results already exists in the database.

  Checking if InMemory option is enabled.
  Obtained list of bugfixes to be applied and the list to be rolled back.
  Now checking Database ORACLE_HOME.

  The opatch utility is at the required version.

  Found patch records in the inventory.

    Missing Bugfix: 11111111  ->  Patch 12345678
    Missing Bugfix: 22222222  ->  Patch 45678912
    Missing Bugfix: 33333333  ->  Patch 98765432
    Missing Bugfix: 44444444  ->  Patch 98765432

  Generating Patch Recommendation Summary.

  ================================================================================
  PATCH RECOMMENDATION SUMMARY
  ================================================================================
  The default patch recommendations to install these missing bugfixes are:
  --------------------------------------------------------------------------------
  Oracle Database Release 12.1.0.2  (No PSU applied)
  --------------------------------------------------------------------------------
    Patch 12345678
      - Filename: p12345678_121020_Linux-x86-64.zip

    Patch 45678912
      - Filename: p45678912_121020_Generic.zip

    Patch 98765432
      - Filename: p98765432_121020_Linux-x86-64.zip

  Apply the required patches and rerun this script.

  See Doc ID 1594274.1 for any special instructions for these patches.
  Note: Footnotes in Doc ID 1594274.1 also apply to corresponding overlay patches.

  Stored Technology Codelevel Checker results in the database successfully.

  Finished prerequisite patch testing : Fri Jan  8 12:48:49 GMT 2016

  Log file for this session: ./checkDBpatch_21875.log

  ================================================================================





Example 2: DB Technology Codelevel Checker (DB-ETCC) - All required bugfixes have been applied.
===============================================================================================

  $ pwd
  /u01/oracle/ORCL/11.2.0/appsutil/etcc

  $ ./checkDBpatch.sh contextfile=/u01/oracle/ORCL/11.2.0/appsutil/ORCL_dbserver1.xml

   +===============================================================+
   |    Copyright (c) 2005, 2016 Oracle and/or its affiliates.     |
   |                     All rights reserved.                      |
   |             Oracle E-Business Suite Release 12.2              |
   |          Database EBS Technology Codelevel Checker            |
   +===============================================================+

  Using context file from command line argument:
  /u01/oracle/ORCL/11.2.0/appsutil/ORCL_dbserver1.xml

  Starting Database EBS Technology Codelevel Checker, Version 120.31
  Fri Jan  8 12:52:39 GMT 2016
  Log file for this session : ./checkDBpatch_23740.log

  Bugfix XML file version: 120.0.12020000.28
  This file will be used for identifying missing bugfixes.

  Mapping XML file version: 120.0.12020000.1
  This file will be used for mapping bugfixes to patches.

  Identifying database release.
  Database release set to 12.1.0.2.

  Connecting to database.
  Database connection successful.

  Checking for DB-ETCC results table.
  Table to store DB-ETCC results already exists in the database.

  Checking if InMemory option is enabled.
  Obtained list of bugfixes to be applied and the list to be rolled back.
  Now checking Database ORACLE_HOME.

  The opatch utility is at the required version.

  Found patch records in the inventory.

  All the required one-off bugfixes are present in Database ORACLE_HOME.

  Stored Technology Codelevel Checker results in the database successfully.

  Finished prerequisite patch testing : Fri Jan  8 12:52:50 GMT 2016

  Log file for this session: ./checkDBpatch_23740.log

  ================================================================================




Example 3: MT Technology Codelevel Checker (MT-ETCC) - Missing bugfixes have been identified.
=============================================================================================

This example shows AD.C.5 where this codelevel does not support storing results in the database.

  $ pwd
  /u01/oracle/temp

  $ ./checkMTpatch.sh

   +===============================================================+
   |    Copyright (c) 2005, 2016 Oracle and/or its affiliates.     |
   |                     All rights reserved.                      |
   |             Oracle E-Business Suite Release 12.2              |
   |           Middle Tier Technology Codelevel Checker            |
   +===============================================================+

  Using context file from currently set applications environment:
  /u01/oracle/VIS/fs2/inst/apps/VIS_myhost/appl/admin/VIS_myhost.xml

  Starting Middle Tier Technology Codelevel Checker, Version: 120.0.12020000.25.
  Fri Jan  8 13:07:40 GMT 2016
  Log file for this session: /u01/etcc/checkMTpatch_29713.log

  Bugfix XML file version: 120.0.12020000.21
  This file will be used for identifying missing bugfixes.

  Mapping XML file version: 120.0.12020000.1
  This file will be used for mapping bugfixes to patches.

  Checking for prerequisite bugfixes in File Edition: run


  Checking for prerequisite bugfixes in File Edition: run

  Enter the password for the APPS user:
  Connecting to database.
  Database connection successful.
  The installed AD.C.5 codelevel does not support storing the results in the database.

  ================================================================================
  Oracle Forms and Reports
  ================================================================================
  Now examining product Oracle Forms and Reports.

  Oracle Home = /u01/oracle/VIS/fs2/EBSapps/10.1.2.
  Product version = 10.1.2.3.0.
  Checking required bugfixes for Oracle Forms and Reports 10.1.2.3.0.
    Missing Bugfix: 1234567  ->  Patch 55566677
  The above list shows missing bugfixes for Oracle Forms and Reports.

  Checking required bugfixes for RSF within Forms 10.1.0.5.0.
    Missing Bugfix: 6644112  ->  Patch 55555555
  The above list shows missing bugfixes for RSF within Forms.

  ================================================================================
  Oracle Fusion Middleware (FMW) - Web Tier
  ================================================================================
  Now examining product Oracle Fusion Middleware (FMW) - Web Tier.

  Oracle Home = /u01/oracle/VIS/fs2/FMW_Home/webtier.
  Product Version = 11.1.1.7.0

  Checking required bugfixes for FMW - Web Tier 11.1.1.7.0.
    Missing Bugfix: 11223344  ->  Patch 99999999
    Missing Bugfix: 22334455  ->  Patch 99999999
    Missing Bugfix: 33445566  ->  Patch 33333333
  The above list shows missing bugfixes for FMW - Web Tier.

  Checking required bugfixes for RSF within FMW Web tier 11.1.0.7.0.
    Missing Bugfix: 22446688  ->  Patch 11111111
  The above list shows missing bugfixes for RSF within FMW Web tier.

  ================================================================================
  Oracle Fusion Middleware (FMW) - oracle_common
  ================================================================================
  Now examining product Oracle Fusion Middleware (FMW) - oracle_common.

  Oracle Home = /u01/oracle/VIS/fs2/FMW_Home/oracle_common.
  Product Version = 11.1.1.7.0

  Checking required bugfixes for FMW - oracle common 11.1.1.7.0.
    Missing Bugfix: 99887766  ->  Patch 11223344
    Missing Bugfix: 55443322  ->  Patch 33344555
  The above list shows missing bugfixes for FMW - oracle common.

  ================================================================================
  Oracle WebLogic Server (WLS)
  ================================================================================
  Now examining product Oracle WebLogic Server (WLS).

  Oracle Home = /u01/oracle/VIS/fs2/FMW_Home/wlserver_10.3.
  Product Version = 10.3.6.0.12

  Note that for Oracle WebLogic Server, patches rather than bugfixes are verified.

  Checking required patches for Oracle WebLogic Server (WLS) 10.3.6.0.12.
    Missing Patch ID: 33445566
    Missing Patch ID: 44556677
    Missing Patch ID: 55667788

  The above list shows missing patches for Oracle WebLogic Server.
  If you have applied other Oracle WebLogic Server patches, they may have included the bugfixes needed.
  Contact Oracle Support if you require assistance in determining whether this is the case.
  
  ================================================================================

  Generating Patch Recommendation Summary.

  ================================================================================
  PATCH RECOMMENDATION SUMMARY
  ================================================================================
  One or more products have bugfixes missing.
  The default patch recommendations to install these missing bugfixes are:

  --------------------------------------------------------------------------------
  Oracle Forms and Reports 10.1.2.3.0
  --------------------------------------------------------------------------------
    Patch 55566677
      - Filename: p55566677_10123_LINUX.zip


  --------------------------------------------------------------------------------
  RSF within Forms 10.1.0.5.0
  --------------------------------------------------------------------------------
    Patch 55555555
      - Filename: p55555555_10105_LINUX.zip


  --------------------------------------------------------------------------------
  Oracle Fusion Middleware (FMW) - Web Tier 11.1.1.7.0
  --------------------------------------------------------------------------------
    Patch 99999999
     - Filename: p99999999_111170_Generic.zip

    Patch 33333333
      - Filename: p33333333_111170_Generic.zip


  --------------------------------------------------------------------------------
  RSF within FMW Web tier 11.1.0.7.0
  --------------------------------------------------------------------------------
    Patch 11111111
      - Filename: p11111111_111070_Linux-x86-64.zip


  --------------------------------------------------------------------------------
  Oracle Fusion Middleware (FMW) - oracle_common 11.1.1.7.0
  --------------------------------------------------------------------------------
    Patch 11223344
      - Filename: p11223344_111160_Generic.zip
  
    Patch 33344555
      - Filename: p33344555_111160_Generic.zip


  --------------------------------------------------------------------------------
  Oracle WebLogic Server (WLS) 10.3.6.0.12
  --------------------------------------------------------------------------------
    Patch 88888888 [SU Patch [AAAA]]
      - Filename: p88888888_1036_Generic.zip

    Patch 66666666 [SU Patch [BBBB]]
      - Filename: p66666666_1036_Generic.zip

    Patch 22222222 [SU Patch [YYYY]]
      - Filename: p22222222_1036_Generic.zip


  Apply the required patches and rerun this script.

  See Doc ID 1594274.1 for any special instructions regarding these patches.
  Footnotes in Doc ID 1594274.1 also apply to corresponding overlay patches.

  Finished checking prerequisite patches for File Edition: run.
  Fri Jan  8 13:08:01 GMT 2016

  Log file for this session: /u01/etcc/checkMTpatch_29713.log

  ================================================================================


Example 4: MT Technology Codelevel Checker (MT-ETCC) - All required bugfixes have been applied.
===============================================================================================

This example shows AD.C.5 where this codelevel does not support storing results in the database

  $ pwd
  /u01/oracle/temp

  $ ./checkMTpatch.sh

   +===============================================================+
   |    Copyright (c) 2005, 2016 Oracle and/or its affiliates.     |
   |                     All rights reserved.                      |
   |             Oracle E-Business Suite Release 12.2              |
   |           Middle Tier Technology Codelevel Checker            |
   +===============================================================+

  Using context file from currently set applications environment:
  /u01/oracle/VIS/fs2/inst/apps/VIS_myhost/appl/admin/VIS_myhost.xml


  Starting Middle Tier Technology Codelevel Checker, Version: 120.0.12020000.25.
  Fri Jan  8 13:56:38 GMT 2016
  Log file for this session: /u01/etcc/checkMTpatch_4168.log

  Bugfix XML file version: 120.0.12020000.21
  This file will be used for identifying missing bugfixes.

  Mapping XML file version: 120.0.12020000.1
  This file will be used for mapping bugfixes to patches.

  Checking for prerequisite bugfixes in File Edition: run

  Enter the password for the APPS user:
  Connecting to database.
  Database connection successful.
  The installed AD.C.5 codelevel does not support storing the results in the database.

  ===============================================================================
  Oracle Forms and Reports
  ===============================================================================
  Now examining product Oracle Forms and Reports.

  Oracle Home = /u01/oracle/VIS/fs2/EBSapps/10.1.2.
  Product version = 10.1.2.3.0.
  Checking required bugfixes for Oracle Forms and Reports 10.1.2.3.0.
  All required bugfixes are present for Oracle Forms and Reports.

  Checking required bugfixes for RSF within Forms 10.1.0.5.0.
  All required bugfixes are present for RSF within Forms.

  ===============================================================================
  Oracle Fusion Middleware (FMW) - Web Tier
  ===============================================================================
  Now examining product Oracle Fusion Middleware (FMW) - Web Tier.

  Oracle Home = /u01/oracle/VIS/fs2/FMW_Home/webtier.
  Product Version = 11.1.1.7.0

  Checking required bugfixes for FMW - Web Tier 11.1.1.7.0.
  All required bugfixes are present for FMW - Web Tier.

  Checking required bugfixes for RSF within FMW Web tier 11.1.0.7.0.
  All required bugfixes are present for RSF within FMW Web tier.

  ===============================================================================
  Oracle Fusion Middleware (FMW) - oracle_common
  ===============================================================================
  Now examining product Oracle Fusion Middleware (FMW) - oracle_common.

  Oracle Home = /u01/oracle/VIS/fs2/FMW_Home/oracle_common.
  Product Version = 11.1.1.7.0

  Checking required bugfixes for FMW - oracle common 11.1.1.7.0.
  All required bugfixes are present for FMW - oracle common.

  ===============================================================================
  Oracle WebLogic Server (WLS)
  ===============================================================================
  Now examining product Oracle WebLogic Server (WLS).

  Oracle Home = /u01/oracle/VIS/fs2/FMW_Home/wlserver_10.3.
  Product Version = 10.3.6.0.12

  Note that for Oracle WebLogic Server, patches rather than bugfixes are verified.

  Checking required patches for Oracle WebLogic Server (WLS) 10.3.6.0.12.
  All required patches are present for Oracle WebLogic Server (WLS).

  ===============================================================================

  All required one-offs are confirmed as present.

  Finished checking prerequisite patches for File Edition: run.
  Fri Jan  8 13:56:58 GMT 2016

  Log file for this session: /u01/etcc/checkMTpatch_4168.log

  ===============================================================================
