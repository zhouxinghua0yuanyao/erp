
==============================================================================
RCA: ISSUES WITH DUPLICATE PAYEE DELETION GDF: 10140168
==============================================================================

Update   - 13857555
Product  - Oracle Payments
Release  - R12
Platform - Generic Platform
Built    - MAY-22-2015 17:22:22

Instructions For Applying This Patch
==============================================================================


Apply The Patch
==============================================================================
For 12.0.X / 12.1.X / pre-upgrade patches (using adpatch), you must shut down
all Application tier services before performing the tasks in this section.
For 12.2.X patches (using adop), you can perform the tasks in this section
without shutting down the Application tier services.



1. Apply patch [required]
Apply the patch with ADOP:
  adop phase=apply patches=13857555

==============================================================================
Description
==============================================================================

Script to Remove Duplicate Payees and their references.

