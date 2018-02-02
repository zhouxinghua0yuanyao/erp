
==============================================================================
RCA PAY ADJ HAS INCORRECT ACCTG BECAUSE LAST DIST ROUNDING IS EXECUTED IN MIDDLE
==============================================================================

Update   - 16674834
Product  - Oracle Payables
Release  - R12
Platform - Generic Platform
Built    - DEC-26-2014 17:26:01

==============================================================================
Description
==============================================================================

Description
    Old Behavior
         1. Payment Adjusted accounting having incorrect accounting
         in following case,
         - Single invoice have multiple adjustment.
         - Adjusted distributions are like +x, -x, +x, -x etc. Mean
         same amount added removed multiple times.
         2. For 0.0 amount payment adjustment gain/loss line are not
         getting calculated.
    New Behavior
        Payment adjusted event will get account with right entries.

==============================================================================
Bugs Fixed
==============================================================================
The following bugs are fixed by this patch:

15831147 - ERROR IN CREATE ACCOUNTING FOR A RECONCILLED PAYMENT(REFUND)
16164378 - CREATE ACCOUNTING WITH ERROR NUMBER 0
16636525 - RCA PAYMENT CLEARING ADJUSTMENT CREATING INCORRECT ROUNDING FOR BANK CHARGE
17600307 - R12.1.3: PAYMENT BASE AMOUNT IS NOT MATCH WITH ACCOUNTED AMOUNT IN XLA TABLE


