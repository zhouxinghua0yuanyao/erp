REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=pls \
REM dbdrv: checkfile(120.0.12010000.5=120.0.12020000.5)(120.0.12000000.1=120.0.12010000.1):~PROD:~PATH:~FILE
/* +======================================================================+ */
/* |    Copyright (c) 2005, 2015 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;

CREATE OR REPLACE PACKAGE GME_BATCH_ONHOLD_PVT AUTHID CURRENT_USER AS
/* $Header: GMEBOHPS.pls 120.0.12020000.5 2015/05/27 03:03:20 qzeng noship $ */


 /*#
  * Save onhold for a specified batch
  * param p_api_version API version field, should be 1.0 
  * param p_init_msg_list Flag to initialize  message list or not, fnd_api.g_true or fnd_api.g_false
  * param p_commit Flag to commit db change or not, fnd_api.g_true or fnd_api.g_false
  * param p_called_from_form Flag to check is it called from form or api, fnd_api.g_true or fnd_api.g_false
  * param p_hold_id hold id, when update a hold row, then it is required
  * param p_batch_id batch id
  * param p_hold_type hold type, P: Pause, S: Stop
  * param p_hold_reason reason id
  * param p_start_date hold start date
  * param p_end_date hold_end_date
  * param p_hold_comment hold comment
  * param p_release_comment hold release comment
  * param x_hold_id generated hold_id for the new insert onhold record
  * param x_return_status  'S'-Success, 'E'-Error, 'U'-Unexpected Error
  * param x_msg_count Number of msg's on message stack
  * param x_msg_data Message list
  */
  PROCEDURE SAVE_ONHOLD (
    p_api_version           IN            NUMBER,
    p_init_msg_list         IN            VARCHAR2,
    p_commit                IN            VARCHAR2,
    p_called_from_form      IN            VARCHAR2 DEFAULT 'F',
    p_hold_id               IN            NUMBER DEFAULT NULL,
    p_batch_id              IN            NUMBER DEFAULT NULL,
    p_hold_type             IN            VARCHAR2,
    p_hold_reason           IN            VARCHAR2,
    p_start_date            IN            DATE,
    p_end_date              IN            DATE,
    p_hold_comment          IN            VARCHAR2 DEFAULT NULL,
    p_release_comment       IN            VARCHAR2 DEFAULT NULL,
    x_hold_id               OUT NOCOPY    NUMBER,
    x_return_status         OUT NOCOPY    VARCHAR2,
    x_msg_count             OUT NOCOPY    NUMBER,
    x_msg_data              OUT NOCOPY    VARCHAR2);

 /*#
  * Fetch the current in use onhold for a specified batch
  * param p_api_version API version field, should be 1.0 
  * param p_init_msg_list Flag to initialize  message list or not, fnd_api.g_true or fnd_api.g_false
  * param p_batch_id batch id
  * param x_onhold_rec onhold record
  * param x_return_status  'S'-Success, 'E'-Error, 'U'-Unexpected Error
  * param x_msg_count Number of msg's on message stack
  * param x_msg_data Message list
  */
   PROCEDURE FETCH_CURRENT_ONHOLD (
      p_api_version           IN            NUMBER,
      p_init_msg_list         IN            VARCHAR2,
      p_batch_id              IN            NUMBER DEFAULT NULL,
      x_onhold_rec            OUT NOCOPY    GME_BATCH_ONHOLD_DTLS%ROWTYPE,
      x_return_status            OUT NOCOPY VARCHAR2,
      x_msg_count                OUT NOCOPY NUMBER,
      x_msg_data                 OUT NOCOPY VARCHAR2);

END GME_BATCH_ONHOLD_PVT;
/
COMMIT;
--show errors;
EXIT;
