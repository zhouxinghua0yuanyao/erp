REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=plb \
REM dbdrv: checkfile(120.0.12010000.9=120.0.12020000.9)(120.0.12000000.1=120.0.12010000.1):~PROD:~PATH:~FILE
/* +======================================================================+ */
/* |    Copyright (c) 2005, 2015 Oracle and/or its affiliates.           | */
/* |                         All rights reserved.                         | */
/* |                           Version 12.0.0                             | */
/* +======================================================================+ */
SET VERIFY OFF
WHENEVER SQLERROR EXIT FAILURE ROLLBACK;
WHENEVER OSERROR EXIT FAILURE ROLLBACK;

CREATE OR REPLACE PACKAGE BODY GME_BATCH_ONHOLD_PVT AS
/* $Header: GMEBOHPB.pls 120.0.12020000.9 2015/05/27 03:04:19 qzeng noship $ */

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
    x_msg_data              OUT NOCOPY    VARCHAR2)
  IS
  CURSOR get_current_row(v_hold_id NUMBER)
      IS
  SELECT start_date,end_date,batch_id
    FROM gme_batch_onhold_dtls
   WHERE hold_id=v_hold_id;
  CURSOR get_last_row(v_batch_id NUMBER)
      IS
  SELECT start_date,end_date
    FROM (SELECT start_date,end_date
            FROM gme_batch_onhold_dtls
           WHERE batch_id = v_batch_id
          ORDER BY hold_id DESC)
   WHERE ROWNUM = 1;
  CURSOR cur_get_batch_status(v_batch_id NUMBER)
      IS
  SELECT batch_status,terminated_ind
    FROM gme_batch_header
   WHERE batch_id=v_batch_id;
  l_count          NUMBER(1) := 0;
  l_start_date     DATE;
  l_end_date       DATE;
  l_found          BOOLEAN := false;
  l_hold_id        NUMBER;
  l_userid         NUMBER  := NVL(fnd_global.user_id, -1);
  l_loginid        NUMBER  := NVL (fnd_global.login_id, -1);
  l_batch_id       NUMBER;
  l_batch_status   NUMBER;
  l_terminated_ind VARCHAR2(1);
  BEGIN
    IF p_init_msg_list = fnd_api.g_true THEN
      fnd_msg_pub.initialize;
    END IF;
    x_return_status := fnd_api.g_ret_sts_success;
    --Update a existing row
    IF p_hold_id IS NOT NULL THEN
      IF p_end_date IS NULL THEN
        gme_common_pvt.log_message('GME_BATCH_ONHOLD_ENDDATE_NULL');
        RAISE fnd_api.g_exc_error;
      END IF;
      OPEN get_current_row(p_hold_id);
      IF get_current_row%NOTFOUND THEN
        l_found := FALSE;
      ELSE
        FETCH get_current_row INTO l_start_date,l_end_date,l_batch_id;
        l_found := TRUE;
      END IF;
      CLOSE get_current_row;
      IF l_found THEN
        IF l_end_date IS NOT NULL THEN
          gme_common_pvt.log_message('GME_BATCH_ONHOLD_CLOSED');
          RAISE fnd_api.g_exc_error;
        END IF;
      ELSE
        gme_common_pvt.log_message('GME_BATCH_ONHOLD_NOTFOUND');
        RAISE fnd_api.g_exc_error;
      END IF;
      IF p_called_from_form = 'F' AND l_start_date > p_end_date THEN
        gme_common_pvt.log_message('GME_BATCH_ONHOLD_ENDDATE_ERR');
        RAISE fnd_api.g_exc_error;
      END IF;
      OPEN cur_get_batch_status(l_batch_id);
      FETCH cur_get_batch_status INTO l_batch_status, l_terminated_ind;
      CLOSE cur_get_batch_status;
      IF l_batch_status IN (4,-1) OR 
         (l_batch_status = 3 AND l_terminated_ind = '1' ) THEN
        gme_common_pvt.log_message('GME_BATCH_ONHOLD_STATUS_ERR');
        RAISE fnd_api.g_exc_error;
      END IF;
      UPDATE gme_batch_onhold_dtls
         SET end_date = p_end_date,
             release_comments = p_release_comment,
             last_update_date=sysdate,
             last_updated_by=l_userid,
             last_update_login=l_loginid
       WHERE hold_id=p_hold_id;
      x_hold_id := p_hold_id;
    ELSE
      IF p_hold_type IS NULL OR p_hold_type NOT IN ('S','P') THEN
        gme_common_pvt.log_message ('GME_INVALID_FIELD', 'FIELD', 'P_HOLD_TYPE');
        RAISE fnd_api.g_exc_error;
      END IF;
      IF p_hold_reason IS NULL THEN
        gme_common_pvt.log_message ('GME_INVALID_FIELD', 'FIELD', 'P_HOLD_REASON');
        RAISE fnd_api.g_exc_error;
      END IF;
      IF p_called_from_form = 'F' AND p_start_date IS NULL THEN
        gme_common_pvt.log_message('GME_BATCH_ONHOLD_STARDATE_NULL');
        RAISE fnd_api.g_exc_error;
      END IF;
      OPEN cur_get_batch_status(l_batch_id);
      FETCH cur_get_batch_status INTO l_batch_status, l_terminated_ind;
      CLOSE cur_get_batch_status;
      IF l_batch_status IN (4,-1) OR 
         (l_batch_status = 3 AND l_terminated_ind = '1' ) THEN
        gme_common_pvt.log_message('GME_BATCH_ONHOLD_STATUS_ERR');
        RAISE fnd_api.g_exc_error;
      END IF;
      l_batch_id := p_batch_id;
      OPEN get_last_row(l_batch_id);
      IF get_last_row%NOTFOUND THEN
        CLOSE get_last_row;
      ELSE
        FETCH get_last_row INTO l_start_date,l_end_date;
        IF l_start_date IS NOT NULL AND l_end_date IS NULL THEN
          CLOSE get_last_row;
          gme_common_pvt.log_message('GME_BATCH_ONHOLD_EXISTED');
          RAISE fnd_api.g_exc_error;
        ELSE
          CLOSE get_last_row;
        END IF;
      END IF;
      SELECT GME_BATCH_ONHOLD_ID_S.NEXTVAL INTO l_hold_id FROM DUAL;
      IF p_called_from_form = 'F' THEN
        INSERT INTO gme_batch_onhold_dtls
         (HOLD_ID,BATCH_ID,HOLD_TYPE,HOLD_REASON,START_DATE,HOLD_COMMENTS,LAST_UPDATE_DATE,LAST_UPDATED_BY,CREATION_DATE,CREATED_BY,LAST_UPDATE_LOGIN)
        VALUES(l_hold_id,p_batch_id,p_hold_type,p_hold_reason,p_start_date,p_hold_comment,sysdate,l_userid,sysdate,l_userid,l_loginid);
      ELSE
        INSERT INTO gme_batch_onhold_dtls
         (HOLD_ID,BATCH_ID,HOLD_TYPE,HOLD_REASON,HOLD_COMMENTS,LAST_UPDATE_DATE,LAST_UPDATED_BY,CREATION_DATE,CREATED_BY,LAST_UPDATE_LOGIN)
        VALUES(l_hold_id,p_batch_id,p_hold_type,p_hold_reason,p_hold_comment,sysdate,l_userid,sysdate,l_userid,l_loginid);
      END IF;
      x_hold_id := l_hold_id;
    END IF;
    IF p_commit = fnd_api.g_true THEN
      COMMIT;
    END IF;
  EXCEPTION
    WHEN fnd_api.g_exc_error THEN
      x_return_status := fnd_api.g_ret_sts_error;
      FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_encoded => FND_API.g_false, p_data => x_msg_data);
  END SAVE_ONHOLD;

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
      x_msg_data                 OUT NOCOPY VARCHAR2)
  IS
  CURSOR get_batch_onhold(v_batch_id NUMBER)
      IS 
  SELECT hold_id,hold_type,hold_reason,start_date,end_date,hold_comments,release_comments
    FROM (SELECT hold_id,hold_type,hold_reason,start_date,end_date,hold_comments,release_comments
            FROM gme_batch_onhold_dtls
           WHERE batch_id = v_batch_id
          ORDER BY hold_id DESC)
   WHERE ROWNUM = 1;
   x_hold_id              gme_batch_onhold_dtls.hold_id%type;
   x_hold_type            gme_batch_onhold_dtls.hold_type%type;
   x_hold_reason          gme_batch_onhold_dtls.hold_reason%type;
   x_start_date           gme_batch_onhold_dtls.start_date%type;
   x_end_date             gme_batch_onhold_dtls.end_date%type;
   x_hold_comments        gme_batch_onhold_dtls.hold_comments%type;
   x_release_comments     gme_batch_onhold_dtls.release_comments%type;
   l_found                BOOLEAN := true;
  BEGIN
    IF p_init_msg_list = fnd_api.g_true THEN
      fnd_msg_pub.initialize;
    END IF;
    x_return_status := fnd_api.g_ret_sts_success;
    OPEN get_batch_onhold(p_batch_id);
    IF get_batch_onhold%NOTFOUND THEN
      l_found := false;
    ELSE
      FETCH get_batch_onhold INTO x_hold_id,x_hold_type,x_hold_reason,x_start_date,x_end_date,x_hold_comments,x_release_comments;
      IF x_end_date IS NOT NULL THEN
        l_found := false;
      END IF;
    END IF;
    CLOSE get_batch_onhold;
    IF l_found THEN
      x_onhold_rec.batch_id := p_batch_id;
      x_onhold_rec.hold_id := x_hold_id;
      x_onhold_rec.hold_type := x_hold_type;
      x_onhold_rec.hold_reason := x_hold_reason;
      x_onhold_rec.start_date := x_start_date;
      x_onhold_rec.end_date := x_end_date;
      x_onhold_rec.hold_comments := x_hold_comments;
      x_onhold_rec.release_comments := x_release_comments;
    END IF;
  EXCEPTION
    WHEN fnd_api.g_exc_error THEN
      x_return_status := fnd_api.g_ret_sts_error;
      FND_MSG_PUB.Count_And_Get (p_count => x_msg_count, p_encoded => FND_API.g_false, p_data => x_msg_data);
  END FETCH_CURRENT_ONHOLD;

END GME_BATCH_ONHOLD_PVT;
/
COMMIT;
--show errors;
EXIT;
