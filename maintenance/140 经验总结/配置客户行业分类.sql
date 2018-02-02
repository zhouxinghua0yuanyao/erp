--1,现在快码HZ_CREATED_BY_MODULES中增加一个USER_ENTERED
--2,将CUX_OM_CUST_B_TYPE注入于行业分类
DECLARE
p_init_msg_list VARCHAR2(32767);
p_code_assignment_rec APPS.HZ_CLASSIFICATION_V2PUB.CODE_ASSIGNMENT_REC_TYPE;
x_return_status VARCHAR2(32767);
x_msg_count NUMBER;
x_msg_data VARCHAR2(32767);
x_code_assignment_id NUMBER;
l_class varchar(30);

BEGIN

  fnd_global.apps_initialize('0','53463','222'); 
  
  -- put your class_category here
  l_class := 'CUX_OM_CUST_B_TYPE'; --for example here, make TEST_CLASS into industrial group
  
  p_init_msg_list := NULL;
  p_code_assignment_rec.CODE_ASSIGNMENT_ID := NULL;
  p_code_assignment_rec.OWNER_TABLE_NAME := 'HZ_CLASS_CATEGORIES';
  p_code_assignment_rec.OWNER_TABLE_ID := NULL;
  p_code_assignment_rec.OWNER_TABLE_KEY_1 := l_class;
  p_code_assignment_rec.OWNER_TABLE_KEY_2 := NULL;
  p_code_assignment_rec.OWNER_TABLE_KEY_3 := NULL;
  p_code_assignment_rec.OWNER_TABLE_KEY_4 := NULL;
  p_code_assignment_rec.OWNER_TABLE_KEY_5 := NULL;
  p_code_assignment_rec.CLASS_CATEGORY := 'CLASS_CATEGORY_GROUP';
  p_code_assignment_rec.CLASS_CODE := 'INDUSTRIAL_GROUP';
  p_code_assignment_rec.PRIMARY_FLAG := 'N';
  p_code_assignment_rec.CONTENT_SOURCE_TYPE := 'USER_ENTERED';
  p_code_assignment_rec.START_DATE_ACTIVE := trunc(sysdate);
  p_code_assignment_rec.END_DATE_ACTIVE := NULL;
  p_code_assignment_rec.STATUS := NULL;
  p_code_assignment_rec.CREATED_BY_MODULE := 'USER_ENTERED';
  p_code_assignment_rec.RANK := NULL;
  p_code_assignment_rec.APPLICATION_ID := NULL;
  p_code_assignment_rec.ACTUAL_CONTENT_SOURCE := 'USER_ENTERED';

-- Now call the stored program

-- Note: You have to use SQL Editor to edit and run this program. Therefore you cannot use bind variables to pass arguments.

  hz_classification_v2pub.create_code_assignment(p_init_msg_list,p_code_assignment_rec,x_return_status,x_msg_count,x_msg_data,x_code_assignment_id);
  commit;
-- Output the results
  dbms_output.put_line(SubStr('x_return_status = '||x_return_status,1,255));
  dbms_output.put_line(SubStr('x_code_assignment_id = '||TO_CHAR(x_code_assignment_id), 1, 255));
  dbms_output.put_line(SubStr('x_msg_count = '||TO_CHAR(x_msg_count), 1, 255));
  dbms_output.put_line(SubStr('x_msg_data = '||x_msg_data,1,255)); 
  
   IF x_msg_count > 1 THEN
      if x_msg_count > 10 then x_msg_count :=10; end if;
   FOR i IN 1..x_msg_count LOOP
     dbms_output.put_line(' Debug '||SubStr(FND_MSG_PUB.Get( p_encoded => FND_API.G_FALSE ),1,255));
   END LOOP;
 END IF; 
 
EXCEPTION
WHEN OTHERS THEN
  dbms_output.put_line(SubStr('Error '||TO_CHAR(SQLCODE)||': '||SQLERRM, 1, 255));
RAISE;
END;
/
