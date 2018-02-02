--spool d:\del_asset.lst 
 set serveroutput on 
 declare 
 l_asset_hdr_rec FA_API_TYPES.asset_hdr_rec_type; 
 l_return_status VARCHAR2(1); 
 l_mesg_count number := 0; 
 l_mesg_len number; 
 l_mesg varchar2(4000); 
 v_book varchar2(15); 
 v_asset_id number; 
 -- 
 CURSOR c_asset IS 
 select book_type_code, asset_id 
 from zxh_BAD_ASSETS; 
 -- 
 BEGIN 
 dbms_output.enable(1000000); 
 FA_SRVR_MSG.Init_Server_Message; 
 OPEN c_asset; 
 LOOP 
 FETCH c_asset 
 INTO v_book, v_asset_id; 
 EXIT WHEN c_asset%NOTFOUND; 
 -- 
 dbms_output.put_line('Processing Asset: '||v_asset_id); 
 -- 
 -- asset header info 
 l_asset_hdr_rec.asset_id := v_asset_id; 
 l_asset_hdr_rec.book_type_code := v_book; 
 FA_DELETION_PUB.do_delete 
 (p_api_version => 1.0, 
 p_init_msg_list => FND_API.G_FALSE, 
 p_commit => FND_API.G_FALSE, 
 p_validation_level => FND_API.G_VALID_LEVEL_FULL, 
 x_return_status => l_return_status, 
 x_msg_count => l_mesg_count, 
 x_msg_data => l_mesg, 
 p_calling_fn => null, 
 px_asset_hdr_rec => l_asset_hdr_rec 
 ); 
 l_mesg_count := fnd_msg_pub.count_msg; 
 if l_mesg_count > 0 then 
 l_mesg := chr(10) || substr(fnd_msg_pub.get 
 (fnd_msg_pub.G_FIRST, 
 fnd_api.G_FALSE), 
 1, 250); 
 dbms_output.put_line(l_mesg); 
 for i in 1..(l_mesg_count - 1) loop 
 l_mesg := 
 substr(fnd_msg_pub.get 
 (fnd_msg_pub.G_NEXT, 
 fnd_api.G_FALSE), 1, 250); 
 dbms_output.put_line(l_mesg); 
 end loop; 
 fnd_msg_pub.delete_msg(); 
 end if; 
 if (l_return_status <> FND_API.G_RET_STS_SUCCESS) then 
 dbms_output.put_line('FAILURE'); 
 else 
 dbms_output.put_line('SUCCESS'); 
 dbms_output.put_line('ASSET_ID' || 
 to_char(l_asset_hdr_rec.asset_id)); 
 dbms_output.put_line('BOOK: ' || l_asset_hdr_rec.book_type_code); 
 end if; 
 END LOOP; -- c_asset. 
 CLOSE c_asset; 

 end; 

--spool off; 
