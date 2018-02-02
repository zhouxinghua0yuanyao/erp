REM This Sql*Script creates and starts workflows for a sales order lines whose line_is is entered as
REM parameter. To be eligible, the line must have no records in wf_items table, be open and not
REM cancelled and have an associated order header flow. The particular order type, line type and
REM OM item type combination must also have a non-upgrade and active runnable workflow process
REM associated. If any of these preconditions is not met, the line will not be picked up by the script.
REM To find out which lines meet all these criteria, you should run the script find_missing_line_wf.sql.
SET serveroutput ON size 1000000
spool create_missing_line_wf.lst
DECLARE
  l_result VARCHAR2(30);
  p_line_rec OE_Order_PUB.Line_Rec_Type;
  l_line_process_name VARCHAR2(30);
  l_item_type         VARCHAR2(30);
  l_aname wf_engine.nametabtyp;
  l_aname2 wf_engine.nametabtyp;
  l_avalue wf_engine.numtabtyp;
  l_avaluetext wf_engine.texttabtyp;
  l_process_activity NUMBER;
  line               VARCHAR2(240);
  l_org_id           NUMBER := -99;
  CURSOR items
  IS
    SELECT l.org_id,
      h.order_number,
      TO_CHAR(l.line_id) item_key,
      l.line_id,
      l.flow_status_code,
      l.open_flag,
      l.booked_flag,
      l.creation_date,
      l.line_type_id,
      h.order_type_id,
      l.item_type_code,
      l.shipped_quantity shq,
      l.line_category_code cat,
      l.fulfilled_flag,
      l.invoice_interface_status_code,
      l.cancelled_flag,
      l.ato_line_id
    FROM oe_order_lines_all l,
      oe_order_headers_all h,
      wf_items hdr_wf
    WHERE l.header_id              = h.header_id
    AND TO_CHAR(l.header_id)       = hdr_wf.item_key
    AND hdr_wf.item_type           = OE_GLOBALS.G_WFI_HDR
    AND l.line_id                  = 74406
    AND (l.open_flag               = 'Y'
    AND NVL(l.cancelled_flag, 'N') = 'N')
    AND NOT EXISTS
      (SELECT 1
      FROM wf_items itm
      WHERE itm.item_type = OE_GLOBALS.G_WFI_LIN
      AND itm.item_key    = TO_CHAR(l.line_id)
      )
  ORDER BY l.org_id,
    h.order_number,
    l.line_id;
FUNCTION Get_ProcessName(
    p_itemtype         IN VARCHAR2 ,
    p_itemkey          IN VARCHAR2 ,
    p_wfasgn_item_type IN VARCHAR2 := FND_API.G_MISS_CHAR )
  RETURN VARCHAR2
IS
  l_process_name VARCHAR2(30);
  CURSOR find_HdrProcessname(itemkey VARCHAR2)
  IS
    SELECT wf_assign.process_name
    FROM oe_workflow_assignments wf_assign,
      oe_order_headers header
    WHERE header.header_id      = to_number(itemkey)
    AND header.order_type_id    = wf_assign.order_type_id
    AND sysdate                >= wf_assign.start_date_active
    AND sysdate                <= NVL(wf_assign.end_date_active, sysdate)
    AND wf_assign.line_type_id IS NULL;
  CURSOR find_LineProcessname(itemkey VARCHAR2)
  IS
    SELECT wf_assign.process_name
    FROM oe_workflow_assignments wf_assign,
      oe_order_headers header,
      oe_order_lines line
    WHERE line.line_id                = to_number(itemkey)
    AND NVL(p_wfasgn_item_type,'-99') = NVL(wf_assign.item_type_code,NVL(p_wfasgn_item_type,'-99'))
    AND header.header_id              = line.header_id
    AND header.order_type_id          = wf_assign.order_type_id
    AND line.line_type_id             = wf_assign.line_type_id
    AND wf_assign.line_type_id       IS NOT NULL
    AND sysdate                      >= wf_assign.start_date_active
    AND sysdate                      <= NVL(wf_assign.end_date_active, sysdate)
    ORDER BY wf_assign.item_type_code;
  --
  l_debug_level CONSTANT NUMBER := oe_debug_pub.g_debug_level;
  --
BEGIN
  IF (p_itemtype = OE_GLOBALS.G_WFI_HDR) THEN
    OPEN find_HdrProcessname(p_itemkey);
    FETCH find_HdrProcessname INTO l_process_name;
    CLOSE find_HdrProcessname;
  ELSE
    OPEN find_LineProcessname(p_itemkey);
    FETCH find_LineProcessname INTO l_process_name;
    CLOSE find_LineProcessname;
  END IF;
  /*
  IF l_process_name IS NULL THEN
  RAISE NO_DATA_FOUND;
  END IF;
  */
  IF l_debug_level > 0 THEN
    oe_debug_pub.add( 'PROCESS NAME IS '||L_PROCESS_NAME ) ;
  END IF;
  IF l_debug_level > 0 THEN
    oe_debug_pub.add( 'EXITING GET_PROCESSNAME' ) ;
  END IF;
  RETURN l_process_name;
EXCEPTION
WHEN NO_DATA_FOUND THEN
  oe_debug_pub.add('Could not find root flow');
  RAISE;
WHEN OTHERS THEN
  RAISE;
END Get_ProcessName;
BEGIN
  dbms_output.put_line('Org id: order number: line id: status: booked flag: open flag: created: WF process');
  dbms_output.put_line('----------------------------------');
  FOR c IN items
  LOOP
    BEGIN
      SAVEPOINT loop_start;
      IF NVL(l_org_id, -99) <> NVL(c.org_id, -99) THEN
        l_org_id            := c.org_id;
        --fnd_client_info.set_org_context(c.org_id);
        mo_global.init('ONT');
        mo_global.set_policy_context('S', c.org_id);
      END IF;
      --FND_PROFILE.PUT( 'OE_DEBUG_LOG_DIRECTORY', '/sqlcom/out/omptmast' );
      oe_debug_pub.setdebuglevel(1);
      p_line_rec          := OE_LINE_UTIL.query_row(c.line_id);
      l_item_type         := OE_Order_Wf_Util.get_wf_item_type(p_line_rec);
      l_line_process_name := NULL;
      l_line_process_name := Get_ProcessName(OE_GLOBALS.G_WFI_LIN, p_Line_rec.line_id, l_item_type);
      dbms_output.put_line(TO_CHAR(c.org_id)||': '||TO_CHAR(c.order_number)||': '||c.item_key|| ': '||c.flow_status_code||': '||c.booked_flag||': '||c.open_flag||': '||TO_CHAR(c.creation_date)||': '||l_line_process_name||':');
      IF l_line_process_name IS NOT NULL AND l_line_process_name NOT LIKE 'UPG%' THEN
        OE_Order_Wf_Util.Set_Line_User_Key(p_line_rec);
        line := substrb(fnd_message.get, 1, 240);
        -- Create Line Work item
        WF_ENGINE.CreateProcess(OE_Globals.G_WFI_LIN,TO_CHAR(p_Line_rec.line_id), l_line_process_name, line);
        oe_debug_pub.add('After WF_ENGINE.CreateProcess');
        oe_debug_pub.add('G_ORG_ID : ' || OE_GLOBALS.G_ORG_ID);
        oe_debug_pub.add('l_org_id : ' || c.org_id);
        -- Set various Line Attributes
        l_aname(1) := 'USER_ID';
        l_avalue(1):= wf_engine.GetItemAttrNumber( OE_GLOBALS.G_WFI_HDR , TO_CHAR(p_line_rec.header_id) , 'USER_ID' );
        l_aname(2) := 'APPLICATION_ID';
        l_avalue(2):= wf_engine.GetItemAttrNumber( OE_GLOBALS.G_WFI_HDR , TO_CHAR(p_line_rec.header_id) , 'APPLICATION_ID' );
        l_aname(3) := 'RESPONSIBILITY_ID';
        l_avalue(3):= wf_engine.GetItemAttrNumber( OE_GLOBALS.G_WFI_HDR , TO_CHAR(p_line_rec.header_id) , 'RESPONSIBILITY_ID' );
        fnd_global.apps_initialize(l_avalue(1), l_avalue(3),l_avalue(2));
        l_aname(4)  := 'ORG_ID';
        l_avalue(4) := c.org_id;
        oe_debug_pub.add('Setting Item Attr Number');
        wf_engine.SetItemAttrNumberArray( OE_GLOBALS.G_WFI_LIN , p_line_rec.line_id , l_aname , l_avalue );
        l_aname2(1)     := 'LINE_CATEGORY';
        l_avaluetext(1) := p_line_rec.line_category_code;
        l_aname2(2)     := 'NOTIFICATION_APPROVER';
        l_avaluetext(2) := FND_PROFILE.VALUE_SPECIFIC('OE_NOTIFICATION_APPROVER', l_avalue(1), l_avalue(3), l_avalue(2));
        oe_debug_pub.add('user id : ' || l_avalue(1));
        oe_debug_pub.add('appl id : ' || l_avalue(2));
        oe_debug_pub.add('resp id : ' || l_avalue(3));
        oe_debug_pub.add('catgr code : ' || l_avaluetext(1));
        oe_debug_pub.add('notif approver : ' || l_avaluetext(2));
        oe_debug_pub.add('Setting Item Attr Text');
        wf_engine.SetItemAttrTextArray( OE_GLOBALS.G_WFI_LIN , p_line_rec.line_id , l_aname2 , l_avaluetext );
        oe_debug_pub.add('Setting Parrent');
        WF_ITEM.Set_Item_Parent(OE_Globals.G_WFI_LIN, TO_CHAR(p_Line_rec.line_id), OE_GLOBALS.G_WFI_HDR, TO_CHAR(p_Line_rec.header_id), '');
        oe_debug_pub.add('Starting a process');
        IF c.flow_status_code = 'AWAITING_SHIPPING' THEN
          wf_engine.handleerror('OEOL', c.item_key, 'SHIP_LINE', 'RETRY', NULL);
          BEGIN
            SELECT st.process_activity
            INTO l_process_activity
            FROM wf_item_activity_statuses st ,
              wf_process_activities wpa
            WHERE wpa.instance_id  = st.process_activity
            AND st.item_type       = 'OEOL'
            AND wpa.activity_name  = 'SHIP_LINE'
            AND st.activity_status = 'ERROR'
            AND st.item_key        = c.item_key
            AND EXISTS
              (SELECT 1
              FROM wsh_delivery_details
              WHERE source_line_id = to_number(item_key)
              AND source_code      = 'OE'
                --  and    released_status = 'C'
              AND oe_interfaced_flag = 'N'
              );
            WF_ITEM_ACTIVITY_STATUS.Create_Status ( itemtype => 'OEOL' , itemkey => c.item_key , actid => l_process_activity , status => wf_engine.eng_notified , result => wf_engine.eng_null , beginning => SYSDATE , ending => NULL );
          EXCEPTION
          WHEN No_Data_Found THEN
            NULL;
          END;
        ELSIF c.flow_status_code = 'SHIPPED' AND NVL(c.shq,0) > 0 THEN
          wf_engine.handleerror('OEOL', c.item_key, 'SHIP_LINE', 'SKIP', 'SHIP_CONFIRM');
        ELSIF c.flow_status_code IN ('AWAITING_FULFILLMENT','SHIPPED') AND c.cat = 'ORDER' AND NVL(c.shq,0) > 0 THEN
          wf_engine.handleerror('OEOL', c.item_key, 'SHIP_LINE', 'SKIP', 'SHIP_CONFIRM');
        ELSIF c.flow_status_code = 'AWAITING_FULFILLMENT' AND c.cat = 'ORDER' AND NVL(c.shq,0) = 0 THEN
          wf_engine.handleerror('OEOL', c.item_key, 'SHIP_LINE', 'SKIP', 'NON_SHIPPABLE');
        ELSIF c.flow_status_code = 'AWAITING_FULFILLMENT' AND c.cat = 'RETURN' AND c.shq IS NOT NULL THEN
          wf_engine.handleerror('OEOL', c.item_key, 'RMA_WAIT_FOR_RECEIVING', 'SKIP', 'COMPLETE');
          wf_engine.handleerror('OEOL', c.item_key, 'RMA_WAIT_FOR_INSPECTION', 'SKIP', 'COMPLETE');
        ELSIF c.flow_status_code = 'AWAITING_FULFILLMENT' AND c.cat = 'RETURN' AND c.shq IS NULL THEN
          wf_engine.handleerror('OEOL', c.item_key, 'RMA_WAIT_FOR_RECEIVING', 'SKIP', 'NOT_ELIGIBLE');
        ELSIF c.flow_status_code = 'FULFILLED' AND c.fulfilled_flag = 'Y' THEN
          wf_engine.handleerror('OEOL', c.item_key, 'FULFILL_LINE', 'SKIP', NULL);
        ELSIF c.flow_status_code = 'INVOICED' AND c.invoice_interface_status_code = 'YES' THEN
          wf_engine.handleerror('OEOL', c.item_key, 'INVOICE_INTERFACE', 'SKIP', 'COMPLETE');
        ELSIF c.flow_status_code = 'CLOSED' AND c.open_flag = 'N' THEN
          wf_engine.handleerror('OEOL', c.item_key, 'CLOSE_CONT_H', 'RETRY', NULL);
        ELSIF c.flow_status_code = 'CANCELLED' AND c.open_flag = 'N' AND c.cancelled_flag = 'Y' THEN
          wf_engine.handleerror('OEOL', c.item_key, 'CLOSE_CONT_H', 'RETRY', NULL);
        ELSIF c.open_flag = 'Y' AND NVL(c.cancelled_flag, 'N') = 'N' AND ( c.flow_status_code = 'ENTERED' OR (c.flow_status_code = 'BOOKED' AND c.booked_flag = 'Y' AND ( c.ato_line_id IS NULL OR c.ato_line_id <> c.line_id))) THEN
          wf_engine.startprocess('OEOL', c.item_key);
        END IF;
      ELSE
        dbms_output.put_line('No eligible workflow assignment found.');
      END IF; -- WF process is not null
    EXCEPTION
    WHEN OTHERS THEN
      dbms_output.put_line(sqlerrm);
      ROLLBACK TO loop_start;
    END;
  END LOOP;
  dbms_output.put_line('File name '||OE_DEBUG_PUB.G_DIR||'/'||OE_DEBUG_PUB.G_FILE);
EXCEPTION
WHEN OTHERS THEN
  oe_debug_pub.add('Exception raised');
  RAISE FND_API.G_EXC_UNEXPECTED_ERROR;
END;
/
COMMIT;
quit
