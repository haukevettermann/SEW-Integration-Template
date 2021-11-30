class /SEW/CL_TIME_CHANGES definition
  public
  create public .

public section.

  types:
    BAL_UPD_TT type table of /SEW/INT_BAL_UPD .
  types:
    RES_MAP_TT type table of /SEW/INT_RES_MAP .
  types:
    BAL_UPT_TT type table of /SEW/INT_BAL_UPD .
  types:
    PERIODS_T type table of PERIOD .
  types:
    TIME_PAY_RESULTS_T type table of /SEW/TIME_PAY_RESULTS .
  types:
    SEQNR_T type table of CDSEQ .

  data MESSAGE_HANDLER type ref to CL_HRPAY00_MESSAGE_HANDLER .
  data SKIPPED_PERNRS type HRAHQ_PERNR_TABLE .
  data DATES type RSDSSELOPT_T .
  data STATUS type RSDSSELOPT_T .
  data SIMU type BOOLEAN .
  data INT_BAL_UPD type /SEW/TT_BAL_UPD .
  data STATUS_HANDLER type ref to /SEW/CL_INT_STATUS_HANDLER .
  data MSG_CONT type ref to /IWBEP/IF_MESSAGE_CONTAINER .

  class-methods BUILD_XML
    importing
      !INT_BAL_UPD type /SEW/TT_BAL_UPD
    exporting
      !XML_STRING type XSTRING .
  class-methods CREATE_CHILD_NODE
    importing
      !NAME type STRING
      !XML_DOCUMENT type ref to IF_IXML_DOCUMENT
      !ORACLEPERNR type /SEW/DD_OBJECTNUMBER
      !MOLGA type MOLGA
      !PERNR type PERNR_D
      !INT_BAL_UPD type /SEW/INT_BAL_UPD
    returning
      value(RVAL) type ref to IF_IXML_ELEMENT .
  class-methods CREATE_PARENT_NODE
    importing
      !NAME type STRING
      !XML_DOCUMENT type ref to IF_IXML_DOCUMENT
      !ORACLEPERNR type /SEW/DD_OBJECTNUMBER
      !MOLGA type MOLGA
      !PERNR type PERNR_D
      !INT_BAL_UPD type /SEW/INT_BAL_UPD
    returning
      value(RVAL) type ref to IF_IXML_ELEMENT .
  class-methods DETERMINE_ZTART_NODE_NAME
    importing
      !ZTART type PT_ZTART
    returning
      value(NODE_NAME) type /SEW/DD_ZTART_NODENAME .
  class-methods GENERATE_SOURCE_SYSTEM_ID
    importing
      !PERNR type PERNR_D
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !ZTART type PT_ZTART
      !TABNAME type TABNAME
    returning
      value(SSID) type STRING .
  class-methods GET_CHANGE_DATE
    importing
      !PERNR type PERNR_D
      !PERIOD type CHAR6
    returning
      value(CHANGE_DATE) type DATS .
  class-methods GET_ORACLE_INBOUND_RECORD
    importing
      !ZTART type PT_ZTART
      !TABNAME type TABNAME
    returning
      value(ORACLE_ZTART) type STRING .
  class-methods GET_SSID_COMPONENT
    importing
      !ZTART type PT_ZTART
      !TABNAME type TABNAME
    returning
      value(COMP) type STRING .
  class-methods GET_ZTART_TEXT
    importing
      !ZTART type PT_ZTART
      !SPRSL type SPRSL optional
    returning
      value(ZTEXT) type TXTLZ .
  class-methods TEST_METHOD
    importing
      !PERNR type PERNR_D .
  methods COMPARE_TABLES
    importing
      !BAL_UPD_OLD type /SEW/CL_TIME_CHANGES=>BAL_UPD_TT
      !BAL_UPD_NEW type /SEW/CL_TIME_CHANGES=>BAL_UPD_TT
    exporting
      !BAL_UPD_DEL type /SEW/CL_TIME_CHANGES=>BAL_UPD_TT
      !BAL_UPD_ADD type /SEW/CL_TIME_CHANGES=>BAL_UPD_TT
      !BAL_UPD_MOD type /SEW/CL_TIME_CHANGES=>BAL_UPD_TT
      !BAL_UPD_NO_CHANGES type /SEW/CL_TIME_CHANGES=>BAL_UPD_TT
      !NO_CHANGES type FLAG .
  methods CONSTRUCTOR
    importing
      !ALV type BOOLEAN
      !BATCH type BOOLEAN
      !SIMU type BOOLEAN
      !DATES type RSDSSELOPT_T .
  methods CREATE_ENTRIES
    importing
      !INT_RES_MAP type /SEW/CL_TIME_CHANGES=>RES_MAP_TT
      !PERNR type PERNR_D
      !CLUSTER_B2 type HRF_TIM_B2
    exporting
      !BAL_UPD type /SEW/CL_TIME_CHANGES=>BAL_UPD_TT .
  methods DETECT_CHANGES
    importing
      !CLUSTER_B2 type HRF_TIM_B2
      !PERNR type PERNR_D
      !ENDDA type ENDDA
      !BEGDA type BEGDA
    exporting
      !BAL_UPD_CHANGES type /SEW/CL_TIME_CHANGES=>BAL_UPD_TT .
  methods GET
    importing
      !DATES type RSDSSELOPT_T
      !STATUS type RSDSSELOPT_T
      !PERNR type RSDSSELOPT_T
      !INTRUN type RSDSSELOPT_T
    returning
      value(FO_AEUP) type /SEW/TT_FO_AEUP .
  methods POST_FO_AEUP
    importing
      !SIMU type BOOLEAN
    exporting
      !FO_AEUP_POST type /SEW/TT_FO_AEUP
      !FO_AEUP_ERROR type /SEW/TT_FO_AEUP
      !FO_AEUP_LOCKED type /SEW/TT_FO_AEUP
    changing
      !FO_AEUP type /SEW/TT_FO_AEUP
      !MESSAGE_HANDLER type ref to CL_HRPAY00_MESSAGE_HANDLER .
  methods PREPARE_PROTOCOL .
  methods PREPARE_TABLES
    importing
      !STATUS type /SEW/DD_STATUS
    changing
      !BAL_UPD_OLD type /SEW/CL_TIME_CHANGES=>BAL_UPD_TT
      !BAL_UPD_NEW type /SEW/CL_TIME_CHANGES=>BAL_UPD_TT .
  methods READ_CLUSTER_B2
    importing
      !IR_PERNR type RSDSSELOPT_T
      !BEGDA type DATS
      !ENDDA type DATS .
  methods READ_CLUSTER_B2_FORCE
    importing
      !IR_PERNR type RSDSSELOPT_T
      !BEGDA type DATS
      !ENDDA type DATS .
  methods SAVE_ENTRIES
    importing
      !BAL_UPD type /SEW/CL_TIME_CHANGES=>BAL_UPD_TT
    returning
      value(IS_OK) type BOOLEAN .
protected section.
private section.
ENDCLASS.



CLASS /SEW/CL_TIME_CHANGES IMPLEMENTATION.


  METHOD build_xml.
    DATA: xml_document_node TYPE REF TO if_ixml_node,
          int_bal_upd_tmp   TYPE TABLE OF /sew/int_bal_upd,
          child_node        TYPE REF TO if_ixml_node.


    DATA(xml) = NEW /sew/cl_int_xml_up( ).

    xml->create_initial_xml( IMPORTING
                               xml_document   = DATA(xml_document)
                               xml_data_node  = DATA(xml_node) )." abstraction node

    MOVE-CORRESPONDING int_bal_upd TO int_bal_upd_tmp.

    SORT int_bal_upd_tmp BY pernr ASCENDING endda ASCENDING begda ASCENDING.

    DATA(tmp_pernr) = int_bal_upd_tmp[ 1 ]-pernr.
    DATA(pernr) = int_bal_upd_tmp[ 1 ]-pernr.
    DATA(oraclepernr) = /sew/cl_forms_utils=>get_oraclepernr( pernr ).
    DATA(molga) = int_bal_upd_tmp[ 1 ]-molga.
    DATA(period) = CONV char6( int_bal_upd_tmp[ 1 ]-begda+0(6) ).
    DATA(tmp_period) = period.
    DATA(xml_pernr_node) = xml_document->create_element( name = 'Person' ).
*    DATA(parent_node) = xml_document->create_element( name = 'PayrollInterfaceInboundRecord' ).
    DATA(parent_node) = /sew/cl_time_changes=>create_parent_node( name         = 'PayrollInterfaceInboundRecord'
                                                                  xml_document = xml_document
                                                                  oraclepernr  = oraclepernr
                                                                  molga        = molga
                                                                  pernr        = pernr
                                                                  int_bal_upd  = int_bal_upd_tmp[ 1 ] ).
    xml_node->append_child( new_child = xml_pernr_node ).
    xml_pernr_node->append_child( new_child = parent_node ).

    LOOP AT int_bal_upd_tmp ASSIGNING FIELD-SYMBOL(<int_bal_upd_tmp>).


      IF <int_bal_upd_tmp>-pernr NE tmp_pernr. " pernr changes

        tmp_pernr = <int_bal_upd_tmp>-pernr.
        tmp_period = <int_bal_upd_tmp>-begda+0(6).

        oraclepernr = /sew/cl_forms_utils=>get_oraclepernr( <int_bal_upd_tmp>-pernr ).


        CLEAR xml_pernr_node.
        CLEAR parent_node.
        CLEAR child_node.

        xml_pernr_node = xml_document->create_element( name = 'Person' ).
        parent_node    = /sew/cl_time_changes=>create_parent_node( name         = 'PayrollInterfaceInboundRecord'
                                                                   xml_document = xml_document
                                                                   oraclepernr  = oraclepernr
                                                                   molga        = molga
                                                                   pernr        = tmp_pernr
                                                                   int_bal_upd  = <int_bal_upd_tmp> ).
        xml_pernr_node->append_child( new_child = parent_node ).
        xml_node->append_child( new_child = xml_pernr_node ).
      ENDIF.

      IF tmp_period NE <int_bal_upd_tmp>-begda+0(6). " period changes
        tmp_pernr = <int_bal_upd_tmp>-pernr.
        tmp_period = <int_bal_upd_tmp>-begda+0(6).

        oraclepernr = /sew/cl_forms_utils=>get_oraclepernr( <int_bal_upd_tmp>-pernr ).

        CLEAR parent_node.
        CLEAR child_node.

        parent_node = /sew/cl_time_changes=>create_parent_node( name         = 'PayrollInterfaceInboundRecord'
                                                                xml_document = xml_document
                                                                oraclepernr  = oraclepernr
                                                                molga        = molga
                                                                pernr        = tmp_pernr
                                                                int_bal_upd  = <int_bal_upd_tmp> ).
        xml_pernr_node->append_child( new_child = parent_node ).
      ENDIF.

      child_node = /sew/cl_time_changes=>create_child_node( name         = 'PayrollInterfaceInboundRecordInfo'
                                                            xml_document = xml_document
                                                            oraclepernr  = oraclepernr
                                                            molga        = molga
                                                            pernr        = tmp_pernr
                                                            int_bal_upd  = <int_bal_upd_tmp> ).


      parent_node->append_child( new_child = child_node ).

    ENDLOOP.

    xml_string = xml->create_xstring_from_xml( xml = xml_document ).

  ENDMETHOD.


  METHOD COMPARE_TABLES.

    DATA: bal_upd_line TYPE /sew/int_bal_upd.

    LOOP AT bal_upd_new ASSIGNING FIELD-SYMBOL(<bal_upd_new>).
      READ TABLE bal_upd_old WITH KEY mandt     = <bal_upd_new>-mandt
                                      molga     = <bal_upd_new>-molga
                                      pernr     = <bal_upd_new>-pernr
                                      tabname   = <bal_upd_new>-tabname
                                      ztart     = <bal_upd_new>-ztart
                                      endda     = <bal_upd_new>-endda
                                      begda     = <bal_upd_new>-begda
                                      INTO DATA(bal_upd_old_line).
      IF sy-subrc NE 0.
        APPEND <bal_upd_new> TO bal_upd_add.
      ELSE.
        IF <bal_upd_new>-unit   NE bal_upd_old_line-unit   OR
           <bal_upd_new>-anzhl  NE bal_upd_old_line-anzhl  OR
*           <bal_upd_new>-aedtm  NE bal_upd_old_line-aedtm  OR
           <bal_upd_new>-ztext  NE bal_upd_old_line-ztext.

          APPEND <bal_upd_new> TO bal_upd_mod.
        ELSE.
          MOVE-CORRESPONDING <bal_upd_new> TO bal_upd_line.
          bal_upd_line-status = '02'.
          APPEND bal_upd_line TO bal_upd_no_changes.
        ENDIF.
      ENDIF.

      CLEAR: bal_upd_old_line.

    ENDLOOP.

    IF bal_upd_del IS INITIAL AND bal_upd_mod IS INITIAL AND bal_upd_add IS INITIAL.
      no_changes = 'X'.
    ENDIF.

  ENDMETHOD.


  METHOD CONSTRUCTOR.

    me->simu  = simu.
    me->dates = dates.
    msg_cont = /iwbep/cl_mgw_msg_container=>get_mgw_msg_container( ).
    "prepare protocol in case dialog is active and ALV is requested
    IF alv   EQ abap_true AND
       batch EQ abap_false.
      message_handler = cl_hrpay00_message_handler=>get_message_handler( ).
    ENDIF.

  ENDMETHOD.


  METHOD create_child_node.

    DATA(country_iso) = /sew/cl_forms_utils=>get_country_by_molga( molga ).
    DATA(begda) = int_bal_upd-begda.
    DATA(endda) = int_bal_upd-endda.

    rval = xml_document->create_element( name = name ).

    DATA(xml_single_node) = xml_document->create_element( name = 'SourceType' ).
    xml_single_node->set_value( value = CONV #( 'ORA_HRY_PAYROLL_RELATIONSHIP' ) ).
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'EntityIdentifier' ).
    xml_single_node->set_value( value = CONV #( oraclepernr ) ).
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'FunctionalCategory' ).
    xml_single_node->set_value( value = CONV #( 'SEW_SAP_PAYROLL' ) ).
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'RecordType' ).
    xml_single_node->set_value( value = CONV #( 'ORA_HRY_PAYROLL_DATA' ) ).
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'LegislativeDataGroupName' ).
    xml_single_node->set_value( value =  country_iso && ` LDG` ).
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'PayrollName' ).
    xml_single_node->set_value( value = country_iso && '_Payroll' ).
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'PeriodName' ).
    xml_single_node->set_value( value = begda+4(2) && ` ` && begda+0(4) && ` Monthly Calendar` ).
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'BatchCode' ).
    xml_single_node->set_value( value = 'PAY_DATA_' && begda+4(2) && '_' && begda+2(2) ).
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'OraInbndRecord' ).
    DATA(oracle_inbd) = /sew/cl_time_changes=>get_oracle_inbound_record( ztart   = int_bal_upd-ztart
                                                                         tabname = int_bal_upd-tabname ).
    xml_single_node->set_value( value = oracle_inbd ).
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'EffCategoryCode' ).
    xml_single_node->set_value( value = 'ORA_HRY_INBD_PAYROLL').
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'IriInformationContext' ).
    xml_single_node->set_value( value = oracle_inbd ).
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'SourceSystemOwner').
    xml_single_node->set_value( value = 'SAP_' && sy-mandt ).
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'SourceSystemID').
    DATA(source_system_id) = /sew/cl_time_changes=>generate_source_system_id( pernr   = pernr
                                                                              begda   = begda
                                                                              endda   = endda
                                                                              ztart   = int_bal_upd-ztart
                                                                              tabname = int_bal_upd-tabname ).
    xml_single_node->set_value( value = source_system_id  ).
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'endDate' && int_bal_upd-ztart ).
    DATA(oracle_endda) = /sew/cl_forms_utils=>convert_date_oracle( date = int_bal_upd-endda ).
    xml_single_node->set_value( value = CONV #( oracle_endda ) ).
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'periodTypeInd' && int_bal_upd-ztart ).
    DATA(period_type_indicator) = COND #( WHEN int_bal_upd-begda = int_bal_upd-endda THEN 'D'
                                          ELSE 'P' ).
    xml_single_node->set_value( value = CONV #( period_type_indicator ) ).
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'startDate' && int_bal_upd-ztart ).
    DATA(oracle_begda) = /sew/cl_forms_utils=>convert_date_oracle( date = int_bal_upd-begda ).
    xml_single_node->set_value( value = CONV #( oracle_begda ) ).
    rval->append_child( new_child = xml_single_node ).

    DATA(node_name) = /sew/cl_time_changes=>determine_ztart_node_name( int_bal_upd-ztart ).
    xml_single_node = xml_document->create_element( name = CONV #( node_name ) ).
    DATA(anzhl_s) = CONV string( int_bal_upd-anzhl ).
    CONDENSE anzhl_s.
    xml_single_node->set_value( value = anzhl_s ).
    rval->append_child( new_child = xml_single_node ).

  ENDMETHOD.


  METHOD create_entries.
    DATA: bal_upd_t TYPE TABLE OF /sew/int_bal_upd,
          bal_upd_l TYPE /sew/int_bal_upd.
    FIELD-SYMBOLS: <res_tab> TYPE ANY TABLE.

    LOOP AT int_res_map ASSIGNING FIELD-SYMBOL(<line_map>).

      ASSIGN COMPONENT 'ft_' && <line_map>-tabname OF STRUCTURE cluster_b2 TO <res_tab>.

      LOOP AT <res_tab> ASSIGNING FIELD-SYMBOL(<tab_line>).
        ASSIGN COMPONENT 'ZTART' OF STRUCTURE <tab_line> TO FIELD-SYMBOL(<ztart>).
        ASSIGN COMPONENT 'DATUM' OF STRUCTURE <tab_line> TO FIELD-SYMBOL(<datum>).
        ASSIGN COMPONENT 'BEGDA' OF STRUCTURE <tab_line> TO FIELD-SYMBOL(<begda>).
        ASSIGN COMPONENT 'ENDDA' OF STRUCTURE <tab_line> TO FIELD-SYMBOL(<endda>).
        ASSIGN COMPONENT 'ANZHL' OF STRUCTURE <tab_line> TO FIELD-SYMBOL(<anzhl>).


        IF <ztart> IS ASSIGNED.
          CHECK <ztart> EQ <line_map>-ztart.

          IF <datum> IS ASSIGNED.
            bal_upd_l-endda   = <datum>.
            bal_upd_l-begda   = <datum>.
          ELSEIF <begda> IS ASSIGNED AND <endda> IS ASSIGNED.
            bal_upd_l-begda   = <begda>.
            bal_upd_l-endda   = <endda>.
          ENDIF.

          IF <anzhl> IS ASSIGNED.
            bal_upd_l-anzhl   = <anzhl>.
          ENDIF.

          bal_upd_l-mandt     = sy-mandt.
          bal_upd_l-molga     = cluster_b2-molga.
          bal_upd_l-pernr     = pernr.
          bal_upd_l-unit      = <line_map>-unit.
          bal_upd_l-aedtm     = me->get_change_date(
                                          EXPORTING
                                            pernr  = pernr
                                            period = CONV #( bal_upd_l-begda+0(6) ) ).
          bal_upd_l-status    = '01'.
          bal_upd_l-ztext     = /sew/cl_forms_utils=>get_ztart_text( ztart = <ztart> ).
          bal_upd_l-tabname   = <line_map>-tabname.
          bal_upd_l-ztart     = <ztart>.
        ENDIF.
        APPEND bal_upd_l TO bal_upd.
        CLEAR: bal_upd_l.
        UNASSIGN: <ztart>, <datum>, <begda>, <endda>, <anzhl>.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD create_parent_node.

    DATA(country_iso) = /sew/cl_forms_utils=>get_country_by_molga( molga ).
    DATA(begda) = int_bal_upd-begda.

    rval = xml_document->create_element( name = name ).

    DATA(xml_single_node) = xml_document->create_element( name = 'SourceType' ).
    xml_single_node->set_value( value = CONV #( 'ORA_HRY_PAYROLL_RELATIONSHIP' ) ).
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'EntityIdentifier' ).
    xml_single_node->set_value( value = CONV #( oraclepernr ) ).
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'FunctionalCategory' ).
    xml_single_node->set_value( value = CONV #( 'SEW_SAP_PAYROLL' ) ).
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'RecordType' ).
    xml_single_node->set_value( value = CONV #( 'ORA_HRY_PAYROLL_DATA' ) ).
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'LegislativeDataGroupName' ).
    xml_single_node->set_value( value =  country_iso && ` LDG` ).
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'PayrollName' ).
    xml_single_node->set_value( value =  country_iso && '_Payroll' ).
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'PeriodName' ).
    xml_single_node->set_value( value = begda+4(2) && ` ` && begda+0(4) && ` Monthly Calendar` ).
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'BatchCode' ).
    xml_single_node->set_value( value = 'PAY_DATA_' && begda+4(2) && '_' && begda+2(2) ).
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'BatchDate' ).
    DATA(oracle_begda) = /sew/cl_forms_utils=>convert_date_oracle( begda ).
    xml_single_node->set_value( value =  oracle_begda && ` 00:00:00`  ).
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'VendorCode' ).
    xml_single_node->set_value( value = 'ORA_HRX_US_EEO').
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'CategoryCode' ).
    xml_single_node->set_value( value = 'ORA_HRY_INBD_PAYROLL').
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'DocumentCode' ).
*    xml_single_node->set_value( value = 'ORA_HRY_INBD_PAYROLL').
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'DocumentType' ).
*    xml_single_node->set_value( value = 'ORA_HRY_INBD_PAYROLL').
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'SourceSystemId' ).
    DATA(ssid_part) = /sew/cl_time_changes=>get_ssid_component( ztart   = int_bal_upd-ztart
                                                                tabname = int_bal_upd-tabname ).
    xml_single_node->set_value( value = 'PER_' && pernr && '_PAY_' && begda+4(2) && '_' && begda+0(4) ). "PER_23860_PAY_12_2020_TS_2021/06/01_2021/06/30
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'SourceSystemOwner' ).
    xml_single_node->set_value( value = 'SAP_' && sy-mandt ).
    rval->append_child( new_child = xml_single_node ).


  ENDMETHOD.


  METHOD DETECT_CHANGES.

    DATA: change_date        TYPE dats,
          bal_upd_t          TYPE STANDARD TABLE OF /sew/int_bal_upd,
          bal_upd_l          TYPE /sew/int_bal_upd,
          res_map_ztart      TYPE TABLE OF pt_ztart,
          tabname            TYPE char40,
          bal_upd_del        TYPE STANDARD TABLE OF /sew/int_bal_upd,
          bal_upd_mod        TYPE STANDARD TABLE OF /sew/int_bal_upd,
          bal_upd_add        TYPE STANDARD TABLE OF /sew/int_bal_upd,
          no_changes         TYPE flag,
          bal_upd_tmp        TYPE STANDARD TABLE OF /sew/int_bal_upd,
          bal_upd_no_changes TYPE STANDARD TABLE OF /sew/int_bal_upd.


*    DATA(oracle_id) = /sew/cl_int_employee_xml_up=>read_oracle_id( pernr = pernr ).

    " read customizing
    SELECT * FROM /sew/int_res_map WHERE begda <= @sy-datum
                                   AND   endda >= @sy-datum
                                   INTO  TABLE @DATA(int_res_map).

    DATA(period_range) = VALUE rsdsselopt_t( ( sign   = 'I'
                                               option = 'BT'
                                               low    = begda
                                               high   = endda ) ).

    SELECT * FROM /sew/int_bal_upd WHERE molga     = @cluster_b2-molga
                                   AND   pernr     = @pernr
                                   AND   status    = '02'
                                   AND   begda     IN @period_range
                                   AND   endda     IN @period_range
                                   INTO  TABLE @DATA(bal_upd_old).



    IF bal_upd_old IS INITIAL. " create initial entries, falls keine entries für die periode existieren
      me->create_entries(
        EXPORTING
          cluster_b2  = cluster_b2
          pernr       = pernr
          int_res_map = int_res_map
        IMPORTING
          bal_upd     = bal_upd_t ).

      bal_upd_changes = bal_upd_t.
      RETURN.
    ENDIF.

    me->create_entries(
      EXPORTING
        cluster_b2  = cluster_b2
        pernr       = pernr
        int_res_map = int_res_map
      IMPORTING
        bal_upd     = bal_upd_t ).

    CHECK bal_upd_t IS NOT INITIAl.


    me->compare_tables(
      EXPORTING
        bal_upd_old        = bal_upd_old
        bal_upd_new        = bal_upd_t
      IMPORTING
        bal_upd_del        = bal_upd_del
        bal_upd_add        = bal_upd_add
        bal_upd_mod        = bal_upd_mod
        bal_upd_no_changes = bal_upd_no_changes
        no_changes         = no_changes ).

    IF no_changes EQ 'X'.
      RETURN.
    ENDIF.

    APPEND LINES OF bal_upd_mod        TO bal_upd_changes.
    APPEND LINES OF bal_upd_add        TO bal_upd_changes.
    APPEND LINES OF bal_upd_no_changes TO bal_upd_changes.

*    Kein record -> neuen erstellen
*    record io   -> nichts tun
*    record nio  -> neuen erstellen

*    IF int_bal_upd IS NOT INITIAL. " falls record da
*      SORT int_bal_upd BY aenddatum DESCENDING.
*      READ TABLE int_bal_upd INDEX 1 INTO DATA(int_bal_upd_line).
*    ENDIF.
*
*    change_date = cluster_b2-fs_dates-dneva. " nicht 100 pro sicher
*
*    IF sy-subrc NE 0 OR int_bal_upd IS INITIAL. " kein record da -> neuen erstellen
*      me->create_entries(
*        EXPORTING
*          cluster_b2 = cluster_b2
*          pernr = pernr
*          int_res_map = int_res_map
**          change_date = change_date
*        IMPORTING
*          bal_upd = bal_upd_t
*       ).
*      APPEND LINES OF bal_upd_t TO bal_upd_changes.
*      RETURN.
*    ENDIF.
*
*
*    IF change_date EQ sy-datum "neuen record erstellen da cluster sich verändert hat
*    OR change_date GT int_bal_upd_line-endda. "cluster b2 changed.
*
*    me->create_entries(
*      EXPORTING
*        cluster_b2  = cluster_b2
*        pernr       = pernr
*        int_res_map = int_res_map
**        change_date = change_date
*      IMPORTING
*        bal_upd = bal_upd_t ).
*
*    APPEND LINES OF bal_upd_t TO bal_upd_changes.
*    CLEAR bal_upd_t.
*    ENDIF.

****
*****      LOOP AT int_res_map ASSIGNING FIELD-SYMBOL(<line_map>).
*****
*****
*****        ASSIGN COMPONENT <line_map>-tabname OF STRUCTURE cluster_b2 TO <res_tab>.
*****
*****        LOOP AT <res_tab> ASSIGNING FIELD-SYMBOL(<tab_line>).
*****          ASSIGN COMPONENT 'ZTART' OF STRUCTURE <tab_line> TO FIELD-SYMBOL(<ztart>).
*****          ASSIGN COMPONENT 'DATUM' OF STRUCTURE <tab_line> TO FIELD-SYMBOL(<datum>).
*****          ASSIGN COMPONENT 'BEGDA' OF STRUCTURE <tab_line> TO FIELD-SYMBOL(<begda>).
*****          ASSIGN COMPONENT 'ENDDA' OF STRUCTURE <tab_line> TO FIELD-SYMBOL(<endda>).
*****          ASSIGN COMPONENT 'ANZHL' OF STRUCTURE <tab_line> TO FIELD-SYMBOL(<anzhl>).
*****
*****
*****          IF <ztart> IS ASSIGNED.
*****            CHECK <ztart> EQ <line_map>-ztart.
*****
*****            IF <datum> IS ASSIGNED.
*****              bal_upd_l-endda = <datum>.
*****              bal_upd_l-begda = <datum>.
*****            ENDIF.
*****
*****            IF <begda> IS ASSIGNED.
*****              bal_upd_l-begda = <begda>.
*****            ENDIF.
*****            IF <endda> IS ASSIGNED.
*****              bal_upd_l-endda = <endda>.
*****            ENDIF.
*****
*****            IF <anzhl> IS ASSIGNED.
*****              bal_upd_l-anzhl = <anzhl>.
*****            ENDIF.
*****
*****            bal_upd_l-mandt = sy-mandt.
*****            bal_upd_l-molga = cluster_b2-molga.
*****            bal_upd_l-pernr = pernr.
*****            bal_upd_l-oracle_id = <line_map>-oracle_id.
*****            bal_upd_l-unit = <line_map>-unit.
*****            bal_upd_l-aenddatum = change_date.
*****            bal_upd_l-status = '01'.
*****          ENDIF.
*****          APPEND bal_upd_l TO bal_upd_t.
*****          CLEAR bal_upd_l.
*****        ENDLOOP.
*****      ENDLOOP.
  ENDMETHOD.


  METHOD determine_ztart_node_name.

    SELECT SINGLE node_name FROM /sew/int_res_map
                            WHERE ztart EQ @ztart
                            INTO @node_name.

    IF sy-subrc NE 0.
      node_name = 'value' && ztart. " if no customized nodeName is found then valueXXXX becomes the node name.
    ENDIF.

  ENDMETHOD.


  METHOD generate_source_system_id.

    DATA(oracle_begda) = /sew/cl_forms_utils=>convert_date_oracle( begda ).
    DATA(oracle_endda) = /sew/cl_forms_utils=>convert_date_oracle( endda ).
    DATA(ssid_comp)    = /sew/cl_time_changes=>get_ssid_component( ztart   = ztart
                                                                   tabname = tabname ).
    CONCATENATE 'PER'
                 pernr
                'PAY'
                 begda+4(2)
                 begda+0(4)
                 ssid_comp
                 oracle_begda
                 oracle_endda
                 INTO ssid SEPARATED BY '_'.


*    PER_23860_PAY_12_2020_TS_2021/06/01_2021/06/30
  ENDMETHOD.


  METHOD GET.

  SELECT * FROM /sew/int_fo_aeup INTO TABLE fo_aeup WHERE pernr     IN pernr  AND
                                                            status  IN status AND
                                                            int_run IN intrun AND
                                                            ( begda IN dates OR
                                                              endda IN dates ) .
  ENDMETHOD.


  METHOD GET_CHANGE_DATE.

    CONCATENATE pernr period '1' INTO DATA(srtfd).

    SELECT SINGLE aedtm FROM pcl2 WHERE relid = 'B2'
                                  AND   srtfd = @srtfd
                                  INTO  @change_date.

  ENDMETHOD.


  METHOD get_oracle_inbound_record.

    SELECT SINGLE oracle_ztart FROM  /sew/int_res_map
                               WHERE ztart   = @ztart
                               AND   tabname = @tabname
                               INTO  @oracle_ztart.

  ENDMETHOD.


  METHOD get_ssid_component.

    SELECT SINGLE ssid_comp FROM /sew/int_res_map
                            WHERE ztart   EQ @ztart
                            AND   tabname EQ @tabname
                            INTO  @comp.

    IF sy-subrc NE 0.
      " error handling
    ENDIF.


  ENDMETHOD.


  METHOD GET_ZTART_TEXT.
    DATA: sprsl_l TYPE sprsl.

    IF sprsl IS INITIAL.
      sprsl_l = 'DE'.
    ELSE.
      sprsl_l = SPRSL.
    ENDIF.

    SELECT SINGLE ztext FROM t555b WHERE ztart = @ztart
                                   AND   sprsl = @sprsl_l
                                   INTO  @ztext.

  ENDMETHOD.


  METHOD POST_FO_AEUP.

    DATA: pernr        TYPE pernr_d,
          it           TYPE REF TO data,
          return       TYPE bapireturn1,
          pernr_locked TYPE rsdsselopt_t,
          error_tab    TYPE hrpad_return_tab,
          error_hrpad  TYPE hrpad_return.


    FIELD-SYMBOLS: <struc>    TYPE any,
                   <infotype> TYPE any.

    LOOP AT fo_aeup ASSIGNING FIELD-SYMBOL(<fo_aeup>).
      me->status_handler->aend_id = <fo_aeup>-aend_id.
      me->status_handler->begda = <fo_aeup>-begda.
      me->status_handler->endda = <fo_aeup>-endda.
*      me->status_handler->molga = <fo_aeup>-molga.  " molga nicht in /sew/int_fo_aeup vorhanden
*      me->status_handler->oraclepernr = <fo_aeup>-oraclepernr.
      me->status_handler->sap_id = <fo_aeup>-pernr.
      me->status_handler->int_run = <fo_aeup>-int_run.

      IF <fo_aeup>-pernr   IN pernr_locked AND
        pernr_locked       IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      IF pernr NE <fo_aeup>-pernr OR
         pernr IS INITIAL.

        IF pernr IS NOT INITIAL.
          CALL FUNCTION 'HR_EMPLOYEE_DEQUEUE'
            EXPORTING
              number = pernr
            IMPORTING
              return = return.

          "check for enqueue pernr
          IF return-type EQ /sew/cl_int_constants=>error.
            me->status_handler->add_log_message( aend_id = <fo_aeup>-aend_id bapiret1 = return ).
            CLEAR: pernr, return.
          ENDIF.

          CALL FUNCTION 'HR_EMPLOYEE_ENQUEUE'
            EXPORTING
              number = <fo_aeup>-pernr
            IMPORTING
              return = return.

          "check for enqueue pernr
          IF return-type EQ /sew/cl_int_constants=>error.
            me->status_handler->add_log_message( aend_id = <fo_aeup>-aend_id bapiret1 = return ).
            CLEAR: return.

            APPEND VALUE #( sign = 'I' option = 'EQ' low = <fo_aeup>-pernr ) TO pernr_locked.
*            APPEND VALUE #( sign = 'I' option = 'EQ' low = <fo_aeup>-cloud_id ) TO pernr_locked.
*          count = max.
            CONTINUE.
          ENDIF.

          pernr = <fo_aeup>-pernr.

        ENDIF.

      ENDIF.

*      DATA(method) = SWITCH char9( <fo_aeup>-infty " /sew/int_fo_aeup hat kein feld infty
*        WHEN '2011' THEN 'POST_TEVE'
*        WHEN '2001' THEN 'POST_OPER'
*        WHEN '2002' THEN 'POST_OPER'
*        WHEN '2006' THEN 'POST_OPER'
*        WHEN '2006' THEN 'POST_OPER' ).




    ENDLOOP.


  ENDMETHOD.


  METHOD prepare_protocol.

    "Check message handler
    IF message_handler IS BOUND.

      "prepare layout options
      DATA(layout) = VALUE slis_layout_alv( zebra = 'X'
                                               colwidth_optimize = 'X' ).

      DATA: root_node TYPE hrpad_pal_node_key VALUE 'ROOT'.

      "Define structure of successful and failed IT_AEND entries ALV table
      message_handler->if_hrpay00_pal_services~create_fcat( EXPORTING i_structure_name = '/SEW/INT_BAL_UPD'
                                                            IMPORTING et_fcat          = DATA(fcat_bal_upd) ).

      "add post table
      IF me->int_bal_upd IS NOT INITIAL.
        message_handler->if_hrpay00_pal_services~add_table(
          EXPORTING
            i_parent_node_key = root_node
            it_fcat           = fcat_bal_upd
            it_append_table   = me->int_bal_upd
            is_layout         = layout
            i_node_txt        = COND string( WHEN simu EQ abap_true
                                             THEN TEXT-005
                                             ELSE TEXT-001 ) ).
      ENDIF.
*
*      "add error table
*      IF it_aend_error IS NOT INITIAL.
*        message_handler->if_hrpay00_pal_services~add_table(
*          EXPORTING
*            i_parent_node_key = root_node
*            it_fcat           = fcat_it_aend
*            it_append_table   = it_aend_error
*            is_layout         = layout
*            i_node_txt        = COND string( WHEN simu EQ abap_true
*                                             THEN TEXT-006
*                                             ELSE TEXT-002 ) ).
*      ENDIF.

      "add skipped pernr's
      IF skipped_pernrs IS NOT INITIAL.
        message_handler->if_hrpay00_pal_services~add_table(
        EXPORTING
          i_parent_node_key = root_node
          it_append_table   = skipped_pernrs
          is_layout         = layout
          i_node_txt        = COND string( WHEN simu EQ abap_true
                                             THEN TEXT-008
                                             ELSE TEXT-004 ) ).
      ENDIF.

      "add message table
      message_handler->if_hrpay00_pal_services~add_messages(
        EXPORTING
          i_parent_node_key = root_node
      ).

      "show pals
      CALL METHOD message_handler->display_pal.

    ENDIF.

***    FIELD-SYMBOLS: <append_table> TYPE any.
***
***    "Check message handler
***    IF message_handler IS BOUND.
***
***      "prepare layout options
***      DATA(layout) = VALUE slis_layout_alv( zebra = 'X'
***                                               colwidth_optimize = 'X' ).
***
***      DATA: root_node TYPE hrpad_pal_node_key VALUE 'ROOT'.
***
***      "Define structure of successful and failed IT_AEND entries ALV table
***      message_handler->if_hrpay00_pal_services~create_fcat( EXPORTING i_structure_name = '/SEW/INT_FO_AEUP'
***                                                            IMPORTING et_fcat          = DATA(fcat_it_aend) ).
***
****      <append_table> = me->int_fo_aeup.
***
***      message_handler->if_hrpay00_pal_services~add_table(
***        EXPORTING
***          i_parent_node_key = root_node
***          it_fcat           = fcat_it_aend
***          it_append_table   = <append_table>
***          is_layout         = layout
***          i_node_txt        = COND string( WHEN simu EQ abap_true
***                                           THEN TEXT-009
***                                           ELSE TEXT-010 ) ).
***      "add skipped pernr's
***      IF skipped_pernrs IS NOT INITIAL.
***        message_handler->if_hrpay00_pal_services~add_table(
***        EXPORTING
***          i_parent_node_key = root_node
***          it_append_table   = skipped_pernrs
***          is_layout         = layout
***          i_node_txt        = COND string( WHEN simu EQ abap_true
***                                             THEN TEXT-008
***                                             ELSE TEXT-004 ) ).
***      ENDIF.
***
***      "add message table
***      message_handler->if_hrpay00_pal_services~add_messages(
***        EXPORTING
***          i_parent_node_key = root_node
***      ).
***
***      "show pals
***      CALL METHOD message_handler->display_pal.
***
***    ENDIF.

  ENDMETHOD.


  METHOD PREPARE_TABLES.

    DATA(change_date) = bal_upd_new[ 1 ]-aedtm.

    LOOP AT bal_upd_old ASSIGNING FIELD-SYMBOL(<line>).
      <line>-status = status.
      <line>-aedtm  = change_date.
    ENDLOOP.
  ENDMETHOD.


  METHOD read_cluster_b2.

    DATA: pernr           TYPE pernr_d,
          cluster_b2      TYPE hrf_tim_b2,
          periods         TYPE /sew/cl_forms_utils=>periods_t,
          bal_upd         TYPE TABLE OF /sew/int_bal_upd,
          bal_upd_changes TYPE TABLE OF /sew/int_bal_upd.


    CALL METHOD /sew/cl_forms_utils=>get_periods(
      EXPORTING
        begda   = begda
        endda   = endda
      IMPORTING
        periods = periods ).

    LOOP AT ir_pernr ASSIGNING FIELD-SYMBOL(<pernr>). "

      LOOP AT periods ASSIGNING FIELD-SYMBOL(<period>).

        pernr = CONV #( <pernr>-low ).

        CALL FUNCTION 'HR_FORMS_TIM_GET_B2_RESULTS'
          EXPORTING
            pernr                 = pernr
            begda                 = <period>-datuv
            endda                 = <period>-datub
          IMPORTING
            tim_b2                = cluster_b2
          EXCEPTIONS
            wrong_cluster_version = 1
            no_read_authority     = 2
            cluster_archieved     = 3
            technical_error       = 4.

*        CALL FUNCTION 'HR_TIME_RESULTS_GET'
*          EXPORTING
*            get_pernr = pernr
*            get_pabrj = pabrj
*            get_pabrp = pabrp
*          TABLES
*            get_zes   = ft_zes.

        CHECK cluster_b2 IS NOT INITIAL AND sy-subrc IS INITIAL.

        me->detect_changes(
          EXPORTING
            cluster_b2 = cluster_b2
            pernr      = pernr
            begda      = <period>-datuv
            endda      = <period>-datub
          IMPORTING
            bal_upd_changes = bal_upd_changes ).

        IF bal_upd_changes IS NOT INITIAL.
          APPEND LINES OF bal_upd_changes TO bal_upd.
          CLEAR bal_upd_changes.
        ENDIF.

      ENDLOOP.
    ENDLOOP.

    APPEND LINES OF bal_upd TO me->int_bal_upd.

    IF bal_upd IS NOT INITIAL.
      me->save_entries(
        EXPORTING
          bal_upd = bal_upd ).
    ENDIF.

  ENDMETHOD.


  METHOD read_cluster_b2_force.

    DATA: pernr       TYPE pernr_d,
          periods     TYPE /sew/cl_forms_utils=>periods_t,
          cluster_b2  TYPE hrf_tim_b2,
          bal_upd_tmp TYPE TABLE OF /sew/int_bal_upd.

    CALL METHOD /sew/cl_forms_utils=>get_periods
      EXPORTING
        begda   = begda
        endda   = endda
      IMPORTING
        periods = periods.

    SELECT * FROM /sew/int_res_map WHERE begda <= @sy-datum
                                   AND   endda >= @sy-datum
                                   INTO  TABLE @DATA(int_res_map).

    LOOP AT ir_pernr ASSIGNING FIELD-SYMBOL(<pernr>).
      LOOP AT periods ASSIGNING FIELD-SYMBOL(<period>).
        pernr = CONV #( <pernr>-low ).

        CALL FUNCTION 'HR_FORMS_TIM_GET_B2_RESULTS'
          EXPORTING
            pernr                 = pernr
            begda                 = <period>-datuv
            endda                 = <period>-datub
          IMPORTING
            tim_b2                = cluster_b2
          EXCEPTIONS
            wrong_cluster_version = 1
            no_read_authority     = 2
            cluster_archieved     = 3
            technical_error       = 4.

        CHECK cluster_b2 IS NOT INITIAL AND sy-subrc IS INITIAL.



        me->create_entries(
          EXPORTING
            cluster_b2  = cluster_b2
            pernr       = pernr
            int_res_map = int_res_map
          IMPORTING
            bal_upd     = bal_upd_tmp ).


        CHECK bal_upd_tmp IS NOT INITIAL.

        APPEND LINES OF bal_upd_tmp TO me->int_bal_upd.

        me->save_entries(
          EXPORTING
            bal_upd = bal_upd_tmp ).

        CLEAR cluster_b2.
        CLEAR bal_upd_tmp.

      ENDLOOP.
    ENDLOOP.


  ENDMETHOD.


  METHOD SAVE_ENTRIES.

    IF bal_upd IS NOT INITIAL AND me->simu NE 'X'.
      MODIFY /sew/int_bal_upd FROM TABLE bal_upd.
*      INSERT /sew/int_bal_upd FROM TABLE bal_upd. "DBSQL_DUPLICATE_KEY_ERROR tritt auf -> aendtm zu key machen?
      COMMIT WORK.
      IF sy-subrc NE 0.
        is_ok = abap_false.
        msg_cont->add_message( iv_msg_type                 = /iwbep/cl_cos_logger=>error
                                 iv_msg_id                 = 'it_aend_mc' " it_aend_mc nicht bekannt
                                 iv_msg_number             = '001'
                                 iv_add_to_response_header = abap_true ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD test_method.


    DATA: int_bal_upd TYPE /sew/tt_bal_upd,
          xml         TYPE REF TO cl_xml_document,
          xml_string  TYPE xstring.




    SELECT * FROM /sew/int_bal_upd WHERE status = '02'
                                   INTO  TABLE @int_bal_upd
                                   UP    TO 500 ROWS.

    CHECK sy-subrc EQ 0.


    CALL METHOD /sew/cl_time_changes=>build_xml
      EXPORTING
        int_bal_upd = int_bal_upd
      IMPORTING
        xml_string  = xml_string.

    IF xml_string IS NOT INITIAL.
      LOOP AT int_bal_upd ASSIGNING FIELD-SYMBOL(<line>).
        <line>-status = '02'.
      ENDLOOP.

*        MODIFY /sew/int_bal_upd FROM TABLE int_bal_upd.
*        COMMIT WORK.
*        status = '02'.
    ELSE.
*      status = '03'.
    ENDIF.

    cl_abap_browser=>show_xml(
      EXPORTING
        xml_xstring =  xml_string  ).


*      CALL FUNCTION 'DISPLAY_XML_STRING'
*        EXPORTING
*          xml_string      = xml_string
*        EXCEPTIONS
*          no_xml_document = 1
*          OTHERS          = 2.






*    DATA: is_ok     TYPE flag,
*          periods   TYPE /sew/cl_time_changes=>periods_t,
*          begda     TYPE dats,
*          endda     TYPE dats,
*          pcl2_temp TYPE STANDARD TABLE OF pcl2,
*          pcl2      TYPE STANDARD TABLE OF pcl2.
**    FIELD-SYMBOLS: <periods> TYPE ANY TABLE.
*
*    begda = '20210101'.
*    endda = '20210731'.
*
*
*
*    CALL METHOD /sew/cl_forms_utils=>get_periods
*      EXPORTING
*        begda   = begda
*        endda   = endda
*      IMPORTING
*        periods = periods.
*
*
**      DATA(oracle_con) = VALUE rsdsselopt_t( FOR <res_map> IN int_res_map ( sign = 'I' option = 'EQ' low = <res_map>-oracle_id ) ).
**    DATA(periods_range) = VALUE rsdsselopt_t( FOR <periods> IN periods ( sign = 'I' option = 'EQ' low = <periods>-datuv+0(6) ) ).
*    DATA(srtfd) = VALUE rsdsselopt_t( FOR <periods> IN periods ( sign = 'I' option = 'EQ' low = pernr && <periods>-datuv+0(6) && '1' ) ).
**    LOOP AT periods_range ASSIGNING FIELD-SYMBOL(<line>).
**      CONCATENATE pernr <line>-low INTO DATA(srtfd).
**      APPEND srtfd
**    ENDLOOP.
*    SELECT * FROM pcl2 WHERE relid = 'B2'
*                       AND   srtfd IN @srtfd
*                       INTO TABLE @pcl2_temp.
*    IF pcl2 IS NOT INITIAL.
*      is_ok = abap_true.
*      APPEND LINES OF pcl2_temp TO pcl2.
*    ENDIF.
**    ENDLOOP.


  ENDMETHOD.
ENDCLASS.
