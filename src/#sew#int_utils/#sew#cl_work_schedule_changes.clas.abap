class /SEW/CL_WORK_SCHEDULE_CHANGES definition
  public
  final
  create public .

public section.

  class-methods CREATE_SHIFT_TIME
    importing
      !DATE type DATS
      !TIME type TIME
    returning
      value(SHIFTDATETIME) type CHAR20 .
  class-methods ADD_TIME
    importing
      !AMOUNT_IN_SECONDS type I
    changing
      !TIME type SYUZEIT .
  class-methods CREATE_SHIFT_EVENT_NODE
    importing
      !WORK_SCHEDULE_TIMES type /SEW/WORK_SCHEDULES_TIMES
      !XML_DOCUMENT type ref to IF_IXML_DOCUMENT
      !REQUEST_NUMBER type STRING
      !EVENT_NUMBER type STRING
      !COUNTER type I
    returning
      value(RVAL) type ref to IF_IXML_ELEMENT .
  class-methods CREATE_REQUEST_TS
    returning
      value(TIMESTAMP) type STRING .
  class-methods CREATE_REQUEST_NODE
    importing
      !WORK_SCHEDULE_TIMES type /SEW/WORK_SCHEDULES_TIMES
      !XML_DOCUMENT type ref to IF_IXML_DOCUMENT
    exporting
      !REQUEST_NUMBER type STRING
      !EVENT_NUMBER type STRING
    returning
      value(RVAL) type ref to IF_IXML_ELEMENT .
  methods CREATE_AND_STORE_XML_FILE
    returning
      value(XML_STRING) type STRING .
  methods CONSTRUCTOR
    importing
      !ALV type BOOLEAN
      !BATCH type BOOLEAN
      !SIMU type BOOLEAN
      !DATES type RSDSSELOPT_T .
  methods GET_WORK_SCHEDULES_TIMES
    importing
      !PERNR_RANGE type RSDSSELOPT_T
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !PNNNN_OLD type PRELP optional
      !PNNNN_NEW type PRELP optional
    returning
      value(WORK_SCHEDULE_TIMES) type /SEW/TT_WORK_SCHEDULES_TIMES .
  methods PREPARE_PROTOCOL .
  methods GET_XSTRING
    returning
      value(XML_XSTRING) type XSTRING .
  methods SUBMIT_WS_RPT
    importing
      !PNNNN_NEW type PRELP
      !PNNNN_OLD type PRELP .
protected section.
private section.

  data INT_WS_MAP_TAB type /SEW/TT_INT_WS_MAP .
  data MESSAGE_HANDLER type ref to CL_HRPAY00_MESSAGE_HANDLER .
  data DATES type RSDSSELOPT_T .
  data SIMU type BOOLEAN .
  data MSG_CONT type ref to /IWBEP/IF_MESSAGE_CONTAINER .
  data WORK_SCHEDULE_TIMES type /SEW/TT_WORK_SCHEDULES_TIMES .
  data SKIPPED_PERNRS type HRAHQ_PERNR_TABLE .
  data XSTRING type XSTRING .

  methods CREATE_FILENAME
    returning
      value(FILENAME) type RLGRAP-FILENAME .
  methods GET_WORK_SCHEDULE
    importing
      !PERNR type PERNR_D
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !NO_AUTH type FLAG
      !PNNNN_OLD type PRELP optional
      !PNNNN_NEW type PRELP optional
    exporting
      !PERWS type PTM_PSP
      !DAYGEN type PWSDAYGEN_TAB
      !DAYINT type PWSDAYINT_TAB .
  methods GET_CUSTOMIZING
    importing
      !DAYINT type PWSDAYINT
      !MOLGA type MOLGA
    returning
      value(INT_WS_MAP) type /SEW/INT_WS_MAP .
  methods BUILD_XML
    importing
      !WORK_SCHEDULE_TIMES type /SEW/TT_WORK_SCHEDULES_TIMES
    returning
      value(XSTRING) type XSTRING .
ENDCLASS.



CLASS /SEW/CL_WORK_SCHEDULE_CHANGES IMPLEMENTATION.


  METHOD add_time.

    DATA: add_time TYPE syuzeit.

    IF amount_in_seconds IS INITIAL.
      EXIT.
    ENDIF.

    MOVE amount_in_seconds TO add_time.

    ADD add_time TO time.
  ENDMETHOD.


METHOD build_xml.

  DATA: event_node              TYPE REF TO if_ixml_node,
        request_node            TYPE REF TO if_ixml_node,
        shift_event_node        TYPE REF TO if_ixml_node,
        shift_node              TYPE REF TO if_ixml_node,
        counter                 TYPE i VALUE 0,
        shift_evt_cnt           TYPE i VALUE 0,
        work_schedule_times_tmp TYPE /sew/tt_work_schedules_times,
        event_number            TYPE string,
        evt_num                 TYPE string,
        request_number          TYPE string,
        begda_tmp               TYPE begda,
        endda_tmp               TYPE endda.

  DATA(xml) = NEW /sew/cl_int_xml_up( ).

  xml->create_initial_xml( IMPORTING xml_document  = DATA(xml_document)
                                     xml_data_node = DATA(xml_node) ). " abstraction node

  MOVE-CORRESPONDING work_schedule_times TO work_schedule_times_tmp.

  CHECK work_schedule_times_tmp IS NOT INITIAL.

  SORT work_schedule_times_tmp ASCENDING BY pernr begda endda begtm endtm.
  DATA(pernr)       = work_schedule_times_tmp[ 1 ]-pernr.
  DATA(tmp_pernr)   = pernr.
  DATA(oraclepernr) = /sew/cl_forms_utils=>get_oraclepernr( tmp_pernr ).

  event_node = xml_document->create_element( name = 'Event' ).

  request_node  = /sew/cl_work_schedule_changes=>create_request_node( EXPORTING work_schedule_times = work_schedule_times_tmp[ 1 ]
                                                                                xml_document        = xml_document
                                                                      IMPORTING event_number        = event_number
                                                                                request_number      = request_number ).

  event_node->append_child( new_child = request_node ).
  xml_node->append_child( new_child = event_node ).

  LOOP AT work_schedule_times_tmp ASSIGNING FIELD-SYMBOL(<work_schedule_times_tmp>).

    IF <work_schedule_times_tmp>-pernr NE tmp_pernr.
      tmp_pernr   = <work_schedule_times_tmp>-pernr.
      oraclepernr = /sew/cl_forms_utils=>get_oraclepernr( tmp_pernr ).
      counter = 1. "JMB20211009 I
      shift_evt_cnt = 1. "JMB20211015 I

      begda_tmp = <work_schedule_times_tmp>-begda. "JMB20211015 I
      endda_tmp = <work_schedule_times_tmp>-endda. "JMB20211015 I

      CLEAR: request_node, shift_node, shift_event_node, event_node, event_number, request_number.

      event_node = xml_document->create_element( 'Event' ).
      request_node = /sew/cl_work_schedule_changes=>create_request_node( EXPORTING work_schedule_times = <work_schedule_times_tmp>
                                                                                   xml_document        = xml_document
                                                                         IMPORTING event_number        = event_number
                                                                                   request_number      = request_number ).
      evt_num = event_number && 'EN' && CONV string( counter ).
      CONDENSE evt_num.
      event_node->append_child( new_child = request_node ).
      xml_node->append_child( new_child = event_node ).
    ENDIF.

**JMB20211014 start insert - check shift start and enddate
*
    IF ( begda_tmp NE <work_schedule_times_tmp>-begda AND
         endda_tmp NE <work_schedule_times_tmp>-endda AND
         <work_schedule_times_tmp>-vtken IS INITIAL ).
      counter = counter + 1.
      shift_evt_cnt = 1.      "JMB20211015 I

**JMB20211015 start insert - set enddate of shift
*
      IF begda_tmp IS NOT INITIAL AND
         endda_tmp IS NOT INITIAL.
        DATA(end_date_node) = xml_document->create_element( name = 'EndDate' ).
        DATA(oracle_endda) = /sew/cl_forms_utils=>convert_date_oracle( endda_tmp ).
        end_date_node->set_value( value = CONV #( oracle_endda ) ).
        shift_node->append_child( new_child = end_date_node ).
      ENDIF.
*JMB20211015 insert end

      begda_tmp = <work_schedule_times_tmp>-begda.
      endda_tmp = <work_schedule_times_tmp>-endda.
      CLEAR: shift_node.
      shift_node = xml_document->create_element( 'Shift' ).

      DATA(evt_num_node) = xml_document->create_element( name = 'EventNumber' ).
      evt_num = event_number && 'EN' && CONV string( counter ).
      CONDENSE evt_num.
      evt_num_node->set_value( value = evt_num ).
      shift_node->append_child( new_child = evt_num_node ).

**JMB20211015 start insert - set startdate of shift
*
      DATA(start_date_node) = xml_document->create_element( name = 'StartDate' ).
      DATA(oracle_begda) = /sew/cl_forms_utils=>convert_date_oracle( begda_tmp ).
      start_date_node->set_value( value = CONV #( oracle_begda ) ).
      shift_node->append_child( new_child = start_date_node ).
*JMB20211015 insert end

      event_node->append_child( new_child = shift_node ).
    ENDIF.
*JMB20211014 insert end

    shift_event_node = /sew/cl_work_schedule_changes=>create_shift_event_node( EXPORTING work_schedule_times = <work_schedule_times_tmp>
                                                                                         xml_document        = xml_document
                                                                                         request_number      = request_number
                                                                                         event_number        = evt_num
                                                                                         counter             = shift_evt_cnt ). "JMB20211009 I

    shift_evt_cnt = shift_evt_cnt + 1.  "JMB20211015 I

*    event_node->append_child( new_child = shift_event_node ).   "JMB20211015 D
    shift_node->append_child( new_child = shift_event_node ).   "JMB20211015 I

**JMB20211018 start insert - set enddate for last shift
*
    AT LAST.
      end_date_node = xml_document->create_element( name = 'EndDate' ).
      oracle_endda = /sew/cl_forms_utils=>convert_date_oracle( endda_tmp ).
      end_date_node->set_value( value = CONV #( oracle_endda ) ).
      shift_node->append_child( new_child = end_date_node ).
    ENDAT.
*JMB20211018 insert end
  ENDLOOP.

  xstring = xml->create_xstring_from_xml( xml = xml_document ).

ENDMETHOD.


  METHOD constructor.

    me->simu  = simu.
    me->dates = dates.
    msg_cont = /iwbep/cl_mgw_msg_container=>get_mgw_msg_container( ).
    "prepare protocol in case dialog is active and ALV is requested
    IF alv   EQ abap_true AND
       batch EQ abap_false.
      message_handler = cl_hrpay00_message_handler=>get_message_handler( ).
    ENDIF.

  ENDMETHOD.


METHOD create_and_store_xml_file.

  DATA: zip_tab      TYPE swxmlcont,
        xml_filename TYPE string,
        filename     TYPE  rlgrap-filename,
        xml_tab      TYPE TABLE OF char255,
        msg          TYPE string.

  me->xstring = me->build_xml( me->work_schedule_times ).

  CHECK me->xstring IS NOT INITIAL.
  xml_string = /sew/cl_int_xml_up=>create_string_from_xstring( xstring = me->xstring ).

  IF me->simu IS INITIAL.

    CALL FUNCTION 'RH_AUTHORITY_CHECK_OFF'.

    DATA(logical_filename) = CONV filename-fileintern('/SEW/HCM_ORACLE_EXTRACTS').

    xml_filename = me->create_filename( ).

    CALL FUNCTION 'FILE_GET_NAME'
      EXPORTING
        logical_filename = logical_filename
        parameter_1      = xml_filename
      IMPORTING
        file_name        = filename
      EXCEPTIONS
        file_not_found   = 1
        OTHERS           = 2.

    CHECK filename IS NOT INITIAL.
    CHECK sy-subrc IS INITIAL.

    OPEN DATASET filename MESSAGE msg FOR OUTPUT IN BINARY MODE.

    IF msg IS NOT INITIAL.
      MESSAGE msg TYPE 'E'.
    ELSE.
      CHECK sy-subrc IS INITIAL.
      TRANSFER me->xstring TO filename.
      CLOSE DATASET filename.
    ENDIF.
  ENDIF.
ENDMETHOD.


  METHOD create_filename.


*    LOOP AT me->dates ASSIGNING FIELD-SYMBOL(<line>).
**      DATA(oracle_begda) = /sew/cl_forms_utils=>convert_date_oracle( CONV #( <line>-low ) ).
**      DATA(oracle_endda) = /sew/cl_forms_utils=>convert_date_oracle( CONV #( <line>-high ) ).
*      DATA(oracle_begda) =  <line>-low .
*      DATA(oracle_endda) =  <line>-high .
*
*      EXIT.
*    ENDLOOP.
    DATA: timestamp TYPE timestampl.

    GET TIME STAMP FIELD timestamp.
    DATA(timestamp_c) = CONV string( timestamp ).

    CONCATENATE 'HCM_ORACLE_EXTRACTS_WORK_SCHEDULES'
*                oracle_begda
*                oracle_endda
                timestamp_c
                INTO filename SEPARATED BY '_'.

*    filename = filename && '.xml'.

  ENDMETHOD.


  METHOD create_request_node.


    rval = xml_document->create_element( 'Request' ).

    DATA(xml_single_node) = xml_document->create_element( 'Pernr' ).
    xml_single_node->set_value( value = CONV #( work_schedule_times-pernr ) ).
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'RequestNumber' ).
    TRY.
        request_number = cl_system_uuid=>create_uuid_x16_static( ).
        event_number   = request_number.
      CATCH cx_uuid_error.
        "error happened
    ENDTRY.

    IF request_number IS NOT INITIAL.
      xml_single_node->set_value( value =  request_number  ).
      rval->append_child( new_child = xml_single_node ).
    ELSE.
      " error
    ENDIF.

    xml_single_node = xml_document->create_element( name = 'RequestTime' ).
    DATA(timestamp) = /sew/cl_work_schedule_changes=>create_request_ts( ).
    xml_single_node->set_value( value = timestamp ).
    rval->append_child( new_child = xml_single_node ) .

    xml_single_node = xml_document->create_element( name = 'CloudPernr' ).
    DATA(cloud_pernr) = /sew/cl_forms_utils=>get_oraclepernr( work_schedule_times-pernr ).
    xml_single_node->set_value( value = CONV #( cloud_pernr ) ).
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'EventNumber' ).
    xml_single_node->set_value( value =  event_number ).
    rval->append_child( new_child = xml_single_node ).
  ENDMETHOD.


  METHOD create_request_ts.
    DATA: ts   TYPE timestampl,
          diff TYPE	tznutcdiff,
          sign TYPE	tznutcsign.


    GET TIME STAMP FIELD ts.
    DATA(ts_string) = CONV string( ts ). " 20210916132053.8646540 -> 2021-09-16 13_20_53.8646540

    CONCATENATE ts_string+0(4)
                ts_string+4(2)
                ts_string+6(2)
                INTO DATA(date_part) SEPARATED BY '-'.

    CONCATENATE ts_string+8(2)
                ts_string+10(2)
                ts_string+12(2)
                INTO DATA(time_part) SEPARATED BY ':'.

    CALL FUNCTION 'TZON_GET_OFFSET'
      EXPORTING
        if_timezone   = sy-zonlo
        if_local_date = sy-datlo
        if_local_time = sy-timlo
      IMPORTING
        ef_utcdiff    = diff
        ef_utcsign    = sign.

    timestamp = date_part && 'T' && time_part && '.' && ts_string+15(3) && sign && diff(2) && ':' && diff+2(2).
    "Format: yyyy-MM-ddThh:hh:hh.zzz+hh:mm

  ENDMETHOD.


METHOD create_shift_event_node.

    rval = xml_document->create_element( name = 'ShiftEvent' ).

**JMB20211015 start deletion - start and end date should be set on shift level
*
*    DATA(xml_single_node) = xml_document->create_element( name = 'StartDate' ).
*    DATA(oracle_begda) = /sew/cl_forms_utils=>convert_date_oracle( work_schedule_times-begda ).
*    xml_single_node->set_value( value = CONV #( oracle_begda ) ).
*    rval->append_child( new_child = xml_single_node ).
*
*    xml_single_node = xml_document->create_element( name = 'EndDate' ).
*    DATA(oracle_endda) = /sew/cl_forms_utils=>convert_date_oracle( work_schedule_times-endda ).
*    xml_single_node->set_value( value = CONV #( oracle_endda ) ).
*    rval->append_child( new_child = xml_single_node ).
*JMB20211015 deletion end

    DATA(xml_single_node) = xml_document->create_element( name = 'ReferenceDate' ).
    xml_single_node->set_value( value = CONV #( /sew/cl_forms_utils=>convert_date_oracle( work_schedule_times-endda ) ) ).
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'StartTime' ).
    DATA(shift_start_time) = /sew/cl_work_schedule_changes=>create_shift_time( date = work_schedule_times-begda
                                                                               time = work_schedule_times-begtm ).
    xml_single_node->set_value( value = CONV #( shift_start_time ) ).
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'EndTime' ).
    DATA(shift_end_time) = /sew/cl_work_schedule_changes=>create_shift_time( date = work_schedule_times-endda
                                                                             time = work_schedule_times-endtm ).
    xml_single_node->set_value( value = CONV #( shift_end_time ) ).
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'SAPCode' ) .
    xml_single_node->set_value( value = CONV #( work_schedule_times-sap_code ) ).

    xml_single_node = xml_document->create_element( name = 'OracleCode' ).
    xml_single_node->set_value( value = CONV #( work_schedule_times-oracle_code ) ).
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'OracleCategory' ).
    xml_single_node->set_value( value = CONV #( work_schedule_times-oracle_category ) ).
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'OracleType' ).
    xml_single_node->set_value( value = CONV #( work_schedule_times-oracle_type ) ).
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'EventNumber' ).
    xml_single_node->set_value( value = event_number ).
    rval->append_child( new_child = xml_single_node ).

**JMB20211009 insert start - add attributes to shift event node
*
    xml_single_node = xml_document->create_element( name = 'ShiftEventNumber' ).
    DATA(sevt_num) = event_number && 'SEN' && CONV string( counter ).
    CONDENSE sevt_num.
    xml_single_node->set_value( value = sevt_num ).
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'AttributeNumber' ).
    DATA(att_num) = event_number && 'AN' && CONV string( counter ).
    CONDENSE att_num.
    xml_single_node->set_value( value = att_num ).
    rval->append_child( new_child = xml_single_node ).

    xml_single_node = xml_document->create_element( name = 'Action' ).
    xml_single_node->set_value( value = 'CREATE' ).
    rval->append_child( new_child = xml_single_node ).
*JMB20211009 insert end

    xml_single_node = xml_document->create_element( name = 'RequestNumber' ).
    xml_single_node->set_value( value = request_number ).
    rval->append_child( new_child = xml_single_node ).

  ENDMETHOD.


  METHOD CREATE_SHIFT_TIME.
*    2018-07-02T18:00:00

    CONCATENATE date+0(4)
                date+4(2)
                date+6(2)
                INTO DATA(date_part)
                SEPARATED BY '-'.
    CONCATENATE time+0(2)
                time+2(2)
                time+4(2)
                INTO DATA(time_part)
                SEPARATED BY ':'.
    CONCATENATE date_part
                'T'
                time_part
                INTO shiftdatetime.

  ENDMETHOD.


  METHOD get_customizing.

    FIELD-SYMBOLS: <int_ws_map_tab> TYPE /sew/int_ws_map.

    IF int_ws_map_tab IS INITIAL.
      SELECT * FROM /sew/int_ws_map INTO TABLE int_ws_map_tab.
*    ELSE.
    ENDIF.

    LOOP AT int_ws_map_tab ASSIGNING <int_ws_map_tab> WHERE molga = molga
                                                        AND motpr = dayint-motpr
                                                        AND tprog = dayint-tprog
                                                        AND varia = dayint-varia
                                                        AND begda <= dayint-datum
                                                        AND endda >= dayint-datum.
      int_ws_map = <int_ws_map_tab>.
      EXIT.
    ENDLOOP.

    "second try with less information
    IF int_ws_map IS INITIAL.


      LOOP AT int_ws_map_tab ASSIGNING <int_ws_map_tab> WHERE molga = molga
                                                          AND motpr = dayint-motpr
                                                          AND tprog = dayint-tprog
                                                          AND begda <= dayint-datum
                                                          AND endda >= dayint-datum.
        int_ws_map = <int_ws_map_tab>.
        EXIT.
      ENDLOOP.

    ENDIF.

    "third try with less information
    IF int_ws_map IS INITIAL.


      LOOP AT int_ws_map_tab ASSIGNING <int_ws_map_tab> WHERE molga = molga
                                                          AND motpr = dayint-motpr
                                                          AND begda <= dayint-datum
                                                          AND endda >= dayint-datum.
        int_ws_map = <int_ws_map_tab>.
        EXIT.
      ENDLOOP.

    ENDIF.

*    ENDIF.

  ENDMETHOD.


  METHOD get_work_schedule.

    DATA: p0000 TYPE TABLE OF p0000,
          p0001 TYPE TABLE OF p0001,
          p0002 TYPE TABLE OF p0002,
          p0007 TYPE TABLE OF p0007,
          p2001 TYPE TABLE OF p2001,
          p2002 TYPE TABLE OF p2002,
          p2003 TYPE TABLE OF p2003,
          it    TYPE REF TO data.

    FIELD-SYMBOLS: <struk_new> TYPE any,
                   <struk_old> TYPE any.

    DATA: constraints TYPE ptarq_tconstr.

**JMB20211022 start insert - check for proceeded data from infotype maintainance
*
    "create infotype structure
    CONCATENATE 'P' pnnnn_new-infty INTO DATA(it_name).
    CREATE DATA it TYPE (it_name).
    ASSIGN it->* TO <struk_old>.
    ASSIGN it->* TO <struk_new>.

    cl_hr_pnnnn_type_cast=>prelp_to_pnnnn( EXPORTING prelp = pnnnn_old
                                           IMPORTING pnnnn = <struk_old> ).

    cl_hr_pnnnn_type_cast=>prelp_to_pnnnn( EXPORTING prelp = pnnnn_new
                                           IMPORTING pnnnn = <struk_new> ).
*JMB20211022 insert end

* read infotype 0000
    CALL METHOD /sew/cl_infotype_utils=>read_infotype_records
      EXPORTING
        pernr         = pernr
        infty         = '0000'
        begda         = begda
        endda         = endda
        no_auth_check = no_auth
      IMPORTING
        pnnnn         = p0000.

* read infotype 0001
    CALL METHOD /sew/cl_infotype_utils=>read_infotype_records
      EXPORTING
        pernr         = pernr
        infty         = '0001'
        begda         = if_hrpa_read_infotype=>low_date
        endda         = endda
        no_auth_check = no_auth
      IMPORTING
        pnnnn         = p0001.

* read infotype 0002
    CALL METHOD /sew/cl_infotype_utils=>read_infotype_records
      EXPORTING
        pernr         = pernr
        infty         = '0002'
        begda         = begda
        endda         = endda
        no_auth_check = no_auth
      IMPORTING
        pnnnn         = p0002.

    IF pnnnn_new-infty EQ '0007'.   "JMB20211022 I
      APPEND <struk_new> TO p0007.  "JMB20211022 I
    ELSE.
      "read infotype 0007
      CALL METHOD /sew/cl_infotype_utils=>read_infotype_records
        EXPORTING
          pernr         = pernr
          infty         = '0007'
          begda         = begda
          endda         = endda
          no_auth_check = no_auth
        IMPORTING
          pnnnn         = p0007.
    ENDIF.

* read infotype 2001
    CALL METHOD /sew/cl_infotype_utils=>read_infotype_records
      EXPORTING
        pernr         = pernr
        infty         = '2001'
        begda         = begda
        endda         = endda
        no_auth_check = no_auth
      IMPORTING
        pnnnn         = p2001.

* read infotype 2002
    CALL METHOD /sew/cl_infotype_utils=>read_infotype_records
      EXPORTING
        pernr         = pernr
        infty         = '2002'
        begda         = begda
        endda         = endda
        no_auth_check = no_auth
      IMPORTING
        pnnnn         = p2002.

* read infotype 2003
    IF pnnnn_new-infty EQ '2003'.   "JMB20211022 I
      APPEND <struk_new> TO p2003.  "JMB20211022 I
    ELSE.
      CALL METHOD /sew/cl_infotype_utils=>read_infotype_records
        EXPORTING
          pernr         = pernr
          infty         = '2003'
          begda         = begda
          endda         = endda
          no_auth_check = no_auth
        IMPORTING
          pnnnn         = p2003.
    ENDIF.


* read the current work schedule
    CALL FUNCTION 'HR_PERSONAL_WORK_SCHEDULE'
      EXPORTING
        pernr         = pernr
        begda         = begda
        endda         = endda
      TABLES
        i0000         = p0000
        i0001         = p0001
        i0002         = p0002
        i0007         = p0007
        i2001         = p2001
        i2002         = p2002
        i2003         = p2003
        perws         = perws
      EXCEPTIONS
        error_occured = 1
        abort_occured = 2
        OTHERS        = 3.

    IF sy-subrc <> 0.
      "Add error log

    ENDIF.

* determine the work schedule times
    CALL FUNCTION 'HR_WORK_SCHEDULE_TIMES'
      EXPORTING
        pernr         = pernr
        begda         = begda
        endda         = endda
*       KUG           = ' '
*       BREAK_OVERTIME                = '1'
*       REFRESH_INFOTYPE_BUFFER       = 'X'
*     IMPORTING
*       WARNING_OCCURED               =
      TABLES
        i0001         = p0001
        i0007         = p0007
        i2003         = p2003
*       I0049         =
        perws         = perws
        daygen        = daygen
        dayint        = dayint
      EXCEPTIONS
        error_occured = 1
        perws_error   = 2
        OTHERS        = 3.

    IF sy-subrc <> 0.
      " Implement suitable error handling here
    ENDIF.

  ENDMETHOD.


METHOD get_work_schedules_times.

  DATA: pernr           TYPE pernr_d,
        dayint_filtered TYPE pwsdayint_tab,
        break_time      TYPE syuzeit,
        break_beg       TYPE syuzeit,
        dayint_xx       TYPE pwsdayint,
        break_sec       TYPE i,
        break_begin     TYPE syuzeit,
        work_begin      TYPE syuzeit,
        dayint_40       TYPE pwsdayint,
        work_sec        TYPE i,
        beguz_initial   TYPE beguz,
        enduz_initial   TYPE enduz.

  CONSTANTS: no_auth    TYPE boole_d VALUE abap_true.

  FIELD-SYMBOLS: <no_auth> TYPE boole_d.

*  CLEAR rt_work_schedule.

  LOOP AT pernr_range ASSIGNING FIELD-SYMBOL(<pernr>).

    pernr = <pernr>-low.

    "get molga
    DATA(molga) = /sew/cl_int_utility=>get_molga( pernr = pernr begda = begda endda = endda ).

    "get work schedule
    get_work_schedule( EXPORTING pernr   = pernr
                                 begda   = begda
                                 endda   = endda
                                 no_auth = no_auth
                                 pnnnn_old = pnnnn_old "JMB20211022 I
                                 pnnnn_new = pnnnn_new "JMB20211022 I
                       IMPORTING   perws = DATA(perws)
                                  daygen = DATA(daygen)
                                  dayint = DATA(dayint) ).

    "get cloud id
    SELECT SINGLE * FROM pa9400 INTO @DATA(pa9400) WHERE pernr EQ @pernr
                                                     AND begda LE @endda
                                                     AND endda GE @begda.

    CHECK sy-subrc EQ 0.

**JMB20211022 start delete - check INTTYPE 20 (Normalarbeitszeit) for each day
*
*    "filter entries for inttype 20 and 40
*    DATA(intypes_range) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = COND #( WHEN line_exists( dayint[ pnorm = 'N' ] )
*                                                                                       THEN '20'
*                                                                                       ELSE '10' ) )
*                                              ( sign = 'I' option = 'EQ' low = '40' ) ).
*
*    LOOP AT dayint ASSIGNING FIELD-SYMBOL(<dayint_raw>) WHERE intyp IN intypes_range.
*      APPEND <dayint_raw> TO dayint_filtered.
*    ENDLOOP.
*JMB20211022 delete end, start insert

    DATA(intypes_range) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '10' )
                                              ( sign = 'I' option = 'EQ' low = '20' )
                                              ( sign = 'I' option = 'EQ' low = '40' ) ).

    LOOP AT dayint ASSIGNING FIELD-SYMBOL(<dayint_raw>) WHERE intyp IN intypes_range.
      APPEND <dayint_raw> TO dayint_filtered.
    ENDLOOP.

    DATA(dayint_20) = VALUE rsdsselopt_t( FOR <datum> IN dayint_filtered WHERE ( intyp EQ '20' ) ( sign = 'I' option = 'EQ' low = <datum>-datum ) ).

    DATA(intypes_10_20) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '10' )
                                              ( sign = 'I' option = 'EQ' low = '20' ) ).
*JMB20211022 insert end

    "Prepare work schedules to ignore
    DATA(ws_ignore) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = 'FREE' )
                                          ( sign = 'I' option = 'EQ' low = 'OFF'  )
                                          ( sign = 'I' option = 'EQ' low = 'FREI' ) ).

    "Prepare export structure
    SORT dayint_filtered ASCENDING BY datum intyp beguz stdaz.

    LOOP AT dayint_filtered ASSIGNING FIELD-SYMBOL(<dayint>).

      CHECK <dayint>-tprog NOT IN ws_ignore.

      DATA(int_ws_map) = me->get_customizing( dayint = <dayint> molga = molga ).
      IF int_ws_map IS INITIAL.
        "error -> no customizing.
        CONTINUE.
      ENDIF.

      IF <dayint>-intyp IN intypes_10_20.

        "Default: normal working time, otherwise scheduled working time
        CHECK ( <dayint>-datum NOT IN dayint_20  AND      "JMB20211022 I
                <dayint>-intyp EQ '10'              ) OR  "JMB20211022 I
                <dayint>-intyp EQ '20'.                   "JMB20211022 I

        APPEND INITIAL LINE TO work_schedule_times ASSIGNING FIELD-SYMBOL(<work_schedule_times>).
        MOVE-CORRESPONDING <dayint> TO dayint_xx. " save intyp line for dynamic work schedule
        CLEAR dayint_40.

        <work_schedule_times>-pernr = pernr.
        <work_schedule_times>-cloud_id = pa9400-oracleid.
        <work_schedule_times>-cloud_pernr = pa9400-oraclepernr.
        <work_schedule_times>-begda = <dayint>-datum.
        <work_schedule_times>-endda = <dayint>-datum.
        <work_schedule_times>-begtm = <dayint>-beguz.
        <work_schedule_times>-endtm = <dayint>-enduz.
        <work_schedule_times>-sap_code = <dayint>-tprog.
        <work_schedule_times>-oracle_code = int_ws_map-oracle_shift_code.
        <work_schedule_times>-oracle_category = int_ws_map-oracle_shift_category.
        <work_schedule_times>-oracle_type = int_ws_map-oracle_shift_type.
        <work_schedule_times>-vtken = <dayint>-vtken. "JMB20211015 I

      ELSEIF <dayint>-intyp EQ '40'.

        "adjust the endtime of the current entry.
        IF <work_schedule_times> IS ASSIGNED AND <work_schedule_times> IS NOT INITIAL.

          "create a new record an copy the existing entry
          APPEND INITIAL LINE TO work_schedule_times ASSIGNING FIELD-SYMBOL(<work_schedule_times_next>).
          MOVE-CORRESPONDING <work_schedule_times> TO <work_schedule_times_next>.


          " dynamic work schedule
          IF <dayint>-beguz IS INITIAL AND <dayint>-enduz IS INITIAL.

            IF dayint_40 IS INITIAL.
              " first intyp 40 line
              work_sec = <dayint>-stdaz * 3600.
              break_begin = dayint_xx-beguz.
              /sew/cl_work_schedule_changes=>add_time( EXPORTING
                                                         amount_in_seconds = work_sec
                                                       CHANGING
                                                         time = break_begin ).
              <work_schedule_times>-endtm = break_begin.

              work_begin = break_begin.
              break_sec = <dayint>-pdunb * 3600.
              /sew/cl_work_schedule_changes=>add_time( EXPORTING
                                                         amount_in_seconds = break_sec
                                                       CHANGING
                                                         time = work_begin ).
              <work_schedule_times_next>-begtm = work_begin.
              <work_schedule_times_next>-endtm = dayint_xx-enduz.

              MOVE-CORRESPONDING <dayint> TO dayint_40.

            ELSE.

              " n-th intyp 40 line with n > 1
              work_sec = <dayint>-stdaz * 3600.
              break_begin = dayint_xx-beguz.
              /sew/cl_work_schedule_changes=>add_time( EXPORTING
                                                         amount_in_seconds = work_sec
                                                       CHANGING
                                                         time = break_begin ).
              <work_schedule_times>-endtm = break_begin.

              work_begin = break_begin.

              break_sec = <dayint>-pdunb * 3600.
              /sew/cl_work_schedule_changes=>add_time( EXPORTING
                                                         amount_in_seconds = break_sec
                                                       CHANGING
                                                         time = work_begin ).
              <work_schedule_times_next>-begtm = work_begin.
              <work_schedule_times_next>-endtm = dayint_xx-enduz.
            ENDIF.
          ELSE.

            "set starttime of new entry to endtime of break.
*          <work_schedule_times_next>-begtm = <dayint>-enduz.


            DATA(new_time) = <dayint>-beguz.

            break_sec =  <dayint>-pdunb * 3600.

            MOVE break_sec TO break_time.
            ADD break_time TO new_time.

            <work_schedule_times_next>-begtm = new_time.


            "set endtime of existing entry to start of break.
            <work_schedule_times>-endtm = <dayint>-beguz.
          ENDIF.
          "finally reassign the field symbols
          ASSIGN <work_schedule_times_next> TO <work_schedule_times>.
          UNASSIGN <work_schedule_times_next>.

        ELSE.
          "problem"
          CONTINUE.
        ENDIF.

      ENDIF.

    ENDLOOP.

    CLEAR dayint_filtered.
    CLEAR pa9400.
  ENDLOOP.

  APPEND LINES OF work_schedule_times TO me->work_schedule_times.

ENDMETHOD.


  METHOD get_xstring.

    IF me->xstring IS NOT INITIAL.
      xml_xstring = me->xstring.
    ENDIF.

  ENDMETHOD.


  METHOD prepare_protocol.

    IF message_handler IS BOUND.

      "prepare layout options
      DATA(layout) = VALUE slis_layout_alv( zebra = 'X'
                                               colwidth_optimize = 'X' ).

      DATA: root_node TYPE hrpad_pal_node_key VALUE 'ROOT'.

      "Define structure of successful and failed IT_AEND entries ALV table
      message_handler->if_hrpay00_pal_services~create_fcat( EXPORTING i_structure_name = '/SEW/WORK_SCHEDULE_TIMES'
                                                            IMPORTING et_fcat          = DATA(fcat_bal_upd) ).

      "add post table
      IF me->work_schedule_times IS NOT INITIAL.
        message_handler->if_hrpay00_pal_services~add_table(
          EXPORTING
            i_parent_node_key = root_node
            it_fcat           = fcat_bal_upd
            it_append_table   = me->work_schedule_times
            is_layout         = layout
            i_node_txt        = COND string( WHEN simu EQ abap_true
                                             THEN TEXT-002
                                             ELSE TEXT-001 ) ).
      ENDIF.

      "add skipped pernr's
      IF skipped_pernrs IS NOT INITIAL.
        message_handler->if_hrpay00_pal_services~add_table(
        EXPORTING
          i_parent_node_key = root_node
          it_append_table   = skipped_pernrs
          is_layout         = layout
          i_node_txt        = COND string( WHEN simu EQ abap_true
                                             THEN TEXT-002
                                             ELSE TEXT-001 ) ).
      ENDIF.

      "add message table
      message_handler->if_hrpay00_pal_services~add_messages(
        EXPORTING
          i_parent_node_key = root_node
      ).

      "show pals
      CALL METHOD message_handler->display_pal.

    ENDIF.
*    FIELD-SYMBOLS: <append_table> TYPE any.
*
*    "Check message handler
*    IF message_handler IS BOUND.
*
*      "prepare layout options
*      DATA(layout) = VALUE slis_layout_alv( zebra = 'X'
*                                            colwidth_optimize = 'X' ).
*
*      DATA: root_node TYPE hrpad_pal_node_key VALUE 'ROOT'.
*
*      "Define structure of successful and failed IT_AEND entries ALV table
*      message_handler->if_hrpay00_pal_services~create_fcat( EXPORTING i_structure_name = '/SEW/WORK_SCHEDULES_TIMES'
*                                                            IMPORTING et_fcat          = DATA(fcat_work_schedules_times) ).
*
*      <append_table> = me->work_schedule_times.
*
*      message_handler->if_hrpay00_pal_services~add_table(
*        EXPORTING
*          i_parent_node_key = root_node
*          it_fcat           = fcat_work_schedules_times
*          it_append_table   = <append_table>
*          is_layout         = layout
*          i_node_txt        = COND string( WHEN simu EQ abap_true
*                                           THEN TEXT-002
*                                           ELSE TEXT-001 ) ).
*      "add skipped pernr's
*      IF skipped_pernrs IS NOT INITIAL.
*        message_handler->if_hrpay00_pal_services~add_table(
*        EXPORTING
*          i_parent_node_key = root_node
*          it_append_table   = skipped_pernrs
*          is_layout         = layout
*          i_node_txt        = COND string( WHEN simu EQ abap_true
*                                           THEN TEXT-002
*                                           ELSE TEXT-001 ) ).
*      ENDIF.
*
*      "add message table
*      message_handler->if_hrpay00_pal_services~add_messages(
*        EXPORTING
*          i_parent_node_key = root_node
*      ).
*
*      "show pals
*      CALL METHOD message_handler->display_pal.
*
*    ENDIF.

  ENDMETHOD.


METHOD submit_ws_rpt.
  DATA: it    TYPE REF TO data,
        pernr TYPE rsdsselopt_t.

  FIELD-SYMBOLS: <struk_new> TYPE any,
                 <struk_old> TYPE any.

  "create infotype structure
  CONCATENATE 'P' pnnnn_new-infty INTO DATA(it_name).
  CREATE DATA it TYPE (it_name).
  ASSIGN it->* TO <struk_old>.
  ASSIGN it->* TO <struk_new>.

  cl_hr_pnnnn_type_cast=>prelp_to_pnnnn( EXPORTING prelp = pnnnn_old
                                         IMPORTING pnnnn = <struk_old> ).

  cl_hr_pnnnn_type_cast=>prelp_to_pnnnn( EXPORTING prelp = pnnnn_new
                                         IMPORTING pnnnn = <struk_new> ).

  APPEND VALUE #( sign = 'I' option = 'EQ' low = pnnnn_new-pernr ) TO pernr.

  CASE pnnnn_new-infty.
    WHEN '0007'.

      ASSIGN COMPONENT 'SCHKZ' OF STRUCTURE <struk_old> TO FIELD-SYMBOL(<schkz_old>).
      ASSIGN COMPONENT 'SCHKZ' OF STRUCTURE <struk_new> TO FIELD-SYMBOL(<schkz_new>).

      CHECK <schkz_old> NE <schkz_new>.

      SUBMIT /sew/rp_work_schedules_changes WITH pnppernr IN pernr
                                            WITH pn-begda EQ pnnnn_new-begda
                                            WITH pn-endda EQ pnnnn_new-endda
                                            WITH pa_simu  EQ abap_false.

    WHEN '2003'.
      SUBMIT /sew/rp_work_schedules_changes WITH pnppernr IN pernr
                                            WITH pn-begda EQ pnnnn_new-begda
                                            WITH pn-endda EQ pnnnn_new-endda
                                            WITH pa_simu  EQ abap_false.
  ENDCASE.
ENDMETHOD.
ENDCLASS.
