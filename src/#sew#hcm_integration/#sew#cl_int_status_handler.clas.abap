class /SEW/CL_INT_STATUS_HANDLER definition
  public
  create public .

public section.

  types S_INSTANCES type ref to /SEW/CL_INT_STATUS_HANDLER .
  types:
    t_fields TYPE TABLE OF /sew/int_msg_f .
  types:
    t_instances TYPE TABLE OF s_instances WITH DEFAULT KEY .
  types:
    t_log TYPE TABLE OF /sew/int_msg_l .
  types:
    t_proc TYPE TABLE OF /sew/int_msg_p .
  types:
    BEGIN OF ty_field,
        infty     TYPE infty,
        subty     TYPE subty,
        field     TYPE char50,
        field_old TYPE char50,
        field_new TYPE char50,
        field4    TYPE char50,
      END OF ty_field .
  types:
    ty_fields TYPE TABLE OF ty_field .

  data MSG_CONT type ref to /IWBEP/IF_MESSAGE_CONTAINER .
  data IT_AEND type /SEW/TT_IT_AEND .
  data IT_AEND_ACTION type /SEW/TT_IT_AEND .
  data OM_AEND type /SEW/TT_OM_AEND .
  data LOG_MESSAGES type T_LOG .
  data CHANGED_VALUES type T_FIELDS .
  data SAP_ID type /SEW/DD_VALUE .
  data CLOUD_ID type /SEW/DD_VALUE .
  data CLOUD_PERNR type /SEW/DD_VALUE .
  data INT_RUN type GUID_32 .
  data AEND_ID type GUID_32 .
  data MOLGA type MOLGA .
  data BUKRS type BUKRS .
  data BEGDA type DATS .
  data ENDDA type DATS .
  class-data INSTANCES type T_INSTANCES .
  data NEW_IT_AEND type /SEW/TT_IT_AEND .
  data NEW_OM_AEND type /SEW/TT_OM_AEND .
  data SIMU_IT_AEND type /SEW/TT_IT_AEND .
  data SIMU_OM_AEND type /SEW/TT_OM_AEND .
  data NEW_LOG_MESSAGES type T_LOG .
  data NEW_LOG_MESSAGES_TMP type T_LOG .
  data NEW_CHANGED_VALUES type T_FIELDS .
  data NEW_PROCESSOR type T_PROC .
  data DEL_IT_AEND type /SEW/TT_IT_AEND .
  data DEL_OM_AEND type /SEW/TT_OM_AEND .
  data OBJECT_SEQ type SEQNR .
  data ORA_BAL type /SEW/TT_ORA_BAL .

  methods DELETE_OLD_RECORDS .
  methods CHECK_FOR_ERROR
    importing
      !TYPE type OTYPE
      !CLOUD_ID type /SEW/DD_VALUE
      !SAP_ID type /SEW/DD_VALUE
      !INT_RUN type GUID_32
      !EXT_TYPE type STRING optional
    exporting
      !HAS_ERROR type BOOLE_D .
  methods GET_PROCESSOR
    importing
      !BEGDA type DATS
      !ENDDA type DATS
      !LOG type /SEW/INT_MSG_L
    exporting
      !MSG_P type T_PROC .
  methods GET_LATEST_STATUS
    returning
      value(STATUS) type /SEW/DD_STATUS .
  methods GET_CHANGED_FIELDS
    importing
      !OTYPE type OTYPE
      !FIELDS type TY_FIELDS
      !IT_RECORD type ANY
      !INFTY type INFTY
      !AEND_ID type GUID_32
      !SIMULATION type BOOLE_D .
  methods GET_OVERALL_STATUS_OM
    returning
      value(STATUS) type /SEW/DD_STATUS .
  methods SET_FIELD_CHANGE
    importing
      !AEND_ID type GUID_32
      !FIELD_ID type GUID_32
      !VALUE_OLD type /SEW/DD_VALUE
      !VALUE_NEW type /SEW/DD_VALUE .
  methods SET_STATUS
    importing
      value(STATUS) type /SEW/DD_STATUS
      !AEND_ID type GUID_32 .
  methods SET_STATUS_ALL
    importing
      value(STATUS) type /SEW/DD_STATUS .
  methods ENHANCE_ENTRIES
    importing
      !AEND_ID type GUID_32 optional
    exporting
      value(IS_OK) type BOOLEAN
    changing
      !IT_AEND type /SEW/INT_IT_AEND optional
      !OM_AEND type /SEW/INT_OM_AEND optional
      !PROC_MSG type /SEW/INT_MSG_P optional .
  methods ADD_CHANGED_FIELDS
    importing
      !AEND_ID type GUID_32
      !INFTY type INFTY
      !FIELD type /SEW/DD_FIELD
      !SUBTY type SUBTY
      !VALUE_OLD type /SEW/DD_VALUE
      !VALUE_NEW type /SEW/DD_VALUE
      !VALUE_CHANGED type /SEW/DD_VALUE optional
      !EDITABLE type BOOLE_D
      !SIMULATION type BOOLE_D .
  methods PERSIST_DATA
    importing
      !SOURCE type STRING
      !SIMU type BOOLEAN optional
    returning
      value(IS_OK) type BOOLEAN .
  methods PERSIST_DATA_COCKPIT
    returning
      value(IS_OK) type BOOLEAN .
  methods PERSIST_DATA_POSTER
    returning
      value(IS_OK) type BOOLEAN .
  methods PERSIST_DATA_SERIALIZER
    returning
      value(IS_OK) type BOOLEAN .
  methods CONSTRUCTOR
    importing
      !SAP_ID type /SEW/DD_VALUE optional
      !CLOUD_ID type /SEW/DD_VALUE optional
      !CLOUD_PERNR type /SEW/DD_VALUE optional
      !INT_RUN type GUID_32 optional
      !MOLGA type MOLGA optional
      !SAP_ID_RNG type RSDSSELOPT_T optional
      !CLOUD_ID_RNG type RSDSSELOPT_T optional
      !INT_RUN_RNG type RSDSSELOPT_T optional
      !INFTY_RNG type RSDSSELOPT_T optional
      !STATUS_RNG type RSDSSELOPT_T optional
      !DATES_RNG type RSDSSELOPT_T optional
      !IT_AEND_ONLY type FLAG optional .
  class-methods GET_INSTANCE
    importing
      !SAP_ID type /SEW/DD_VALUE
      !CLOUD_ID type /SEW/DD_VALUE
      !CLOUD_PERNR type /SEW/DD_VALUE optional
      !INT_RUN type GUID_32
      !MOLGA type MOLGA
    returning
      value(INSTANCE) type ref to /SEW/CL_INT_STATUS_HANDLER .
  class-methods GET_ALL_INSTANCES
    returning
      value(INSTANCES) type T_INSTANCES .
  methods ADD_NEW_AEND
    importing
      !ORA_BAL type /SEW/INT_ORA_BAL optional
    changing
      !OM_AEND type /SEW/INT_OM_AEND optional
      !IT_AEND type /SEW/INT_IT_AEND optional .
  methods ADD_LOG_MESSAGE
    importing
      !AEND_ID type GUID_32
      !BAPIRET1 type BAPIRET1 optional
      !HRPAD_RETURN type HRPAD_RETURN_TAB optional
      !SIMU type BOOLE_D optional .
  methods MAP_BAPIRET1
    importing
      !BAPIRET1 type BAPIRET1
    changing
      !MESSAGE_LOG type /SEW/INT_MSG_L .
  methods MAP_HRPAD_RETURN
    importing
      !HRPAD_RETURN type HRPAD_RETURN
    changing
      !MESSAGE_LOG type /SEW/INT_MSG_L .
  class-methods GENERATE_GUID
    returning
      value(GUID) type GUID_32 .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /SEW/CL_INT_STATUS_HANDLER IMPLEMENTATION.


  METHOD add_changed_fields.
    APPEND INITIAL LINE TO me->new_changed_values ASSIGNING FIELD-SYMBOL(<changed_value>).
    <changed_value>-aend_id = aend_id.
    <changed_value>-field_id = /sew/cl_int_status_handler=>generate_guid( ).
    <changed_value>-field = field.
    <changed_value>-new_val = value_new.
    <changed_value>-old_val = value_old.
    <changed_value>-aedtm = sy-datum.
    <changed_value>-uname = sy-uname.
    <changed_value>-infty = infty.
    <changed_value>-subty = subty.
    <changed_value>-aend_id = aend_id.
    <changed_value>-is_simu = simulation.
  ENDMETHOD.


  METHOD add_log_message.
    CLEAR: me->new_log_messages_tmp.
*    CHECK bapiret1 IS NOT INITIAL.
    IF bapiret1 IS SUPPLIED.
      IF bapiret1-type IS NOT INITIAL.
*      CHECK bapiret1 IS NOT INITIAL.
        APPEND INITIAL LINE TO me->new_log_messages_tmp ASSIGNING FIELD-SYMBOL(<message>).
*     Map bapiret fields to log structure
        me->map_bapiret1(
          EXPORTING
            bapiret1    = bapiret1
          CHANGING
            message_log = <message> ).
        <message>-aend_id = COND #( WHEN me->aend_id IS NOT INITIAL THEN me->aend_id ELSE aend_id ).
        <message>-aedtm = sy-datum.
        <message>-uname = sy-uname.
        <message>-msg_guid = /sew/cl_int_status_handler=>generate_guid( ).
*        IF simu = abap_true.
        <message>-is_simu = simu.
*          ENDIF.
*      me->save_processor( ).
      ENDIF.
    ENDIF.
    IF hrpad_return IS SUPPLIED.
      IF hrpad_return IS NOT INITIAL.
*      CHECK hrpad_return IS NOT INITIAL.
*     Map hrpad return fields to log structure
        LOOP AT hrpad_return INTO DATA(hrpad_struc).
          IF hrpad_struc-type IS NOT INITIAL.
            APPEND INITIAL LINE TO me->new_log_messages_tmp ASSIGNING <message>.

            me->map_hrpad_return(
              EXPORTING
                hrpad_return    = hrpad_struc
              CHANGING
                message_log = <message> ).
            <message>-aend_id = COND #( WHEN me->aend_id IS NOT INITIAL THEN me->aend_id ELSE aend_id ).
            <message>-aedtm = sy-datum.
            <message>-uname = sy-uname.
            <message>-msg_guid = /sew/cl_int_status_handler=>generate_guid( ).
            <message>-is_simu = simu.
*        me->save_processor( ).
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
    "Add according processor
    IF me->new_log_messages_tmp IS NOT INITIAL.
      LOOP AT me->new_log_messages_tmp ASSIGNING <message>.
        me->get_processor( EXPORTING log = <message> begda = me->begda endda = me->endda IMPORTING msg_p = DATA(processors) ).
        LOOP AT processors ASSIGNING FIELD-SYMBOL(<processor>).
*      READ TABLE me->new_processor WITH KEY
*                                            int_run = <processor>-int_run
*                                            sap_id = <processor>-sap_id
*                                            cloud_id = <processor>-cloud_id
*                                            aend_id = <processor>-aend_id
*                                            msg_guid = <processor>-msg_guid
*                                            sbgrp_code = <processor>-sbgrp_code
*                                            sb_code = <processor>-sb_code
*                                            user_name = <processor>-user_name TRANSPORTING NO FIELDS.
          IF sy-subrc IS INITIAL.
            APPEND <processor> TO me->new_processor.
          ENDIF.
        ENDLOOP.
        CLEAR processors.
      ENDLOOP.
      APPEND LINES OF me->new_log_messages_tmp TO me->new_log_messages.
    ENDIF.
  ENDMETHOD.


METHOD add_new_aend.
**JMB20211017 start insert - store balances provided by Oracle
*
  IF ora_bal IS SUPPLIED.
    APPEND ora_bal TO me->ora_bal.
  ENDIF.
*JMB20211017 insert end

  IF it_aend IS SUPPLIED.
    me->enhance_entries(
      CHANGING
        it_aend = it_aend ).
    APPEND it_aend TO me->new_it_aend.
  ENDIF.
  IF om_aend IS SUPPLIED.
    me->enhance_entries(
      CHANGING
        om_aend = om_aend ).
    APPEND om_aend TO me->new_om_aend.
  ENDIF.
ENDMETHOD.


  METHOD check_for_error.
    IF type = /sew/cl_int_constants=>person.
      SELECT SINGLE * FROM /sew/int_it_aend INTO @DATA(it_aend) WHERE cloud_id = @cloud_id AND pernr = @sap_id
                                      AND status = @/sew/cl_int_constants=>booking_status-error AND int_run NE @int_run.
      IF it_aend IS NOT INITIAL.
        has_error = abap_true.
      ENDIF.
      SELECT SINGLE * FROM /sew/int_om_aend INTO @DATA(om_aend_check_pa) WHERE ( status = @/sew/cl_int_constants=>booking_status-initial "OR status = @/sew/cl_int_constants=>booking_status-error_des
                                    OR status = @/sew/cl_int_constants=>booking_status-error ) AND ext_type = 'DT'.
      IF om_aend_check_pa IS NOT INITIAL.
        has_error = abap_true.
      ENDIF.
    ELSEIF type = /sew/cl_int_constants=>position OR type = /sew/cl_int_constants=>orgunit.
      IF ext_type = 'D'.
        SELECT SINGLE * FROM /sew/int_om_aend INTO @DATA(om_aend) WHERE cloud_id = @cloud_id AND sap_id = @sap_id AND ext_type = @ext_type
                                        AND status = @/sew/cl_int_constants=>booking_status-error AND int_run NE @int_run.
        IF om_aend IS NOT INITIAL.
          has_error = abap_true.
        ENDIF.
      ELSEIF ext_type = 'DT'.
        SELECT SINGLE * FROM /sew/int_om_aend INTO @DATA(om_aend_dt) WHERE cloud_id = @cloud_id AND sap_id = @sap_id AND ext_type = @ext_type
                                        AND status = @/sew/cl_int_constants=>booking_status-error AND int_run NE @int_run.
        IF om_aend_dt IS NOT INITIAL.
          has_error = abap_true.
        ENDIF.
        SELECT SINGLE * FROM /sew/int_om_aend INTO @DATA(om_aend_check_dt) WHERE ( status = @/sew/cl_int_constants=>booking_status-initial "OR status = @/sew/cl_int_constants=>booking_status-error_des
                                      OR status = @/sew/cl_int_constants=>booking_status-error ) AND ext_type = 'D' AND subty ne 'B012'.
        IF om_aend_check_dt IS NOT INITIAL.
          has_error = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


METHOD constructor.

  DATA: status_rng_int TYPE rsdsselopt_t.

  TYPES range TYPE RANGE OF guid_32.

* Instantiate message container
  msg_cont = /iwbep/cl_mgw_msg_container=>get_mgw_msg_container( ).

* Serializer logic without ranges
  IF sap_id IS NOT INITIAL OR cloud_id IS NOT INITIAL OR int_run IS NOT INITIAL OR molga IS NOT INITIAL.

    "Set instance attributes
    me->sap_id = sap_id.
    me->cloud_id = cloud_id.
    me->int_run = int_run.
    me->molga = molga.
    me->cloud_pernr = cloud_pernr.

    "Select respective entries from it_aend and om_aend.
    SELECT * FROM /sew/int_it_aend WHERE int_run = @int_run
                                     AND pernr = @sap_id
                                     AND cloud_id = @cloud_id
                                     AND molga = @molga
             INTO TABLE @me->it_aend.

    SELECT * FROM /sew/int_om_aend WHERE int_run = @int_run
                                     AND sap_id = @sap_id
                                     AND cloud_id = @cloud_id
                                     AND molga = @molga
             INTO TABLE @me->om_aend.

* Booking report logic with ranges
  ELSE.

    "prepare default status logic
    IF status_rng IS INITIAL.
      status_rng_int = VALUE rsdsselopt_t( ( sign = 'I' option = 'NE' low = '02' high = '' )
                                           ( sign = 'I' option = 'NE' low = '05' high = '' )
                                           ( sign = 'I' option = 'NE' low = '07' high = '' )
                                           ( sign = 'I' option = 'NE' low = '03' high = '' ) ).
    ELSE.
      status_rng_int = status_rng.
    ENDIF.

    "get data from int_it_aend
    SELECT * FROM /sew/int_it_aend WHERE pernr    IN @sap_id_rng
                                     AND infty    IN @infty_rng
                                     AND status   IN @status_rng_int
                                     AND int_run  IN @int_run_rng
                                     AND cloud_id IN @cloud_id_rng
                                     AND ( begda  IN @dates_rng OR
                                           endda  IN @dates_rng )
             INTO TABLE @me->it_aend.

*    SELECT * FROM /sew/int_it_aend WHERE pernr    IN @sap_id_rng
*                                     AND infty    IN @infty_rng
*                                     AND status   IN @status_rng_int
*                                     AND int_run  IN @int_run_rng
*                                     AND cloud_id IN @cloud_id_rng
*                                     AND action <> @( VALUE #( ) )
*                                     AND ( begda  IN @dates_rng OR
*                                           endda  IN @dates_rng )
*             INTO TABLE @me->it_aend_action.

    "get data from int_om_aend
    IF it_aend_only = abap_false.
      SELECT * FROM /sew/int_om_aend WHERE sap_id   IN @sap_id_rng
                                       AND status   IN @status_rng_int
                                       AND int_run  IN @int_run_rng
                                       AND cloud_id IN @cloud_id_rng
                                       AND ( begda  IN @dates_rng OR
                                             endda  IN @dates_rng )
               INTO TABLE @me->om_aend.
    ENDIF.
  ENDIF.

* Build range of change ids based on tables for om and pa change
  DATA(range_aend_id) = CORRESPONDING range( BASE ( VALUE range( FOR change IN me->it_aend ( sign = 'I' option = 'EQ' low = change-aend_id ) ) )
                                                    VALUE range( FOR change IN me->it_aend ( sign = 'I' option = 'EQ' low = change-aend_id ) ) ).

* Select corresponding messages
  IF range_aend_id IS NOT INITIAL.
    SELECT * FROM /sew/int_msg_l WHERE aend_id IN @range_aend_id INTO TABLE @me->log_messages.
  ENDIF.

* Select corresponding field values
  IF range_aend_id IS NOT INITIAL.
    SELECT * FROM /sew/int_msg_f WHERE aend_id IN @range_aend_id INTO TABLE @me->changed_values.
  ENDIF.

ENDMETHOD.


  METHOD delete_old_records.
    DATA:
      aend_id_range TYPE rsdsselopt_t,
      aend_id       LIKE LINE OF aend_id_range.

    IF me->it_aend IS NOT INITIAL.
      LOOP AT me->it_aend INTO DATA(record_pa). "WHERE status = /sew/cl_int_constants=>booking_status-initial.
        CLEAR: aend_id.
        aend_id-low = record_pa-aend_id.
        aend_id-option = 'EQ'.
        aend_id-sign = 'I'.
        APPEND aend_id TO aend_id_range.
      ENDLOOP.
    ENDIF.

    IF me->om_aend IS NOT INITIAL.
      LOOP AT om_aend INTO DATA(record_om).
        CLEAR: aend_id.
        aend_id-low = record_om-aend_id.
        aend_id-option = 'EQ'.
        aend_id-sign = 'I'.
        APPEND aend_id TO aend_id_range.
      ENDLOOP.
    ENDIF.
    IF aend_id_range IS NOT INITIAL.
      SELECT * FROM /sew/int_msg_l INTO TABLE @DATA(msg_l) WHERE aend_id IN @aend_id_range.
      SELECT * FROM /sew/int_msg_f INTO TABLE @DATA(msg_f) WHERE aend_id IN @aend_id_range.
      SELECT * FROM /sew/int_msg_p INTO TABLE @DATA(msg_p) WHERE aend_id IN @aend_id_range.

      DELETE /sew/int_msg_l FROM TABLE msg_l.
      DELETE /sew/int_msg_p FROM TABLE msg_p.
      DELETE /sew/int_msg_f FROM TABLE msg_f.
    ENDIF.
  ENDMETHOD.


  METHOD enhance_entries.
    IF it_aend IS SUPPLIED.
*      DATA(data_transform) = /sew/cl_int_data_transform=>get_instance( ).
      it_aend-int_run = me->int_run.
      IF it_aend-pernr IS INITIAL.
        it_aend-pernr = me->sap_id.
      ENDIF.
      it_aend-cloud_id = me->cloud_id.
      it_aend-cloud_pernr = me->cloud_pernr.
*      it_aend-aend_id = /sew/cl_int_utility=>create_guid( ).
      it_aend-legal_entity = me->bukrs.
      it_aend-molga = me->molga.
      it_aend-uname = sy-uname.
      it_aend-aedtm = sy-datum.
      GET TIME STAMP FIELD it_aend-timestamp.
      IF it_aend-status IS INITIAL.
        it_aend-status = /sew/cl_int_constants=>booking_status-initial.
      ENDIF.
      IF it_aend-operation IS INITIAL.
        IF me->sap_id IS INITIAL.
          it_aend-operation = /sew/cl_int_constants=>infty_operation-pa_insert.
        ENDIF.
        IF me->sap_id IS INITIAL OR me->sap_id = '00000000'.
          it_aend-operation = /sew/cl_int_constants=>infty_operation-pa_insert.
        ELSEIF me->sap_id IS NOT INITIAL AND it_aend-endda = /sew/cl_int_constants=>highdate.
          it_aend-operation = /sew/cl_int_constants=>infty_operation-pa_insert.
        ENDIF.
        IF it_aend-endda NE /sew/cl_int_constants=>highdate.
          IF it_aend-action = /sew/cl_int_constants=>termination AND it_aend-infty = /sew/cl_int_constants=>it0000.
            it_aend-operation = /sew/cl_int_constants=>infty_operation-pa_insert.
          ELSE.
            IF it_aend-infty = /sew/cl_int_constants=>it0002 OR it_aend-infty = '9402'.
              it_aend-operation = /sew/cl_int_constants=>infty_operation-pa_copy.
            ELSE.
              it_aend-operation = /sew/cl_int_constants=>infty_operation-pa_delimit.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    IF om_aend IS SUPPLIED.
*      data_transform = /sew/cl_int_data_transform=>get_instance( ).
      om_aend-int_run = me->int_run.
      om_aend-sap_id = me->sap_id.
      om_aend-cloud_id = me->cloud_id.
*      om_aend-aend_id = /sew/cl_int_utility=>create_guid( ).
      om_aend-molga = me->molga.
      om_aend-legal_entity = me->bukrs.
      om_aend-istat = '1'.
      om_aend-plvar = /sew/cl_int_constants=>plvar.
      om_aend-uname = sy-uname.
      om_aend-aedtm = sy-datum.
      IF me->object_seq = '001'.
        om_aend-ext_type = 'DT'.
      ELSE.
        om_aend-ext_type = 'D'.
      ENDIF.
      GET TIME STAMP FIELD om_aend-timestamp.
      IF om_aend-status IS INITIAL.
        om_aend-status = /sew/cl_int_constants=>booking_status-initial.
      ENDIF.
      IF om_aend-operation IS INITIAL.
        IF me->sap_id IS INITIAL OR me->sap_id = '00000000'.
          om_aend-operation = /sew/cl_int_constants=>infty_operation-om_insert.
        ELSEIF me->sap_id IS NOT INITIAL AND om_aend-endda = /sew/cl_int_constants=>highdate.
          om_aend-operation = /sew/cl_int_constants=>infty_operation-om_modify.
        ENDIF.
        IF om_aend-endda NE /sew/cl_int_constants=>highdate.
          om_aend-operation = /sew/cl_int_constants=>infty_operation-om_delimit.
        ENDIF.
      ENDIF.
    ENDIF.
    IF proc_msg IS SUPPLIED.
*      data_transform = /sew/cl_int_data_transform=>get_instance( ).
      proc_msg-int_run = me->int_run.
      proc_msg-sap_id = me->sap_id.
      proc_msg-cloud_id = me->cloud_id.
      proc_msg-aend_id = COND #( WHEN me->aend_id IS NOT INITIAL THEN me->aend_id ELSE aend_id ).
*      proc_msg-aend_id = me->aend_id.
*      proc_msg-molga = me->molga.
      proc_msg-uname = sy-uname.
      proc_msg-aedtm = sy-datum.
      proc_msg-processor_guid = /sew/cl_int_utility=>create_guid( ).
*      GET TIME STAMP FIELD proc_msg-timestamp.
*      proc_msg-status = '01'.
    ENDIF.
  ENDMETHOD.


  METHOD generate_guid.
    TRY.
        guid = cl_system_uuid=>create_uuid_c32_static( ).
      CATCH cx_uuid_error INTO DATA(error).
    ENDTRY.
  ENDMETHOD.


  METHOD get_all_instances.
    instances = /sew/cl_int_status_handler=>instances.
  ENDMETHOD.


  METHOD get_changed_fields.
    DATA: msg_field      TYPE /sew/int_msg_f,
          msg_fields     TYPE /sew/tt_msg_f,
          field          LIKE LINE OF fields,
          lr_structdescr TYPE REF TO cl_abap_structdescr.
    FIELD-SYMBOLS: <fs_it_new> TYPE any,
                   <fs_subty>  TYPE any.

    ASSIGN it_record TO <fs_it_new>.

    lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV #( 'P' && infty ) ).
*    CREATE DATA lr_struc_new TYPE HANDLE lr_structdescr.
    DATA(components) = lr_structdescr->get_ddic_field_list( ).
    LOOP AT components INTO DATA(component) WHERE fieldname = 'SUBTY'.
      ASSIGN COMPONENT component-fieldname OF STRUCTURE <fs_it_new> TO <fs_subty>.
    ENDLOOP.

    LOOP AT fields ASSIGNING FIELD-SYMBOL(<fs_field>). "INTO field.
      me->add_changed_fields( aend_id = aend_id
                              infty = infty
                              subty = <fs_field>-subty
                              field = CONV #( <fs_field>-field )
                              value_old = CONV #( <fs_field>-field_old )
                              value_new = CONV #( <fs_field>-field_new )
                              simulation = simulation
                              editable = /sew/cl_int_customizing_xml=>get_instance(
                                           molga = me->molga
                                           object_type = otype
                                         )->check_field_changeable( field = conv #( <fs_field>-field ) ) ).

    ENDLOOP.

*    me->save_changed_fields( EXPORTING msg_fields = msg_fields IMPORTING is_ok = DATA(is_ok) ).

  ENDMETHOD.


  METHOD get_instance.
    LOOP AT instances ASSIGNING FIELD-SYMBOL(<instance>).
      IF <instance>->sap_id = sap_id
        AND <instance>->cloud_id = cloud_id
        AND <instance>->cloud_pernr = cloud_pernr
        AND <instance>->int_run = int_run
        AND <instance>->molga = molga.
        instance = <instance>.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF instance IS NOT BOUND.
      instance = NEW /sew/cl_int_status_handler(
        sap_id   = sap_id
        cloud_id = cloud_id
        cloud_pernr = cloud_pernr
        int_run  = int_run
        molga    = molga ).
      APPEND instance TO instances.
    ENDIF.
  ENDMETHOD.


  METHOD get_latest_status.
    SORT me->it_aend BY timestamp DESCENDING.
    SORT me->om_aend BY timestamp DESCENDING.
*   Get current status
    status = COND #( WHEN me->it_aend IS NOT INITIAL THEN VALUE #( me->it_aend[ 1 ]-status OPTIONAL )
                     WHEN me->om_aend IS NOT INITIAL THEN VALUE #( me->om_aend[ 1 ]-status OPTIONAL ) ).
  ENDMETHOD.


  METHOD get_overall_status_om.
*    select * from /sew/int_om_aend where status = /sew/cl_int_constants=>error.
  ENDMETHOD.


  METHOD get_processor.
    DATA: it0001_tab TYPE p0001_tab,
          pernr      TYPE pernr_d,
          infty      TYPE infty.

    "IT_AEND logic
    IF me->it_aend IS NOT INITIAL.
      IF me->sap_id IS NOT INITIAL.
        pernr = me->sap_id.
        infty = /sew/cl_int_constants=>it0001.
        CALL FUNCTION 'HR_READ_INFOTYPE'
          EXPORTING
            pernr     = pernr
            infty     = infty
            begda     = begda
            endda     = endda
          TABLES
            infty_tab = it0001_tab.
        IF it0001_tab IS NOT INITIAL.
          READ TABLE it0001_tab ASSIGNING FIELD-SYMBOL(<it0001>) INDEX 1.
          DATA(sachp) = <it0001>-sachp.
          DATA(sacha) = <it0001>-sacha.
          DATA(sachz) = <it0001>-sachz.
          DATA(sachgrp) = <it0001>-sbmod.
          APPEND INITIAL LINE TO msg_p ASSIGNING FIELD-SYMBOL(<msg_p>).
          IF sachgrp IS NOT INITIAL.
            <msg_p>-sbgrp_code = sachgrp.
            DATA(has_sachgrp) = abap_true.
          ENDIF.
          IF sachp IS NOT INITIAL.
            DATA(has_sachx) = abap_true.
            <msg_p>-sb_code = sachp.
          ELSEIF sacha IS NOT INITIAL.
            has_sachx = abap_true.
            <msg_p>-sb_code = sacha.
          ELSEIF sachz IS NOT INITIAL.
            has_sachx = abap_true.
            <msg_p>-sb_code = sachz.
          ELSE.
            DATA(no_sachx) = abap_true.
          ENDIF.
          IF no_sachx = abap_false.
            DATA(sbgrp_select) = COND #( WHEN has_sachgrp IS NOT INITIAL THEN <msg_p>-sbgrp_code ELSE '*' ).
            DATA(sachx_select) = COND #( WHEN has_sachx IS NOT INITIAL THEN <msg_p>-sb_code ELSE '*' ).
            SELECT * FROM /sew/int_superus INTO TABLE @DATA(superusers) WHERE sbgrp_code = @<msg_p>-sbgrp_code AND sb_code = @<msg_p>-sb_code .
            LOOP AT superusers ASSIGNING FIELD-SYMBOL(<superuser>).
              APPEND INITIAL LINE TO msg_p ASSIGNING <msg_p>.
              <msg_p>-user_name = <superuser>-uname.
              me->enhance_entries( CHANGING proc_msg = <msg_p> ).
            ENDLOOP.
          ELSE.
          ENDIF.
        ENDIF.
        IF <it0001> IS ASSIGNED.
          DATA(bukrs_select) = COND #( WHEN <it0001>-bukrs IS NOT INITIAL THEN <it0001>-bukrs
                                       WHEN me->bukrs IS NOT INITIAL THEN me->bukrs
                                       ELSE '*' ).
        ELSE.
          bukrs_select = '*'.
        ENDIF.
        SELECT * FROM /sew/int_superus INTO TABLE superusers WHERE bukrs = bukrs_select.
        LOOP AT superusers ASSIGNING <superuser>.
          APPEND INITIAL LINE TO msg_p ASSIGNING <msg_p>.
          <msg_p>-user_name = <superuser>-uname.
          me->enhance_entries( CHANGING proc_msg = <msg_p> ).
        ENDLOOP.
      ENDIF.
      IF superusers IS INITIAL.
        SELECT * FROM /sew/int_superus INTO TABLE superusers WHERE bukrs = '*' AND sbgrp_code = '*' AND sb_code = '*'.
        LOOP AT superusers ASSIGNING <superuser>.
          APPEND INITIAL LINE TO msg_p ASSIGNING <msg_p>.
          <msg_p>-user_name = <superuser>-uname.
          me->enhance_entries( CHANGING proc_msg = <msg_p> ).
        ENDLOOP.
      ENDIF.
*   OM_AEND logic
    ELSEIF me->om_aend IS NOT INITIAL.
      IF me->sap_id IS NOT INITIAL.
        bukrs_select = COND #( WHEN me->bukrs IS NOT INITIAL THEN me->bukrs
                               ELSE '*' ).
        SELECT * FROM /sew/int_superus INTO TABLE superusers WHERE bukrs = bukrs_select.
        LOOP AT superusers ASSIGNING <superuser>.
          APPEND INITIAL LINE TO msg_p ASSIGNING <msg_p>.
          <msg_p>-user_name = <superuser>-uname.
          me->enhance_entries( CHANGING proc_msg = <msg_p> ).
        ENDLOOP.
      ENDIF.
    ENDIF.

    "Serializer handling
    IF me->it_aend IS INITIAL AND me->om_aend IS INITIAL.
      bukrs_select = COND #( WHEN me->bukrs IS NOT INITIAL THEN me->bukrs
                            ELSE '*' ).
      SELECT * FROM /sew/int_superus INTO TABLE superusers WHERE bukrs = bukrs_select.
      LOOP AT superusers ASSIGNING <superuser>.
        APPEND INITIAL LINE TO msg_p ASSIGNING <msg_p>.
        <msg_p>-user_name = <superuser>-uname.
        me->enhance_entries( EXPORTING aend_id = log-aend_id CHANGING proc_msg = <msg_p> ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD map_bapiret1.
    message_log = CORRESPONDING #( bapiret1 ).
    message_log-num = bapiret1-number.
    IF message_log-message IS INITIAL.
      /sew/cl_int_utility=>get_message( EXPORTING msgid = message_log-id msgno = message_log-num msgv1 = message_log-message_v1 msgv2 = message_log-message_v2
                                                msgv3 = message_log-message_v3 msgv4 = message_log-message_v4
                                        IMPORTING message = message_log-message ).
    ENDIF.
  ENDMETHOD.


  METHOD map_hrpad_return.
    message_log = CORRESPONDING #( hrpad_return ).
    message_log-num = hrpad_return-number.
    /sew/cl_int_utility=>get_message( EXPORTING msgid = message_log-id msgno = message_log-num msgv1 = message_log-message_v1 msgv2 = message_log-message_v2
                                                msgv3 = message_log-message_v3 msgv4 = message_log-message_v4
                                  IMPORTING message = message_log-message ).
    "Get field list of Error Fields
    LOOP AT hrpad_return-field_list ASSIGNING FIELD-SYMBOL(<field>).
      IF message_log-message_v1 IS INITIAL.
        message_log-message_v1 = <field>.
      ELSEIF message_log-message_v2 IS INITIAL.
        message_log-message_v2 = <field>.
      ELSEIF message_log-message_v3 IS INITIAL.
        message_log-message_v3 = <field>.
      ELSEIF message_log-message_v4 IS INITIAL.
        message_log-message_v4 = <field>.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD persist_data.
    CASE source.
        "Serializer
      WHEN /sew/cl_int_constants=>serializer.
        is_ok = me->persist_data_serializer( ).
        "IT Poster
      WHEN /sew/cl_int_constants=>poster.
        is_ok = me->persist_data_poster( ).
        "INT Cockpit
      WHEN /sew/cl_int_constants=>cockpit.
        is_ok = me->persist_data_cockpit( ).
    ENDCASE.

    IF is_ok = abap_true.
      IF simu = abap_true.
        ROLLBACK WORK.
        "Set simu status
        IF me->simu_it_aend IS NOT INITIAL.
          MODIFY /sew/int_it_aend FROM TABLE me->simu_it_aend.

        ENDIF.
        IF me->simu_om_aend IS NOT INITIAL.
          MODIFY /sew/int_om_aend FROM TABLE me->simu_om_aend.
        ENDIF.
      ENDIF.
      "If not a testrun we need to clear old values
      IF source = /sew/cl_int_constants=>poster.
        me->delete_old_records( ).
      ENDIF.
      IF me->new_changed_values IS NOT INITIAL.
        MODIFY /sew/int_msg_f FROM TABLE me->new_changed_values.
      ENDIF.
      IF me->new_log_messages IS NOT INITIAL.
        MODIFY /sew/int_msg_l FROM TABLE me->new_log_messages.
      ENDIF.
      IF me->new_processor IS NOT INITIAL.
        MODIFY /sew/int_msg_p FROM TABLE me->new_processor.
      ENDIF.
    ENDIF.
*    IF simu NE abap_true.
    COMMIT WORK.
*    ENDIF.
  ENDMETHOD.


  METHOD persist_data_cockpit.

    IF me->new_it_aend IS NOT INITIAL.
      MODIFY /sew/int_it_aend FROM TABLE me->new_it_aend.
      IF sy-subrc IS INITIAL.
        is_ok = abap_true.
      ENDIF.
    ENDIF.
    IF me->it_aend IS NOT INITIAL.
      MODIFY /sew/int_it_aend FROM TABLE me->it_aend.
      IF sy-subrc IS INITIAL.
        is_ok = abap_true.
      ENDIF.
    ENDIF.

    IF me->new_om_aend IS NOT INITIAL.
      MODIFY /sew/int_om_aend FROM TABLE me->new_om_aend.
      IF sy-subrc IS INITIAL.
        is_ok = abap_true.
      ENDIF.
    ENDIF.
    IF me->om_aend IS NOT INITIAL.
      MODIFY /sew/int_om_aend FROM TABLE me->om_aend.
      IF sy-subrc IS INITIAL.
        is_ok = abap_true.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD persist_data_poster.

    IF me->del_it_aend IS NOT INITIAL.
      DELETE /sew/int_it_aend FROM TABLE me->del_it_aend.
      IF sy-subrc IS INITIAL.
        is_ok = abap_true.
      ENDIF.
    ENDIF.
    IF me->new_it_aend IS NOT INITIAL.
      MODIFY /sew/int_it_aend FROM TABLE me->new_it_aend.
      IF sy-subrc IS INITIAL.
        is_ok = abap_true.
      ENDIF.
    ENDIF.

    IF me->del_om_aend IS NOT INITIAL.
      DELETE /sew/int_om_aend FROM TABLE me->del_om_aend.
      IF sy-subrc IS INITIAL.
        is_ok = abap_true.
      ENDIF.
    ENDIF.
    IF me->new_om_aend IS NOT INITIAL.
      MODIFY /sew/int_om_aend FROM TABLE me->new_om_aend.
      IF sy-subrc IS INITIAL.
        is_ok = abap_true.
      ENDIF.
    ENDIF.

  ENDMETHOD.


METHOD persist_data_serializer.

**JMB20211017 start insert - store balances received from Oracle
*
  IF me->ora_bal IS NOT INITIAL.
    is_ok = NEW /sew/cl_int_ora_bal( )->insert( me->ora_bal ).
  ENDIF.
*JMB20211017 insert end

  IF me->new_it_aend IS NOT INITIAL.
    MODIFY /sew/int_it_aend FROM TABLE me->new_it_aend.
    IF sy-subrc IS INITIAL.
      is_ok = abap_true.
    ENDIF.
  ENDIF.

  IF me->it_aend IS NOT INITIAL.
    MODIFY /sew/int_it_aend FROM TABLE me->it_aend.
    IF sy-subrc IS INITIAL.
      is_ok = abap_true.
    ENDIF.
  ENDIF.

  IF me->new_om_aend IS NOT INITIAL.
    MODIFY /sew/int_om_aend FROM TABLE me->new_om_aend.
    IF sy-subrc IS INITIAL.
      is_ok = abap_true.
    ENDIF.
  ENDIF.

  IF me->om_aend IS NOT INITIAL.
    MODIFY /sew/int_om_aend FROM TABLE me->om_aend.
    IF sy-subrc IS INITIAL.
      is_ok = abap_true.
    ENDIF.
  ENDIF.
ENDMETHOD.


  METHOD set_field_change.
    DATA:
          it          TYPE REF TO data.
    FIELD-SYMBOLS: <struc>    TYPE any.
*   Set status it_aend
    DATA(it_aend) = VALUE /sew/int_it_aend( me->it_aend[ aend_id = aend_id ] OPTIONAL ).
    IF it_aend IS INITIAL.
      DATA(bapiret1) = VALUE bapiret1( type = 'E' id = /sew/cl_int_constants=>msg_class_int number = 004 ).
      me->add_log_message(
        EXPORTING
          aend_id      = aend_id
          bapiret1     = bapiret1 ).
    ELSE.
*   Auslesen Tabelle mit Feldwerten
      READ TABLE me->changed_values WITH KEY field_id = field_id INTO DATA(changed_field).
      changed_field-changed_val = value_new.
*       Create infotype structure
      CONCATENATE 'P' it_aend-infty INTO DATA(it_name).
      CONCATENATE 'P' it_aend-infty '-' changed_field-field INTO DATA(it_field).
      CREATE DATA it TYPE (it_name).
      ASSIGN it->* TO <struc>.
      DATA(prelp) = CORRESPONDING prelp( it_aend ).
      cl_hr_pnnnn_type_cast=>prelp_to_pnnnn(
        EXPORTING
          prelp = prelp
        IMPORTING
          pnnnn = <struc> ).
      ASSIGN COMPONENT changed_field-field OF STRUCTURE <struc> TO FIELD-SYMBOL(<field>).
      IF <field> IS ASSIGNED AND <field> = value_old."ToDo Data Conversion "conv"?
*       Set new field value
*        field = CONV it_field( new_value ).
        <field> = value_new.
*       Transform to it_aend
        /sew/cl_int_type_cast=>pnnnn_to_prelp(
          EXPORTING
            pnnnn   = <struc>
            aend_id = aend_id
          IMPORTING
            prelp   = DATA(sew_prelp) ).
        it_aend = CORRESPONDING #( sew_prelp ).
        it_aend-status = /sew/cl_int_constants=>booking_status-manual.
        APPEND it_aend TO me->new_it_aend.
*       Kein neuer Eintrag. Update des bestehenden.
        APPEND changed_field TO me->new_changed_values.
      ELSE.
        bapiret1 = VALUE bapiret1( type = 'E' id = /sew/cl_int_constants=>msg_class_int number = 005 ).
        me->add_log_message(
          EXPORTING
            aend_id      = aend_id
            bapiret1     = bapiret1 ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD set_status.
*   Set status it_aend
    DATA(it_aend) = VALUE /sew/int_it_aend( me->it_aend[ aend_id = aend_id ] OPTIONAL ).
    IF it_aend IS NOT INITIAL.
      it_aend-status = status.
      APPEND it_aend TO me->new_it_aend.
    ENDIF.
*   Set status om_aend
    DATA(om_aend) = VALUE /sew/int_om_aend( me->om_aend[ aend_id = aend_id ] OPTIONAL ).
    IF om_aend IS NOT INITIAL.
      om_aend-status = status.
      APPEND om_aend TO me->new_om_aend.
    ENDIF.
*    LOOP AT me->it_aend INTO DATA(it_aend).
*      it_aend-status = status.
*      it_aend-aedtm = sy-datum.
*      it_aend-uname = sy-uname.
*    ENDLOOP.
*    LOOP AT me->om_aend INTO DATA(om_aend).
*      om_aend-status = status.
*      om_aend-aedtm = sy-datum.
*      om_aend-uname = sy-uname.
*      APPEND om_aend TO me->new_om_aend.
*    ENDLOOP.
  ENDMETHOD.


  METHOD set_status_all.
*   Set status it_aend for all entries
    IF me->it_aend IS NOT INITIAL.
      LOOP AT me->it_aend INTO DATA(it_aend).
        it_aend-status = status.
        it_aend-aedtm = sy-datum.
        it_aend-uname = sy-uname.
      ENDLOOP.
    ENDIF.
    IF me->om_aend IS NOT INITIAL.
      LOOP AT me->om_aend INTO DATA(om_aend).
        om_aend-status = status.
        om_aend-aedtm = sy-datum.
        om_aend-uname = sy-uname.
        APPEND om_aend TO me->new_om_aend.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
