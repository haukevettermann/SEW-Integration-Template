class /SEW/CL_INT_ALV definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ts_message.
    TYPES pernr TYPE pernr_d.
    TYPES ename TYPE emnam.
    TYPES msgty TYPE char8.
    TYPES message TYPE string.
    TYPES END OF ts_message .
  types:
    tt_message TYPE TABLE OF ts_message .
  types TS_STATUS type ref to /SEW/CL_INT_STATUS_HANDLER .

* §1a. Define reference variables
  data G_MESSAGE_HANDLER type ref to CL_HRPAY00_MESSAGE_HANDLER .
  data TESTRUN type BOOLE_D .
  data ROOT_NODE_SUCCESS type HRPAD_PAL_NODE_KEY .
  data LAYOUT type SLIS_LAYOUT_ALV .
  data G_FIELDCAT_OM type SLIS_T_FIELDCAT_ALV .
  data ROOT_NODE_ERROR type HRPAD_PAL_NODE_KEY .
  data G_FIELDCAT_IT type SLIS_T_FIELDCAT_ALV .
  data G_FIELDCAT_MESSAGES type SLIS_T_FIELDCAT_ALV .
  data ROOT_NODE_MESSAGES type HRPAD_PAL_NODE_KEY .
  data G_FIELDCAT_ORA_BAL type SLIS_T_FIELDCAT_ALV .

  methods DEFINE_NEW_ALV
    importing
      !TABLE_TYPE type ANY
    returning
      value(FIELD_CAT) type SLIS_T_FIELDCAT_ALV .
  methods CONSTRUCTOR
    importing
      !TESTRUN type BOOLE_D .
  methods BUILD_ALV_STRUCTURE .
  methods BUILD_LOG .
  methods DISPLAY_ALV .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS /SEW/CL_INT_ALV IMPLEMENTATION.


  METHOD build_alv_structure.
    DATA:
      empl_node     TYPE hrpad_pal_node_key.
*    CALL METHOD g_message_handler->add_node
*      EXPORTING
*        i_parent_node_key = me->root_node_success
*        i_node_txt        = COND #( WHEN /sew/cl_int_statics=>it_aend IS NOT INITIAL THEN 'IT AEND erfolgreich'
*                                    WHEN /sew/cl_int_statics=>om_aend IS NOT INITIAL THEN 'OM AEND erfolgreich' )
*      IMPORTING
*        e_sub_node_key    = DATA(it_aend_successfull).
*    CALL METHOD g_message_handler->add_node
*      EXPORTING
*        i_parent_node_key = me->root_node_error
*        i_node_txt        = COND #( WHEN /sew/cl_int_statics=>it_aend IS NOT INITIAL THEN 'IT AEND erfolgreich'
*                                    WHEN /sew/cl_int_statics=>om_aend IS NOT INITIAL THEN 'OM AEND erfolgreich' )
*      IMPORTING
*        e_sub_node_key    = DATA(it_aend_error).
*    CALL METHOD g_message_handler->add_node
*      EXPORTING
*        i_parent_node_key = me->root_node_error
*        i_node_txt        = 'Nachrichten'
*      IMPORTING
*        e_sub_node_key    = DATA(messages).

**JMB20211017 start insert - add Oracle balances to display output
*
    IF /sew/cl_int_statics=>ora_bal IS NOT INITIAL.
      CALL METHOD g_message_handler->add_table
        EXPORTING
          i_rttc            = abap_true
          i_parent_node_key = me->root_node_success
          it_fcat           = me->g_fieldcat_it
          is_layout         = me->layout
          it_append_table   = /sew/cl_int_statics=>ora_bal
          i_node_txt        = 'Oracle balances erfolgreich'.
    ENDIF.
*JMB20211017 insert end

    IF /sew/cl_int_statics=>it_aend IS NOT INITIAL.
      CALL METHOD g_message_handler->add_table
        EXPORTING
          i_rttc            = abap_true
          i_parent_node_key = me->root_node_success
          it_fcat           = me->g_fieldcat_it
          is_layout         = me->layout
          it_append_table   = VALUE /sew/cl_int_statics=>t_it_aend( FOR it IN /sew/cl_int_statics=>it_aend WHERE ( status NE /sew/cl_int_constants=>booking_status-error_des ) ( it ) )
          i_node_txt        = 'IT AEND erfolgreich'.
      CALL METHOD g_message_handler->add_table
        EXPORTING
          i_rttc            = abap_true
          i_parent_node_key = me->root_node_error
          it_fcat           = me->g_fieldcat_it
          is_layout         = me->layout
          it_append_table   = VALUE /sew/cl_int_statics=>t_it_aend( FOR it IN /sew/cl_int_statics=>it_aend WHERE ( status EQ /sew/cl_int_constants=>booking_status-error_des ) ( it ) )
          i_node_txt        = 'IT AEND fehlerhaft'.
    ENDIF.
    IF /sew/cl_int_statics=>om_aend IS NOT INITIAL.
      CALL METHOD g_message_handler->add_table
        EXPORTING
          i_rttc            = abap_true
          i_parent_node_key = me->root_node_success
          it_fcat           = me->g_fieldcat_om
          is_layout         = me->layout
          it_append_table   = VALUE /sew/cl_int_statics=>t_om_aend( FOR om IN /sew/cl_int_statics=>om_aend WHERE ( status NE /sew/cl_int_constants=>booking_status-error_des ) ( om ) )
          i_node_txt        = 'OM AEND erfolgreich'.
      CALL METHOD g_message_handler->add_table
        EXPORTING
          i_rttc            = abap_true
          i_parent_node_key = me->root_node_error
          it_fcat           = me->g_fieldcat_om
          is_layout         = me->layout
          it_append_table   = VALUE /sew/cl_int_statics=>t_om_aend( FOR om IN /sew/cl_int_statics=>om_aend WHERE ( status EQ /sew/cl_int_constants=>booking_status-error_des ) ( om ) )
          i_node_txt        = 'OM AEND fehlerhaft'.
    ENDIF.
    IF /sew/cl_int_statics=>messages IS NOT INITIAL.
      CALL METHOD g_message_handler->add_table
        EXPORTING
          i_rttc            = abap_true
          i_parent_node_key = me->root_node_messages
          it_fcat           = me->g_fieldcat_messages
          is_layout         = me->layout
          it_append_table   = /sew/cl_int_statics=>messages
          i_node_txt        = 'Nachrichten'.
    ENDIF.
  ENDMETHOD.


  METHOD build_log.
    DATA:
      log          TYPE TABLE OF  solisti1,
      log_string   TYPE string,
      log_error    TYPE TABLE OF  solisti1,
      log_messages TYPE TABLE OF  solisti1.

    DATA: print_parameters TYPE pri_params,
          valid_flag       TYPE c LENGTH 1,
          lv_layout        TYPE pri_params-paart.
    lv_layout = 'X_65_255'.
    CALL FUNCTION 'GET_PRINT_PARAMETERS'
      EXPORTING
        no_dialog            = abap_true
        layout               = lv_layout
      IMPORTING
        out_parameters       = print_parameters
        valid                = valid_flag
      EXCEPTIONS
        invalid_print_params = 2
        OTHERS               = 4.
    NEW-PAGE WITH-TITLE PRINT ON PARAMETERS print_parameters NO DIALOG.
    IF /sew/cl_int_statics=>messages IS INITIAL AND /sew/cl_int_statics=>it_aend IS INITIAL AND /sew/cl_int_statics=>om_aend IS INITIAL.
      WRITE 'Keine Änderung'.
    ELSE.
      NEW-LINE.
      IF /sew/cl_int_statics=>it_aend IS NOT INITIAL.
        WRITE: 0'PERNR', 10'CLOUD ID', 26'MOLGA', 31'BEGDA', 40'ENDDA', 49'MASSN', 53'INFTY', 59'SUBTY', 65'DATA1'.
        LOOP AT /sew/cl_int_statics=>it_aend ASSIGNING FIELD-SYMBOL(<it_aend>).
          NEW-LINE.
          WRITE: / <it_aend>-pernr UNDER 'PERNR',
                   <it_aend>-cloud_id UNDER 'CLOUD ID',
                   <it_aend>-molga UNDER 'MOLGA',
                   <it_aend>-begda UNDER 'BEGDA',
                   <it_aend>-endda UNDER 'ENDDA',
                   <it_aend>-action UNDER 'MASSN',
                   <it_aend>-infty UNDER 'INFTY',
                   <it_aend>-subty UNDER 'SUBTY',
                   <it_aend>-data1 UNDER 'DATA1'.
        ENDLOOP.
      ENDIF.
      IF /sew/cl_int_statics=>om_aend IS NOT INITIAL.
        WRITE: 0'PERNR', 10'CLOUD ID', 26'MOLGA', 31'BEGDA', 40'ENDDA', 49'INFTY', 55'SUBTY', 61'VDATA'.
        LOOP AT /sew/cl_int_statics=>om_aend ASSIGNING FIELD-SYMBOL(<om_aend>).
          NEW-LINE.
          WRITE: / <om_aend>-sap_id UNDER 'PERNR',
                   <om_aend>-cloud_id UNDER 'CLOUD ID',
                   <om_aend>-molga UNDER 'MOLGA',
                   <om_aend>-begda UNDER 'BEGDA',
                   <om_aend>-endda UNDER 'ENDDA',
                   <om_aend>-infty UNDER 'INFTY',
                   <om_aend>-subty UNDER 'SUBTY',
                   <om_aend>-vdata UNDER 'VDATA1'.
        ENDLOOP.
      ENDIF.
      IF /sew/cl_int_statics=>messages IS NOT INITIAL.
        WRITE: 0'SIMULATION', 4'TYPE', 8'ID', 40'VAR 1', 60'VAR 2', 80'VAR 3', 100'VAR 4'.
        LOOP AT /sew/cl_int_statics=>messages ASSIGNING FIELD-SYMBOL(<messages>).
          NEW-LINE.
          WRITE: / <messages>-is_simu UNDER 'SIMULATION',
                   <messages>-type UNDER 'TYPE',
                   <messages>-id UNDER 'ID',
                   <messages>-message_v1 UNDER 'VAR 1',
                   <messages>-message_v2 UNDER 'VAR 2',
                   <messages>-message_v3 UNDER 'VAR 3',
                   <messages>-message_v4 UNDER 'VAR 4'.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    DATA: lv_is_layout       TYPE slis_layout_alv,
          ls_display_profile TYPE hrpad_pal_disp_prof,
          lt_expanded        TYPE hrpad_pal_t_header_nodes.
    DATA:
      lo_columns           TYPE REF TO cl_salv_columns_table,
      lo_aggregations      TYPE REF TO cl_salv_aggregations,
      lo_salv_table        TYPE REF TO cl_salv_table,
      lr_table             TYPE REF TO data,
      lt_slis_fieldcat_alv TYPE slis_t_fieldcat_alv.
    FIELD-SYMBOLS:
      <table>         TYPE table.
    me->testrun = testrun.

**JMB20211018 start insert - export logic to create alv
*
    g_fieldcat_om       = define_new_alv( table_type = /sew/cl_int_statics=>om_aend ).
    g_fieldcat_it       = define_new_alv( table_type = /sew/cl_int_statics=>it_aend ).
    g_fieldcat_messages = define_new_alv( table_type = /sew/cl_int_statics=>messages ).
    g_fieldcat_ora_bal  = define_new_alv( table_type = /sew/cl_int_statics=>ora_bal ).
*JMB20211018 insert end

    g_message_handler = cl_hrpay00_message_handler=>get_message_handler( ).
    CALL METHOD g_message_handler->if_hrpay00_pal_services~create_fcat
      EXPORTING
        i_structure_name = CONV #( '/SEW/INT_IT_AEND' )
      IMPORTING
        et_fcat          = g_fieldcat_it.
    CALL METHOD g_message_handler->add_node
      EXPORTING
        i_parent_node_key = 'ROOT'
        i_node_txt        = COND #( WHEN me->testrun = abap_false THEN 'Erfolgreich' ELSE 'Erfolgreich (Testlauf)' )
*       i_icon            =
      IMPORTING
        e_sub_node_key    = me->root_node_success.
    CALL METHOD g_message_handler->add_node
      EXPORTING
        i_parent_node_key = 'ROOT'
        i_node_txt        = COND #( WHEN me->testrun = abap_false THEN 'Fehlerhaft' ELSE 'Fehlerhaft (Testlauf)' )
*       i_icon            =
      IMPORTING
        e_sub_node_key    = me->root_node_error.
    CALL METHOD g_message_handler->add_node
      EXPORTING
        i_parent_node_key = 'ROOT'
        i_node_txt        = COND #( WHEN me->testrun = abap_false THEN 'Nachrichten' ELSE 'Nachrichten (Testlauf)' )
*       i_icon            =
      IMPORTING
        e_sub_node_key    = me->root_node_messages.

*   Create instance of message handler
    " Layout
    layout-zebra  = 'X'.
    layout-colwidth_optimize = 'X'.
  ENDMETHOD.


METHOD define_new_alv.
  DATA: lo_salv_table        TYPE REF TO cl_salv_table,
        lr_table             TYPE REF TO data,
        lt_slis_fieldcat_alv TYPE slis_t_fieldcat_alv.

  FIELD-SYMBOLS: <table>         TYPE table.

  CREATE DATA lr_table LIKE table_type.
  ASSIGN lr_table->* TO <table>.

  TRY.
      cl_salv_table=>factory( EXPORTING list_display = abap_false
                              IMPORTING r_salv_table = lo_salv_table
                              CHANGING  t_table      = <table> ).
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
  ENDTRY.

  DATA(lo_columns)      = lo_salv_table->get_columns( ).
  DATA(lo_aggregations) = lo_salv_table->get_aggregations( ).
  field_cat = CORRESPONDING #( cl_salv_controller_metadata=>get_lvc_fieldcatalog( r_columns      = lo_columns
                                                                                  r_aggregations = lo_aggregations ) ).
ENDMETHOD.


  METHOD display_alv.
    DATA:
          ls_display_profile TYPE hrpad_pal_disp_prof.
    ls_display_profile-flat = abap_false.
    ls_display_profile-tree_size = 25.
    g_message_handler->display_pal(
    is_display_profile = ls_display_profile ).
  ENDMETHOD.
ENDCLASS.
