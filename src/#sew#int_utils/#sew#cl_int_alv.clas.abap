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

  data TESTRUN type BOOLE_D .
  data ALV_GRID_1 type ref to CL_GUI_ALV_GRID .
  data ALV_GRID_2 type ref to CL_GUI_ALV_GRID .
  data ALV_GRID_3 type ref to CL_GUI_ALV_GRID .
  data ERROR_ENTRIES_OM type /SEW/CL_INT_STATICS=>T_OM_AEND .
  data ERROR_ENTRIES_PA type /SEW/CL_INT_STATICS=>T_IT_AEND .
  data SUCCESSFUL_ENTRIES_PA type /SEW/CL_INT_STATICS=>T_IT_AEND .
  data SUCCESSFUL_ENTRIES_OM type /SEW/CL_INT_STATICS=>T_OM_AEND .

  events DOUBLE_CLICK .

  methods HANDLE_DOUBLE_CLICK
    for event DOUBLE_CLICK of CL_GUI_ALV_GRID .
  methods CONSTRUCTOR
    importing
      !TESTRUN type BOOLE_D .
  methods BUILD_ALV_STRUCTURE .
  methods BUILD_LOG .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /SEW/CL_INT_ALV IMPLEMENTATION.


  METHOD build_alv_structure.
    DATA: lo_gui_splitter TYPE REF TO cl_gui_splitter_container.
*    DATA: lo_alv_grid_1 TYPE REF TO cl_gui_alv_grid.
*    DATA: lo_alv_grid_2 TYPE REF TO cl_gui_alv_grid.
*    DATA: lo_alv_grid_3 TYPE REF TO cl_gui_alv_grid.
    DATA: lo_container_1 TYPE REF TO cl_gui_container.
    DATA: lo_container_2 TYPE REF TO cl_gui_container.
    DATA: lo_container_3 TYPE REF TO cl_gui_container.
    DATA: layout TYPE lvc_s_layo.
* Create splitter object
    CREATE OBJECT lo_gui_splitter
      EXPORTING
        parent                  = cl_gui_custom_container=>default_screen
        no_autodef_progid_dynnr = abap_true
        rows                    = 3
        columns                 = 1
      EXCEPTIONS
        cntl_error              = 1
        cntl_system_error       = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
* Instantiate Container
    lo_container_1 = lo_gui_splitter->get_container( row = 1 column = 1 ).
    lo_container_2 = lo_gui_splitter->get_container( row = 2 column = 1 ).
    lo_container_3 = lo_gui_splitter->get_container( row = 3 column = 1 ).
* Set width
    lo_gui_splitter->set_column_width( id = 1 width = 30 ).
* Instanciate ALV
    CREATE OBJECT alv_grid_1
      EXPORTING
        i_parent          = lo_container_1
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
    CREATE OBJECT alv_grid_2
      EXPORTING
        i_parent          = lo_container_2
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
    CREATE OBJECT alv_grid_3
      EXPORTING
        i_parent          = lo_container_3
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
* Create ALV and set for display
    me->successful_entries_pa = VALUE /sew/cl_int_statics=>t_it_aend( FOR it IN /sew/cl_int_statics=>it_aend WHERE ( status NE /sew/cl_int_constants=>booking_status-error_des ) ( it ) ).
    IF me->successful_entries_pa IS NOT INITIAL.

      layout-zebra = abap_true.
      layout-grid_title = 'Erfolgreich Deserialisiert'.
      alv_grid_1->set_table_for_first_display(
        EXPORTING
          i_structure_name = '/SEW/INT_IT_AEND'
          is_layout = layout
        CHANGING
          it_outtab = successful_entries_pa
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error = 2
          too_many_lines = 3 ).
      me->error_entries_pa = VALUE /sew/cl_int_statics=>t_it_aend( FOR it IN /sew/cl_int_statics=>it_aend WHERE ( status EQ /sew/cl_int_constants=>booking_status-error_des ) ( it ) ).
      layout-grid_title = 'Fehler bei Deserialisierung'.
      alv_grid_2->set_table_for_first_display(
        EXPORTING
          i_structure_name = '/SEW/INT_IT_AEND'
          is_layout = layout
        CHANGING
          it_outtab = error_entries_pa
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error = 2
          too_many_lines = 3 ).
    ELSE.
      me->successful_entries_om = VALUE /sew/cl_int_statics=>t_om_aend( FOR it_om IN /sew/cl_int_statics=>om_aend WHERE ( status NE /sew/cl_int_constants=>booking_status-error_des ) ( it_om ) ).

      layout-zebra = abap_true.
      layout-grid_title = 'Erfolgreich Deserialisiert'.
      alv_grid_1->set_table_for_first_display(
        EXPORTING
          i_structure_name = '/SEW/INT_OM_AEND'
          is_layout = layout
        CHANGING
          it_outtab = successful_entries_om
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error = 2
          too_many_lines = 3 ).
      me->error_entries_om = VALUE /sew/cl_int_statics=>t_om_aend( FOR it_om IN /sew/cl_int_statics=>om_aend WHERE ( status EQ /sew/cl_int_constants=>booking_status-error_des ) ( it_om ) ).
      layout-grid_title = 'Fehler bei Deserialisierung'.
      alv_grid_2->set_table_for_first_display(
        EXPORTING
          i_structure_name = '/SEW/INT_OM_AEND'
          is_layout = layout
        CHANGING
          it_outtab = error_entries_om
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error = 2
          too_many_lines = 3 ).
    ENDIF.

    layout-grid_title = 'Messages'.
    alv_grid_3->set_table_for_first_display(
      EXPORTING
        i_structure_name = '/SEW/INT_MSG_L'
        is_layout = layout
      CHANGING
        it_outtab = /sew/cl_int_statics=>messages
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error = 2
        too_many_lines = 3 ).
* Register events
*    SET HANDLER gref_event_receiver->handle_user_command FOR lo_alv_grid_2.
    SET HANDLER me->handle_double_click   FOR alv_grid_1.
    SET HANDLER me->handle_double_click   FOR alv_grid_2.

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

  ENDMETHOD.


  METHOD handle_double_click.
    DATA:
      rows      TYPE lvc_t_roid,
      prelp     TYPE prelp,
      prelp_tab TYPE TABLE OF prelp,
      wplog     TYPE wplog,
      wplog_tab TYPE TABLE OF wplog,
      it        TYPE REF TO data.
    FIELD-SYMBOLS: <table>       TYPE STANDARD TABLE.
    alv_grid_1->get_selected_rows(
      IMPORTING
        et_row_no     = rows ).
    IF rows IS NOT INITIAL.
      DATA(row) = rows[ 1 ].
      DATA(success_clicked) = abap_true.
      alv_grid_1->refresh_table_display( ).
    ELSE.
      alv_grid_2->get_selected_rows(
        IMPORTING
          et_row_no     = rows ).
      IF rows IS NOT INITIAL.
        row = rows[ 1 ].
        DATA(error_clicked) = abap_true.
        alv_grid_2->refresh_table_display( ).
      ENDIF.
    ENDIF.
    IF successful_entries_pa IS NOT INITIAL OR error_entries_pa IS NOT INITIAL.
      DATA(selected_entry) = COND /sew/int_it_aend(
                                      WHEN success_clicked = abap_true THEN VALUE #( successful_entries_pa[ row-row_id ] OPTIONAL )
                                      WHEN error_clicked = abap_true THEN VALUE #( error_entries_pa[ row-row_id ] OPTIONAL ) ).
      IF selected_entry IS INITIAL.
        MESSAGE 'Kein Eintrag gefunden' TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
      CLEAR: rows, row.
      MOVE-CORRESPONDING selected_entry TO prelp.
      APPEND prelp TO prelp_tab.        "
* Create infotype table
      CONCATENATE 'P' selected_entry-infty INTO DATA(infotype).
      CREATE DATA it TYPE TABLE OF (infotype).
      ASSIGN it->* TO <table>.
      cl_hr_pnnnn_type_cast=>prelp_to_pnnnn_tab(
        EXPORTING
          prelp_tab = prelp_tab
        IMPORTING
          pnnnn_tab = <table> ).
    ELSE.
      DATA(selected_entry_om) = COND /sew/int_om_aend(
                                      WHEN success_clicked = abap_true THEN VALUE #( successful_entries_om[ row-row_id ] OPTIONAL )
                                      WHEN error_clicked = abap_true THEN VALUE #( error_entries_om[ row-row_id ] OPTIONAL ) ).
      IF selected_entry_om IS INITIAL.
        MESSAGE 'Kein Eintrag gefunden' TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
      CLEAR: rows, row.
      MOVE-CORRESPONDING selected_entry_om TO wplog.
      APPEND wplog TO wplog_tab.        "
* Create infotype table

      "create infotype structure
      CONCATENATE 'P' selected_entry_om-infty INTO DATA(it_name).
      CREATE DATA it TYPE TABLE OF (it_name).
      ASSIGN it->* TO <table>.

      cl_hr_pnnnn_type_cast=>wplog_to_pnnnn_tab(
        EXPORTING
          wplog_tab = wplog_tab
        IMPORTING
          pnnnn_tab = <table> ).
    ENDIF.

    DATA go_alv TYPE REF TO cl_salv_table.
    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = go_alv
          CHANGING
            t_table      = <table> ).
      CATCH cx_salv_msg.
    ENDTRY.
    DATA: lr_functions TYPE REF TO cl_salv_functions_list.
    lr_functions = go_alv->get_functions( ).
    lr_functions->set_all( 'X' ).
    IF go_alv IS BOUND.
      go_alv->set_screen_popup(
        start_column = 25
        end_column  = 200
        start_line  = 1
        end_line    = 10 ).
      go_alv->display( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
