*----------------------------------------------------------------------*
***INCLUDE /SEW/LTABLESI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  /SEW/INFOTYPE_FIELD_IMPORT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE /sew/infotype_field_import OUTPUT.
  TYPES BEGIN OF s_key_folder.
  TYPES mandt TYPE sy-mandt.
  TYPES otype TYPE otype.
  TYPES object_seqnr TYPE seqnr.
  TYPES molga TYPE molga.
  TYPES infty TYPE infty.
  TYPES seqnr TYPE seqnr.
  TYPES folder TYPE /sew/dd_folder.
  TYPES END OF s_key_folder.
  TYPES BEGIN OF s_key_fields.
  TYPES mandt TYPE sy-mandt.
  TYPES otype TYPE otype.
  TYPES object_seqnr TYPE seqnr.
  TYPES molga TYPE molga.
  TYPES infty TYPE infty.
  TYPES seqnr TYPE seqnr.
  TYPES folder TYPE /sew/dd_folder.
  TYPES field TYPE  /sew/dd_field.
  TYPES END OF s_key_fields.
  DATA:
    selected_rows TYPE salv_t_row,
    folder_key    TYPE s_key_folder,
    fields_keys   TYPE TABLE OF s_key_fields.
  DATA: BEGIN OF ls_infotyp.
          INCLUDE STRUCTURE /sew/int_infotyp.
          INCLUDE STRUCTURE vimtbflags.
        DATA: END OF ls_infotyp.
  DATA: lt_infotype LIKE TABLE OF ls_infotyp.
  FIELD-SYMBOLS <infotyp> LIKE ls_infotyp.
  CLEAR: lt_infotype, selected_rows.
  IF sy-ucomm = 'LOAD_FIELDS'.
    LOOP AT extract ASSIGNING FIELD-SYMBOL(<extract>).
      UNASSIGN <infotyp>.
      ASSIGN <extract> TO <infotyp> CASTING.
      IF <infotyp>-vim_mark = 'M'.
        APPEND <infotyp> TO lt_infotype.
      ENDIF.
    ENDLOOP.
    IF lines( lt_infotype ) <= 0.
      MESSAGE |Bitte Infotyp auswählen.| TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
    IF lines( lt_infotype ) > 1.
      MESSAGE |Bitte nur einen Infotyp auswählen.| TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
    DATA: folders TYPE TABLE OF sval.
    APPEND INITIAL LINE TO folders ASSIGNING FIELD-SYMBOL(<folder>).
    <folder>-tabname = '/SEW/INT_FOLDERS'.
    <folder>-fieldname  = 'FOLDER'.
    <folder>-fieldtext = TEXT-001.
    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
*       NO_VALUE_CHECK  = ' '
        popup_title     = 'TEST'
*       START_COLUMN    = '5'
*       START_ROW       = '5'
*     IMPORTING
*       RETURNCODE      =
      TABLES
        fields          = folders
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.
    READ TABLE folders INDEX 1 ASSIGNING <folder>.
    IF <folder>-value IS INITIAL.
      MESSAGE |Bitte einen Ordner auswählen (Groß- & Kleinschreibung beachten).| TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.
      DATA(folder) = CONV string( <folder>-value ).
    ENDIF.

    DATA: lv_popup_answer TYPE spar.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Änderungen gesichert?'(d01)
        text_question         = 'Aktuelle Änderungen gehen verloren! sind Sie sicher?'(d02)
        text_button_1         = 'Ja'(d03)
        text_button_2         = 'Abbrechen'(d04)
        icon_button_2         = 'ICON_CANCEL'
        default_button        = 1
        display_cancel_button = abap_false
        popup_type            = 'ICON_MESSAGE_WARNING'
      IMPORTING
        answer                = lv_popup_answer.
    IF lv_popup_answer = 2.
      EXIT.
    ENDIF.
    DATA(lo_customizing) = NEW /sew/cl_int_customizing(  ).
    LOOP AT lt_infotype ASSIGNING <infotyp>.
      lo_customizing->get_components( iv_infty = <infotyp>-infty iv_molga = <infotyp>-molga iv_object = <infotyp>-object ).
      lo_customizing->read_customizing( iv_folder = conv #( folder ) iv_infty = <infotyp>-infty iv_molga = <infotyp>-molga iv_object = <infotyp>-object iv_infty_seqnr = <infotyp>-seqnr iv_object_seqnr = <infotyp>-object_seqnr ).
      lo_customizing->build_table( iv_infty = <infotyp>-infty iv_molga = <infotyp>-molga iv_object = <infotyp>-object ).
      CLEAR: lo_customizing->gt_components, lo_customizing->gt_customizing.
    ENDLOOP.
* create alv class
    cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_popup_alv) CHANGING t_table = lo_customizing->gt_bapiret_wi ).
* header
    DATA(lo_header) = NEW cl_salv_form_layout_grid( ).
    lo_header->create_label( row = 1 column = 1 )->set_text( 'Import' ).
    lo_popup_alv->set_top_of_list( lo_header ).
* disable functions
    DATA(lo_functions) = lo_popup_alv->get_functions( ).
    lo_functions->set_default( abap_false ).
    "lo_functions->add_function(  name = 'CANC' icon = conv #( icon_close ) tooltip = conv #( text-cls )
    "   position = if_salv_c_function_position=>right_of_salv_functions ).
* columns
    DATA lo_column TYPE REF TO cl_salv_column_table.
    DATA(lo_columns) = lo_popup_alv->get_columns( ).
    lo_columns->set_optimize( abap_true ).
    lo_columns->get_column( 'TYPE' )->set_visible( abap_false ).
    lo_column ?= lo_columns->get_column( 'ICON' ).
    lo_column->set_icon( abap_true ).
* selection mode
    lo_popup_alv->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).
    lo_popup_alv->get_selections( )->set_selected_rows( selected_rows ).
* ALV as Popup
    lo_popup_alv->set_screen_popup( start_column = 30 end_column = 140 start_line = 5 end_line = 25 ).
    lo_popup_alv->set_screen_status( pfstatus = 'ST850' report = 'SAPLKKBL' ).
    DATA(lo_events) = lo_popup_alv->get_event( ).
    lcl_event_handler=>lo_popup = lo_popup_alv.
    SET HANDLER lcl_event_handler=>on_function_click FOR lo_events.
* Display
    lo_popup_alv->display( ).
    IF lcl_event_handler=>lv_cancelled = abap_false.
      DATA(object) = lt_infotype[ 1 ]-object.
      DATA(object_seqnr) = lt_infotype[ 1 ]-object_seqnr.
      DATA(infty) = lt_infotype[ 1 ]-infty.
      DATA(seqnr) = lt_infotype[ 1 ]-seqnr.
      DATA(molga) = lt_infotype[ 1 ]-molga.
*     check if folder exists
      SELECT SINGLE * FROM /sew/int_folders INTO @DATA(existing_folder) WHERE object = @object
                                                                               AND object_seqnr = @object_seqnr
                                                                               AND infty = @infty
                                                                               AND seqnr = @seqnr
                                                                               AND molga = @molga
                                                                               AND folder = @folder.
      IF sy-subrc IS NOT INITIAL.
        existing_folder-object = object.
        existing_folder-object_seqnr = object_seqnr.
        existing_folder-infty = infty.
        existing_folder-seqnr = seqnr.
        existing_folder-molga = molga.
        existing_folder-folder = folder.
        MODIFY /sew/int_folders FROM existing_folder.
        IF sy-subrc IS INITIAL.
          folder_key = CORRESPONDING #( existing_folder ).
          folder_key-mandt = sy-mandt.
        ENDIF.
      ENDIF.
* loop at selected entries
      CLEAR: selected_rows.
      selected_rows = lo_popup_alv->get_selections( )->get_selected_rows( ).
      CLEAR: lo_customizing->gt_customizing.
      LOOP AT selected_rows INTO DATA(lv_index).
        READ TABLE lo_customizing->gt_bapiret_wi INDEX lv_index ASSIGNING FIELD-SYMBOL(<ls_bapiret_selected>).
        APPEND INITIAL LINE TO lo_customizing->gt_customizing ASSIGNING FIELD-SYMBOL(<customizing>).
        <customizing>-mandt = sy-mandt.
        <customizing>-object = object.
        <customizing>-object_seqnr = object_seqnr.
        <customizing>-infty = infty.
        <customizing>-seqnr = seqnr.
        <customizing>-molga = molga.
        <customizing>-folder = folder.
        <customizing>-field = <ls_bapiret_selected>-field+6.
        APPEND INITIAL LINE TO fields_keys ASSIGNING FIELD-SYMBOL(<field_key>).
        <field_key> = CORRESPONDING #( <customizing> ).
      ENDLOOP.
      DATA(lines) = lines( lo_customizing->gt_customizing ).
      MODIFY /sew/int_fields FROM TABLE lo_customizing->gt_customizing.
      IF sy-subrc IS INITIAL.
        COMMIT WORK AND WAIT.
        MESSAGE |Update von | && lines && | Einträgen erfolgreich.| TYPE 'S' DISPLAY LIKE 'S'.
* leave to current screen to cleanup data and force reload
      ELSE.
        ROLLBACK WORK.
        MESSAGE |Fehler beim Update von | && lines && | Einträgen. Keine Daten gespeichert.| TYPE 'S' DISPLAY LIKE 'W'.
      ENDIF.
    ENDIF.
  ENDIF.
  FREE: lo_customizing.

*  IF folder_key IS NOT INITIAL OR fields_keys IS NOT INITIAL.
* create transport at the end
*    DATA:
*      table_e071k TYPE tr_keys,
**      e071k       LIKE LINE OF table_e071k,
*      table_ko200 TYPE STANDARD TABLE OF ko200,
*      ko200       LIKE LINE OF table_ko200,
*      timestamp   TYPE timestamp,
*      task        TYPE trkorr.
*
** view cluster transport entry
*    ko200-as4pos   = 1.
*    ko200-pgmid    = 'R3TR'.
*    ko200-object   = 'CDAT'.
*    ko200-obj_name = '/SEW/INTEGRATION'.
*    ko200-objfunc  = 'K'.
*    ko200-lang     = sy-langu.
*    APPEND ko200 TO table_ko200.
** report folder view customizing entry
*    IF folder_key IS NOT INITIAL.
*      e071k-trkorr     = ko200-trkorr.
*      e071k-pgmid      = ko200-pgmid.
*      e071k-object     = 'TABU'.
*      e071k-viewname   = '/SEW/INT_FO_V'.
*      e071k-objname    = '/SEW/INT_FOLDERS'.
*      e071k-as4pos     = 1.
*      e071k-mastertype = ko200-object.
*      e071k-mastername = ko200-obj_name.
*      e071k-lang       = sy-langu.
*      e071k-tabkey     = folder_key.
*      APPEND e071k TO table_e071k.
*    ENDIF.
*    IF fields_keys IS NOT INITIAL.
*      LOOP AT fields_keys INTO DATA(field_key).
** report main view customizing entry
*        e071k-trkorr     = ko200-trkorr.
*        e071k-pgmid      = ko200-pgmid.
*        e071k-object     = 'TABU'.
*        e071k-viewname   = '/SEW/INT_F_V'.
*        e071k-objname    = '/SEW/INT_FIELDS'.
*        e071k-as4pos     = 1.
*        e071k-mastertype = ko200-object.
*        e071k-mastername = ko200-obj_name.
*        e071k-lang       = sy-langu.
*        e071k-tabkey     = field_key.
*        APPEND e071k TO table_e071k.
*      ENDLOOP.
*    ENDIF.
** check object first
*    CALL FUNCTION 'TR_OBJECTS_CHECK'
*      EXPORTING
*        iv_no_standard_editor   = abap_true
*        iv_no_show_option       = abap_true
*      IMPORTING
*        we_task                 = task
*      TABLES
*        wt_ko200                = table_ko200
*        wt_e071k                = table_e071k
*      EXCEPTIONS
*        cancel_edit_other_error = 1
*        show_only_other_error   = 2
*        OTHERS                  = 3.
*
** success? add it
*    IF sy-subrc = 0.
*      CALL FUNCTION 'TR_OBJECTS_INSERT'
*        EXPORTING
*          wi_order                = ko200-trkorr
*          iv_no_standard_editor   = abap_true
*          iv_no_show_option       = abap_true
*        TABLES
*          wt_ko200                = table_ko200
*          wt_e071k                = table_e071k
*        EXCEPTIONS
*          cancel_edit_other_error = 1
*          show_only_other_error   = 2
*          OTHERS                  = 3.
*    ENDIF.
*  ELSE.
*    EXIT.
*  ENDIF.

* leave to current screen to cleanup data and force reload
  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
    WAIT UP TO 1 SECONDS.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  /SEW/INFOTYPE_FIELD_IMPORT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
