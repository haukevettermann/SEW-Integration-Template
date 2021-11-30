*&---------------------------------------------------------------------*
*& Report /SEW/RP_ORA_BALANCE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /sew/rp_ora_balance.

TABLES: pernr,
        pa2006.

NODES: peras.

CONSTANTS: container TYPE c VALUE 'C_ALV',
           dynnr     TYPE syst_dynnr VALUE '9000'.

DATA: awr,
      r_pernr      TYPE rsdsselopt_t,
      ora_bal_comp TYPE /sew/cl_rp_ora_bal=>t_ora_bal_comp,
      o_ora_bal    TYPE REF TO /sew/cl_rp_ora_bal,
      selection    TYPE REF TO cl_salv_selections,
      salv_table   TYPE REF TO cl_salv_table.

*&---------------------------------------------------------------------*
*&       Class /SEW/events
*&---------------------------------------------------------------------*
*        Class to catch events fires by toolbar
*----------------------------------------------------------------------*
CLASS /sew/events DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: on_adt_clk FOR EVENT added_function OF cl_salv_events_table IMPORTING e_salv_function
                                                                                         sender.
ENDCLASS.

*&---------------------------------------------------------------------*
*&       Class (Implementation)  /SEW/events
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS /sew/events IMPLEMENTATION.
  METHOD on_adt_clk.
    selection = salv_table->get_selections( ).

    "check if rows were selected
    IF lines( selection->get_selected_rows( ) ) EQ 0.
      MESSAGE i040(/sew/hcm_integration).
      RETURN.
    ENDIF.

    "open confirmation dialog
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = TEXT-001
        text_question         = TEXT-002
        text_button_1         = 'Ja'(003)
        text_button_2         = 'Nein'(004)
        default_button        = '1'
        display_cancel_button = 'X'
      IMPORTING
        answer                = awr.

    "continue only when action is approved
    CHECK awr EQ '1'.

    "get relevant entries
    o_ora_bal->get_corrv_entries( sel_rows = selection->get_selected_rows( )
                                  bal_comp = ora_bal_comp ).

    "generate correction entries
    o_ora_bal->create_corrv_entries( ).

    "get request data
    salv_table->refresh( refresh_mode = if_salv_c_refresh=>full ).
  ENDMETHOD.
ENDCLASS.               "/SEW/events

"selection of further options
SELECTION-SCREEN BEGIN OF BLOCK sel_opt WITH FRAME TITLE TEXT-005.
SELECT-OPTIONS ktart FOR pa2006-ktart.
SELECTION-SCREEN END OF BLOCK sel_opt.

START-OF-SELECTION.

  "Restric pernr in case of no selection
  IF pnppernr[] IS INITIAL.
    pnppernr[] = NEW /sew/cl_int_ora_bal( )->get_peras( begda = pn-begda endda = pn-endda ).
  ENDIF.

  "clear range, so that get peras can fill the structure according auth check.
  REFRESH r_pernr[].

GET peras.
  "collect all pernr
  APPEND VALUE rsdsselopt( sign = 'I' option = 'EQ' low = pernr-pernr ) TO r_pernr.

END-OF-SELECTION.

  DATA(ora_bal) = NEW /sew/cl_int_ora_bal( ).

  "prepare range-tables
  DATA(r_ktart) = VALUE rsdsselopt_t( FOR s_ktart IN ktart[] ( sign   = s_ktart-sign
                                                               option = s_ktart-option
                                                               low    = s_ktart-low
                                                               high   = s_ktart-high ) ).

  DATA(bal_entries) = ora_bal->get( begda = pn-begda
                                    endda = pn-endda
                                    ktart = r_ktart
                                    pernr = r_pernr ).

  o_ora_bal = NEW /sew/cl_rp_ora_bal( pernr = r_pernr ).

  "get corresponding IT2006 entries
  DATA(p2006) = o_ora_bal->get_p2006( CHANGING ora_bal = bal_entries ).
  o_ora_bal->get_open_bookings( ).

  "collect data based on display type
  o_ora_bal->combine_entries( EXPORTING ora_bal      = bal_entries
                                        p2006        = p2006
                              IMPORTING ora_bal_comp = ora_bal_comp ).

  "Prepare ALV
  DATA(alv) = NEW cl_gui_docking_container( parent = cl_gui_container=>screen0
                                            ratio  = 90
                                            repid  = sy-repid
                                            dynnr  = dynnr ).

  cl_salv_table=>factory( EXPORTING r_container  = alv
                          IMPORTING r_salv_table = salv_table
                          CHANGING  t_table      = ora_bal_comp ).

  SET HANDLER /sew/events=>on_adt_clk FOR salv_table->get_event( ).

  o_ora_bal->prepare_alv( salv_table ).

  salv_table->display( ).

  "call screen
  CALL SCREEN 9000.

  INCLUDE /sew/ora_balance_o01.

  INCLUDE /sew/ora_balance_i01.
