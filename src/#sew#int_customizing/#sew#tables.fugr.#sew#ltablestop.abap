* regenerated at 11.01.2021 12:31:47
FUNCTION-POOL /sew/tables                MESSAGE-ID sv.

* INCLUDE /SEW/LTABLESD...                   " Local class definition
INCLUDE lsvimdat                                . "general data decl.
INCLUDE /sew/ltablest00                         . "view rel. data dcl.


*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.

    CLASS-DATA:
      lo_popup     TYPE REF TO cl_salv_table,
      lv_cancelled TYPE boole_d.

    CLASS-METHODS on_function_click
      FOR EVENT if_salv_events_functions~added_function
        OF cl_salv_events_table IMPORTING e_salv_function.
ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_function_click.
    lv_cancelled = abap_false.

    CASE e_salv_function.
      WHEN 'GOON'.
        lo_popup->close_screen( ).
*       do action
      WHEN 'ABR'.
        lv_cancelled = abap_true.
        lo_popup->close_screen( ).
*       cancel
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
