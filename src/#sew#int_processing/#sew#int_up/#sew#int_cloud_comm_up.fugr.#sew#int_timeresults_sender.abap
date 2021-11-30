FUNCTION /sew/int_timeresults_sender.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(OBJECT_TYPE) TYPE  CHAR1
*"  EXPORTING
*"     VALUE(XML_STRING) TYPE  XSTRING
*"     VALUE(STATUS) TYPE  /SEW/DD_STATUS
*"----------------------------------------------------------------------

  DATA: int_bal_upd TYPE /sew/tt_bal_upd,
        xml         TYPE REF TO cl_xml_document.

  CASE object_type.
    WHEN 'T'.
      SELECT * FROM /sew/int_bal_upd WHERE status = '01'
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

        MODIFY /sew/int_bal_upd FROM TABLE int_bal_upd.
        COMMIT WORK.
        status = '02'.
      ELSE.
        status = '03'.
      ENDIF.

*      cl_abap_browser=>show_xml(
*        EXPORTING
*          xml_xstring =  xml_string  ).
    WHEN 'R'.

  ENDCASE.

ENDFUNCTION.
