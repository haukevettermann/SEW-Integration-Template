FUNCTION /sew/int_document_sender.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(DOCUMENT_TYPE) TYPE  CHAR1
*"  EXPORTING
*"     VALUE(ZIP_XSTRING) TYPE  XSTRING
*"     VALUE(STATUS) TYPE  CHAR1
*"     VALUE(ZIP_DEL_XSTRING) TYPE  XSTRING
*"----------------------------------------------------------------------

  DATA: dor              TYPE string,
        data             TYPE string,
        data_del         TYPE string,
        tmp_data         TYPE string,
        metadata         TYPE string,
        file_name        TYPE string,
        content_x        TYPE xstring,
        content_del_x    TYPE xstring,
        zip_tab          TYPE swxmlcont,
        zip_filename     TYPE string VALUE 'TimeStatements.zip',
        int_fo_aeup_mod  TYPE TABLE OF /sew/int_fo_aeup,
        int_fo_aeup_line TYPE /sew/int_fo_aeup.


  "read 100 rows per call
  SELECT * FROM /sew/int_fo_aeup WHERE otype  EQ @document_type
                                 AND   status EQ '01'
                                 INTO  TABLE  @DATA(int_fo_aeup)
                                 UP    TO     100 ROWS.

  CHECK sy-subrc EQ 0.

  "zip
  DATA(zip) = NEW cl_abap_zip( ).
  DATA(zip_del) = NEW cl_abap_zip( ). "JMB20211025 I

  metadata  = /sew/cl_forms_utils=>get_time_metadata( ).

  LOOP AT int_fo_aeup ASSIGNING FIELD-SYMBOL(<int_fo_aeup>).

**JMB20211025 start insert - create delete file, due to merge function isn´t working in Oracle
*
    DATA(tmp_data_del) = /sew/cl_forms_utils=>get_time_data( int_fo_aeup = <int_fo_aeup>
                                                             del         = abap_true ).

    CONCATENATE data_del
                cl_abap_char_utilities=>newline
                tmp_data_del
                INTO data_del.
*JMB20211025 insert end

    tmp_data = /sew/cl_forms_utils=>get_time_data( <int_fo_aeup> ).

    CONCATENATE data
                cl_abap_char_utilities=>newline
                tmp_data
                INTO data.

    file_name = 'BlobFiles/' && /sew/cl_forms_utils=>generate_file_name( pernr = <int_fo_aeup>-pernr
                                                                         begda = <int_fo_aeup>-begda
                                                                         otype = <int_fo_aeup>-otype ).

    zip->add( EXPORTING name    = file_name               " Name (von Groß-/Kleinschreibung abhängig)
                        content = <int_fo_aeup>-xstring ).

    MOVE-CORRESPONDING <int_fo_aeup> TO int_fo_aeup_line.
    int_fo_aeup_line-status = '02'.
    APPEND int_fo_aeup_line TO int_fo_aeup_mod.
  ENDLOOP.

  CONCATENATE metadata data INTO dor SEPARATED BY cl_abap_char_utilities=>newline.

  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text   = dor
    IMPORTING
      buffer = content_x.

  zip->add( name = 'DocumentsOfRecord.dat'
            content = content_x ).

  zip_xstring = zip->save( ).

  "status change
  CHECK sy-subrc EQ 0.

**JMB20211025 start insert - create deletion file for update of time statement
*
  CONCATENATE metadata data_del INTO DATA(dor_del) SEPARATED BY cl_abap_char_utilities=>newline.  "JMB20211025 I

  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text   = dor_del
    IMPORTING
      buffer = content_del_x.

  zip_del->add( name = 'DocumentsOfRecord.dat'
                content = content_del_x ).

  zip_del_xstring = zip_del->save( ).
*JMB20211025 insert end

  "status change
  CHECK sy-subrc EQ 0.

  MODIFY /sew/int_fo_aeup FROM TABLE int_fo_aeup_mod.
  COMMIT WORK.

  status = '02'.

*
*  CALL METHOD cl_gui_frontend_services=>gui_download
*    EXPORTING
*      filename = zip_filename
*      filetype = 'BIN'
*    CHANGING
*      data_tab = zip_tab.

  CHECK sy-subrc NE 0.
  " error handling.
  status = '03'.

ENDFUNCTION.
