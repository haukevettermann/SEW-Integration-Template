FUNCTION /sew/int_extract_receiver.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(XML_STRING) TYPE  XSTRING
*"     VALUE(FILE_NAME) TYPE  CHAR70
*"  EXPORTING
*"     VALUE(RESPONSE) TYPE  BAPIRET1
*"----------------------------------------------------------------------

*  response-message_v1 = 'D02/400 Receiverflow erreicht'.

  DATA:
    filename TYPE string,
    message  TYPE bapiret1,
    xml      TYPE REF TO if_ixml_document,
    file     TYPE string VALUE 'Worker_Extract.xml'.
  /sew/cl_int_xml=>create_xml_interface(
  EXPORTING
    xstring      = xml_string
  IMPORTING
    message      = DATA(ls_message)
    xml_document = xml ).
  DATA(object_collector) = NEW /sew/cl_int_object_collector( ).
  object_collector->set_full_xml( xml = xml ).
  object_collector->read_integration_id(
    EXPORTING
      integration_id_path = /sew/cl_int_constants=>integration_run_id_path
    IMPORTING
     message             = message
      integration_id      = DATA(int_id) ).
  file = file_name.
  CONDENSE file.
  SPLIT file AT '|' INTO DATA(name) DATA(date).
  REPLACE ALL OCCURRENCES OF ':' IN date WITH space.
  REPLACE ALL OCCURRENCES OF '-' IN date WITH space.
  CONDENSE date.
  CLEAR file.
  CONCATENATE name int_id INTO file SEPARATED BY '_'.
  CONCATENATE file date INTO file SEPARATED BY '.'.
  CONDENSE file.
  IF xml_string IS NOT INITIAL.
    DATA(logical_filename) = CONV filename-fileintern( '/SEW/HCM_ORACLE_EXTRACTS').
    CALL FUNCTION 'FILE_GET_NAME'
      EXPORTING
        logical_filename = logical_filename
        parameter_1      = file
      IMPORTING
        file_name        = filename
      EXCEPTIONS
        file_not_found   = 1
        OTHERS           = 2.
*    CALL FUNCTION 'AUTHORITY_CHECK_DATASET'
*      EXPORTING
**       PROGRAM          =
*        activity         = '36'
*        filename         = filename
*      EXCEPTIONS
*        no_authority     = 1
*        activity_unknown = 2
*        OTHERS           = 3.
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*    ENDIF.
    TRANSLATE filename TO UPPER CASE.
    TRY.
        OPEN DATASET filename FOR OUTPUT IN BINARY MODE.

        IF sy-subrc IS INITIAL.
          TRANSFER xml_string TO filename.
          CLOSE DATASET filename.
        ELSE.
          response-type = 'E'.
          response-message = 'D002/400 Receiverflow hat Fehler beim Speichern des Files'.
        ENDIF.
      CATCH cx_sy_file_authority INTO DATA(exception).
    ENDTRY.
    IF sy-subrc IS INITIAL.
      response-type = 'S'.
      response-message = 'D002/400 Receiverflow hat File erfolgreich gespeichert'.
*      MESSAGE 'D002/400 Receiverflow hat File erfolgreich gespeichert' TYPE 'S'.
    ELSE.
      response-type = 'E'.
      response-message = 'D002/400 Receiverflow hat Fehler beim Speichern des Files'.
    ENDIF.
  ELSE.
*    MESSAGE 'D002/400 Receiverflow XML ist leer' TYPE 'S' DISPLAY LIKE 'I'.
  ENDIF.

ENDFUNCTION.
