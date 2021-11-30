FUNCTION /sew/int_report_receiver.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(XML_STRING) TYPE  XSTRING
*"     VALUE(FILE_NAME) TYPE  CHAR70
*"  EXPORTING
*"     VALUE(RESPONSE) TYPE  BAPIRET1
*"----------------------------------------------------------------------

  DATA: filename TYPE string,
        message  TYPE bapiret1,
        xml      TYPE REF TO if_ixml_document,
        file     TYPE string.

  file = file_name.
  CONDENSE file.
  SPLIT file AT '|' INTO DATA(name) DATA(date).
  REPLACE ALL OCCURRENCES OF ':' IN date WITH space.
  REPLACE ALL OCCURRENCES OF '-' IN date WITH space.
  CONDENSE date.
  CONDENSE name.

  CLEAR file.

  file = SWITCH #( file_name
                   WHEN '' THEN 'Filename_missing' && sy-datum && sy-uzeit
                   ELSE name && '.' && date ).
  CONDENSE file.

  CHECK xml_string IS NOT INITIAL.

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

  response-type = 'E'.
  TRY.
      OPEN DATASET filename FOR OUTPUT IN BINARY MODE.

      IF sy-subrc IS INITIAL.
        TRANSFER xml_string TO filename.
        CLOSE DATASET filename.
      ELSE.
        response-message = 'D002/400 Receiverflow hat Fehler beim Speichern des Files'.
        RETURN.
      ENDIF.
    CATCH cx_sy_file_authority INTO DATA(exception).
  ENDTRY.

  response-message = 'D002/400 Receiverflow hat Fehler beim Speichern des Files'.

  CHECK sy-subrc IS INITIAL.
  response-type = 'S'.
  response-message = 'D002/400 Receiverflow hat File erfolgreich gespeichert'.
ENDFUNCTION.
