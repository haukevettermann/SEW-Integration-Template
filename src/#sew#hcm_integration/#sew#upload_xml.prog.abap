*&---------------------------------------------------------------------*
*& Report /SEW/UPLOAD_XML
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /sew/upload_xml.
DATA:
  path            TYPE string,
  filename        TYPE string,
  filename_length TYPE string,
  message         TYPE bapiret1,
  data            TYPE /sew/cl_int_xml=>tt_data,
  file_table      TYPE TABLE OF file_table,
  rc              TYPE i.
SELECTION-SCREEN BEGIN OF BLOCK frame1.
PARAMETERS: p_file TYPE string DEFAULT 'Worker_Extract.xml'.
PARAMETERS: p_rarch TYPE boole_d as CHECKBOX.
SELECTION-SCREEN   END OF BLOCK frame1.
** Get physical path
*DATA(logical_filename) = COND filename-fileintern( WHEN sy-sysid = 'D02' AND p_rarch = abap_false THEN '/SEW/HCM_ORACLE_EXTRACTS_DEV'
*                                                   WHEN sy-sysid = 'D02' AND p_rarch = abap_true THEN '/SEW/HCM_ORACLE_EXTRACTS_ARCHIVE_DEV'
*                                                   WHEN sy-sysid = 'Q02' AND p_rarch = abap_false THEN '/SEW/HCM_ORACLE_EXTRACTS_QS'
*                                                   WHEN sy-sysid = 'Q02' AND p_rarch = abap_true THEN '/SEW/HCM_ORACLE_EXTRACTS_ARCHIVE_QS'
*                                                   WHEN sy-sysid = 'P02' AND p_rarch = abap_true THEN '/SEW/HCM_ORACLE_EXTRACTS_PROD'
*                                                   WHEN sy-sysid = 'P02' AND p_rarch = abap_true THEN '/SEW/HCM_ORACLE_EXTRACTS_ARCHIVE_PROD' ).

DATA(logical_filename) = COND filename-fileintern( WHEN p_rarch = abap_false THEN /sew/cl_int_general_settings=>get_al11_down( )
                                                   WHEN p_rarch = abap_true THEN /sew/cl_int_general_settings=>get_al11_archive( ) ).
cl_gui_frontend_services=>file_open_dialog(
  CHANGING
    file_table              = file_table
    rc                      = rc
  EXCEPTIONS
    file_open_dialog_failed = 1                " Dialog: "Datei Öffnen" fehlgeschlagen
    cntl_error              = 2                " Controlfehler
*    error_no_gui            = 3                " Kein GUI verfügbar
    not_supported_by_gui    = 4                " Nicht unterstützt von GUI
    OTHERS                  = 5 ).
path = VALUE #( file_table[ 1 ]-filename OPTIONAL ).

CALL FUNCTION 'FILE_GET_NAME'
  EXPORTING
    logical_filename = logical_filename
    parameter_1      = p_file
  IMPORTING
    file_name        = filename
  EXCEPTIONS
    file_not_found   = 1
    OTHERS           = 2.
DATA(p_file_length) = p_file && 'length'.
CALL FUNCTION 'FILE_GET_NAME'
  EXPORTING
    logical_filename = logical_filename
    parameter_1      = p_file_length
  IMPORTING
    file_name        = filename_length
  EXCEPTIONS
    file_not_found   = 1
    OTHERS           = 2.
/sew/cl_int_xml=>upload_gui_file(
  EXPORTING
    path    = path
  IMPORTING
    length  = DATA(length)
    data    = data
    message = message ).
/sew/cl_int_xml=>convert_xml_to_xstring(
  EXPORTING
    length  = length
    data    = data
  IMPORTING
    xstring = DATA(xstring)
    message = message ).
*CALL FUNCTION '/SEW/INT_EXTRACT_RECEIVER'
*  EXPORTING
*    xml_string = xstring
*    file_name  = 'Test_T'
** IMPORTING
**   RESPONSE   =
*  .

OPEN DATASET filename FOR OUTPUT IN BINARY MODE.
IF sy-subrc IS INITIAL.
  TRANSFER xstring TO filename.
  CLOSE DATASET filename.
ELSE.
  MESSAGE 'Fehler beim Hochladen' TYPE 'S' DISPLAY LIKE 'E'.
ENDIF.
IF sy-subrc IS INITIAL.
  MESSAGE 'Erfolgreich hochgeladen' TYPE 'S'.
ENDIF.
