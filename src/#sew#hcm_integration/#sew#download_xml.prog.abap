*&---------------------------------------------------------------------*
*& Report /SEW/UPLOAD_XML
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /sew/download_xml.
DATA ref_ixml TYPE REF TO if_ixml .
DATA ref_ixml_document TYPE REF TO if_ixml_document .
SELECTION-SCREEN BEGIN OF BLOCK frame1.
PARAMETERS: p_file TYPE string DEFAULT 'Worker_Extract.xml'.
PARAMETERS: p_rarch TYPE boole_d as CHECKBOX.
PARAMETERS: p1 TYPE boole_d RADIOBUTTON GROUP 1.
PARAMETERS: p2 TYPE boole_d RADIOBUTTON GROUP 1.
SELECTION-SCREEN   END OF BLOCK frame1.
DATA:
  path       TYPE string,
  string     TYPE string,
  xstring    TYPE xstring,
  filename   TYPE string,
  convin     TYPE REF TO cl_abap_conv_in_ce,
  message    TYPE bapiret1,
  data       TYPE /sew/cl_int_xml=>tt_data,
  file_table TYPE TABLE OF file_table,
  rc         TYPE i,
  xml        TYPE REF TO if_ixml_document,
  binary_tab TYPE solix_tab,
  length     TYPE i,
  file_list  TYPE TABLE OF epsfili,
  mask       TYPE epsf-epsfilnam.
** Get physical path
*DATA(logical_filename) = COND filename-fileintern( WHEN sy-sysid = 'D02' AND p_rarch = abap_false THEN '/SEW/HCM_ORACLE_EXTRACTS_DEV'
*                                                   WHEN sy-sysid = 'D02' AND p_rarch = abap_true THEN '/SEW/HCM_ORACLE_EXTRACTS_ARCHIVE_DEV'
*                                                   WHEN sy-sysid = 'Q02' AND p_rarch = abap_false THEN '/SEW/HCM_ORACLE_EXTRACTS_QS'
*                                                   WHEN sy-sysid = 'Q02' AND p_rarch = abap_true THEN '/SEW/HCM_ORACLE_EXTRACTS_ARCHIVE_QS'
*                                                   WHEN sy-sysid = 'P02' AND p_rarch = abap_true THEN '/SEW/HCM_ORACLE_EXTRACTS_PROD'
*                                                   WHEN sy-sysid = 'P02' AND p_rarch = abap_true THEN '/SEW/HCM_ORACLE_EXTRACTS_ARCHIVE_PROD' ).

DATA(logical_filename) = COND filename-fileintern( when p_rarch = abap_false THEN /sew/cl_int_general_settings=>get_al11_down( )
                                                   when p_rarch = abap_true THEN /sew/cl_int_general_settings=>get_al11_archive( ) ).
CALL FUNCTION 'FILE_GET_NAME'
  EXPORTING
    logical_filename = logical_filename
    parameter_1      = p_file
  IMPORTING
    file_name        = filename
  EXCEPTIONS
    file_not_found   = 1
    OTHERS           = 2.
OPEN DATASET filename FOR INPUT IN BINARY MODE.
IF sy-subrc IS INITIAL.
  READ DATASET filename INTO xstring.
  CLOSE DATASET filename.
  MESSAGE 'Erfolgreich heruntergeladen' TYPE 'S'.
ELSE.
  MESSAGE 'Fehler beim Herunterladen' TYPE 'S' DISPLAY LIKE 'E'.
ENDIF.
IF p1 = abap_true.
**--convert xstring to string
  CALL METHOD cl_abap_conv_in_ce=>create
    EXPORTING
      encoding    = 'UTF-8'
*      endian      = 'L'
*      ignore_cerr = 'X'
*      replacement = '#'
      input       = xstring
    RECEIVING
      conv        = convin.
  CALL METHOD convin->read
    IMPORTING
      data = string.
  cl_abap_browser=>show_xml(
  EXPORTING
    xml_string   =  string ).
ELSEIF p2 = abap_true.
**--convert xstring to string
  CALL METHOD cl_abap_conv_in_ce=>create
    EXPORTING
      encoding    = 'UTF-8'
      endian      = 'L'
      ignore_cerr = 'X'
      replacement = '#'
      input       = xstring
    RECEIVING
      conv        = convin.
  CALL METHOD convin->read
    IMPORTING
      data = xstring.
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
  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer        = xstring
*     APPEND_TO_TABLE       = ' '
    IMPORTING
      output_length = length
    TABLES
      binary_tab    = binary_tab.
  /sew/cl_int_xml=>download_gui_file(
    EXPORTING
      path    = path
      data    = CONV #( binary_tab )
   IMPORTING
   message = message ).
  IF sy-subrc IS INITIAL.
  ENDIF.
ENDIF.
