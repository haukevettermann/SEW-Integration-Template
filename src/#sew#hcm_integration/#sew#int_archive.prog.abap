*&---------------------------------------------------------------------*
*& Report /SEW/INT_TEST_XML
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /sew/int_archive.
DATA:
  xstring          TYPE xstring,
  directory        TYPE epsf-epsdirnam,
  filename         TYPE string,
  filename_archive TYPE string,
  file_list        TYPE TABLE OF /sew/file_list.
SELECTION-SCREEN BEGIN OF BLOCK frame1.
PARAMETERS: p_file  TYPE string OBLIGATORY,
            p_full  RADIOBUTTON GROUP r2,
            p_list  RADIOBUTTON GROUP r2,
            p_arch  RADIOBUTTON GROUP r1,
            p_rarch RADIOBUTTON GROUP r1.
SELECTION-SCREEN   END OF BLOCK frame1.
TRANSLATE p_file TO UPPER CASE.
* Get physical path
*DATA(logical_filename) = COND filename-fileintern( WHEN sy-sysid = 'D02' AND p_rarch = abap_false THEN '/SEW/HCM_ORACLE_EXTRACTS_DEV'
*                                                   WHEN sy-sysid = 'D02' AND p_rarch = abap_true THEN '/SEW/HCM_ORACLE_EXTRACTS_ARCHIVE_DEV'
*                                                   WHEN sy-sysid = 'Q02' AND p_rarch = abap_false THEN '/SEW/HCM_ORACLE_EXTRACTS_QS'
*                                                   WHEN sy-sysid = 'Q02' AND p_rarch = abap_true THEN '/SEW/HCM_ORACLE_EXTRACTS_ARCHIVE_QS'
*                                                   WHEN sy-sysid = 'P02' AND p_rarch = abap_true THEN '/SEW/HCM_ORACLE_EXTRACTS_PROD'
*                                                   WHEN sy-sysid = 'P02' AND p_rarch = abap_true THEN '/SEW/HCM_ORACLE_EXTRACTS_ARCHIVE_PROD' ).
DATA(logical_filename) = COND filename-fileintern( WHEN p_rarch = abap_false THEN /sew/cl_int_general_settings=>get_al11_down( )
                                                   WHEN p_rarch = abap_true THEN /sew/cl_int_general_settings=>get_al11_archive( ) ).
IF p_list = abap_true.
  CALL FUNCTION 'FILE_GET_NAME'
    EXPORTING
      logical_filename = logical_filename
*     parameter_1      = p_file
    IMPORTING
      file_name        = directory
    EXCEPTIONS
      file_not_found   = 1
      OTHERS           = 2.
* Get list of files
  IF strlen( p_file ) > 40.
    DATA(mask) = CONV epsf-epsfilnam( p_file(40) && '*' ).
  ELSE.
    mask = CONV epsf-epsfilnam( p_file && '*' ).
  ENDIF.
  CONDENSE mask NO-GAPS.
  CALL FUNCTION '/SEW/INT_GET_DIRECTORY_LISTING'
    EXPORTING
      dir_name               = directory
      file_mask              = mask
    TABLES
      dir_list               = file_list
    EXCEPTIONS
      invalid_eps_subdir     = 1
      sapgparam_failed       = 2
      build_directory_failed = 3
      no_authorization       = 4
      read_directory_failed  = 5
      too_many_read_errors   = 6
      empty_directory_list   = 7
      OTHERS                 = 8.
ELSE.
  APPEND INITIAL LINE TO file_list ASSIGNING FIELD-SYMBOL(<new_file>).
  <new_file>-name = p_file.
ENDIF.
IF file_list IS NOT INITIAL.
*  build sortable list with timestamp.  EXTRACT timestamp from name.
  LOOP AT file_list ASSIGNING FIELD-SYMBOL(<file>).
* get physical path
    CALL FUNCTION 'FILE_GET_NAME'
      EXPORTING
        logical_filename = logical_filename
        parameter_1      = <file>-name
      IMPORTING
        file_name        = filename
      EXCEPTIONS
        file_not_found   = 1
        OTHERS           = 2.
    OPEN DATASET filename FOR INPUT IN BINARY MODE.
*OPEN DATASET <file_sorted>-file_name FOR INPUT IN BINARY MODE.
    IF  sy-subrc IS INITIAL.
      READ DATASET filename INTO xstring.
*  READ DATASET <file_sorted>-file_name INTO xstring.
      CLOSE DATASET filename.
*  CLOSE DATASET <file_sorted>-file_name.
      MESSAGE 'Erfolgreich archiviert: ' && <file>-name TYPE 'S'.
* get physical path
      DATA(logical_filename2) = COND filename-fileintern( WHEN sy-sysid = 'D02' AND p_rarch = abap_false THEN '/SEW/HCM_ORACLE_EXTRACTS_ARCHIVE_DEV'
                                                         WHEN sy-sysid = 'D02' AND p_rarch = abap_true THEN '/SEW/HCM_ORACLE_EXTRACTS_DEV'
                                                         WHEN sy-sysid = 'Q02' AND p_rarch = abap_false THEN '/SEW/HCM_ORACLE_EXTRACTS_ARCHIVE_QS'
                                                         WHEN sy-sysid = 'Q02' AND p_rarch = abap_true THEN '/SEW/HCM_ORACLE_EXTRACTS_QS'
                                                         WHEN sy-sysid = 'P02' AND p_rarch = abap_true THEN '/SEW/HCM_ORACLE_EXTRACTS_ARCHIVE_PROD'
                                                         WHEN sy-sysid = 'P02' AND p_rarch = abap_true THEN '/SEW/HCM_ORACLE_EXTRACTS_PROD' ).
      CALL FUNCTION 'FILE_GET_NAME'
        EXPORTING
          logical_filename = logical_filename2
          parameter_1      = <file>-name
        IMPORTING
          file_name        = filename_archive
        EXCEPTIONS
          file_not_found   = 1
          OTHERS           = 2.
      IF sy-subrc <> 0.
        MESSAGE 'Keine Berechtigung zum Schreiben' TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
      TRY.
          OPEN DATASET filename_archive FOR OUTPUT IN BINARY MODE.
          IF sy-subrc IS INITIAL.
            TRANSFER xstring TO filename_archive.
            CLOSE DATASET filename_archive.
            IF sy-subrc IS INITIAL.
              DELETE DATASET filename.
            ENDIF.
          ELSE.
            MESSAGE 'Fehler beim Archivieren' TYPE 'S' DISPLAY LIKE 'E'.
          ENDIF.
        CATCH cx_sy_file_authority INTO DATA(exception).
      ENDTRY.
    ENDIF.
  ENDLOOP.
ELSE.
  MESSAGE 'No files found' TYPE 'S' DISPLAY LIKE 'E'.
ENDIF.
