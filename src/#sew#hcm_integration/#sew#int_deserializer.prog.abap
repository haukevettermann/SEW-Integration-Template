*&---------------------------------------------------------------------*
*& Report /SEW/INT_TEST_XML
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /sew/int_deserializer.
TYPES BEGIN OF file_list_sorted.
TYPES: file_name TYPE string.
TYPES timestamp TYPE timestamp.
TYPES END OF file_list_sorted.
DATA:
  xstring          TYPE xstring,
  directory        TYPE epsf-epsdirnam,
  filename         TYPE string,
  filename_archive TYPE string,
*  filename         TYPE string,
  xml              TYPE REF TO if_ixml_document,
  file_list        TYPE TABLE OF /sew/file_list,
  file_list_sorted TYPE TABLE OF file_list_sorted,
  timestamp        TYPE c LENGTH 13,
  id               TYPE vrm_id,
  value_list       TYPE vrm_values,
  molga            TYPE molga,
  otype            TYPE hrotype,
  sequence         TYPE seqnr,
  pernr            TYPE pernr_d,
  oracle_id        TYPE /sew/dd_element,
  country_code     TYPE intca.
* Begin of selection Screen
SELECTION-SCREEN BEGIN OF BLOCK frame1.
PARAMETERS: p_file   TYPE string OBLIGATORY,
            p_full   RADIOBUTTON GROUP r2,
            p_list   RADIOBUTTON GROUP r2,
            p_rarch  TYPE boole_d,
            p_scenar TYPE /sew/dd_scenario AS LISTBOX VISIBLE LENGTH 30 OBLIGATORY.
*            p_otype  TYPE hrotype OBLIGATORY, "/SEW/DD_EXT_TYPE
*            p_dt     TYPE boole_d.
SELECTION-SCREEN   END OF BLOCK frame1.
SELECTION-SCREEN BEGIN OF BLOCK frame2.
SELECT-OPTIONS:
p_sid  FOR pernr,
p_cid  FOR oracle_id. "hrobjid
PARAMETERS:
  p_arch TYPE boole_d,
  p_test TYPE boole_d.
SELECTION-SCREEN   END OF BLOCK frame2.

AT SELECTION-SCREEN OUTPUT.
  SELECT * FROM /sew/int_objects INTO TABLE @DATA(objects).
  LOOP AT objects ASSIGNING FIELD-SYMBOL(<object>).
    APPEND INITIAL LINE TO value_list ASSIGNING FIELD-SYMBOL(<value>).
    <value>-key = <object>-molga && '-' && <object>-object && '-' && <object>-object_seqnr.
    <value>-text = <object>-scenario && ' | ' && <object>-molga.
  ENDLOOP.
  id = 'P_SCENAR'.
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = id
      values = value_list.

START-OF-SELECTION.
* Set otype and sequence in order to read customizing.
  IF p_scenar+0(1) = '*'.
    molga = p_scenar+0(1).
    otype = p_scenar+2(2).
    IF otype+1 = '-'.
      otype = p_scenar+2(1).
      sequence = p_scenar+4(3).
    ELSE.
      sequence = p_scenar+5(3).
    ENDIF.
  ELSE.
    molga = p_scenar+0(2).
    otype = p_scenar+3(2).
    IF otype+1 = '-'.
      otype = p_scenar+3(1).
*      sequence = p_scenar+4(3).
      sequence = p_scenar+6(3).
    ELSE.
      sequence = p_scenar+6(3).
    ENDIF.
  ENDIF.
* End of selection Screen
  TRANSLATE p_file TO UPPER CASE.
* Set static attributes
  /sew/cl_int_statics=>read_archive = p_rarch.
  /sew/cl_int_statics=>archive = p_arch.
  /sew/cl_int_statics=>test_run = p_test.
** Get physical path
  DATA(logical_filename) = COND filename-fileintern( WHEN p_rarch = abap_false THEN /sew/cl_int_general_settings=>get_al11_down( )
                                                   WHEN p_rarch = abap_true THEN /sew/cl_int_general_settings=>get_al11_archive( ) ).
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
  IF p_list = abap_true.
    DATA(mask) = CONV epsf-epsfilnam( COND #( WHEN strlen( p_file ) > 40 THEN p_file(40) ELSE p_file ) && '*' ).
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
    SELECT SINGLE intca FROM t500l WHERE molga = @molga INTO @DATA(country).
*  build sortable list with timestamp.  EXTRACT timestamp from name.
    LOOP AT file_list ASSIGNING FIELD-SYMBOL(<file>).

      SPLIT <file>-name AT '_' INTO DATA(dont_care) DATA(dont_care2).

      SPLIT dont_care2 AT '.' INTO DATA(dont_care3) timestamp.
      IF otype = /sew/cl_int_constants=>person.
        SPLIT dont_care3 AT '_' INTO DATA(dont_care4) DATA(dont_care5) country_code DATA(dont_care7) DATA(dont_care9).
      ELSEIF otype = /sew/cl_int_constants=>orgunit.
        SPLIT dont_care3 AT '_' INTO DATA(dont_care8) country_code DATA(dont_care10).
      ENDIF.

      IF ( ( ( country_code = country ) OR ( molga = '*' AND dont_care7 = 'ROW' ) ) OR ( otype = /sew/cl_int_constants=>person AND sequence = 001 ) ).
        APPEND INITIAL LINE TO file_list_sorted ASSIGNING FIELD-SYMBOL(<file_sorted>).
        <file_sorted>-file_name = <file>-name.
        <file_sorted>-timestamp = timestamp.
      ENDIF.
      CLEAR: timestamp, country_code.
    ENDLOOP.
* Sort list by timestamp oldest to newest and ignore files that have no timestamp
    SORT file_list_sorted BY timestamp ASCENDING.
*  DELETE file_list_sorted WHERE timestamp IS INITIAL OR timestamp = 0.
    LOOP AT file_list_sorted ASSIGNING <file_sorted>.
* get physical path
      CALL FUNCTION 'FILE_GET_NAME'
        EXPORTING
          logical_filename = logical_filename
          parameter_1      = <file_sorted>-file_name
        IMPORTING
          file_name        = filename
        EXCEPTIONS
          file_not_found   = 1
          OTHERS           = 2.
      OPEN DATASET filename FOR INPUT IN BINARY MODE.
*OPEN DATASET <file_sorted>-file_name FOR INPUT IN BINARY MODE.
      IF sy-subrc IS INITIAL.
        READ DATASET filename INTO xstring.
*  READ DATASET <file_sorted>-file_name INTO xstring.
        CLOSE DATASET filename.
*  CLOSE DATASET <file_sorted>-file_name.
        MESSAGE 'Erfolgreich heruntergeladen' TYPE 'S'.
      ELSE.
        MESSAGE 'Fehler beim Herunterladen' TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
      /sew/cl_int_xml=>create_xml_interface(
        EXPORTING
          xstring      = xstring
        IMPORTING
          message      = DATA(message)
          xml_document = xml ).
      IF message IS NOT INITIAL.
        IF sy-batch IS INITIAL.
          MESSAGE ID message-id TYPE 'S' NUMBER message-number DISPLAY LIKE 'E'.
        ELSE.
          WRITE message-id(message-number).
        ENDIF.
        EXIT.
      ENDIF.
      DATA(object_collector) = NEW /sew/cl_int_object_collector( sequence = sequence ).
      object_collector->set_full_xml( xml = xml ).
      object_collector->read_customzing_objects( molga = molga object = CONV #( otype ) sequence = sequence ).
* Collect objects based on object customizing
      object_collector->collect_objects( ).
      LOOP AT object_collector->objects ASSIGNING FIELD-SYMBOL(<object>). "WHERE id_sap IS NOT INITIAL. "JMB20211018 I - Proceed only employees with a SAP PERNR
        IF ( <object>-object_seqnr = 001 OR <object>-object_seqnr = 002 OR <object>-object_seqnr = 003 OR <object>-object_seqnr = 004
          OR <object>-object_seqnr = 005 OR <object>-object_seqnr = 006 OR <object>-object_seqnr = 007 ) AND <object>-object = /sew/cl_int_constants=>person.
          IF <object>-id_sap IS INITIAL.
            CONTINUE.
          ENDIF.
        ENDIF.
        IF p_sid IS NOT INITIAL.
          CHECK <object>-id_sap IN p_sid.
        ENDIF.
        IF p_cid IS NOT INITIAL.
          CHECK <object>-id_cloud IN p_cid.
        ENDIF.
* Instantiate Object Handler
        <object>-object_handler = NEW /sew/cl_int_object_handler( molga       = <object>-molga
                                                                  bukrs       = <object>-bukrs
                                                                  object_type = <object>-object
                                                                  xml_node    = <object>-xml_node
                                                                  cloud_id    = CONV #( <object>-id_cloud )
                                                                  cloud_pernr = <object>-pernr_cloud
                                                                  sap_id      = CONV #( <object>-id_sap )
                                                                  int_run     = <object>-int_run
                                                                  seqnr       = <object>-object_seqnr
                                                                  projected_start_date = <object>-projected_start_date ).

        "Read customizing (customizing is a singleton object and will only be read once per object and molga
        <object>-object_handler->read_customizing( object_type = <object>-object object_seqnr = <object>-object_seqnr ).

        "Start of processing the customizing.
        <object>-object_handler->process_customzing( ).
      ENDLOOP.

      "Archive file
      IF p_test = abap_false AND p_arch = abap_true AND p_rarch IS INITIAL.

* Get physical path
        DATA(logical_filename2) = CONV filename-fileintern( '/SEW/HCM_ORACLE_EXTRACTS_ARCHIVE' ).
*        DATA(logical_filename2) = COND filename-fileintern( WHEN sy-sysid = 'D02' THEN '/SEW/HCM_ORACLE_EXTRACTS_ARCHIVE_DEV'
*                                                            WHEN sy-sysid = 'Q02' THEN '/SEW/HCM_ORACLE_EXTRACTS_ARCHIVE_QS'
*                                                            WHEN sy-sysid = 'P02' THEN '/SEW/HCM_ORACLE_EXTRACTS_ARCHIVE_PROD' ).
        CALL FUNCTION 'FILE_GET_NAME'
          EXPORTING
            logical_filename = logical_filename2
            parameter_1      = <file_sorted>-file_name
          IMPORTING
            file_name        = filename_archive
          EXCEPTIONS
            file_not_found   = 1
            OTHERS           = 2.
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
    IF sy-batch IS INITIAL.
      MESSAGE 'No files found' TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.
      WRITE 'No files found'.
    ENDIF.
  ENDIF.

  DATA(alv) = NEW /sew/cl_int_alv( testrun = p_test ).
  IF sy-batch IS INITIAL.
    alv->build_alv_structure( ).
*    alv->display_alv( ).
    WRITE: space.
  ELSE.
    alv->build_log( ).
  ENDIF.
