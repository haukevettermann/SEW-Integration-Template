class /SEW/CL_INT_XML definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ts_xpath_data.
        INCLUDE TYPE /sew/int_fields.
        TYPES id TYPE string.
    TYPES begda TYPE begda.
    TYPES endda TYPE endda.
    TYPES value TYPE text100.
    TYPES END OF ts_xpath_data .
  types:
    tt_xpath_data TYPE TABLE OF ts_xpath_data .
  types BEGDA type BEGDA .
  types:
    BEGIN OF s_folder.
    TYPES node TYPE REF TO if_ixml_node.
    TYPES END OF s_folder .
  types:
    BEGIN OF s_folder_with_begda.
    TYPES begda TYPE begda.
    TYPES endda TYPE endda.
    TYPES node TYPE REF TO if_ixml_node.
    TYPES END OF s_folder_with_begda .
  types:
    t_folder_with_begda TYPE TABLE OF s_folder_with_begda .
  types:
    t_folder TYPE TABLE OF s_folder .
  types:
    tt_data TYPE TABLE OF docs .
  types:
    tt_customizing TYPE TABLE OF /sew/int_fields .

  data GO_XML type ref to IF_IXML_DOCUMENT .
  data GT_CUSTOMIZING type TT_CUSTOMIZING .
  data GO_PROCESSOR type ref to CL_XSLT_PROCESSOR .
  data GO_COLLECTION type ref to IF_IXML_NODE_COLLECTION .

  class-methods GET_XPATH_ELEMENT
    importing
      !ELEMENT_XPATH type STRING
      !XML_NODE type ref to IF_IXML_NODE
    exporting
      !VALUE type TEXT100
      !MESSAGE type BAPIRET1 .
  class-methods GET_XPATH_FOLDER
    importing
      !FOLDER_XPATH type STRING
      !XML_DOCUMENT type ref to IF_IXML_DOCUMENT
    exporting
      !NODES type T_FOLDER
      !MESSAGE type BAPIRET1 .
  class-methods GET_XPATH_FOLDER_WITH_BEGDA
    importing
      !FOLDER_XPATH type STRING
      !XML_DOCUMENT type ref to IF_IXML_NODE
    exporting
      !NODES type T_FOLDER_WITH_BEGDA
      !MESSAGE type BAPIRET1 .
  class-methods CREATE_XML_INTERFACE
    importing
      !XSTRING type XSTRING
    exporting
      !MESSAGE type BAPIRET1
      !XML_DOCUMENT type ref to IF_IXML_DOCUMENT .
  class-methods CONVERT_XML_TO_XSTRING
    importing
      !LENGTH type SAPB-LENGTH
      !DATA type TT_DATA
    exporting
      !XSTRING type XSTRING
      !MESSAGE type BAPIRET1 .
  class-methods DOWNLOAD_AL11_FILE
    importing
      value(PATH) type SAPB-SAPPFAD
    exporting
      !LENGTH type SAPB-LENGTH
      !DATA type TT_DATA
      !ERROR type BOOLE_D
      !MESSAGE type BAPIRET1 .
  class-methods UPLOAD_AL11_FILE
    importing
      value(PATH) type STRING
    exporting
      !LENGTH type SAPB-LENGTH
      !DATA type TT_DATA
      !ERROR type BOOLE_D
      !MESSAGE type BAPIRET1 .
  class-methods DOWNLOAD_GUI_FILE
    importing
      !PATH type STRING
      !DATA type SOLIX_TAB
    exporting
      !MESSAGE type BAPIRET1 .
  class-methods UPLOAD_GUI_FILE
    importing
      !PATH type STRING
    exporting
      !LENGTH type SAPB-LENGTH
      !DATA type TT_DATA
      !MESSAGE type BAPIRET1 .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /SEW/CL_INT_XML IMPLEMENTATION.


  METHOD convert_xml_to_xstring.
    DATA: lv_length TYPE i.
    lv_length = length.
    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = lv_length
*       FIRST_LINE   = 0
*       LAST_LINE    = 0
      IMPORTING
        buffer       = xstring
      TABLES
        binary_tab   = data
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.
  ENDMETHOD.


  METHOD create_xml_interface.
    DATA:
      ld_i_reftype            TYPE etref_typ,
      ld_i_active_in_scenario TYPE ettool_scenario.
    TRY.
    CALL METHOD cl_apl_ecatt_xml_data=>create_from_xstring(
      EXPORTING
        i_reftype            = ld_i_reftype
        i_active_in_scenario = ld_i_active_in_scenario
        im_xml_as_xstream    = xstring
      IMPORTING
        ep_ixml_doc          = xml_document
      EXCEPTIONS
        OTHERS               = 1 ).
    CATCH CX_ECATT_APL_XML.
      message-id = /sew/cl_int_constants=>msg_class_int.
      message-number = /sew/cl_int_constants=>msg_no-m21.
    ENDTRY.
  ENDMETHOD.


  METHOD download_al11_file.
    DATA: loc_length     TYPE p,
          line           TYPE docs,
          check_filename TYPE authb-filename,
          slen           TYPE i.
    CONSTANTS gc_fname TYPE fileintern VALUE 'ARCHIVLINK_FILE_READ'.
* check authority for file access
    slen = strlen( path ).
    IF slen <= 60.
      check_filename(60) = path.
      CALL FUNCTION 'AUTHORITY_CHECK_DATASET'
        EXPORTING
*         PROGRAM          =
          activity         = 'READ'
          filename         = check_filename
        EXCEPTIONS
          no_authority     = 1
          activity_unknown = 2
          OTHERS           = 3.
      CASE sy-subrc.
        WHEN 0.
        WHEN 1.
          error = abap_true.
        WHEN OTHERS.
          error = abap_true.
      ENDCASE.
    ENDIF.
* open file on applicationserver
    CALL FUNCTION 'FILE_VALIDATE_NAME'
      EXPORTING
        logical_filename           = gc_fname
      CHANGING
        physical_filename          = path
      EXCEPTIONS
        logical_filename_not_found = 1
        validation_failed          = 2
        OTHERS                     = 3.
    IF sy-subrc <> 0.
      error = abap_true.
    ENDIF.
    OPEN DATASET path FOR INPUT IN BINARY MODE.
    IF sy-subrc <> 0.
      error = abap_true.
    ENDIF.
* read in information
    REFRESH data.
    CLEAR length.
    DO.
      READ DATASET path INTO line LENGTH loc_length.
      IF sy-subrc = 0.
        APPEND line TO data.
        length = length + loc_length.
      ELSE.
        IF sy-subrc = 8.
* open not possible
          error = abap_true.
        ELSE.
* end of file reached
          IF loc_length NE 0.
            APPEND line TO data.
            length = length + loc_length.
          ENDIF.
          EXIT.
        ENDIF.
      ENDIF.
    ENDDO.
* close file on apllication server
    CLOSE DATASET path.
  ENDMETHOD.


  METHOD download_gui_file.
    DATA: lv_length   TYPE i.
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = path
*     IMPORTING
*       FILELENGTH              =
      TABLES
        data_tab                = data
*       FIELDNAMES              =
     EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.
  ENDMETHOD.


  METHOD get_xpath_element.
    DATA:
      lr_processor  TYPE REF TO cl_xslt_processor,
      lr_collection TYPE REF TO if_ixml_node_collection.
    CREATE OBJECT: lr_processor.
    lr_processor->set_source_node( node = xml_node ).
    lr_processor->set_expression( expression = CONV #( element_xpath ) ).
    lr_processor->run( progname = space ).
    lr_collection = lr_processor->get_nodes( ).
    IF lr_collection IS BOUND.
      DATA(lr_iterator) = lr_collection->create_iterator( ).
      DATA(lo_node) = lr_iterator->get_next( ).
      IF lo_node IS BOUND.
        value = lo_node->get_value( ).
        IF value IS INITIAL.
*          value = 'DELETED'.
        ENDIF.
      ELSE.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_xpath_folder.
    DATA:
      lr_processor  TYPE REF TO cl_xslt_processor,
      lr_collection TYPE REF TO if_ixml_node_collection.
    CREATE OBJECT: lr_processor.
    lr_processor->set_source_node( node = xml_document ).
    lr_processor->set_expression( expression = CONV #( folder_xpath ) ).
    lr_processor->run( progname = space ).
    lr_collection = lr_processor->get_nodes( ).
    IF lr_collection IS BOUND.
      DATA(lr_iterator) = lr_collection->create_iterator( ).
      DATA(lo_node) = lr_iterator->get_next( ).
      WHILE lo_node IS BOUND.
        APPEND INITIAL LINE TO nodes ASSIGNING FIELD-SYMBOL(<node>).
        <node>-node = lo_node.
        FREE lo_node.
        lo_node = lr_iterator->get_next( ).
      ENDWHILE.
    ENDIF.
  ENDMETHOD.


  METHOD get_xpath_folder_with_begda.
    DATA:
      lr_processor  TYPE REF TO cl_xslt_processor,
      lr_collection TYPE REF TO if_ixml_node_collection.
    CREATE OBJECT: lr_processor.
    lr_processor->set_source_node( node = xml_document ).
    CHECK folder_xpath IS NOT INITIAL.
*   Check if folder is global object folder
    DATA(folder_object_name) = xml_document->get_name( ).
    DATA(folder_xpath_name) = folder_xpath.
    REPLACE ALL OCCURRENCES OF '/' IN folder_xpath_name WITH ''.
    IF folder_object_name = folder_xpath_name.
      APPEND INITIAL LINE TO nodes ASSIGNING FIELD-SYMBOL(<node>).
      <node>-node = xml_document.
      /sew/cl_int_xml=>get_xpath_element(
        EXPORTING
          element_xpath = /sew/cl_int_constants=>xpath_begda
          xml_node      = xml_document
        IMPORTING
          value         = DATA(value)
          message       = message ).
      IF value IS NOT INITIAL.
        <node>-begda = /sew/cl_int_conversion=>convert_date( value_in = CONV #( value ) ).
      ELSE.
        <node>-begda = /sew/cl_int_constants=>lowdate.
      ENDIF.
      /sew/cl_int_xml=>get_xpath_element(
        EXPORTING
          element_xpath = /sew/cl_int_constants=>xpath_endda
          xml_node      = xml_document
        IMPORTING
          value         = value
          message       = message ).
      IF value IS NOT INITIAL.
        <node>-endda = /sew/cl_int_conversion=>convert_date( value_in = CONV #( value ) ).
      ELSE.
        <node>-endda = /sew/cl_int_constants=>highdate.
      ENDIF.
    ELSE.
      lr_processor->set_expression( expression = CONV #( folder_xpath ) ).
      lr_processor->run( progname = space ).
      lr_collection = lr_processor->get_nodes( ).
      IF lr_collection IS BOUND.
        DATA(lr_iterator) = lr_collection->create_iterator( ).
        DATA(lo_node) = lr_iterator->get_next( ).
        WHILE lo_node IS BOUND.
*       In case of job info or employment data only process primary assignment.
          IF folder_xpath = /sew/cl_int_constants=>folders-employmentdata OR folder_xpath = /sew/cl_int_constants=>folders-jobinformation.
            IF folder_xpath = /sew/cl_int_constants=>folders-employmentdata.
              /sew/cl_int_xml=>get_xpath_element(
               EXPORTING
                 element_xpath = '//TermActionCode'
                 xml_node      = lo_node
               IMPORTING
                 value         = value
                 message       = message ).
            ENDIF.
            DATA(range) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = 'ORA_ADD_PWK_WORK_RELATION' )
           ( sign = 'I' option = 'EQ' low = 'ADD_PEN_WKR' )
           ( sign = 'I' option = 'EQ' low = 'REHIRE' ) ).
            IF value NOT IN range.
*            IF value NE 'ADD_PEN_WKR'.
              /sew/cl_int_xml=>get_xpath_element(
                EXPORTING
                  element_xpath = COND #( WHEN folder_xpath = /sew/cl_int_constants=>folders-employmentdata THEN /sew/cl_int_constants=>primary
                                          WHEN folder_xpath = /sew/cl_int_constants=>folders-jobinformation THEN /sew/cl_int_constants=>primaryassignment )
                  xml_node      = lo_node
                IMPORTING
                  value         = value
                  message       = message ).
              IF value NE 'Y'.
                FREE lo_node.
                lo_node = lr_iterator->get_next( ).
                CONTINUE.
              ENDIF.
            ENDIF.
          ENDIF.
          APPEND INITIAL LINE TO nodes ASSIGNING <node>.
          <node>-node = lo_node.
          CLEAR value.
          /sew/cl_int_xml=>get_xpath_element(
            EXPORTING
              element_xpath = /sew/cl_int_constants=>xpath_begda
              xml_node      = lo_node
            IMPORTING
              value         = value
              message       = message ).
          <node>-begda = /sew/cl_int_conversion=>convert_date( value_in = CONV #( value ) ).
          CLEAR value.
          /sew/cl_int_xml=>get_xpath_element(
            EXPORTING
              element_xpath = /sew/cl_int_constants=>xpath_endda
              xml_node      = lo_node
            IMPORTING
              value         = value
              message       = message ).
          <node>-endda = /sew/cl_int_conversion=>convert_date( value_in = CONV #( value ) ).
          FREE lo_node.
          lo_node = lr_iterator->get_next( ).
        ENDWHILE.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD UPLOAD_AL11_FILE.
  ENDMETHOD.


  METHOD upload_gui_file.
    DATA: lv_length   TYPE i.
    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                = path
        filetype                = 'BIN'
*       HAS_FIELD_SEPARATOR     = ' '
*       HEADER_LENGTH           = 0
*       READ_BY_LINE            = 'X'
*       DAT_MODE                = ' '
*       CODEPAGE                = ' '
*       IGNORE_CERR             = ABAP_TRUE
*       REPLACEMENT             = '#'
*       CHECK_BOM               = ' '
*       VIRUS_SCAN_PROFILE      =
*       NO_AUTH_CHECK           = ' '
      IMPORTING
        filelength              = lv_length
*       HEADER                  =
      TABLES
        data_tab                = data
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        OTHERS                  = 17.
    length = lv_length.
  ENDMETHOD.
ENDCLASS.
