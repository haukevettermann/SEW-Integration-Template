FUNCTION /sew/int_extract_sender.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(OBJECT_TYPE) TYPE  CHAR1
*"  EXPORTING
*"     VALUE(XML_STRING) TYPE  XSTRING
*"     VALUE(STATUS) TYPE  CHAR1
*"----------------------------------------------------------------------

  DATA: is_aendup  TYPE /sew/int_it_aeup,
        up_success TYPE TABLE OF /sew/int_it_aeup,
        s0000      TYPE p0000,
        s0002      TYPE p0002,
        s0001      TYPE p0001,
        sel_infty  TYPE rsdsselopt_t,
        filename   TYPE string,
        message    TYPE bapiret1.

**JMB20211111 start insert - check for time-specific objects
*
  DATA(p_obj) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = /sew/cl_int_constants=>person )
                                    ( sign = 'I' option = 'EQ' low = /sew/cl_int_constants=>absence )
                                    ( sign = 'I' option = 'EQ' low = /sew/cl_int_constants=>quota ) ).

  CASE object_type.
    WHEN /sew/cl_int_constants=>absence.
      sel_infty = VALUE #( ( sign = 'I' option = 'EQ' low = '2001' ) ).
    WHEN /sew/cl_int_constants=>quota.
      sel_infty = VALUE #( ( sign = 'I' option = 'EQ' low = '2006' ) ).
    WHEN /sew/cl_int_constants=>person.
      sel_infty = VALUE #( ( sign = 'I' option = 'NE' low = '2001' )
                           ( sign = 'I' option = 'NE' low = '2006' ) ).
  ENDCASE.
*JMB20211111 insert end

  IF object_type IN p_obj.
    SELECT SINGLE * FROM /sew/int_objects INTO @DATA(ls_object) WHERE object = @object_type.
    SELECT * FROM /sew/int_it_aeup INTO TABLE @DATA(changed_data) WHERE status EQ '01' AND
                                                                        infty  IN @sel_infty.  "JMB20211111 I

    DATA(changed_data_tmp) = changed_data.
    SORT changed_data_tmp ASCENDING BY pernr legal_entity infty timestamp.
    DELETE ADJACENT DUPLICATES FROM changed_data_tmp COMPARING pernr legal_entity.

    DATA(xml) = NEW /sew/cl_int_xml_up( ).
    xml->create_initial_xml( IMPORTING xml_document = DATA(new_xml)
                                       xml_data_node = DATA(xml_data_node) ).

    LOOP AT changed_data_tmp ASSIGNING FIELD-SYMBOL(<change>).

      DATA(changed_data_pernr) = VALUE /sew/cl_int_employee_xml_up=>t_itaendup( FOR     change IN changed_data
                                                                                WHERE ( pernr = <change>-pernr ) ( change ) ).

      xml->add_node_object_employee( EXPORTING object_id_cloud = CONV #( /sew/cl_int_employee_xml_up=>read_oracle_id( EXPORTING pernr   = <change>-pernr
                                                                                                                                date    = <change>-begda
                                                                                                                      IMPORTING message =  message ) )
                                               object_id_sap   = CONV #( <change>-pernr )
                                               xpath_oracle    = CONV #( ls_object-id_oracle )
                                               xpath_sap       = CONV #( ls_object-id_sap )
                                               xpath_object    = CONV #( ls_object-folder )
                                               xml_document    = new_xml
                                               aend_up         = VALUE #( changed_data_pernr[ infty = '' ] OPTIONAL )
                                     IMPORTING xml_node        = DATA(xml_node)
                                      CHANGING xml_data_node   = xml_data_node ).

      <change>-status = '02'.

      DELETE changed_data_pernr WHERE infty IS INITIAL.

      LOOP AT changed_data_pernr ASSIGNING FIELD-SYMBOL(<data_pernr>).

        DATA(data_pernr) = VALUE /sew/cl_int_employee_xml_up=>t_itaendup( ( <data_pernr> ) ).

        DATA(employee) = NEW /sew/cl_int_employee_xml_up( changed_data = data_pernr
                                                          pernr        = <data_pernr>-pernr
                                                          xml_node     = xml_node
                                                          xml_document = new_xml ).

        employee->read_molga( IMPORTING message = message ).
        IF message-type = /sew/cl_int_constants=>error.
          CONTINUE.
        ENDIF.

        employee->read_customizing( EXPORTING export  = abap_true
                                    IMPORTING message = message ).
        IF message-type = /sew/cl_int_constants=>error.
          CONTINUE.
        ENDIF.

        employee->process_changed_data( IMPORTING message = message ).
        IF message-type = /sew/cl_int_constants=>error.
          CONTINUE.
        ENDIF.

        <data_pernr>-status = '02'. "JMB20211005 I

        employee->build_xml( ).
      ENDLOOP.
      APPEND LINES OF changed_data_pernr TO up_success.   "JMB20211005 I
    ENDLOOP.

    DATA(xstring) = xml->create_xstring_from_xml( xml = new_xml ).
    DATA(string) = xml->create_string_from_xstring( xstring = xstring ).
    xml_string = xstring.

    IF xml_string IS NOT INITIAL.
*      MODIFY /sew/int_it_aeup FROM TABLE changed_data_tmp. "JMB20211005 D
      MODIFY /sew/int_it_aeup FROM TABLE up_success. "JMB20211005 I
      COMMIT WORK.
    ENDIF.

*    cl_abap_browser=>show_xml( EXPORTING xml_string = string ).

    IF changed_data_tmp IS INITIAL.
      status = 'E'.
    ENDIF.

    "Departments
  ELSEIF object_type = 'D'.
    SELECT SINGLE * FROM /sew/int_objects INTO @DATA(ls_object_om) WHERE object = 'O'.
    SELECT * FROM /sew/int_om_aeup INTO TABLE @DATA(changed_data_om) WHERE status = '01'.

    DATA(changed_data_om_tmp) = changed_data_om.
    SORT changed_data_om_tmp ASCENDING BY sap_id legal_entity.
    DELETE ADJACENT DUPLICATES FROM changed_data_om_tmp COMPARING sap_id legal_entity.

    DATA(xml_om) = NEW /sew/cl_int_xml_up( ).
    xml_om->create_initial_xml( IMPORTING xml_document = DATA(new_xml_om) xml_data_node = DATA(xml_data_node_om) ).
    LOOP AT changed_data_om_tmp ASSIGNING FIELD-SYMBOL(<change_om>).
      DATA(changed_data_om_sap_id) = VALUE /sew/cl_int_om_xml_up=>t_omaendup( FOR change_om IN changed_data_om WHERE ( sap_id = <change_om>-sap_id ) ( change_om ) ).

      xml_om->add_node_object_om( EXPORTING object_id_cloud = CONV #( /sew/cl_int_om_xml_up=>read_oracle_id( EXPORTING objid     = <change_om>-sap_id
                                                                                                                       date      = <change_om>-begda
                                                                                                             IMPORTING message   =  message ) )
                                            object_id_sap   = CONV #( <change_om>-sap_id )
                                            xpath_oracle    = CONV #( ls_object_om-id_oracle )
                                            xpath_sap       = CONV #( ls_object_om-id_sap )
                                            xpath_object    = CONV #( ls_object_om-folder )
                                            xml_document    = new_xml_om
                                            aend_up         = VALUE #( changed_data_om_sap_id[ infty = '' ] OPTIONAL )
                                  IMPORTING xml_node        = DATA(xml_node_om)
                                  CHANGING  xml_data_node   = xml_data_node_om ).

      <change_om>-status = '02'.
      DELETE changed_data_om_sap_id WHERE infty IS INITIAL.

      DATA(om) = NEW /sew/cl_int_om_xml_up( changed_data = changed_data_om_sap_id objid = <change_om>-sap_id xml_node = xml_node_om xml_document = new_xml_om ).

      om->read_molga( IMPORTING message = message ).
      IF message-type = /sew/cl_int_constants=>error.
        CONTINUE.
      ENDIF.

      om->read_customizing( IMPORTING message = message ).
      IF message-type = /sew/cl_int_constants=>error.
        CONTINUE.
      ENDIF.

      om->process_changed_data( IMPORTING message = message ).
      IF message-type = /sew/cl_int_constants=>error.
        CONTINUE.
      ENDIF.
      om->build_xml( ).
    ENDLOOP.

    DATA(xstring_om) = xml_om->create_xstring_from_xml( xml = new_xml_om ).
    DATA(string_om)  = xml_om->create_string_from_xstring( xstring = xstring_om ).

    xml_string = xstring_om.
    IF xml_string IS NOT INITIAL.
      MODIFY /sew/int_om_aeup FROM TABLE changed_data_om_tmp.
      COMMIT WORK.
    ENDIF.

    IF changed_data_om_tmp IS INITIAL.
      status = 'E'.
    ENDIF.

*    cl_abap_browser=>show_xml( EXPORTING xml_string   =  string_om ).  " XML in String

**JMB20210928 start insert - export work schedule xml
*
  ELSEIF object_type = 'W'.
    TYPES BEGIN OF file_list_sorted.
    TYPES: file_name TYPE string.
    TYPES timestamp TYPE timestamp.
    TYPES END OF file_list_sorted.

    DATA: file_list        TYPE TABLE OF /sew/file_list,
          file_list_sorted TYPE TABLE OF file_list_sorted,
          directory        TYPE epsf-epsdirnam,
          filename_archive TYPE string,
          timestamp        TYPE c LENGTH 13,
          string_ws        TYPE string.

    DATA(logical_filename) = CONV filename-fileintern( '/SEW/HCM_ORACLE_EXTRACTS' ).
    CALL FUNCTION 'FILE_GET_NAME'
      EXPORTING
        logical_filename = logical_filename
      IMPORTING
        file_name        = directory
      EXCEPTIONS
        file_not_found   = 1
        OTHERS           = 2.

    DATA(mask) = CONV epsf-epsfilnam( 'HCM_ORACLE_EXTRACTS_WORK_SCHEDULES' && '*' ).
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

    IF file_list IS NOT INITIAL.

      "build sortable list with timestamp.  EXTRACT timestamp from name.
      LOOP AT file_list ASSIGNING FIELD-SYMBOL(<file>).
        APPEND INITIAL LINE TO file_list_sorted ASSIGNING FIELD-SYMBOL(<file_sorted>).
        <file_sorted>-file_name = <file>-name.
        SPLIT <file>-name AT 'SCHEDULES_' INTO DATA(file_name) DATA(file_timestamp).
        SPLIT file_timestamp AT '.' INTO DATA(file_time) DATA(not_needed).
        file_time = COND #( WHEN strlen( file_time ) > 14 THEN sy-datum && sy-uzeit
                            ELSE file_time ).

        <file_sorted>-timestamp = file_time.
        CLEAR timestamp.
      ENDLOOP.

      "Sort list by timestamp oldest to newest
      SORT file_list_sorted BY timestamp ASCENDING.

      LOOP AT file_list_sorted ASSIGNING <file_sorted>.
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
        IF sy-subrc IS INITIAL.
          READ DATASET filename INTO xml_string.
          CLOSE DATASET filename.

          "Archive file
          DATA(logical_archive) = CONV filename-fileintern( '/SEW/HCM_ORACLE_EXTRACTS_ARCHIVE' ).
          CALL FUNCTION 'FILE_GET_NAME'
            EXPORTING
              logical_filename = logical_archive
              parameter_1      = <file_sorted>-file_name
            IMPORTING
              file_name        = filename_archive
            EXCEPTIONS
              file_not_found   = 1
              OTHERS           = 2.

          TRY.
              OPEN DATASET filename_archive FOR OUTPUT IN BINARY MODE.
              IF sy-subrc IS INITIAL.
                TRANSFER xml_string TO filename_archive.
                CLOSE DATASET filename_archive.
                IF sy-subrc IS INITIAL.
                  DELETE DATASET filename.
                ENDIF.
              ENDIF.
            CATCH cx_sy_file_authority INTO DATA(exception).
          ENDTRY.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
*JMB20210928 end insert

  ENDIF.
ENDFUNCTION.
