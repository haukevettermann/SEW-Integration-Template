*&---------------------------------------------------------------------*
*& Report /SEW/INT_TEST_XML
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /sew/int_test_xml_up.
DATA:
  is_aendup TYPE /sew/int_it_aeup,
  s0000     TYPE p0000,
  s0002     TYPE p0002,
  s0001     TYPE p0001,
  filename  TYPE string,
  message   TYPE bapiret1.
*SELECT SINGLE * FROM pa0000 WHERE pernr = '93360160' AND begda = '20200113' AND massn = '01' INTO CORRESPONDING FIELDS OF @s0000.
*SELECT SINGLE * FROM pa0001 WHERE pernr = '93360160' AND begda = '20200113' INTO CORRESPONDING FIELDS OF @s0001.
*SELECT SINGLE * FROM pa0002 WHERE pernr = '93360160' AND begda = '20200113' INTO CORRESPONDING FIELDS OF @s0002.
*
*DELETE FROM /sew/int_it_aeup.
*cl_hr_pnnnn_type_cast=>pnnnn_to_prelp(
*  EXPORTING
*   pnnnn = CONV p0000( s0000 )
*  IMPORTING
*    prelp = DATA(prelp) ).
*is_aendup = CORRESPONDING #( prelp ).
*is_aendup-status = 01.
*is_aendup-operation = 'I'.
*is_aendup-timestamp = sy-timlo.
*is_aendup-infty = '0000'.
*MODIFY /sew/int_it_aeup FROM is_aendup.
*cl_hr_pnnnn_type_cast=>pnnnn_to_prelp(
*  EXPORTING
*    pnnnn = CONV p0001( s0001 )
*  IMPORTING
*    prelp = prelp ).
*is_aendup = CORRESPONDING #( prelp ).
*is_aendup-status = 01.
*is_aendup-operation = 'I'.
*is_aendup-timestamp = sy-timlo.
*is_aendup-infty = '0001'.
*MODIFY /sew/int_it_aeup FROM is_aendup.
*cl_hr_pnnnn_type_cast=>pnnnn_to_prelp(
*  EXPORTING
*    pnnnn = CONV p0002( s0002 )
*  IMPORTING
*    prelp = prelp ).
*is_aendup = CORRESPONDING #( prelp ).
*is_aendup-status = 01.
*is_aendup-operation = 'I'.
*is_aendup-timestamp = sy-timlo.
*is_aendup-infty = '0002'.
*MODIFY /sew/int_it_aeup FROM is_aendup.
*COMMIT WORK AND WAIT.
SELECT SINGLE * FROM /sew/int_objects INTO @DATA(ls_object) WHERE object = @/sew/cl_int_constants=>person.
SELECT * FROM /sew/int_it_aeup INTO TABLE @DATA(changed_data) WHERE status = @/sew/cl_int_constants=>booking_status-initial AND int_run = '005056AE579A1EDBA2F7BE6D59883113'.
DATA(pernr) = VALUE pernr_tab( FOR change IN changed_data ( change-pernr ) ).
SORT pernr. DELETE ADJACENT DUPLICATES FROM pernr.
LOOP AT pernr ASSIGNING FIELD-SYMBOL(<pernr>).
  DATA(changed_data_pernr) = VALUE /sew/cl_int_employee_xml_up=>t_itaendup( FOR change IN changed_data WHERE ( pernr = <pernr> ) ( change ) ).
  DATA(xml) = NEW /sew/cl_int_xml_up( ).
  xml->create_initial_xml( IMPORTING xml_document = DATA(new_xml) xml_data_node  = DATA(xml_data_node) ).
  xml->add_node_object_employee(
    EXPORTING
      object_id_cloud = CONV #( /sew/cl_int_employee_xml_up=>read_oracle_id(
                                                                                 EXPORTING
                                                                                   pernr     = <pernr>
                                                                                   date      = sy-datum
                                                                                 IMPORTING
                                                                                   message   =  message ) )
      object_id_sap    = CONV #( <pernr> )
      xpath_oracle     = CONV #( ls_object-id_oracle )
      xpath_sap        = CONV #( ls_object-id_sap )
      xpath_object     = CONV #( ls_object-folder )
      xml_document = new_xml
      aend_up = VALUE #( changed_data_pernr[ infty = '' ] OPTIONAL )
    IMPORTING
      xml_node      = DATA(xml_node)
    CHANGING
      xml_data_node     = xml_data_node ).
  DELETE changed_data_pernr WHERE infty IS INITIAL.
  DATA(employee) = NEW /sew/cl_int_employee_xml_up( changed_data = changed_data_pernr pernr = <pernr> xml_node = xml_node xml_document = new_xml ).
  employee->read_molga(
*    IMPORTING
*      message =
  ).
  employee->read_customizing(
*    IMPORTING
*      message =
  ).
  employee->process_changed_data(
*    IMPORTING
*      message =
  ).
  employee->build_xml(
*    IMPORTING
*      message =
    ).
  DATA(xstring) = xml->create_xstring_from_xml( xml = new_xml ).
  DATA(string) = xml->create_string_from_xstring( xstring = xstring ).
**--Displaying xml
  cl_abap_browser=>show_xml(
    EXPORTING
      xml_string   =  string   " XML in String

  ).
ENDLOOP.
