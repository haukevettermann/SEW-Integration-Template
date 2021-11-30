*&---------------------------------------------------------------------*
*& Report /SEW/INT_TEST_XML
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /sew/int_test_xml.
*DATA:
*  lt_data    TYPE /sew/cl_int_xml=>tt_data,
*  lv_length  TYPE sapb-length,
*  lv_error   TYPE boole_d,
*  ls_message TYPE bapiret1,
*  xstring TYPE xstring,
*  filename type string,
*  xml        TYPE REF TO if_ixml_document.
*CALL FUNCTION 'FILE_GET_NAME'
*  EXPORTING
*    logical_filename = '/SEW/HCM_ORACLE_EXTRACTS'
*    parameter_1      = 'Worker_Extract.xml'
*  IMPORTING
*    file_name        = filename
*  EXCEPTIONS
*    file_not_found   = 1
*    OTHERS           = 2.
*OPEN DATASET filename FOR INPUT IN BINARY MODE.
*IF sy-subrc IS INITIAL.
*  READ DATASET filename INTO xstring.
*  CLOSE DATASET filename.
*  MESSAGE 'Erfolgreich heruntergeladen' TYPE 'S'.
*ELSE.
*  MESSAGE 'Fehler beim Herunterladen' TYPE 'S' DISPLAY LIKE 'E'.
*ENDIF.
*/sew/cl_int_xml=>create_xml_interface(
*  EXPORTING
*    xstring      = xstring
*  IMPORTING
*    message      = ls_message
*    xml_document = xml ).
*
*DATA(lo_object) = NEW /sew/cl_int_object_xml( ).
*lo_object->set_full_xml( xml = xml ).
*lo_object->read_customzing_objects(
**    iv_object =                  " Objekttyp
*).
*lo_object->collect_objects(
**    iv_object =
*).
*LOOP AT lo_object->objects ASSIGNING FIELD-SYMBOL(<object>).
**  CLEAR: go_data_transform->prelp_tab, go_data_transform->prelp_tab_orig, go_data_transform->prelp_tab_nochng.
**  DATA(lo_employee) = /sew/cl_int_infty_proc_xml=>get_instance( molga = '01' xml_node = <object>-xml_node cloud_id = CONV #( <object>-id_cloud ) sap_id = CONV #( <object>-id_sap ) ).
*  <object>-employee_processor = NEW /sew/cl_int_employee_xml( molga = <object>-molga xml_node = <object>-xml_node cloud_id = CONV #( <object>-id_cloud ) sap_id = CONV #( <object>-id_sap ) ).
**  go_data_transform->sap_id = <object>-id_sap.
**  go_data_transform->cloud_id = <object>-id_cloud.
*  <object>-employee_processor->read_customizing( ).
*  <object>-employee_processor->process_customzing( ).
*
*  LOOP AT <object>-employee_processor->infotypes ASSIGNING FIELD-SYMBOL(<it>).
*    <object>-employee_processor->prelp_tab = VALUE #( BASE <object>-employee_processor->prelp_tab  ( LINES OF <it>->prelp_tab ) ).
*    <object>-employee_processor->prelp_tab_orig = VALUE #( BASE <object>-employee_processor->prelp_tab_orig  ( LINES OF <it>->prelp_tab_orig ) ).
*    <object>-employee_processor->prelp_tab_nochng = VALUE #( BASE <object>-employee_processor->prelp_tab_nochng  ( LINES OF <it>->prelp_tab_nochng ) ).
*    IF <object>-employee_processor->action IS INITIAL.
*      <object>-employee_processor->action = <it>->action.
*    ENDIF.
*  ENDLOOP.
*  "Saving per Pernr here or for whole INT_RUN after loop // for optimized performance after loop would be better
*  DATA(infotype_changes) = NEW /sew/cl_int_it_aend( ).
*  infotype_changes->action = <object>-employee_processor->action.
*  infotype_changes->int_run = <object>-int_run.
*  infotype_changes->sap_id = <object>-id_sap.
*  infotype_changes->cloud_id = <object>-id_cloud.
*  infotype_changes->molga = <object>-molga.
*  IF infotype_changes->save_entries( infotype_changes->prelp_to_it_aend( prelp_new = <object>-employee_processor->prelp_tab
*                                                                                                               prelp_orig = <object>-employee_processor->prelp_tab_orig
*                                                                                                               prelp_nochng = <object>-employee_processor->prelp_tab_nochng ) ) = abap_true.
*    COMMIT WORK.
*  ENDIF.


*ENDLOOP.
