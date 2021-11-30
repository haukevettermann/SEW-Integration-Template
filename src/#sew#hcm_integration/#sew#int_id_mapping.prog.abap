*&---------------------------------------------------------------------*
*& Report /SEW/INT_TEST_XML
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /sew/int_id_mapping.

DATA:
  aend_id_range TYPE rsdsselopt_t,
  aend_id       LIKE LINE OF aend_id_range.
*SELECTION-SCREEN BEGIN OF BLOCK frame1.
*PARAMETERS: p_sapid  TYPE hrobjid OBLIGATORY,
*            p_cldid  TYPE /sew/dd_objectid OBLIGATORY,
*            p_intrun TYPE guid_32 OBLIGATORY,
*            p_test   TYPE boole_d.
*SELECTION-SCREEN   END OF BLOCK frame1.
SELECTION-SCREEN BEGIN OF BLOCK frame1.
*PARAMETERS: p_test TYPE boole_d,
*            p_arch TYPE boole_d.
PARAMETERS: p_rbpa RADIOBUTTON GROUP rbg DEFAULT 'X'.
PARAMETERS: p_rbom RADIOBUTTON GROUP rbg.
SELECTION-SCREEN   END OF BLOCK frame1.

TYPES: BEGIN OF ty_alv,
         pernr     TYPE string,
         oracle_id TYPE string,
         firstname TYPE string,
         lastname  TYPE string,
       END OF ty_alv.
TYPES g_alv TYPE TABLE OF ty_alv.
* Set static attributes
*/sew/cl_int_statics=>read_archive = p_rarch.
*/sew/cl_int_statics=>archive = p_arch.
*/sew/cl_int_statics=>test_run = p_test.
* Get physical path
*IF p_rbpa = abap_true.
*  SELECT * FROM /sew/int_it_aend INTO TABLE @DATA(it_aend) WHERE pernr = @p_sapid AND cloud_id = @p_cldid AND int_run = @p_intrun.
*ELSE.
*  SELECT * FROM /sew/int_om_aend INTO TABLE @DATA(om_aend) WHERE sap_id = @p_sapid AND cloud_id = @p_cldid AND int_run = @p_intrun.
*ENDIF.
*
*LOOP AT it_aend INTO DATA(record_pa).
*  CLEAR: aend_id.
*  aend_id-low = record_pa-aend_id.
*  aend_id-option = 'EQ'.
*  aend_id-sign = 'I'.
*  APPEND aend_id TO aend_id_range.
*ENDLOOP.
*
*LOOP AT om_aend INTO DATA(record_om).
*  CLEAR: aend_id.
*  aend_id-low = record_om-aend_id.
*  aend_id-option = 'EQ'.
*  aend_id-sign = 'I'.
*  APPEND aend_id TO aend_id_range.
*ENDLOOP.
*
*SELECT * FROM /sew/int_msg_l INTO TABLE @DATA(msg_l) WHERE aend_id IN @aend_id_range.
*SELECT * FROM /sew/int_msg_f INTO TABLE @DATA(msg_f) WHERE aend_id IN @aend_id_range.
*SELECT * FROM /sew/int_msg_p INTO TABLE @DATA(msg_p) WHERE aend_id IN @aend_id_range AND int_run = @p_intrun AND sap_id = @p_sapid AND cloud_id = @p_cldid.
*IF p_test = abap_false.
*  DELETE /sew/int_msg_l FROM TABLE msg_l.
*  DELETE /sew/int_msg_p FROM TABLE msg_p.
*  DELETE /sew/int_msg_f FROM TABLE msg_f.
*  LOOP AT it_aend ASSIGNING FIELD-SYMBOL(<record_pa>).
*    <record_pa>-status = '01'.
*  ENDLOOP.
*  LOOP AT om_aend ASSIGNING FIELD-SYMBOL(<record_om>).
*    <record_om>-status = '01'.
*  ENDLOOP.
*  MODIFY /sew/int_it_aend FROM TABLE it_aend.
*  MODIFY /sew/int_om_aend FROM TABLE om_aend.
*  IF sy-subrc IS INITIAL.
*    MESSAGE 'Erfolgreich zurückgesetzt' TYPE 'S'.
*  ENDIF.
*ENDIF.
DATA: alv_tab  TYPE TABLE OF ty_alv,
      alv_line LIKE LINE OF alv_tab.


IF p_rbpa = abap_true.
  SELECT * FROM pa9400 INTO TABLE @DATA(pa9400_tab) WHERE begda LE @sy-datum AND endda GE @sy-datum.
ELSEIF p_rbom = abap_true.
  SELECT * FROM hrp9401 INTO TABLE @DATA(hrp9401_tab) WHERE begda LE @sy-datum AND endda GE @sy-datum.
ENDIF.

IF pa9400_tab IS NOT INITIAL.
  LOOP AT pa9400_tab ASSIGNING FIELD-SYMBOL(<pa_9400>).
    SELECT SINGLE * FROM pa0002 INTO @DATA(pa9400) WHERE pernr = @<pa_9400>-pernr AND begda LE @sy-datum AND endda GE @sy-datum.
    CLEAR: alv_line.
    alv_line-pernr = <pa_9400>-pernr.
    alv_line-oracle_id = <pa_9400>-oracleid.
    alv_line-firstname = pa9400-vorna.
    alv_line-lastname = pa9400-nachn.
    APPEND alv_line TO alv_tab.
    CLEAR: pa9400.
  ENDLOOP.
ENDIF.

IF hrp9401_tab IS NOT INITIAL.
  LOOP AT hrp9401_tab ASSIGNING FIELD-SYMBOL(<hrp_9401>).
    CLEAR: alv_line.
    alv_line-pernr = <hrp_9401>-objid.
    alv_line-oracle_id = <hrp_9401>-oracleid.
    APPEND alv_line TO alv_tab.
  ENDLOOP.
ENDIF.

DATA: alv       TYPE REF TO cl_salv_table,
      message   TYPE bapiret2-message,
      functions TYPE REF TO cl_salv_functions_list,
      columns   TYPE REF TO cl_salv_columns,
      column    TYPE REF TO cl_salv_column_table,
      settings  TYPE REF TO cl_salv_functional_settings,
      lx_col    TYPE REF TO cx_salv_not_found.

TRY.
    cl_salv_table=>factory(
    IMPORTING r_salv_table = alv
      CHANGING t_table = alv_tab ).
  CATCH cx_salv_msg INTO DATA(lx_msg).
    message = lx_msg->get_text( ).
    MESSAGE message TYPE 'W'.
ENDTRY.

functions = alv->get_functions( ).
functions->set_all( abap_true ).

*TRY.
*    columns = alv->get_columns( ).
*    column ?= columns->get_column('PERNR').
*    column->set_icon( if_salv_c_bool_sap=>true ).
*    column->set_alignment( if_salv_c_alignment=>centered ).
*    column->set_long_text( value = 'SAP Personalnummer' ).
*    column->set_output_length( value = 10 ).
*  CATCH cx_salv_not_found INTO lx_col.
*    message = lx_col->get_longtext( ).
*    MESSAGE message TYPE 'W'.
*ENDTRY.
*
*TRY.
*    columns = alv->get_columns( ).
*    column ?= columns->get_column('ORACLE_ID').
*    column->set_icon( if_salv_c_bool_sap=>true ).
*    column->set_alignment( if_salv_c_alignment=>centered ).
*    column->set_long_text( value = 'Oracle ID' ).
*    column->set_output_length( value = 20 ).
*  CATCH cx_salv_not_found INTO lx_col.
*    message = lx_col->get_longtext( ).
*    MESSAGE message TYPE 'W'.
*ENDTRY.
*
*TRY.
*    columns = alv->get_columns( ).
*    column ?= columns->get_column('FIRSTNAME').
*    column->set_icon( if_salv_c_bool_sap=>true ).
*    column->set_alignment( if_salv_c_alignment=>centered ).
*    column->set_long_text( value = 'Vorname' ).
*    column->set_output_length( value = 20 ).
*  CATCH cx_salv_not_found INTO lx_col.
*    message = lx_col->get_longtext( ).
*    MESSAGE message TYPE 'W'.
*ENDTRY.
*
*TRY.
*    columns = alv->get_columns( ).
*    column ?= columns->get_column('LASTNAME').
*    column->set_icon( if_salv_c_bool_sap=>true ).
*    column->set_alignment( if_salv_c_alignment=>centered ).
*    column->set_long_text( value = 'Nachname' ).
*    column->set_output_length( value = 20 ).
*  CATCH cx_salv_not_found INTO lx_col.
*    message = lx_col->get_longtext( ).
*    MESSAGE message TYPE 'W'.
*ENDTRY.

*DATA(lv_xml_type) = if_salv_bs_xml=>c_type_mhtml.
*DATA(lv_xml) = alv->to_xml( xml_type = lv_xml_type ).
*cl_abap_browser=>show_xml(
*EXPORTING
**  xml_string   =  CONV #( lv_xml )
*  xml_xstring = lv_xml ).

alv->display( ).
