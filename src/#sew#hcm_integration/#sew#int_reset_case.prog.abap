*&---------------------------------------------------------------------*
*& Report /SEW/INT_TEST_XML
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /sew/int_reset_case.

DATA:
  aend_id_range TYPE rsdsselopt_t,
  aend_id       LIKE LINE OF aend_id_range.
SELECTION-SCREEN BEGIN OF BLOCK frame1.
PARAMETERS: p_sapid  TYPE hrobjid OBLIGATORY,
            p_cldid  TYPE /sew/dd_objectid OBLIGATORY,
            p_intrun TYPE guid_32 OBLIGATORY,
            p_stat TYPE stat2 OBLIGATORY,
            p_test   TYPE boole_d.
SELECTION-SCREEN   END OF BLOCK frame1.
SELECTION-SCREEN BEGIN OF BLOCK frame2.
*PARAMETERS: p_test TYPE boole_d,
*            p_arch TYPE boole_d.
PARAMETERS: p_rbpa RADIOBUTTON GROUP rbg DEFAULT 'X'.
PARAMETERS: p_rbom RADIOBUTTON GROUP rbg.
SELECTION-SCREEN   END OF BLOCK frame2.

* Set static attributes
*/sew/cl_int_statics=>read_archive = p_rarch.
*/sew/cl_int_statics=>archive = p_arch.
*/sew/cl_int_statics=>test_run = p_test.
* Get physical path
IF p_rbpa = abap_true.
  SELECT * FROM /sew/int_it_aend INTO TABLE @DATA(it_aend) WHERE pernr = @p_sapid AND cloud_id = @p_cldid AND int_run = @p_intrun.
ELSE.
  SELECT * FROM /sew/int_om_aend INTO TABLE @DATA(om_aend) WHERE sap_id = @p_sapid AND cloud_id = @p_cldid AND int_run = @p_intrun.
ENDIF.

LOOP AT it_aend INTO DATA(record_pa).
  CLEAR: aend_id.
  aend_id-low = record_pa-aend_id.
  aend_id-option = 'EQ'.
  aend_id-sign = 'I'.
  APPEND aend_id TO aend_id_range.
ENDLOOP.

LOOP AT om_aend INTO DATA(record_om).
  CLEAR: aend_id.
  aend_id-low = record_om-aend_id.
  aend_id-option = 'EQ'.
  aend_id-sign = 'I'.
  APPEND aend_id TO aend_id_range.
ENDLOOP.

SELECT * FROM /sew/int_msg_l INTO TABLE @DATA(msg_l) WHERE aend_id IN @aend_id_range.
SELECT * FROM /sew/int_msg_f INTO TABLE @DATA(msg_f) WHERE aend_id IN @aend_id_range.
SELECT * FROM /sew/int_msg_p INTO TABLE @DATA(msg_p) WHERE aend_id IN @aend_id_range AND int_run = @p_intrun AND sap_id = @p_sapid AND cloud_id = @p_cldid.
IF p_test = abap_false.
  DELETE /sew/int_msg_l FROM TABLE msg_l.
  DELETE /sew/int_msg_p FROM TABLE msg_p.
  DELETE /sew/int_msg_f FROM TABLE msg_f.
  LOOP AT it_aend ASSIGNING FIELD-SYMBOL(<record_pa>).
    <record_pa>-status = '01'.
  ENDLOOP.
  LOOP AT om_aend ASSIGNING FIELD-SYMBOL(<record_om>).
    <record_om>-status = '01'.
  ENDLOOP.
  MODIFY /sew/int_it_aend FROM TABLE it_aend.
  MODIFY /sew/int_om_aend FROM TABLE om_aend.
  IF sy-subrc IS INITIAL.
    MESSAGE 'Erfolgreich zurückgesetzt' TYPE 'S'.
  ENDIF.
ENDIF.

*DATA(alv) = NEW /sew/cl_int_alv( testrun = p_test ).
*alv->build_alv_structure( ).
*alv->display_alv( ).
