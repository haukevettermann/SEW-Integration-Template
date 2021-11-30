*&---------------------------------------------------------------------*
*& Report /SEW/INT_TEST_XML
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /sew/int_clear_aend_tab.

DATA:
  aend_id_range TYPE rsdsselopt_t,
  aend_id       LIKE LINE OF aend_id_range.
SELECTION-SCREEN BEGIN OF BLOCK frame1.
PARAMETERS: "p_sapid  TYPE hrobjid OBLIGATORY,
  "p_cldid  TYPE /sew/dd_objectid OBLIGATORY,
  p_intrun TYPE guid_32 OBLIGATORY,
  p_stat   TYPE /sew/dd_status,
  p_test   TYPE boole_d.
SELECTION-SCREEN   END OF BLOCK frame1.
SELECTION-SCREEN BEGIN OF BLOCK frame2.
*PARAMETERS: p_test TYPE boole_d,
*            p_arch TYPE boole_d.
PARAMETERS: p_rbpa RADIOBUTTON GROUP rbg DEFAULT 'X'.
PARAMETERS: p_rbom RADIOBUTTON GROUP rbg.
SELECTION-SCREEN   END OF BLOCK frame2.
CHECK p_intrun IS NOT INITIAL.
IF p_rbpa = abap_true.
  SELECT * FROM /sew/int_it_aend INTO TABLE @DATA(it_aend_orig) WHERE int_run = @p_intrun. "AND status = @p_stat.
ELSE.
  SELECT * FROM /sew/int_om_aend INTO TABLE @DATA(om_aend_orig) WHERE int_run = @p_intrun. "AND status = @p_stat.
ENDIF.
WHILE lines( it_aend_orig ) > 0 OR lines( om_aend_orig ) > 0.
  IF it_aend_orig IS NOT INITIAL.
    DATA(it_aend) = it_aend_orig.
    DELETE it_aend FROM 1001.
    DATA(lines) = lines( it_aend_orig ).
    IF lines >= 1000.
      DELETE it_aend_orig FROM 1 TO 1000.
    ELSE.
      DELETE it_aend_orig FROM 1 TO lines.
    ENDIF.
    aend_id_range = VALUE #( FOR pa IN it_aend ( option = 'EQ' sign = 'I' low = pa-aend_id ) ).
  ELSEIF om_aend_orig IS NOT INITIAL.
    DATA(om_aend) = om_aend_orig.
    DELETE om_aend FROM 1001.
    lines = lines( om_aend_orig ).
    IF lines >= 1000.
      DELETE om_aend_orig FROM 1 TO 1000.
    ELSE.
      DELETE om_aend_orig FROM 1 TO lines.
    ENDIF.
    aend_id_range = VALUE #( FOR om IN om_aend ( option = 'EQ' sign = 'I' low = om-aend_id ) ).
  ENDIF.
  SELECT * FROM /sew/int_msg_l INTO TABLE @DATA(msg_l) WHERE aend_id IN @aend_id_range.
  SELECT * FROM /sew/int_msg_f INTO TABLE @DATA(msg_f) WHERE aend_id IN @aend_id_range.
  SELECT * FROM /sew/int_msg_p INTO TABLE @DATA(msg_p) WHERE aend_id IN @aend_id_range AND int_run = @p_intrun.
  IF p_test = abap_false.
    IF msg_l IS NOT INITIAL.
      DELETE /sew/int_msg_l FROM TABLE msg_l.
    ENDIF.
    IF msg_p IS NOT INITIAL.
      DELETE /sew/int_msg_p FROM TABLE msg_p.
    ENDIF.
    IF msg_f IS NOT INITIAL.
      DELETE /sew/int_msg_f FROM TABLE msg_f.
    ENDIF.
    IF it_aend IS NOT INITIAL.
      DELETE /sew/int_it_aend FROM TABLE it_aend.
    ENDIF.
    IF om_aend IS NOT INITIAL.
      DELETE /sew/int_om_aend FROM TABLE om_aend.
    ENDIF.
    COMMIT WORK.
    IF sy-subrc IS INITIAL.
      COMMIT WORK.
      MESSAGE 'Erfolgreich zurückgesetzt' TYPE 'S'.
    ENDIF.
  ENDIF.
  IF it_aend_orig IS INITIAL AND om_aend_orig IS INITIAL.
    EXIT.
  ENDIF.
ENDWHILE.
