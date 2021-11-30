*&---------------------------------------------------------------------*
*& Report /SEW/INT_TEST_XML
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /sew/int_clear_case.

DATA:
  aend_id_range TYPE rsdsselopt_t,
  aend_id       LIKE LINE OF aend_id_range.
SELECTION-SCREEN BEGIN OF BLOCK frame1.
PARAMETERS: "p_sapid  TYPE hrobjid OBLIGATORY,
  "p_cldid  TYPE /sew/dd_objectid OBLIGATORY,
  p_intrun TYPE guid_32 OBLIGATORY,
  p_id     TYPE /sew/dd_element	OBLIGATORY.
SELECTION-SCREEN   END OF BLOCK frame1.
SELECT * FROM /sew/int_it_aend INTO TABLE @DATA(it_aend) WHERE int_run = @p_intrun AND cloud_id = @p_id. "AND status = @p_stat.
CHECK it_aend IS NOT INITIAL.
aend_id_range = VALUE #( FOR pa IN it_aend ( option = 'EQ' sign = 'I' low = pa-aend_id ) ).
CHECK aend_id_range IS NOT INITIAL.
SELECT * FROM /sew/int_msg_l INTO TABLE @DATA(msg_l) WHERE aend_id IN @aend_id_range.
SELECT * FROM /sew/int_msg_f INTO TABLE @DATA(msg_f) WHERE aend_id IN @aend_id_range.
SELECT * FROM /sew/int_msg_p INTO TABLE @DATA(msg_p) WHERE aend_id IN @aend_id_range AND int_run = @p_intrun.
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
COMMIT WORK.
IF sy-subrc IS INITIAL.
  COMMIT WORK.
  MESSAGE 'Erfolgreich zurückgesetzt' TYPE 'S'.
ENDIF.
