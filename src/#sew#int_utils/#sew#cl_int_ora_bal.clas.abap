class /SEW/CL_INT_ORA_BAL definition
  public
  create public .

public section.

  methods GET_PERAS
    importing
      !BEGDA type BEGDA
      !ENDDA type ENDDA
    returning
      value(PERNR) type RSDSSELOPT_T .
  methods GET
    importing
      !PERNR type RSDSSELOPT_T optional
      !ORACLEPERNR type RSDSSELOPT_T optional
      !KTART type RSDSSELOPT_T optional
      !ENDDA type ENDDA optional
      !BEGDA type BEGDA optional
    returning
      value(ORA_BAL) type /SEW/TT_ORA_BAL .
  methods INSERT
    importing
      !ORA_BAL type /SEW/TT_ORA_BAL
    returning
      value(IS_OK) type BOOLEAN .
  methods DELETE
    importing
      !ORA_BAL type /SEW/TT_ORA_BAL
    returning
      value(IS_OK) type BOOLEAN .
protected section.
private section.
ENDCLASS.



CLASS /SEW/CL_INT_ORA_BAL IMPLEMENTATION.


METHOD delete.

  DELETE /sew/int_ora_bal FROM TABLE ora_bal.

  CHECK sy-subrc EQ 0.
  is_ok = abap_true.

ENDMETHOD.


METHOD get.
  SELECT * INTO TABLE ora_bal FROM /sew/int_ora_bal WHERE pernr        IN pernr       AND
                                                          oracle_pernr IN oraclepernr AND
                                                          ktart        IN ktart       AND
                                                          plan_begda   LE endda       AND
                                                          plan_endda   GE begda.
ENDMETHOD.


METHOD get_peras.

  SELECT DISTINCT pernr FROM /sew/int_ora_bal INTO TABLE @DATA(pernr_tmp) WHERE plan_begda LE @endda AND
                                                                                plan_endda GE @begda.

  LOOP AT pernr_tmp ASSIGNING FIELD-SYMBOL(<pernr>).
    APPEND VALUE rsdsselopt( sign = 'I' option = 'EQ' low = <pernr> ) TO pernr.
  ENDLOOP.

ENDMETHOD.


METHOD insert.

  MODIFY /sew/int_ora_bal FROM TABLE ora_bal.

  CHECK sy-subrc EQ 0.
  is_ok = abap_true.

ENDMETHOD.
ENDCLASS.
