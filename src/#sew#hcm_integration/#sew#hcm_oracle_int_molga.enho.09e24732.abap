"Name: \PR:SAPLHRPAD00HIREEMPLOYEE\FO:INITIAL_CHECKS\SE:END\EI
ENHANCEMENT 0 /SEW/HCM_ORACLE_INT_MOLGA.
*
  "Only use for oracle integration run when it's a hire
  IF sy-cprog = '/SEW/RP_IT_AEND_POST' AND pernr IS INITIAL.
    molga = '99'.
    viekn = '99'.
    ENDIF.
ENDENHANCEMENT.
