"Name: \PR:SAPFP50G\FO:UPDATE_PERNR\SE:BEGIN\EI
ENHANCEMENT 0 /SEW/HCM_ORALCE_INT_OMPA_INT.
IF sy-cprog = '/SEW/RP_IT_AEND_POST'.
  GET PARAMETER ID 'HC' FIELD DATA(global_hc).
  IF global_hc = abap_false.
  RETURN.
  ENDIF.
ENDIF.
ENDENHANCEMENT.