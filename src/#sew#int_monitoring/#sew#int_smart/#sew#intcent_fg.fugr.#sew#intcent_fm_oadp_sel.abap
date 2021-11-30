FUNCTION /sew/intcent_fm_oadp_sel.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     REFERENCE(USER) TYPE  SYUNAME OPTIONAL
*"     REFERENCE(BEGDA) TYPE  OBJEC-BEGDA DEFAULT SY-DATUM
*"     REFERENCE(ENDDA) TYPE  OBJEC-ENDDA DEFAULT SY-DATUM
*"     REFERENCE(EVPATH) TYPE  GDSTR-WEGID OPTIONAL
*"     REFERENCE(EVALPATH) TYPE  GDSTR-WEGID OPTIONAL
*"     REFERENCE(DEPTH) TYPE  GDSTR-DEPTH DEFAULT 0
*"     REFERENCE(PLVAR) LIKE  OBJEC-PLVAR OPTIONAL
*"  TABLES
*"      PATHROOTS STRUCTURE  HRROOTOB OPTIONAL
*"      ROOT_OBJECTS STRUCTURE  HRROOTOB OPTIONAL
*"      ROOT_OBJEC STRUCTURE  OBJEC OPTIONAL
*"      ROOT_STRUC STRUCTURE  STRUC OPTIONAL
*"      PARAMVALUES STRUCTURE  HRWPC_S_OADP_PARAMVALUE OPTIONAL
*"      RESULT_OBJEC STRUCTURE  OBJEC OPTIONAL
*"      RESULT_STRUC STRUCTURE  STRUC OPTIONAL
*"----------------------------------------------------------------------
  DATA :
*    lt_result_tab   TYPE STANDARD TABLE OF swhactor,
    lt_result_objec TYPE STANDARD TABLE OF objec,
    lt_result_struc TYPE STANDARD TABLE OF struc,
    lv_plvar        TYPE plvar.

  IF pathroots IS REQUESTED.
    CLEAR: lt_result_objec, lt_result_struc.

    CALL FUNCTION 'RH_STRUC_GET'
      EXPORTING
        act_otype      = 'US'
        act_objid      = CONV objid( user )
        act_wegid      = evalpath
        act_begda      = begda
        act_endda      = endda
        act_tdepth     = depth
      TABLES
        result_struc   = lt_result_struc
      EXCEPTIONS
        no_plvar_found = 1
        no_entry_found = 2
        OTHERS         = 3.

    pathroots[] = CORRESPONDING #( lt_result_struc ).
  ELSE.
    LOOP AT root_objects INTO DATA(ls_rootobjec) WHERE otype = 'US'.
      CLEAR: lt_result_objec, lt_result_struc.

      CALL FUNCTION 'RH_STRUC_GET'
        EXPORTING
          act_otype      = ls_rootobjec-otype
          act_objid      = ls_rootobjec-objid
          act_wegid      = evpath
          act_begda      = begda
          act_endda      = endda
          act_plvar      = lv_plvar
          act_tdepth     = depth
        IMPORTING
          act_plvar      = lv_plvar
        TABLES
          result_objec   = lt_result_objec
          result_struc   = lt_result_struc
        EXCEPTIONS
          no_plvar_found = 1
          no_entry_found = 2
          OTHERS         = 3.

      DATA(lv_lines) = lines( result_struc ).
      LOOP AT lt_result_struc INTO DATA(ls_result_struc) WHERE otype = 'US'.
        DATA(ls_result_objec) = lt_result_objec[ sy-tabix ].

* count up seqnr because SAP is deleting entries with same seqnr
        ls_result_struc-seqnr = lv_lines = lv_lines + 1.
        APPEND ls_result_objec TO result_objec.
        APPEND ls_result_struc TO result_struc.

* read sachx table, furthermore check for super user
        SELECT * FROM t526 INTO TABLE @DATA(lt_t526) WHERE usrid = @ls_result_struc-objid.
        SELECT * FROM /sew/int_superus INTO TABLE @DATA(lt_superus) WHERE uname = @ls_result_struc-objid.

        IF line_exists( lt_superus[ sbgrp_code = '*' sb_code = '*' ] ).
          CLEAR: lt_t526.
          SELECT * FROM t526 INTO TABLE @lt_t526.
        ELSEIF line_exists( lt_superus[ sb_code = '*' ] ).
          CLEAR: lt_t526.
          DATA: lr_range_sachgrp TYPE RANGE OF sbmod.
          lr_range_sachgrp = VALUE #( FOR <ls_superus> IN lt_superus ( sign = 'I' option = 'EQ' low = <ls_superus>-sbgrp_code ) ).
          SELECT * FROM t526 INTO TABLE @lt_t526 WHERE werks IN @lr_range_sachgrp.
        ELSE.
          LOOP AT lt_superus INTO DATA(ls_superus) WHERE sbgrp_code NE '*' AND sb_code NE '*'.
            SELECT * FROM t526 INTO TABLE @DATA(lt_t526_tmp) WHERE werks = @ls_superus-sbgrp_code.
            APPEND LINES OF lt_t526_tmp TO lt_t526.
            CLEAR: lt_t526_tmp.
          ENDLOOP.
        ENDIF.

* append items from t625 to resulting table
        LOOP AT lt_t526 INTO DATA(ls_t526).
          ls_result_struc-seqnr = lv_lines = lv_lines + 1.
          ls_result_objec-otype = ls_result_struc-otype = 'XG'.
          ls_result_objec-objid = ls_result_struc-objid = ls_result_objec-realo = |{ ls_t526-werks }{ ls_t526-sachx }|.

          APPEND ls_result_objec TO result_objec.
          APPEND ls_result_struc TO result_struc.
        ENDLOOP.

* append items from superuser to esulting table
        LOOP AT lt_superus INTO ls_superus.
          if ls_superus-sbgrp_code = '*'.
            ls_superus-sbgrp_code = ''.
          endif.
          if ls_superus-sb_code = '*'.
            ls_superus-sb_code = ''.
          endif.

          ls_result_struc-seqnr = lv_lines = lv_lines + 1.
          ls_result_objec-otype = ls_result_struc-otype = 'XG'.
          ls_result_objec-objid = ls_result_struc-objid = ls_result_objec-realo = |{ ls_superus-sbgrp_code }{ ls_superus-sb_code }|.

          APPEND ls_result_objec TO result_objec.
          APPEND ls_result_struc TO result_struc.
        ENDLOOP.

        IF line_exists( lt_superus[ sbgrp_code = '*' sb_code = '*' ] ).
          ls_result_struc-seqnr = lv_lines = lv_lines + 1.
          ls_result_objec-otype = ls_result_struc-otype = 'US'.
          ls_result_objec-objid = ls_result_struc-objid = ls_result_objec-realo = |{ sy-uname }|.

          APPEND ls_result_objec TO result_objec.
          APPEND ls_result_struc TO result_struc.
        endif.
      ENDLOOP.
    ENDLOOP.

    SORT result_objec BY otype objid realo. SORT result_struc BY otype objid.
    DELETE ADJACENT DUPLICATES FROM result_objec COMPARING otype objid realo.
    DELETE ADJACENT DUPLICATES FROM result_struc COMPARING otype objid.
  ENDIF.
ENDFUNCTION.
