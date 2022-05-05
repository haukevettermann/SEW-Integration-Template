class /SEW/CL_MIG_ASSIGNMENT_MAN definition
  public
  create public .

public section.

  data PERNR type RSDSSELOPT_T .
  data BEGDA type BEGDA .
  data ENDDA type ENDDA .
  data COFU type BOOLEAN .
  data COGL type BOOLEAN .
  data MOLGA type RSDSSELOPT_T .
  data P0001 type P0001_TAB .
  data ASSIGNMENT_MAN_STRUCTURE type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  constants ASSIGNMENT_SUPER type STRING value 'AssignmentSupervisor' ##NO_TEXT.
  constants ASSIGNMENT type STRING value 'ASSIGNMENT_' ##NO_TEXT.
  constants ASSIGNMENT_MAN type STRING value '_ASSIGNMENT_MANAGER' ##NO_TEXT.
  data P0000 type P0000_TAB .
  constants WT type STRING value '_WT' ##NO_TEXT.
  constants ASSIGN type STRING value 'ASN_' ##NO_TEXT.

  methods PROCEED_COFU_ASSIGN_MANAGER
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !WORKER type ref to /SEW/CL_MIG_WORKER
    returning
      value(DATA) type STRING .
  methods PROCEED_COGL_ASSIGN_MANAGER
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !WORKER type ref to /SEW/CL_MIG_WORKER
      !WORKTERMS type ref to /SEW/CL_MIG_WORK_TERMS
    returning
      value(DATA) type STRING .
  methods PROCEED_COGU_ASSIGN_MANAGER
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !WORKER type ref to /SEW/CL_MIG_WORKER
      !WORKTERMS type ref to /SEW/CL_MIG_WORK_TERMS
    returning
      value(DATA) type STRING .
  methods CONSTRUCTOR
    importing
      !PERNR type RSDSSELOPT_T
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !COFU type BOOLEAN
      !COGL type BOOLEAN
      !MOLGA type RSDSSELOPT_T
      !COGU type BOOLEAN .
  methods CREATE_METADATA
    returning
      value(METADATA) type STRING .
  methods GET_COFU_MANAGERS_OF_EMP
    importing
      !PERIODS type HRPERIODS_TAB
      !PERNR type PERNR_D .
  methods MAP_COFU_DATA
    importing
      !PERIODS type HRPERIODS_TAB
      !PERNR type PERNR_D
    returning
      value(DATA) type STRING .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF leader_objid_con ,
      pernr_emp TYPE pernr_d,
      pernr_man TYPE pernr_d,
      begda TYPE begda,
      endda TYPE endda,
    END OF leader_objid_con .

  data MAPPING_VALUES_MASSN type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPPING .
  data MAPPING_FIELDS_MASSN type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPP_FI .
  data:
    leader_emp TYPE TABLE OF leader_objid_con WITH DEFAULT KEY .
  data LEADER_PER type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  data VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  data PERNR_HISTORY type RSDSSELOPT_T .
  data COGU type BOOLEAN .

  methods CREATE_HIRE_ENTRY
    returning
      value(DATA) type STRING .
  methods MAP_MIG_VALUES
    importing
      !P0000 type P0000
    exporting
      !MASSN type /SEW/DD_VALUE .
  methods GET_MANAGER
    importing
      !PERNR type PERNR_D
      !BEGDA type BEGDA
      !ENDDA type ENDDA
    returning
      value(MANAGER_PERNR) type PERNR_D .
  methods GET_MANAGERS_OF_EMP .
  methods GET_COFU_DATA .
  methods GET_COGL_DATA .
  methods GET_MAPPING_FIELDS .
  methods GET_MAPPING_VALUES .
  methods MAP_COGL_DATA
    returning
      value(DATA) type STRING .
  methods MAP_COGU_DATA
    returning
      value(DATA) type STRING .
ENDCLASS.



CLASS /SEW/CL_MIG_ASSIGNMENT_MAN IMPLEMENTATION.


METHOD constructor.

  me->pernr = pernr.
  me->begda = begda.
  me->endda = endda.
  me->cofu  = cofu.
  me->cogu  = cogu.
  me->cogl  = cogl.
  me->molga = molga.

  IF cogl EQ abap_true OR
     cogu EQ abap_true.
    assignment_man_structure = VALUE #( ( name = 1  value = /sew/cl_mig_utils=>merge )
                                        ( name = 2  value = assignment_super )
                                        ( name = 3  value = 'SourceSystemId' )
                                        ( name = 4  value = 'SourceSystemOwner' )
                                        ( name = 5  value = 'AssignmentId(SourceSystemId)' )
                                        ( name = 6  value = 'EffectiveStartDate' )
                                        ( name = 7  value = 'EffectiveEndDate' )
                                        ( name = 8  value = 'ManagerAssignmentId(SourceSystemId)' )
                                        ( name = 13 value = 'ManagerId(SourceSystemId)' )
                                        ( name = 9  value = 'ManagerType' )
                                        ( name = 10 value = 'PersonId(SourceSystemId)' )
                                        ( name = 11 value = 'PrimaryFlag' )
                                        ( name = 12 value = 'ActionCode' ) ).

  ELSEIF cofu EQ abap_true.
    assignment_man_structure = VALUE #( ( name = 1  value = /sew/cl_mig_utils=>merge )
                                        ( name = 2  value = assignment_super )
                                        ( name = 3  value = 'SourceSystemId' )
                                        ( name = 4  value = 'SourceSystemOwner' )
                                        ( name = 5  value = 'AssignmentId(SourceSystemId)' )
                                        ( name = 6  value = 'EffectiveStartDate' )
                                        ( name = 7  value = 'EffectiveEndDate' )
                                        ( name = 8  value = 'ManagerAssignmentId(SourceSystemId)' )
                                        ( name = 13 value = 'ManagerId(SourceSystemId)' )
                                        ( name = 9  value = 'ManagerType' )
                                        ( name = 10 value = 'PersonId(SourceSystemId)' )
                                        ( name = 11 value = 'PrimaryFlag' )
                                        ( name = 12 value = 'ActionCode' ) ).
  ENDIF.
ENDMETHOD.


METHOD create_hire_entry.

  DATA: plvar           TYPE plvar,
        internal_number TYPE string,
        leader_assign   TYPE string,
        leader_pernr    TYPE string.

  "get history assignment
  DATA(p0001_history) = p0001.

  DELETE p0001_history WHERE begda GT sy-datum.
  DELETE p0001_history WHERE begda LE sy-datum AND
                             endda GE sy-datum.

  "delete history from main table
  LOOP AT p0001_history ASSIGNING FIELD-SYMBOL(<p0001_historxy>).
    DELETE TABLE p0001 FROM <p0001_historxy>.
  ENDLOOP.

  "only first assignment relevant
  SORT p0001_history BY pernr begda ASCENDING.
  DELETE ADJACENT DUPLICATES FROM p0001_history COMPARING pernr.

  "get actual assignment
  DATA(p0001_actual) = p0001.
  DELETE p0001_actual WHERE endda LT sy-datum OR
                            begda GT sy-datum.

  DATA(massn_term) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '03' ) ).

  "get active plvar
  CALL FUNCTION 'RH_GET_PLVAR'
    IMPORTING
      plvar = plvar.

  CONCATENATE /sew/cl_mig_utils=>sap sy-mandt INTO DATA(sys_id).

  LOOP AT p0001_history ASSIGNING FIELD-SYMBOL(<p0001>).

    "get relevant P0000
    LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000>) WHERE begda LE <p0001>-endda AND
                                                        endda GE <p0001>-begda AND
                                                        pernr EQ <p0001>-pernr.
      EXIT.
    ENDLOOP.

    CHECK sy-subrc EQ 0.

    "get actual assignment and set enddate of history entry to actual begda - 1
    LOOP AT p0001_actual ASSIGNING FIELD-SYMBOL(<p0001_actual>) WHERE pernr EQ <p0001>-pernr.
      <p0001>-endda = <p0001_actual>-begda - 1.
      EXIT.
    ENDLOOP.

    CHECK sy-subrc EQ 0.

    "in case of termination ignore entry
*    CHECK <p0000>-massn NOT IN massn_term. "JMB20210312 D: Provide entry even if it´s provided in WorkRelationship

    map_mig_values( EXPORTING p0000 = <p0000>
                    IMPORTING massn = DATA(massn) ).

    DATA(begda_tmp) = /sew/cl_mig_utils=>convert_date( <p0001>-begda ).
    DATA(endda_tmp) = /sew/cl_mig_utils=>convert_date( <p0001>-endda ).
    CONCATENATE assign <p0001>-pernr assignment_man INTO DATA(src_id).
    CONCATENATE assign <p0001>-pernr INTO DATA(asn_id).

    "get source id
    DATA(src_sys_id) = /sew/cl_mig_utils=>get_src_id( pernr = <p0001>-pernr
                                                      begda = <p0001>-begda
                                                      endda = <p0001>-endda
                                                      vp_src_id = vp_src_id ).

    "get boss
    LOOP AT leader_emp ASSIGNING FIELD-SYMBOL(<leader>) WHERE pernr_emp EQ <p0001>-pernr AND
                                                              begda LE <p0001>-endda AND
                                                              endda GE <p0001>-begda.
      leader_assign = COND #( WHEN <leader>-pernr_man IS NOT INITIAL
                              THEN assign && CONV string( <leader>-pernr_man )
                              ELSE '' ).
      READ TABLE leader_per INTO DATA(leader_per_e) WITH KEY name = <leader>-pernr_man.
      leader_pernr = leader_per_e-value.
      EXIT.
    ENDLOOP.

    IF leader_assign IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0001>-pernr ) TO pernr_history.
      CONCATENATE /sew/cl_mig_utils=>merge
                  assignment_super
                  src_id
                  sys_id
                  asn_id
                  begda_tmp
                  endda_tmp
                  leader_assign
                  leader_pernr
                  'LINE_MANAGER'
                  src_sys_id
                  /sew/cl_mig_utils=>yes
                  massn
      INTO DATA(data_tmp) SEPARATED BY /sew/cl_mig_utils=>separator.

      CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
    ENDIF.

    CLEAR: leader_pernr, leader_assign.
  ENDLOOP.
ENDMETHOD.


  METHOD create_metadata.

    DESCRIBE TABLE assignment_man_structure LINES DATA(length).

    LOOP AT assignment_man_structure ASSIGNING FIELD-SYMBOL(<assign_man_struc>).

      "set METADATA title
      CASE <assign_man_struc>-name.
        WHEN 1.
          CONCATENATE /sew/cl_mig_utils=>metadata /sew/cl_mig_utils=>separator INTO metadata.
          CONTINUE.
      ENDCASE.

      CONCATENATE metadata <assign_man_struc>-value INTO metadata.

      "set separator
      CHECK length NE sy-tabix.
      CONCATENATE metadata /sew/cl_mig_utils=>separator INTO metadata.
    ENDLOOP.

  ENDMETHOD.


METHOD GET_COFU_DATA.
  "Get IT0000
  SELECT pernr,
         begda,
         endda,
         massn INTO CORRESPONDING FIELDS OF TABLE @p0000 FROM pa0000 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.

  "Get IT0001
  SELECT pernr,
         begda,
         endda,
         orgeh INTO CORRESPONDING FIELDS OF TABLE @p0001 FROM pa0001 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.
ENDMETHOD.


METHOD GET_COFU_MANAGERS_OF_EMP.

  DATA: plvar         TYPE plvar,
        manager_table TYPE objec_t,
        pernr_emp     TYPE TABLE OF leader_objid_con WITH DEFAULT KEY,
        leader_pernr  TYPE rsdsselopt_t.

  CALL FUNCTION 'RH_GET_PLVAR'
    IMPORTING
      plvar = plvar.

  pernr_emp = VALUE #( FOR <period> in periods ( pernr_emp = pernr
                                                 begda     = <period>-begda
                                                 endda     = <period>-endda ) ).

  SORT pernr_emp BY pernr_emp begda endda.
  DELETE ADJACENT DUPLICATES FROM pernr_emp.

  LOOP AT pernr_emp ASSIGNING FIELD-SYMBOL(<pernr_emp>).

    "check pernr already was readed in date range
    LOOP AT leader_emp ASSIGNING FIELD-SYMBOL(<leader>) WHERE pernr_emp EQ <pernr_emp>-pernr_emp AND
                                                              begda     LE <pernr_emp>-endda     AND
                                                              endda     GE <pernr_emp>-begda.
      EXIT.
    ENDLOOP.

    CHECK sy-subrc NE 0.

    "get manager
    DATA(manager_pernr) = get_manager( pernr = <pernr_emp>-pernr_emp
                                       begda = <pernr_emp>-begda
                                       endda = <pernr_emp>-endda ).

    APPEND VALUE #( pernr_emp = <pernr_emp>-pernr_emp
                    begda = <pernr_emp>-begda
                    endda = <pernr_emp>-endda
                    pernr_man = manager_pernr ) TO leader_emp.

    APPEND VALUE #( sign   = 'I'
                    option = 'EQ'
                    low    = manager_pernr ) TO leader_pernr.
  ENDLOOP.

  CHECK leader_pernr IS NOT INITIAL.  "JMB20210911 I

  "get worker ID of leader
  NEW /sew/cl_mig_worker( molga = molga
                          begda = begda
                          endda = endda
                          cogl  = cogl
                          cofu  = cofu
                          cogu  = cogu
                          pernr = leader_pernr )->proceed_cogl_worker( IMPORTING vp_src_id = leader_per ).

ENDMETHOD.


METHOD get_cogl_data.
  "Get IT0000
  SELECT pernr,
         begda,
         endda,
         massn INTO CORRESPONDING FIELDS OF TABLE @p0000 FROM pa0000 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.

  "Get IT0001
  SELECT pernr,
         begda,
         endda,
         orgeh INTO CORRESPONDING FIELDS OF TABLE @p0001 FROM pa0001 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.
ENDMETHOD.


METHOD get_manager.

  DATA: lt_leader_obj   TYPE hrobject_t,
        lv_sobid        TYPE sobid,
        lv_active_plvar TYPE plvar,
        lt_p1001        TYPE STANDARD TABLE OF p1001,
        ls_p1001        TYPE p1001,
        lv_pernr        TYPE pernr_d.

  CLEAR manager_pernr.

* get active plan variant
  CALL FUNCTION 'RH_GET_ACTIVE_WF_PLVAR'
    IMPORTING
      act_plvar = lv_active_plvar.

* get leading positions
  lv_sobid = pernr.
  CALL FUNCTION 'RH_GET_LEADING_POSITION'
    EXPORTING
      plvar             = lv_active_plvar
      otype             = cl_hrpa_tclas=>otype_employee
      sobid             = lv_sobid
      date              = begda
      auth              = abap_false
      consider_vac_pos  = abap_true
    TABLES
      leading_pos       = lt_leader_obj
    EXCEPTIONS
      no_lead_pos_found = 1
      OTHERS            = 2.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

* get holders of leading positions
  CALL FUNCTION 'RH_READ_INFTY_1001'
    EXPORTING
      with_stru_auth   = abap_true
      subty            = 'A008'
      begda            = begda
      endda            = endda
    TABLES
      objects          = lt_leader_obj
      i1001            = lt_p1001
    EXCEPTIONS
      nothing_found    = 1
      wrong_condition  = 2
      wrong_parameters = 3
      OTHERS           = 4.
  IF sy-subrc <> 0.
    RETURN.
  ELSE.
    READ TABLE lt_p1001 INTO DATA(p1001) INDEX 1.
    manager_pernr = p1001-sobid.
  ENDIF.

  CHECK manager_pernr IS NOT INITIAL.

  "check inactive employment
  CHECK /sew/cl_mig_utils=>check_active_emp_status( pernr = manager_pernr ) EQ abap_false.

  CLEAR: manager_pernr.
ENDMETHOD.


METHOD GET_MANAGERS_OF_EMP.

  DATA: plvar         TYPE plvar,
        manager_table TYPE objec_t,
        pernr_emp     TYPE TABLE OF leader_objid_con WITH DEFAULT KEY,
        leader_pernr  TYPE rsdsselopt_t.

  CALL FUNCTION 'RH_GET_PLVAR'
    IMPORTING
      plvar = plvar.

  pernr_emp = VALUE #( FOR <p0001> in p0001 ( pernr_emp = <p0001>-pernr
                                              begda     = <p0001>-begda
                                              endda     = <p0001>-endda ) ).

  SORT pernr_emp BY pernr_emp begda endda.
  DELETE ADJACENT DUPLICATES FROM pernr_emp.

  LOOP AT pernr_emp ASSIGNING FIELD-SYMBOL(<pernr_emp>).

    "check pernr already was readed in date range
    LOOP AT leader_emp ASSIGNING FIELD-SYMBOL(<leader>) WHERE pernr_emp EQ <pernr_emp>-pernr_emp AND
                                                              begda     LE <pernr_emp>-endda     AND
                                                              endda     GE <pernr_emp>-begda.
      EXIT.
    ENDLOOP.

    CHECK sy-subrc NE 0.

    "get manager
    DATA(manager_pernr) = get_manager( pernr = <pernr_emp>-pernr_emp
                                       begda = <pernr_emp>-begda
                                       endda = <pernr_emp>-endda ).

    APPEND VALUE #( pernr_emp = <pernr_emp>-pernr_emp
                    begda = <pernr_emp>-begda
                    endda = <pernr_emp>-endda
                    pernr_man = manager_pernr ) TO leader_emp.

    APPEND VALUE #( sign   = 'I'
                    option = 'EQ'
                    low    = manager_pernr ) TO leader_pernr.
  ENDLOOP.

  CHECK leader_pernr IS NOT INITIAL.  "JMB20210911 I

  "get worker ID of leader
  NEW /sew/cl_mig_worker( molga = molga
                          begda = begda
                          endda = endda
                          cogl  = cogl
                          cofu  = cofu
                          cogu  = cogu
                          pernr = leader_pernr )->proceed_cogl_worker( IMPORTING vp_src_id = leader_per ).

ENDMETHOD.


METHOD get_mapping_fields.

  "get mapping fields for actioncode
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0000
                                                   sap_field    = /sew/cl_mig_utils=>massn
                                                   oracle_field = /sew/cl_mig_utils=>actioncode
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_massn ).
ENDMETHOD.


METHOD get_mapping_values.

  "get mapping values for actioncode
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0000
                                                   sap_field    = /sew/cl_mig_utils=>massn
                                                   oracle_field = /sew/cl_mig_utils=>actioncode
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_massn ).
ENDMETHOD.


METHOD MAP_COFU_DATA.

  DATA: leader_assign TYPE string,
        leader_pernr  TYPE string.

  DATA(massn_term) = SWITCH rsdsselopt_t( sy-mandt
                                         WHEN /sew/cl_int_constants=>cofu_mandant-germany     THEN VALUE #( ( sign = 'I' option = 'EQ' low = '10' )
                                                                                                            ( sign = 'I' option = 'EQ' low = '11' )
                                                                                                            ( sign = 'I' option = 'EQ' low = '13' )
                                                                                                            ( sign = 'I' option = 'EQ' low = '18' )
                                                                                                            ( sign = 'I' option = 'EQ' low = '35' )
                                                                                                            ( sign = 'I' option = 'EQ' low = '99' )
                                                                                                            ( sign = 'I' option = 'EQ' low = 'ZZ' ) )
                                         WHEN /sew/cl_int_constants=>cofu_mandant-france      THEN VALUE #( ( sign = 'I' option = 'EQ' low = '09' )
                                                                                                            ( sign = 'I' option = 'EQ' low = '10' )
                                                                                                            ( sign = 'I' option = 'EQ' low = '11' )
                                                                                                            ( sign = 'I' option = 'EQ' low = '18' )
                                                                                                            ( sign = 'I' option = 'EQ' low = '32' )
                                                                                                            ( sign = 'I' option = 'EQ' low = '34' ) )
                                         WHEN /sew/cl_int_constants=>cofu_mandant-netherlands THEN VALUE #( ( sign = 'I' option = 'EQ' low = 'Z4' ) )
                                         "default Italy/Austria
                                         ELSE VALUE #( ( sign = 'I' option = 'EQ' low = '03' )
                                                       ( sign = 'I' option = 'EQ' low = 'ZZ' ) ) ).

  CONCATENATE /sew/cl_mig_utils=>sap sy-mandt INTO DATA(sys_id).

  LOOP AT periods ASSIGNING FIELD-SYMBOL(<period>).
    "get boss
    LOOP AT leader_emp ASSIGNING FIELD-SYMBOL(<leader>) WHERE pernr_emp EQ pernr AND
                                                              begda     LE <period>-endda AND
                                                              endda     GE <period>-begda.
      leader_assign = COND #( WHEN <leader>-pernr_man IS NOT INITIAL
                              THEN assign && CONV string( <leader>-pernr_man )
                              ELSE '' ).
      READ TABLE leader_per INTO DATA(leader_per_e) WITH KEY name = <leader>-pernr_man.
      leader_pernr = leader_per_e-value.
      EXIT.
    ENDLOOP.

    CHECK leader_assign IS NOT INITIAL.

    "get relevant P0000
    LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000>) WHERE begda LE <period>-endda AND
                                                        endda GE <period>-begda AND
                                                        pernr EQ pernr.
      EXIT.
    ENDLOOP.

    CHECK sy-subrc EQ 0.

    "in case of termination ignore entry
*      CHECK <p0000>-massn NOT IN massn_term. "JMB20210312 D: Provide entry even if it´s provided in WorkRelationship

    map_mig_values( EXPORTING p0000 = <p0000>
                    IMPORTING massn = DATA(massn) ).

    "check hire entry for employee
    IF pernr  IN pernr_history AND
        pernr_history IS NOT INITIAL.
      "actioncode for these entries will be MANAGER_CHANGE
      massn = 'MANAGER_CHANGE'.
    ELSE.
      massn = COND #( WHEN massn NE 'HIRE'
                      THEN 'HIRE'
                      ELSE massn ).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = pernr ) TO pernr_history.
    ENDIF.

    DATA(begda_tmp) = /sew/cl_mig_utils=>convert_date( <period>-begda ).
    DATA(endda_tmp) = /sew/cl_mig_utils=>convert_date( <period>-endda ).
    CONCATENATE /sew/cl_mig_utils=>assign pernr assignment_man INTO DATA(src_id).
    CONCATENATE /sew/cl_mig_utils=>assign pernr INTO DATA(asn_id).

    "get source id
    DATA(src_sys_id) = /sew/cl_mig_utils=>get_src_id( pernr = pernr
                                                      begda = <period>-begda
                                                      endda = <period>-endda
                                                      vp_src_id = vp_src_id ).

    CONCATENATE /sew/cl_mig_utils=>merge
                assignment_super
                src_id
                sys_id
                asn_id
                begda_tmp
                endda_tmp
                leader_assign
                leader_pernr
                'LINE_MANAGER'
                src_sys_id
                /sew/cl_mig_utils=>yes
                massn
    INTO DATA(data_tmp) SEPARATED BY /sew/cl_mig_utils=>separator.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.

    CLEAR: leader_pernr, leader_assign.
  ENDLOOP.
ENDMETHOD.


METHOD map_cogl_data.

  DATA: leader_assign TYPE string,
        leader_pernr  TYPE string.

  DATA(massn_term) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '03' ) ).

  CONCATENATE /sew/cl_mig_utils=>sap sy-mandt INTO DATA(sys_id).

  DELETE leader_emp WHERE pernr_man EQ '00021945'.  "JMB20210714 I - Don´t pass CEO in CoGl as manager

  LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>).
    "get boss
    LOOP AT leader_emp ASSIGNING FIELD-SYMBOL(<leader>) WHERE pernr_emp EQ <p0001>-pernr AND
                                                              begda     LE <p0001>-endda AND
                                                              endda     GE <p0001>-begda.
      leader_assign = COND #( WHEN <leader>-pernr_man IS NOT INITIAL
                              THEN assign && CONV string( <leader>-pernr_man )
                              ELSE '' ).
      READ TABLE leader_per INTO DATA(leader_per_e) WITH KEY name = <leader>-pernr_man.
      leader_pernr = leader_per_e-value.
      EXIT.
    ENDLOOP.

    CHECK leader_assign IS NOT INITIAL.

    "get relevant P0000
    LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000>) WHERE begda LE <p0001>-endda AND
                                                        endda GE <p0001>-begda AND
                                                        pernr EQ <p0001>-pernr.
      EXIT.
    ENDLOOP.

    CHECK sy-subrc EQ 0.

    "in case of termination ignore entry
    CHECK <p0000>-massn NOT IN massn_term.

    "check for terminations right after actual record
    DATA(datum) = CONV datum( <p0001>-endda + 1 ).
    LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000_term>) WHERE begda LE datum         AND
                                                             endda GE datum         AND
                                                             pernr EQ <p0001>-pernr AND
                                                             massn IN massn_term.
      <p0001>-endda = <p0000_term>-endda.
    ENDLOOP.

    map_mig_values( EXPORTING p0000 = <p0000>
                    IMPORTING massn = DATA(massn) ).

    "check hire entry for employee
    IF <p0001>-pernr  IN pernr_history AND
        pernr_history IS NOT INITIAL.
      "actioncode for these entries will be MANAGER_CHANGE
      massn = 'MANAGER_CHANGE'.
    ELSE.
      massn = COND #( WHEN massn NE 'HIRE'
                      THEN 'HIRE'
                      ELSE massn ).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0001>-pernr ) TO pernr_history.
    ENDIF.

    DATA(begda_tmp) = /sew/cl_mig_utils=>convert_date( <p0001>-begda ).
    DATA(endda_tmp) = /sew/cl_mig_utils=>convert_date( <p0001>-endda ).
    CONCATENATE /sew/cl_mig_utils=>assign <p0001>-pernr assignment_man INTO DATA(src_id).
    CONCATENATE /sew/cl_mig_utils=>assign <p0001>-pernr INTO DATA(asn_id).

    "get source id
    DATA(src_sys_id) = /sew/cl_mig_utils=>get_src_id( pernr = <p0001>-pernr
                                                      begda = <p0001>-begda
                                                      endda = <p0001>-endda
                                                      vp_src_id = vp_src_id ).

    CONCATENATE /sew/cl_mig_utils=>merge
                assignment_super
                src_id
                sys_id
                asn_id
                begda_tmp
                endda_tmp
                leader_assign
                leader_pernr
                'LINE_MANAGER'
                src_sys_id
                /sew/cl_mig_utils=>yes
                massn
    INTO DATA(data_tmp) SEPARATED BY /sew/cl_mig_utils=>separator.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.

    CLEAR: leader_pernr, leader_assign.
  ENDLOOP.
ENDMETHOD.


METHOD MAP_COGU_DATA.

  DATA: leader_assign TYPE string,
        leader_pernr  TYPE string.

  DATA(massn_term) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '03' ) ).

  CONCATENATE /sew/cl_mig_utils=>sap sy-mandt INTO DATA(sys_id).

  LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>).
    "get boss
    LOOP AT leader_emp ASSIGNING FIELD-SYMBOL(<leader>) WHERE pernr_emp EQ <p0001>-pernr AND
                                                              begda     LE <p0001>-endda AND
                                                              endda     GE <p0001>-begda.
      leader_assign = COND #( WHEN <leader>-pernr_man IS NOT INITIAL
                              THEN assign && CONV string( <leader>-pernr_man )
                              ELSE '' ).
      READ TABLE leader_per INTO DATA(leader_per_e) WITH KEY name = <leader>-pernr_man.
      leader_pernr = leader_per_e-value.
      EXIT.
    ENDLOOP.

    CHECK leader_assign IS NOT INITIAL.

    "get relevant P0000
    LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000>) WHERE begda LE <p0001>-endda AND
                                                        endda GE <p0001>-begda AND
                                                        pernr EQ <p0001>-pernr.
      EXIT.
    ENDLOOP.

    CHECK sy-subrc EQ 0.

    "in case of termination ignore entry
    CHECK <p0000>-massn NOT IN massn_term.

    "check for terminations right after actual record
    DATA(datum) = CONV datum( <p0001>-endda + 1 ).
    LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000_term>) WHERE begda LE datum         AND
                                                             endda GE datum         AND
                                                             pernr EQ <p0001>-pernr AND
                                                             massn IN massn_term.
      <p0001>-endda = <p0000_term>-endda.
    ENDLOOP.

    map_mig_values( EXPORTING p0000 = <p0000>
                    IMPORTING massn = DATA(massn) ).

    "check hire entry for employee
    IF <p0001>-pernr  IN pernr_history AND
        pernr_history IS NOT INITIAL.
      "actioncode for these entries will be MANAGER_CHANGE
      massn = 'MANAGER_CHANGE'.
    ELSE.
      massn = COND #( WHEN massn NE 'HIRE'
                      THEN 'HIRE'
                      ELSE massn ).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0001>-pernr ) TO pernr_history.
    ENDIF.

    DATA(begda_tmp) = /sew/cl_mig_utils=>convert_date( <p0001>-begda ).
    DATA(endda_tmp) = /sew/cl_mig_utils=>convert_date( <p0001>-endda ).
    CONCATENATE /sew/cl_mig_utils=>assign <p0001>-pernr assignment_man INTO DATA(src_id).
    CONCATENATE /sew/cl_mig_utils=>assign <p0001>-pernr INTO DATA(asn_id).

    "get source id
    DATA(src_sys_id) = /sew/cl_mig_utils=>get_src_id( pernr = <p0001>-pernr
                                                      begda = <p0001>-begda
                                                      endda = <p0001>-endda
                                                      vp_src_id = vp_src_id ).

    CONCATENATE /sew/cl_mig_utils=>merge
                assignment_super
                src_id
                sys_id
                asn_id
                begda_tmp
                endda_tmp
                leader_assign
                leader_pernr
                'LINE_MANAGER'
                src_sys_id
                /sew/cl_mig_utils=>yes
                massn
    INTO DATA(data_tmp) SEPARATED BY /sew/cl_mig_utils=>separator.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.

    CLEAR: leader_pernr, leader_assign.
  ENDLOOP.
ENDMETHOD.


METHOD MAP_MIG_VALUES.
  DATA: value_tmp TYPE /sew/dd_value.

  "Process MASSN mapping
  value_tmp = CONV #( p0000-massn ).
  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0000
      field_sap      = /sew/cl_mig_utils=>massn
      field_oracle   = /sew/cl_mig_utils=>actioncode
      mapping_fields = CONV #( mapping_fields_massn )
      mapping_values = CONV #( mapping_values_massn )
    CHANGING
      value          = value_tmp ).

  massn = value_tmp.
ENDMETHOD.


METHOD PROCEED_COFU_ASSIGN_MANAGER.
  me->vp_src_id = vp_src_id.
  p0000         = worker->p0000.
  get_mapping_fields( ).
  get_mapping_values( ).
ENDMETHOD.


METHOD proceed_cogl_assign_manager.
  me->vp_src_id = vp_src_id.

  get_cogl_data( ).
  /sew/cl_mig_utils=>update_begin_date( EXPORTING p0000 = worker->p0000
                                         CHANGING p0001 = p0001 ).
*  DATA(hire_data) = create_hire_entry( ).  "JMB20210427 D - Not needed due to only actual assignment is necessary

  /sew/cl_mig_utils=>check_assign_supervisor( CHANGING p0001 = p0001 ).

  "get only actual and future managers
  DELETE p0001 WHERE endda LT sy-datum.

  get_managers_of_emp( ).
  get_mapping_fields( ).
  get_mapping_values( ).

  data = map_cogl_data( ).

*  CONCATENATE hire_data data INTO data.  "JMB20210427 D
ENDMETHOD.


METHOD PROCEED_COGU_ASSIGN_MANAGER.
  me->vp_src_id = vp_src_id.

  get_cogl_data( ).
  /sew/cl_mig_utils=>update_begin_date( EXPORTING p0000 = worker->p0000
                                         CHANGING p0001 = p0001 ).

  /sew/cl_mig_utils=>check_assign_supervisor( CHANGING p0001 = p0001 ).

  "get only actual and future managers
  DELETE p0001 WHERE endda LT sy-datum.

  get_managers_of_emp( ).
  get_mapping_fields( ).
  get_mapping_values( ).

  data = map_cogu_data( ).

*  CONCATENATE hire_data data INTO data.  "JMB20210427 D
ENDMETHOD.
ENDCLASS.
