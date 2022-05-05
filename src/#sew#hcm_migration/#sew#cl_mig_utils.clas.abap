class /SEW/CL_MIG_UTILS definition
  public
  create public .

public section.

  types:
    /sew/tt_int_mapping TYPE TABLE OF /sew/int_mapping .
  types:
    /sew/tt_int_mapp_fi TYPE TABLE OF /sew/int_mapp_fi .

  constants SAP type STRING value 'SAP_' ##NO_TEXT.
  constants SEPARATOR type CHAR1 value '|' ##NO_TEXT.
  constants MERGE type STRING value 'MERGE' ##NO_TEXT.
  constants METADATA type STRING value 'METADATA' ##NO_TEXT.
  constants IT0002 type INFTY value '0002' ##NO_TEXT.
  constants IT0006 type INFTY value '0006' ##NO_TEXT.
  constants IT0001 type INFTY value '0001' ##NO_TEXT.
  constants IT0021 type INFTY value '0021' ##NO_TEXT.
  constants IT0105 type INFTY value '0105' ##NO_TEXT.
  constants IT0105_0010 type SUBTY value '0010' ##NO_TEXT.
  constants IT0105_9002 type SUBTY value '9002' ##NO_TEXT.
  constants IT0105_9004 type SUBTY value '9004' ##NO_TEXT.
  constants IT0105_9005 type SUBTY value '9005' ##NO_TEXT.
  constants IT0105_9003 type SUBTY value '9003' ##NO_TEXT.
  constants IT0105_9901 type SUBTY value '9901' ##NO_TEXT.
  constants IT0105_9906 type SUBTY value '9906' ##NO_TEXT.
  constants IT0105_9905 type SUBTY value '9905' ##NO_TEXT.
  constants IT0105_9902 type SUBTY value '9902' ##NO_TEXT.
  constants IT0185 type INFTY value '0185' ##NO_TEXT.
  constants IT0105_9998 type SUBTY value '9998' ##NO_TEXT.
  constants IT0105_9900 type SUBTY value '9900' ##NO_TEXT.
  constants IT0050 type INFTY value '0050' ##NO_TEXT.
  constants IT0701 type INFTY value '0701' ##NO_TEXT.
  constants IT0000 type INFTY value '0000' ##NO_TEXT.
  constants YES type STRING value 'Y' ##NO_TEXT.
  constants NO type STRING value 'N' ##NO_TEXT.
  constants PERSK_ASSIGN_TYPE type /SEW/DD_FIELD value 'PERSK_ASSIGN_TYPE' ##NO_TEXT.
  constants PERSK_ASSIGN_CATEGORY type /SEW/DD_FIELD value 'PERSK_ASSIGN_CATEGORY' ##NO_TEXT.
  constants PERSK_ASSIGN_PERSON_TYPE type /SEW/DD_FIELD value 'PERSK_ASSIGN_PERSON_TYPE' ##NO_TEXT.
  constants PERSK_PERSON_TYPE type /SEW/DD_FIELD value 'PERSK_PERSON_TYPE' ##NO_TEXT.
  constants PERSK_SYSTEM_PERSON type /SEW/DD_FIELD value 'PERSK_SYSTEM_PERSON' ##NO_TEXT.
  constants PERSK_WORKER_TYPE type /SEW/DD_FIELD value 'PERSK_WORKER_TYPE' ##NO_TEXT.
  constants MASSN type /SEW/DD_FIELD value 'MASSN' ##NO_TEXT.
  constants ACTIONCODE type /SEW/DD_FIELD value 'ACTIONCODE' ##NO_TEXT.
  constants LEGISLATIONCODE type /SEW/DD_FIELD value 'LEGISLATIONCODE' ##NO_TEXT.
  constants WORKERTYPE type /SEW/DD_FIELD value 'WORKERTYPE' ##NO_TEXT.
  constants PERSG type /SEW/DD_FIELD value 'PERSG' ##NO_TEXT.
  constants MASSG type /SEW/DD_FIELD value 'MASSG' ##NO_TEXT.
  constants REASONCODE type /SEW/DD_FIELD value 'REASONCODE' ##NO_TEXT.
  constants BTRTL type /SEW/DD_FIELD value 'BTRTL' ##NO_TEXT.
  constants LOCATIONCODE type /SEW/DD_FIELD value 'LOCATIONCODE' ##NO_TEXT.
  constants WERKS type /SEW/DD_FIELD value 'WERKS' ##NO_TEXT.
  constants LEGALEMPLOYERNAME type /SEW/DD_FIELD value 'LEGALEMPLOYERNAME' ##NO_TEXT.
  constants BUKRS type /SEW/DD_FIELD value 'BUKRS' ##NO_TEXT.
  constants BUSINESSUNITSHORTCODE type /SEW/DD_FIELD value 'BUSINESSUNITSHORTCODE' ##NO_TEXT.
  constants DEFAULT_BUSINESS_UNIT type STRING value 'Data Migration BU' ##NO_TEXT.
  constants DEPARTMENTNAME type /SEW/DD_FIELD value 'DEPARTMENTNAME' ##NO_TEXT.
  constants ORGEH type /SEW/DD_FIELD value 'ORGEH' ##NO_TEXT.
  constants HIRE type MASSN value '01' ##NO_TEXT.
  constants ORACLE_HD type DATUM value '47121231' ##NO_TEXT.
  constants IT0105_0001 type SUBTY value '0001' ##NO_TEXT.
  constants ASSIGN type STRING value 'ASN_' ##NO_TEXT.
  constants IT0105_9001 type SUBTY value '9001' ##NO_TEXT.
  constants CONTACT_TYPE type /SEW/DD_FIELD value 'CONTACTTYPE' ##NO_TEXT.
  constants IT0004 type INFTY value '0004' ##NO_TEXT.
  constants DISABILITY_CATEGORY type /SEW/DD_FIELD value 'DISABILITYCATEGORY' ##NO_TEXT.
  constants SBGRU type /SEW/DD_FIELD value 'SBGRU' ##NO_TEXT.
  constants COMPONENTCODE type /SEW/DD_FIELD value 'COMPONENTCODE' ##NO_TEXT.
  constants FASEX type /SEW/DD_FIELD value 'FASEX' ##NO_TEXT.

  class-methods GET_HR_PERIODS
    importing
      !TABLE type TABLE
    changing
      !HR_PERIODS type HRPERIODS_TAB .
  class-methods SUMMARIZE_PAST
    importing
      !MIG_DATE type DATUM optional
    changing
      !HR_PERIODS type HRPERIODS_TAB .
  class-methods CHECK_ACTIVE_EMP_STATUS
    importing
      !DATE type BEGDA default SY-DATLO
      !PERNR type PERNR_D
    returning
      value(STATUS) type BOOLEAN .
  class-methods CHECK_ASSIGN_SUPERVISOR
    importing
      !COFU type BOOLEAN optional
      !ALL_PERIODS type BOOLEAN optional
    exporting
      !MANAGER_PERNR type RSDSSELOPT_T
    changing
      !P0001 type P0001_TAB .
  class-methods CHECK_ASSIGN_SUPERVISOR_V2
    importing
      !OBJECTS type OBJEC_T
    returning
      value(MANAGER_ASSIGNMENT) type /SEW/TT_POSITION_MANAGER_DATA .
  class-methods GET_LEGISLATION_CODES
    importing
      !BUKRS type RSDSSELOPT_T
    returning
      value(LAND1_MAP) type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  class-methods GET_MAPPING_FIELDS
    importing
      !MOLGA type RSDSSELOPT_T
      !INFTY type INFTY
      !SAP_FIELD type /SEW/DD_FIELD
      !ORACLE_FIELD type /SEW/DD_FIELD
      !EXPORT type /SEW/DD_EXPORT
    exporting
      value(MAPPING_FIELDS) type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPP_FI .
  class-methods GET_MAPPING_VALUES
    importing
      !MOLGA type RSDSSELOPT_T
      !INFTY type INFTY
      !SAP_FIELD type /SEW/DD_FIELD
      !ORACLE_FIELD type /SEW/DD_FIELD
      !EXPORT type /SEW/DD_EXPORT
    exporting
      value(MAPPING_VALUES) type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPPING .
  class-methods GET_MNGRS_TO_ORGUNIT
    importing
      !ORGEH type RSDSSELOPT_T
      !BEGDA type BEGDA
      !ENDDA type ENDDA
    exporting
      !MANAGERS type HRP1001_T
      !LEADER_POSITIONS type HRP1001_T .
  class-methods IS_MANAGER
    importing
      !P0001 type P0001
      !PLVAR type PLVAR optional
    returning
      value(IS_MANAGER_ORACLE) type CHAR1 .
  class-methods SUMMARIZE_IT0000_COFU
    changing
      !P0000 type P0000_TAB .
  class-methods SUMMARIZE_IT0000_COGL
    changing
      !P0000 type P0000_TAB .
  class-methods SUMMARIZE_IT0002
    changing
      !P0002 type P0002_TAB .
  class-methods UPDATE_BEGIN_DATE
    importing
      !P0000 type P0000_TAB
      !CREATE_HIRE type BOOLEAN optional
    changing
      !P0001 type P0001_TAB optional
      !P0002 type P0002_TAB optional .
  class-methods GET_SRC_ID
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !PERNR type PERNR_D
      !BEGDA type BEGDA
      !ENDDA type ENDDA
    returning
      value(SRC_ID) type STRING .
  class-methods CONVERT_DATE
    importing
      !DATUM type DATUM
    returning
      value(FORM_DATUM) type STRING .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS /SEW/CL_MIG_UTILS IMPLEMENTATION.


  METHOD check_active_emp_status.

    DATA: p0000  TYPE p0000_tab,
          active TYPE stat2 VALUE '3'.

    status = abap_false.

    NEW /sew/cl_int_it_operation( )->read_paxxxx( EXPORTING pernr = pernr
                                                            infty = CONV #( /sew/cl_int_constants=>it0000 )
                                                            begda = date
                                                            endda = date
                                                            simu  = abap_false
                                                  IMPORTING record_tab = p0000 ).

    CHECK p0000 IS NOT INITIAL.
    READ TABLE p0000 ASSIGNING FIELD-SYMBOL(<p0000>) INDEX 1.

    CHECK <p0000> IS ASSIGNED.
    CHECK <p0000>-stat2 EQ active.
    status      = abap_true.

  ENDMETHOD.


METHOD check_assign_supervisor.

  DATA: plvar             TYPE plvar,
        leader_type       TYPE otype,
        leader_id         TYPE realo,
        datum             TYPE datum,
        manager_info      TYPE objec_t,
        begin_dates       TYPE rsdsselopt_t,
        hr_periods        TYPE hrperiods_tab,
        hr_periods_search TYPE hrperiods_tab,
        hr_periods_all    TYPE hrperiods_tab,
        p0001_tmp         TYPE p0001_tab,
        count             TYPE i VALUE 0,
        index             TYPE i VALUE 0.

  CALL FUNCTION 'RH_GET_PLVAR'
    IMPORTING
      plvar = plvar.

  LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>).
    DATA(begda) = <p0001>-begda.
    DATA(endda) = <p0001>-endda.
    DATA(all_collect) = abap_false.

    IF <p0001>-orgeh IS INITIAL. "JMB20210811 I
      APPEND <p0001> TO p0001_tmp.
      CONTINUE.
    ENDIF.

    APPEND VALUE #( begda = <p0001>-begda
                    endda = <p0001>-endda ) TO hr_periods.

    WHILE all_collect EQ abap_false.
      CALL FUNCTION 'HRCM_ORGUNIT_MANAGER_GET'
        EXPORTING
          plvar              = plvar
          otype              = 'O'
          objid              = <p0001>-orgeh
          begda              = begda
          endda              = endda
          path_id            = 'MAN_O'
        TABLES
          manager_info_table = manager_info
        EXCEPTIONS
          nothing_found      = 1
          path_error         = 2
          root_error         = 3.

      DELETE manager_info WHERE otype NE 'P'.
      READ TABLE manager_info INTO DATA(pernr_found) WITH KEY objid = <p0001>-pernr. "JMB20220110 I

**JMB20220127 start insert - in case employee is manager itself of the OrgUnit, get manager from OrgUnit above
*
      READ TABLE manager_info INTO DATA(manager) WITH KEY objid = <p0001>-pernr.
      DELETE manager_info WHERE objid EQ <p0001>-pernr. "JMB20220110 I
*JMB20220127 insert end

      LOOP AT manager_info ASSIGNING FIELD-SYMBOL(<mngr_info>).
        APPEND VALUE #( begda = <mngr_info>-begda
                        endda = <mngr_info>-endda ) TO hr_periods.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = <mngr_info>-begda ) TO begin_dates.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = <mngr_info>-objid ) TO manager_pernr.
      ENDLOOP.

**JMB20210802 start insert - in case no manager was found pass period
*
      IF manager_info IS INITIAL.

**JMB20211129 start insert - in case no manager was found, check assignment of OrgUnit to get time range
*
        SELECT begda, endda, sobid FROM hrp1001 INTO TABLE @DATA(o1001_orgunit) WHERE otype EQ 'O'            AND
                                                                                      objid EQ @<p0001>-orgeh AND
                                                                                      rsign EQ 'A'            AND
                                                                                      relat EQ '002'          AND
                                                                                      begda LE @endda         AND
                                                                                      endda GE @begda
                                                                                ORDER BY begda ASCENDING.

        IF o1001_orgunit IS NOT INITIAL.
          READ TABLE o1001_orgunit ASSIGNING FIELD-SYMBOL(<o1001_a002>) INDEX 1.

**JMB20220127 start insert - in case employee is manager itself of the OrgUnit, get manager from OrgUnit above
*
          IF manager IS NOT INITIAL.
            DATA(org_orgeh) = <p0001>-orgeh.
            <p0001>-orgeh   = <o1001_a002>-sobid.
            CONTINUE.
          ENDIF.
*JMB20220127 insert end

          IF <o1001_a002>-endda LT endda.
            endda = <o1001_a002>-endda.
          ENDIF.

          IF begda NOT IN begin_dates.
            APPEND VALUE #( sign = 'I' option = 'EQ' low = begda ) TO begin_dates.
            CLEAR: o1001_orgunit.
            CONTINUE.
          ENDIF.
        ENDIF.
*JMB20211129 insert end

        APPEND VALUE #( begda = begda
                        endda = endda ) TO hr_periods.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = begda ) TO begin_dates.
      ENDIF.
*JMB20210802 end insert

**JMB20220127 start insert - in case employee is manager itself of the OrgUnit, get manager from OrgUnit above
*
      CLEAR: manager.
      IF org_orgeh IS NOT INITIAL.
        <p0001>-orgeh = org_orgeh.
        CLEAR: org_orgeh.
      ENDIF.
*JMB20220127 insert end

      "build periods
      CALL FUNCTION 'RHXPROVIDE_PERIODS'
        TABLES
          provide_tab = hr_periods.

      LOOP AT hr_periods ASSIGNING FIELD-SYMBOL(<hr_periods>) WHERE begda NOT IN begin_dates.
        APPEND <hr_periods> TO hr_periods_search.
        count = count + 1.
      ENDLOOP.

**JMB20210527 start insert - in case not all periods are needed
*
      IF all_periods  EQ abap_false AND
         manager_info IS INITIAL.
        DATA(hr_period) = VALUE hrperiods( begda = begda
                                           endda = endda ).
        IF pernr_found IS INITIAL.  "JMB20220110 I
          DELETE TABLE hr_periods FROM hr_period.
        ENDIF.

      ENDIF.

      CLEAR: pernr_found.
*JMB20210527 insert end

      "get next period to be checked
      IF index LT count.
        index = index + 1.
        READ TABLE hr_periods_search ASSIGNING FIELD-SYMBOL(<search_period>) INDEX index.
        begda = <search_period>-begda.
        endda = <search_period>-endda.
      ELSE.
        all_collect = abap_true.
      ENDIF.

      CLEAR: manager_info. ", hr_periods.
    ENDWHILE.

    APPEND LINES OF hr_periods TO hr_periods_all.

    SORT hr_periods_all BY begda ASCENDING.

    "collect all old entries in actual IT0001 entry in one entry
    DATA(hr_periods_old) = hr_periods_all.
    DELETE hr_periods_old WHERE endda GE sy-datum.

    "check if there were older periods for COGL
    IF hr_periods_old IS NOT INITIAL AND
       cofu           IS INITIAL.
      "get earliest entry
      READ TABLE hr_periods_old ASSIGNING FIELD-SYMBOL(<hr_periods_old>) INDEX 1.

      IF sy-subrc EQ 0.
        <p0001>-begda = <hr_periods_old>-begda.
      ENDIF.

      "get latest entry
      SORT hr_periods_old BY begda DESCENDING.
      READ TABLE hr_periods_old ASSIGNING <hr_periods_old> INDEX 1.

      IF sy-subrc EQ 0.
        <p0001>-endda = <hr_periods_old>-endda.
      ENDIF.

      APPEND <p0001> TO p0001_tmp.

      "pass actual and future entries
      LOOP AT hr_periods_old ASSIGNING <hr_periods_old>.
        DELETE TABLE hr_periods_all FROM <hr_periods_old>.
      ENDLOOP.
    ENDIF.

    LOOP AT hr_periods_all ASSIGNING FIELD-SYMBOL(<hr_periods_all>).
      <p0001>-begda = <hr_periods_all>-begda.
      <p0001>-endda = <hr_periods_all>-endda.

      APPEND <p0001> TO p0001_tmp.
    ENDLOOP.

    CLEAR: manager_info, hr_periods, hr_periods_search, hr_periods_all, hr_periods_old, index, count, begin_dates.
  ENDLOOP.

  SORT manager_pernr BY low.
  DELETE ADJACENT DUPLICATES FROM manager_pernr COMPARING low.

  p0001 = p0001_tmp.
ENDMETHOD.


METHOD check_assign_supervisor_v2.


*  DATA: plvar             TYPE plvar,
*        leader_type       TYPE otype,
*        leader_id         TYPE realo,
*        datum             TYPE datum,
*        manager_info      TYPE objec_t,
*        begin_dates       TYPE rsdsselopt_t,
*        hr_periods        TYPE hrperiods_tab,
*        hr_periods_search TYPE hrperiods_tab,
*        hr_periods_all    TYPE hrperiods_tab,
*        p0001_tmp         TYPE p0001_tab,
*        count             TYPE i VALUE 0,
*        index             TYPE i VALUE 0.
*
*  CALL FUNCTION 'RH_GET_PLVAR'
*    IMPORTING
*      plvar = plvar.
*
*  LOOP AT objects ASSIGNING FIELD-SYMBOL(<object>).
*    DATA(begda) = <object>-begda.
*    DATA(endda) = <object>-endda.
*    DATA(all_collect) = abap_false.
*
*    APPEND VALUE #( begda = <object>-begda
*                    endda = <object>-endda ) TO hr_periods.
*
*    WHILE all_collect EQ abap_false.
*      CALL FUNCTION 'HRCM_ORGUNIT_MANAGER_GET'
*        EXPORTING
*          plvar              = plvar
*          otype              = 'O'
*          objid              = <object>-objid
*          begda              = begda
*          endda              = endda
*          path_id            = 'MAN_O'
*        TABLES
*          manager_info_table = manager_info
*        EXCEPTIONS
*          nothing_found      = 1
*          path_error         = 2
*          root_error         = 3.
*
*      DELETE manager_info WHERE otype NE 'P'.
*
*      LOOP AT manager_info ASSIGNING FIELD-SYMBOL(<mngr_info>).
*        APPEND VALUE #( begda = <mngr_info>-begda
*                        endda = <mngr_info>-endda ) TO hr_periods.
*        APPEND VALUE #( sign = 'I' option = 'EQ' low = <mngr_info>-begda ) TO begin_dates.
*        APPEND INITIAL LINE TO manager_assignment  ASSIGNING FIELD-SYMBOL(<manager_assignment>).
*        <manager_assignment>-objid = <object>-objid.
*        <manager_assignment>-begda = <mngr_info>-begda.
*        <manager_assignment>-endda = <mngr_info>-endda.
*        <manager_assignment>-manager_id = <mngr_info>-objid.
*      ENDLOOP.
*
***JMB20210802 start insert - in case no manager was found pass period
**
*      IF manager_info IS INITIAL.
*        APPEND VALUE #( begda = begda
*                        endda = endda ) TO hr_periods.
*        APPEND VALUE #( sign = 'I' option = 'EQ' low = begda ) TO begin_dates.
*      ENDIF.
**JMB20210802 end insert
*
*      "build periods
*      CALL FUNCTION 'RHXPROVIDE_PERIODS'
*        TABLES
*          provide_tab = hr_periods.
*
*      LOOP AT hr_periods ASSIGNING FIELD-SYMBOL(<hr_periods>) WHERE begda NOT IN begin_dates.
*        APPEND <hr_periods> TO hr_periods_search.
*        count = count + 1.
**        DELETE TABLE hr_periods FROM <hr_periods>.
*      ENDLOOP.
*
***JMB20210527 start insert - in case not all periods are needed
**
*      IF all_periods  EQ abap_false AND
*         manager_info IS INITIAL.
*        DATA(hr_period) = VALUE hrperiods( begda = begda
*                                           endda = endda ).
*        DELETE TABLE hr_periods FROM hr_period.
*      ENDIF.
**JMB20210527 insert end
*
*      "get next period to be checked
*      IF index LT count.
*        index = index + 1.
*        READ TABLE hr_periods_search ASSIGNING FIELD-SYMBOL(<search_period>) INDEX index.
*        begda = <search_period>-begda.
*        endda = <search_period>-endda.
*      ELSE.
*        all_collect = abap_true.
*      ENDIF.
*
*      CLEAR: manager_info. ", hr_periods.
*    ENDWHILE.
*
*    APPEND LINES OF hr_periods TO hr_periods_all.
*
*    SORT hr_periods_all BY begda ASCENDING.
*
*    "collect all old entries in actual IT0001 entry in one entry
*    DATA(hr_periods_old) = hr_periods_all.
*    DELETE hr_periods_old WHERE endda GE sy-datum.
*
*    "check if there were older periods for COGL
*    IF hr_periods_old IS NOT INITIAL AND
*       cofu           IS INITIAL.
*      "get earliest entry
*      READ TABLE hr_periods_old ASSIGNING FIELD-SYMBOL(<hr_periods_old>) INDEX 1.
*
*      IF sy-subrc EQ 0.
*        <p0001>-begda = <hr_periods_old>-begda.
*      ENDIF.
*
*      "get latest entry
*      SORT hr_periods_old BY begda DESCENDING.
*      READ TABLE hr_periods_old ASSIGNING <hr_periods_old> INDEX 1.
*
*      IF sy-subrc EQ 0.
*        <p0001>-endda = <hr_periods_old>-endda.
*      ENDIF.
*
*      APPEND <p0001> TO p0001_tmp.
*
*      "pass actual and future entries
*      LOOP AT hr_periods_old ASSIGNING <hr_periods_old>.
*        DELETE TABLE hr_periods_all FROM <hr_periods_old>.
*      ENDLOOP.
*    ENDIF.
*
*    LOOP AT hr_periods_all ASSIGNING FIELD-SYMBOL(<hr_periods_all>).
*      <p0001>-begda = <hr_periods_all>-begda.
*      <p0001>-endda = <hr_periods_all>-endda.
*
*      APPEND <p0001> TO p0001_tmp.
*    ENDLOOP.
*
*    CLEAR: manager_info, hr_periods, hr_periods_search, hr_periods_all, hr_periods_old, index, count, begin_dates.
*  ENDLOOP.
*
*  SORT manager_pernr BY low.
*  DELETE ADJACENT DUPLICATES FROM manager_pernr COMPARING low.
*
*  p0001 = p0001_tmp.
ENDMETHOD.


METHOD convert_date.
  CHECK datum IS NOT INITIAL AND
        datum NE '00000000'.

  DATA(datum_tmp) = datum.

  "for oracle highdate is 31.12.4712
  IF datum_tmp GT oracle_hd.
    datum_tmp = oracle_hd.
  ENDIF.

  CONCATENATE datum_tmp+0(4) datum+4(2) datum+6(2) INTO form_datum SEPARATED BY '/'.
ENDMETHOD.


METHOD get_hr_periods.

  LOOP AT table ASSIGNING FIELD-SYMBOL(<entry>).
    ASSIGN COMPONENT 'BEGDA' OF STRUCTURE <entry> TO FIELD-SYMBOL(<begda>).
    ASSIGN COMPONENT 'ENDDA' OF STRUCTURE <entry> TO FIELD-SYMBOL(<endda>).

    CHECK <begda> IS ASSIGNED AND
          <endda> IS ASSIGNED.

    APPEND VALUE #( begda = <begda>
                    endda = <endda> ) TO hr_periods.
  ENDLOOP.

  CALL FUNCTION 'RHXPROVIDE_PERIODS'
    TABLES
      provide_tab = hr_periods.
ENDMETHOD.


METHOD get_legislation_codes.
  SELECT bukrs, land1 FROM t001 INTO TABLE @DATA(land1_tmp) WHERE bukrs IN @bukrs.

  land1_map = VALUE #( FOR <land1_tmp> IN land1_tmp ( name = <land1_tmp>-bukrs value = <land1_tmp>-land1 ) ).
ENDMETHOD.


METHOD get_mapping_fields.

  IF molga IS NOT INITIAL.
    "get mapping values
    SELECT * FROM /sew/int_mapp_fi INTO TABLE @mapping_fields WHERE molga        IN @molga        AND
                                                                    infty        =  @infty        AND
                                                                    field_sap    =  @sap_field    AND
                                                                    field_oracle =  @oracle_field AND
                                                                    export       =  @export.
  ENDIF.

  IF sy-subrc NE 0 OR
     molga    IS INITIAL.
    SELECT * FROM /sew/int_mapp_fi INTO TABLE @mapping_fields WHERE molga        EQ '*'           AND
                                                                    infty        =  @infty        AND
                                                                    field_sap    =  @sap_field    AND
                                                                    field_oracle =  @oracle_field AND
                                                                    export       =  @export.
  ENDIF.

ENDMETHOD.


  METHOD get_mapping_values.

    "get mapping values
    SELECT * FROM /sew/int_mapping INTO TABLE @mapping_values WHERE molga        IN @molga        AND
                                                                    infty        =  @infty        AND
                                                                    field_sap    =  @sap_field    AND
                                                                    field_oracle =  @oracle_field AND
                                                                    export       =  @export.

    IF sy-subrc NE 0.
      SELECT * FROM /sew/int_mapping INTO TABLE @mapping_values WHERE molga        EQ '*'           AND
                                                                      infty        =  @infty        AND
                                                                      field_sap    =  @sap_field    AND
                                                                      field_oracle =  @oracle_field AND
                                                                      export       =  @export.
    ENDIF.

  ENDMETHOD.


  METHOD get_mngrs_to_orgunit.

    "get leader position in orgunit
    SELECT objid, begda, endda, sobid INTO CORRESPONDING FIELDS OF TABLE @leader_positions FROM hrp1001 WHERE objid IN @orgeh AND
                                                                                                              otype EQ 'O'    AND
                                                                                                              rsign EQ 'B'    AND
                                                                                                              relat EQ '012'  AND
                                                                                                              sclas EQ 'S'    AND
                                                                                                              begda LE @endda AND
                                                                                                              endda GE @begda.

    "get all leader position
    DATA(position) = VALUE rsdsselopt_t( FOR <position> IN leader_positions ( sign = 'I' option = 'EQ' low = <position>-sobid ) ).

    "get pernr of leader position
    IF position IS NOT INITIAL.
      SELECT objid, begda, endda, sobid INTO CORRESPONDING FIELDS OF TABLE @managers FROM hrp1001 WHERE objid IN @position AND
                                                                                                        otype EQ 'S'       AND
                                                                                                        rsign EQ 'A'       AND
                                                                                                        relat EQ '008'     AND
                                                                                                        sclas EQ 'P'       AND
                                                                                                        begda LE @endda    AND
                                                                                                        endda GE @begda.
    ENDIF.

  ENDMETHOD.


  METHOD get_src_id.

    LOOP AT vp_src_id ASSIGNING FIELD-SYMBOL(<src_id>) WHERE name EQ pernr.

      "get date
      DATA(length) = strlen( <src_id>-value ).
      DATA(start)  = length - 8.
      DATA(datum)  = CONV datum( <src_id>-value+start(8) ).

      "default
      src_id = <src_id>-value.
      CHECK datum BETWEEN begda AND endda.
      src_id = <src_id>-value.
      EXIT.

    ENDLOOP.
  ENDMETHOD.


  METHOD is_manager.

    DATA: hrp1001   TYPE hrp1001_t,
          plvar_tmp TYPE plvar.

    plvar_tmp = plvar.
    IF plvar_tmp IS INITIAL.
      "get active plvar
      CALL FUNCTION 'RH_GET_PLVAR'
        IMPORTING
          plvar = plvar_tmp.
    ENDIF.

    is_manager_oracle = /sew/cl_mig_utils=>no.

    CHECK p0001-plans IS NOT INITIAL.

    "check manager
    CALL FUNCTION 'RH_READ_INFTY_1001'
      EXPORTING
        plvar         = plvar_tmp
        otype         = 'S'
        objid         = p0001-plans
        subty         = 'A012'
        begda         = p0001-begda
        endda         = p0001-endda
      TABLES
        i1001         = hrp1001
      EXCEPTIONS
        nothing_found = 1.

    IF hrp1001 IS NOT INITIAL.
      is_manager_oracle = /sew/cl_mig_utils=>yes.
    ENDIF.

  ENDMETHOD.


METHOD summarize_it0000_cofu.
  DATA: pernr_old TYPE  rsdsselopt_t.

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
                                          WHEN /sew/cl_int_constants=>cofu_mandant-australia   THEN VALUE #( ( sign = 'I' option = 'EQ' low = '10' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '20' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '25' ) )
                                          WHEN /sew/cl_int_constants=>cofu_mandant-newzealand  THEN VALUE #( ( sign = 'I' option = 'EQ' low = '25' ) )
                                          "default Italy/Austria
                                          ELSE VALUE #( ( sign = 'I' option = 'EQ' low = '03' )
                                                        ( sign = 'I' option = 'EQ' low = 'ZZ' ) ) ).

  DATA(massn_hire) = SWITCH rsdsselopt_t( sy-mandt
                                          WHEN /sew/cl_int_constants=>cofu_mandant-germany     THEN VALUE #( ( sign = 'I' option = 'EQ' low = '01' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '06' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '08' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '15' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '16' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '17' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '20' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '60' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '62' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '70' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '91' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '92' ) )
                                          WHEN /sew/cl_int_constants=>cofu_mandant-france      THEN VALUE #( ( sign = 'I' option = 'EQ' low = '01' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '97' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '98' ) )
                                          WHEN /sew/cl_int_constants=>cofu_mandant-netherlands THEN VALUE #( ( sign = 'I' option = 'EQ' low = 'Z1' )
                                                                                                             ( sign = 'I' option = 'EQ' low = 'Z5' )
                                                                                                             ( sign = 'I' option = 'EQ' low = 'Z8' )
                                                                                                             ( sign = 'I' option = 'EQ' low = 'Z9' )
                                                                                                             ( sign = 'I' option = 'EQ' low = 'ZA' ) )
                                          WHEN /sew/cl_int_constants=>cofu_mandant-australia   THEN VALUE #( ( sign = 'I' option = 'EQ' low = '01' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '12' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '13' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '90' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '91' ) )
                                          WHEN /sew/cl_int_constants=>cofu_mandant-newzealand  THEN VALUE #( ( sign = 'I' option = 'EQ' low = '01' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '90' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '91' ) )
                                          "default Italy/Austria
                                          ELSE VALUE #( ( sign = 'I' option = 'EQ' low = '01' )
                                                        ( sign = 'I' option = 'EQ' low = '10' ) ) ).

  DATA(massn_mig) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = 'ZO' ) ).  "JMB20211201 I

  "keep only terminations/hire in past
  DATA(p0000_term) = p0000.
  DATA(p0000_hire) = p0000.
  DATA(p0000_mig)  = p0000.

  DELETE p0000_term WHERE massn NOT IN massn_term.
  DELETE p0000_hire WHERE massn NOT IN massn_hire.
  DELETE p0000_mig  WHERE massn NOT IN massn_mig.

  DELETE p0000_term WHERE begda GE sy-datum.
  DELETE p0000_hire WHERE begda GE sy-datum.

  CHECK p0000_term IS NOT INITIAL.

  "Get latest termination action and earliest hire
  SORT p0000_term BY pernr endda DESCENDING.
  SORT p0000_hire BY pernr begda ASCENDING.

  LOOP AT p0000_term ASSIGNING FIELD-SYMBOL(<p0000>).
    CHECK <p0000>-pernr NOT IN pernr_old OR
          pernr_old     IS INITIAL.

    "delete all actions older than latest termination
    DELETE p0000 WHERE pernr EQ <p0000>-pernr AND
                       endda LT <p0000>-begda.

**JMB20210928 start insert - set enddate of first hire entry to begindate of last termination - 1
*
    "append earliest hire
    LOOP AT p0000_hire ASSIGNING FIELD-SYMBOL(<p0000_hire>) WHERE pernr EQ <p0000>-pernr.
      <p0000_hire>-endda = <p0000>-begda - 1.
      APPEND <p0000_hire> TO p0000.
      EXIT.
    ENDLOOP.
*JMB20210928 insert end

    APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0000>-pernr ) TO pernr_old.
  ENDLOOP.
  SORT p0000 BY pernr begda.
ENDMETHOD.


  METHOD summarize_it0000_cogl.
    DATA: pernr_old TYPE  rsdsselopt_t.

    DATA(massn_term) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '03' ) ).
    DATA(massn_hire) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '01' ) ).

    "keep only terminations/hire and in past
    DATA(p0000_term) = p0000.
    DATA(p0000_hire) = p0000.
    DELETE p0000_term WHERE massn NOT IN massn_term.
    DELETE p0000_hire WHERE massn NOT IN massn_hire.

    DELETE p0000_term WHERE begda GE sy-datum.
    DELETE p0000_hire WHERE begda GE sy-datum.

    CHECK p0000_term IS NOT INITIAL.

    "Get latest termination action and earliest hire
    SORT p0000_term BY pernr endda DESCENDING.

    LOOP AT p0000_term ASSIGNING FIELD-SYMBOL(<p0000>).
      CHECK <p0000>-pernr NOT IN pernr_old OR
            pernr_old     IS INITIAL.

      "delete all actions older than latest termination
      DELETE p0000 WHERE pernr EQ <p0000>-pernr AND
                         endda LT <p0000>-begda.

      APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0000>-pernr ) TO pernr_old.
    ENDLOOP.

    CLEAR pernr_old.

    SORT p0000_hire BY pernr begda ASCENDING.

    "append earliest hire
    LOOP AT p0000_hire ASSIGNING <p0000>.
      CHECK <p0000>-pernr NOT IN pernr_old OR
            pernr_old     IS INITIAL.

      APPEND <p0000> TO p0000.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0000>-pernr ) TO pernr_old.
    ENDLOOP.

    SORT p0000 BY pernr begda.
  ENDMETHOD.


  METHOD summarize_it0002.
    DATA: p0002_tmp TYPE p0002_tab,
          p0002_sum TYPE p0002.
    LOOP AT p0002 ASSIGNING FIELD-SYMBOL(<p0002>).

      IF p0002_sum-pernr NE <p0002>-pernr.
        IF p0002_sum-pernr IS NOT INITIAL.
          APPEND p0002_sum TO p0002_tmp.
        ENDIF.
        CLEAR: p0002_sum.
        p0002_sum = <p0002>.
        CONTINUE.
      ENDIF.

      "check for lowest date and highest date
      IF p0002_sum-begda GT <p0002>-begda.
        p0002_sum-begda = <p0002>-begda.
      ENDIF.

      CHECK p0002_sum-endda LT <p0002>-endda.
      DATA(begda) = p0002_sum-begda.
      p0002_sum = <p0002>.
      p0002_sum-begda = begda.

    ENDLOOP.
    APPEND p0002_sum TO p0002_tmp.
    p0002 = p0002_tmp.
  ENDMETHOD.


METHOD summarize_past.

  DATA: period_old TYPE hrperiods.

  "get actual entry
  LOOP AT hr_periods ASSIGNING FIELD-SYMBOL(<period>) WHERE begda LE sy-datum AND
                                                            endda GE sy-datum.
    EXIT.
  ENDLOOP.

  "in case migration date is available, pass all periods from date to future
  IF mig_date IS NOT INITIAL.
    LOOP AT hr_periods ASSIGNING <period> WHERE begda LE mig_date AND
                                                endda GE mig_date.
      EXIT.
    ENDLOOP.
  ENDIF.

  CHECK <period> IS ASSIGNED.

  "get past entries
  LOOP AT hr_periods ASSIGNING FIELD-SYMBOL(<period_old>) WHERE endda LT <period>-begda.

    IF ( period_old-begda IS INITIAL AND
         <period>-begda   NE <period_old>-begda ) OR
       ( period_old-begda IS NOT INITIAL AND
         <period>-begda   NE <period_old>-begda AND
         period_old-begda GT <period_old>-begda ).
      period_old-begda = <period_old>-begda.
    ENDIF.

    IF ( period_old-endda IS INITIAL AND
         <period>-endda   NE <period_old>-endda ) OR
       ( period_old-endda IS NOT INITIAL AND
         <period>-endda   NE <period_old>-endda AND
         period_old-endda LT <period_old>-endda ).
      period_old-endda = <period_old>-endda.
    ENDIF.
  ENDLOOP.

  DELETE hr_periods WHERE endda LT <period>-begda.

  CHECK period_old IS NOT INITIAL.

  APPEND period_old TO hr_periods.

  SORT hr_periods BY begda ASCENDING.

ENDMETHOD.


  METHOD update_begin_date.
    DATA: pernr_old TYPE rsdsselopt_t,
          p0001_tmp TYPE p0001,
          p0002_tmp TYPE p0002.

    "delete all assignments older than first action
    CHECK p0000 IS NOT INITIAL.
    SORT p0001 BY pernr begda ASCENDING.
    SORT p0002 BY pernr begda ASCENDING.

    LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000>).
      CHECK <p0000>-pernr NOT IN pernr_old OR
            pernr_old     IS INITIAL.

      "delete all records that are older than earliest hire entry
      DELETE p0001 WHERE pernr EQ <p0000>-pernr AND
                         endda LT <p0000>-begda.

      DELETE p0002 WHERE pernr EQ <p0000>-pernr AND
                         endda LT <p0000>-begda.

      LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>) WHERE pernr EQ <p0000>-pernr.
        IF create_hire   EQ abap_true AND
           <p0001>-begda GT <p0000>-begda.

          p0001_tmp = <p0001>.
          p0001_tmp-endda = <p0001>-begda - 1.
          p0001_tmp-begda = <p0000>-begda.
          APPEND p0001_tmp TO p0001.
          CLEAR p0001_tmp.

*          <p0001>-begda = <p0000>-begda. "JMB20211030 I  "JMB20220502 D
**JMB20211030 start deletion - pass new date as begin date, due to each PERNR will be proceeded only once
*
*          p0001_tmp = <p0001>.
*          p0001_tmp-endda = <p0000>-endda.
*          p0001_tmp-begda = <p0000>-begda.
*          APPEND p0001_tmp TO p0001.
*          CLEAR p0001_tmp.
*JMB20211030 deletion end

        ELSE.
          IF <p0001>-begda GT <p0000>-begda.
            p0001_tmp = <p0001>.
            p0001_tmp-endda = <p0001>-begda - 1.
            p0001_tmp-begda = <p0000>-begda.
            APPEND p0001_tmp TO p0001.
            CLEAR p0001_tmp.
          ELSE.
            <p0001>-begda = <p0000>-begda.
          ENDIF.
        ENDIF.

        EXIT.
      ENDLOOP.

      LOOP AT p0002 ASSIGNING FIELD-SYMBOL(<p0002>) WHERE pernr EQ <p0000>-pernr.
        <p0002>-begda = <p0000>-begda.
        EXIT.
      ENDLOOP.

      APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0000>-pernr ) TO pernr_old.
    ENDLOOP.
    SORT p0001 BY pernr begda.
  ENDMETHOD.
ENDCLASS.
