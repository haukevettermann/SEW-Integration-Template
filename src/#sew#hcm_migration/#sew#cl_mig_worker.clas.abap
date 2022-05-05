class /SEW/CL_MIG_WORKER definition
  public
  create public .

public section.

  data P0000 type P0000_TAB .
  data PERNR type RSDSSELOPT_T .
  data BEGDA type BEGDA .
  data ENDDA type ENDDA .
  data COFU type BOOLEAN .
  data COGL type BOOLEAN .
  data MOLGA type RSDSSELOPT_T .
  data P0001 type P0001_TAB .
  data VP_WORKER_STRUCTURE type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  data VP_LCL_PERNR type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  constants WORKER type STRING value 'Worker' ##NO_TEXT.
  constants PER type STRING value 'PER_' ##NO_TEXT.
  constants CORRESPONDENCELANGUAGE type /SEW/DD_FIELD value 'CORRESPONDENCELANGUAGE' ##NO_TEXT.
  constants SPRSL type /SEW/DD_FIELD value 'SPRSL' ##NO_TEXT.
  data P0041 type P0041_TAB .
  data P9400 type /SEW/P9400_TAB .
  data P0016 type P0016_TAB .

  methods PROCEED_COFU_WORKER
    exporting
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
    returning
      value(DATA) type STRING .
  methods PROCEED_COGL_WORKER
    exporting
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
    returning
      value(DATA) type STRING .
  methods PROCEED_COGU_WORKER
    exporting
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
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
  PROTECTED SECTION.
private section.

  data P0002 type P0002_TAB .
  data MAPPING_FIELDS_SPRSL type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPP_FI .
  data MAPPING_FIELDS_MASSN type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPP_FI .
  data MAPPING_VALUES_SPRSL type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPPING .
  data MAPPING_VALUES_MASSN type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPPING .
  data COGU type BOOLEAN .
  data T005S_TMP type RCF_T_T005U .

  methods CREATE_LCL_PERNR
    importing
      !SYS_ID type STRING
      !SRC_SYS_ID type STRING
      !PERNR type PERNR_D
      !BEGDA type STRING
      !ENDDA type STRING
      !BUKRS type BUKRS .
  methods GET_COFU_DATA .
  methods GET_COGL_DATA .
  methods MAP_MIG_VALUES
    importing
      !P0002 type P0002
      !P0000 type P0000
    exporting
      !MASSN type /SEW/DD_VALUE
      !SPRSL type /SEW/DD_VALUE .
  methods GET_MAPPING_FIELDS .
  methods GET_MAPPING_VALUES .
  methods MAP_COFU_DATA
    exporting
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
    returning
      value(DATA) type STRING .
  methods MAP_COGL_DATA
    exporting
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
    returning
      value(DATA) type STRING .
  methods MAP_COGU_DATA
    exporting
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
    returning
      value(DATA) type STRING .
ENDCLASS.



CLASS /SEW/CL_MIG_WORKER IMPLEMENTATION.


METHOD constructor.

  me->pernr = pernr.
  me->begda = begda.
  me->endda = endda.
  me->cofu = cofu.
  me->cogu = cogu.
  me->cogl = cogl.
  me->molga = molga.

  IF cogl EQ abap_true OR
     cogu EQ abap_true.
    vp_worker_structure = VALUE #( ( name = 1  value = /sew/cl_mig_utils=>merge )
                                   ( name = 2  value = worker )
                                   ( name = 3  value = 'SourceSystemId' )
                                   ( name = 4  value = 'SourceSystemOwner' )
                                   ( name = 5  value = 'PersonNumber' )
                                   ( name = 6  value = 'EffectiveStartDate' )
                                   ( name = 7  value = 'EffectiveEndDate' )
                                   ( name = 8  value = 'CorrespondenceLanguage' )
                                   ( name = 9  value = 'StartDate' )
                                   ( name = 10 value = 'DateOfBirth' )
                                   ( name = 11 value = 'ActionCode' ) ).
  ELSEIF cofu EQ abap_true.
    vp_worker_structure = VALUE #( ( name = 1  value = /sew/cl_mig_utils=>merge )
                                   ( name = 2  value = worker )
                                   ( name = 3  value = 'SourceSystemId' )
                                   ( name = 4  value = 'SourceSystemOwner' )
                                   ( name = 5  value = 'PersonNumber' )
                                   ( name = 6  value = 'EffectiveStartDate' )
                                   ( name = 7  value = 'EffectiveEndDate' )
                                   ( name = 8  value = 'CorrespondenceLanguage' )
                                   ( name = 9  value = 'StartDate' )
                                   ( name = 10 value = 'DateOfBirth' )
                                   ( name = 11 value = 'CountryOfBirth' )
                                   ( name = 12 value = 'ActionCode' )
                                   ( name = 13 value = 'TownOfBirth' )
                                   ( name = 13 value = 'RegionOfBirth' ) ).
  ENDIF.
ENDMETHOD.


METHOD create_lcl_pernr.
  DATA: src_id TYPE string.

  CONSTANTS: timestamp TYPE string VALUE '00:00:00'.

  CONCATENATE begda timestamp INTO DATA(begda_tmp) SEPARATED BY space.
  CONCATENATE endda timestamp INTO DATA(endda_tmp) SEPARATED BY space.

  "JMB20211012 I - different externalIdentifiers per client - JMB20211210 I - Only default Id for all countries
  DATA(ext_id_type) = 'SEW_LOCAL_PERSONAL_NO'.
*JMB20211210 start delete - only one Id per country
*
*                     SWITCH string( sy-mandt
*                                     WHEN /sew/cl_int_constants=>cofu_mandant-germany     THEN 'SEW_LOCAL_PERSON_NO_DE'
*                                     WHEN /sew/cl_int_constants=>cofu_mandant-france      THEN 'SEW_LOCAL_PERSON_NO_FR'
*                                     WHEN /sew/cl_int_constants=>cofu_mandant-netherlands THEN 'SEW_LOCAL_PERSON_NO_NL'
*                                     WHEN /sew/cl_int_constants=>cofu_mandant-newzealand  THEN 'SEW_LOCAL_PERSON_NO_NZ'
*                                     WHEN /sew/cl_int_constants=>cofu_mandant-australia   THEN 'SEW_LOCAL_PERSON_NO_AU'
*                                     ELSE 'SEW_LOCAL_PERSONAL_NO' ).
*JMB20211210 delete end

  DATA(ext_id_0105) = CONV string( 1 ).
  CONDENSE ext_id_0105.

**JMB20210805 start insert - in case of CoGu old SourceId syntax can be used
*
  CLEAR: src_id.
  IF cogu EQ abap_true OR
     cofu EQ abap_true.
    CONCATENATE /sew/cl_mig_external_ident=>ext pernr '_' 'D' INTO src_id. "JMB20210712 D - pass only OracleId
  ENDIF.
*JMB20210805 insert end

**JMB20210712 start insert - Import in a second step with Oracle Id
*
  IF cogl EQ abap_true.
    LOOP AT p9400 ASSIGNING FIELD-SYMBOL(<p9400>) WHERE pernr EQ pernr AND
                                                        begda LE endda AND
                                                        endda GE begda.
      CONCATENATE /sew/cl_mig_external_ident=>ext <p9400>-oracleid '_' 'D' INTO src_id.
      EXIT.
    ENDLOOP.
  ENDIF.

  CHECK src_id IS NOT INITIAL.
*JMB20210712 insert end

  DATA(comments) = CONV string( bukrs ).

  "ID needs to be unique
  CONCATENATE src_id '_' ext_id_0105 INTO src_id.

  CONCATENATE /sew/cl_mig_utils=>merge
              /sew/cl_mig_external_ident=>external_ident_data
              src_id
              sys_id
              src_sys_id
              ext_id_0105
              pernr
              ext_id_type
              begda_tmp
              endda_tmp
              comments
  INTO DATA(data_tmp) SEPARATED BY /sew/cl_mig_utils=>separator.

  APPEND VALUE #( name  = pernr
                  value = data_tmp ) TO vp_lcl_pernr.
ENDMETHOD.


METHOD create_metadata.

  DESCRIBE TABLE vp_worker_structure LINES DATA(length).

  LOOP AT vp_worker_structure ASSIGNING FIELD-SYMBOL(<worker_struc>).

    "set METADATA title
    CASE <worker_struc>-name.
      WHEN 1.
        CONCATENATE /sew/cl_mig_utils=>metadata /sew/cl_mig_utils=>separator INTO metadata.
        CONTINUE.
    ENDCASE.

    CONCATENATE metadata <worker_struc>-value INTO metadata.

    "set separator
    CHECK length NE sy-tabix.
    CONCATENATE metadata /sew/cl_mig_utils=>separator INTO metadata.
  ENDLOOP.

ENDMETHOD.


METHOD get_cofu_data.

  "Get IT0000
  SELECT pernr,
         begda,
         endda,
         massn,
         massg,
         stat1,
         stat2 INTO CORRESPONDING FIELDS OF TABLE @p0000 FROM pa0000 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.
  "Get all IT0001
  SELECT pernr,
         begda,
         endda,
         bukrs INTO CORRESPONDING FIELDS OF TABLE @p0001 FROM pa0001 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.
  "Get IT0002
  SELECT pernr,
         begda,
         endda,
         sprsl,
         gblnd,
         gbdat,
         gbort,
         gbdep INTO CORRESPONDING FIELDS OF TABLE @p0002 FROM pa0002 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.

  DATA(land1) = VALUE rsdsselopt_t( FOR <p0002> IN p0002 ( sign = 'I' option = 'EQ' low = <p0002>-gblnd ) ).

  SELECT land1, bland, bezei INTO CORRESPONDING FIELDS OF TABLE @t005s_tmp FROM t005u WHERE land1 IN @land1 AND
                                                                                            spras EQ @sy-langu.

  "Get IT9400 - JMB20210712 I
  SELECT pernr,
         begda,
         endda,
         oracleid INTO CORRESPONDING FIELDS OF TABLE @p9400 FROM pa9400 WHERE pernr IN @pernr AND
                                                                              begda LE @endda AND
                                                                              endda GE @begda.

  "Get IT0041
  SELECT * INTO CORRESPONDING FIELDS OF TABLE @p0041 FROM pa0041 WHERE pernr IN @pernr AND
                                                                       begda LE @endda AND
                                                                       endda GE @begda.

  "in case of Germany and netherlands entry date should be read from IT0016
  CHECK sy-mandt EQ /sew/cl_int_constants=>cofu_mandant-germany OR
        sy-mandt EQ /sew/cl_int_constants=>cofu_mandant-netherlands.

  CLEAR: p0041.

  "Get IT0016
  SELECT pernr, begda, endda, kondt INTO CORRESPONDING FIELDS OF TABLE @p0016 FROM pa0016 WHERE pernr IN @pernr    AND
                                                                                                begda LE @sy-datum AND "JMB20220207 I
                                                                                                endda GE @sy-datum.    "JMB20220207 I
  "begda LE @endda AND  "JMB20220207 D
  "endda GE @begda.     "JMB20220207 D

ENDMETHOD.


METHOD get_cogl_data.

  "Get IT0000
  SELECT pernr,
         begda,
         endda,
         massn,
         massg,
         stat2 INTO CORRESPONDING FIELDS OF TABLE @p0000 FROM pa0000 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.
  "Get all IT0001
  SELECT pernr,
         begda,
         endda,
         bukrs INTO CORRESPONDING FIELDS OF TABLE @p0001 FROM pa0001 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.
  "Get IT0002
  SELECT pernr,
         begda,
         endda,
         sprsl,
         gbdat INTO CORRESPONDING FIELDS OF TABLE @p0002 FROM pa0002 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.

  "Get IT9400 - JMB20210712 I
  SELECT pernr,
         begda,
         endda,
         oracleid INTO CORRESPONDING FIELDS OF TABLE @p9400 FROM pa9400 WHERE pernr IN @pernr AND
                                                                              begda LE @endda AND
                                                                              endda GE @begda.

  "Get IT0041
  SELECT * INTO CORRESPONDING FIELDS OF TABLE @p0041 FROM pa0041 WHERE pernr IN @pernr AND
                                                                       begda LE @endda AND
                                                                       endda GE @begda.

ENDMETHOD.


METHOD get_mapping_fields.

  "get mapping fields for language
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0002
                                                   sap_field    = sprsl
                                                   oracle_field = correspondencelanguage
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_sprsl ).

  "get mapping fields for actioncode
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0000
                                                   sap_field    = /sew/cl_mig_utils=>massn
                                                   oracle_field = /sew/cl_mig_utils=>actioncode
                                                   export       = abap_true
                                        IMPORTING mapping_fields = mapping_fields_massn ).
ENDMETHOD.


METHOD get_mapping_values.

  "get mapping values for language
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0002
                                                   sap_field    = sprsl
                                                   oracle_field = correspondencelanguage
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_sprsl ).

  "get mapping values for actioncode
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0000
                                                   sap_field    = /sew/cl_mig_utils=>massn
                                                   oracle_field = /sew/cl_mig_utils=>actioncode
                                                   export       = abap_true
                                        IMPORTING mapping_values = mapping_values_massn ).
ENDMETHOD.


METHOD map_cofu_data.

  DATA: language        TYPE string,
        src_id          TYPE string,
        sys_id          TYPE string,
        pernr_tmp       TYPE pernr_d,
        senior_date     TYPE dardt,
        message_handler TYPE REF TO if_hrpa_message_handler.

  CREATE OBJECT message_handler TYPE cl_hrpa_message_list.

  CONCATENATE /sew/cl_mig_utils=>sap sy-mandt INTO sys_id.

  LOOP AT p0002 ASSIGNING FIELD-SYMBOL(<p0002>).

    LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000>) WHERE begda LE <p0002>-endda AND
                                                        endda GE <p0002>-begda AND
                                                        pernr EQ <p0002>-pernr.
      EXIT.
    ENDLOOP.

    CHECK sy-subrc EQ 0.

    CLEAR senior_date.

    "get seniority date
    LOOP AT p0041 ASSIGNING FIELD-SYMBOL(<p0041>) WHERE pernr EQ <p0000>-pernr AND
                                                        begda LE <p0002>-endda AND
                                                        endda GE <p0002>-begda.

      DATA(senior_datar) = SWITCH datar( sy-mandt
                                         WHEN /sew/cl_int_constants=>cofu_mandant-austria THEN '99'
                                         WHEN /sew/cl_int_constants=>cofu_mandant-italy   THEN '99'
                                         WHEN /sew/cl_int_constants=>cofu_mandant-france  THEN '30' ).

      CALL FUNCTION 'HR_ECM_READ_IT0041_DATE_TYPE'
        EXPORTING
          datar           = senior_datar
          p0041           = <p0041>
          message_handler = message_handler
        IMPORTING
          date            = senior_date.
      EXIT.
    ENDLOOP.

    "get seniority date
    LOOP AT p0016 ASSIGNING FIELD-SYMBOL(<p0016>) WHERE pernr EQ <p0000>-pernr AND
                                                        begda LE <p0002>-endda AND
                                                        endda GE <p0002>-begda AND
                                                        ( kondt IS NOT INITIAL OR
                                                          kondt NE '' ).
      IF senior_date     IS INITIAL OR
         ( <p0016>-kondt LT senior_date ).
        senior_date = <p0016>-kondt.
      ENDIF.
    ENDLOOP.

    IF senior_date IS NOT INITIAL.

      <p0002>-begda = COND #( WHEN senior_date LT <p0002>-begda                              THEN senior_date
                              WHEN sy-mandt    EQ /sew/cl_int_constants=>cofu_mandant-france THEN senior_date "JMB20211030 I - C400129651-5814
                              ELSE <p0002>-begda ).

      "relevant for assignment and work terms
      IF senior_date LT <p0000>-begda OR
         sy-mandt    EQ /sew/cl_int_constants=>cofu_mandant-france.            "JMB20211030 I

**JMB20211030 start insert - in case of France pass IT0041/0030 - C400129651-5814
*
        DATA(p0000_tmp) = <p0000>.
        CASE sy-mandt.
          WHEN /sew/cl_int_constants=>cofu_mandant-france.
            p0000_tmp-begda = senior_date.
            DELETE p0000 WHERE endda EQ <p0000>-endda AND
                               pernr EQ <p0002>-pernr.
          WHEN OTHERS.
            p0000_tmp-endda = <p0000>-begda - 1.
            p0000_tmp-begda = senior_date.
        ENDCASE.

        APPEND p0000_tmp TO p0000.
        SORT p0000 BY pernr begda.
        CLEAR p0000_tmp.

        IF <p0000> IS NOT ASSIGNED.
          LOOP AT p0000 ASSIGNING <p0000> WHERE begda LE <p0002>-endda AND
                                                endda GE <p0002>-begda AND
                                                pernr EQ <p0002>-pernr.
            EXIT.
          ENDLOOP.
        ENDIF.
*JMB20211030 insert end

      ENDIF.
    ENDIF.

    map_mig_values( EXPORTING p0000 = <p0000>
                              p0002 = <p0002>
                    IMPORTING massn = DATA(massn)
                              sprsl = DATA(sprsl_tmp) ).

    DATA(begda_tmp) = /sew/cl_mig_utils=>convert_date( <p0002>-begda ).
    DATA(endda_tmp) = /sew/cl_mig_utils=>convert_date( <p0002>-endda ).
    DATA(gbdat_tmp) = /sew/cl_mig_utils=>convert_date( <p0002>-gbdat ).

    CONCATENATE per <p0002>-pernr INTO src_id.

    "add src_id to value pair table
    APPEND VALUE #( name = <p0002>-pernr value = src_id ) TO vp_src_id.

    CONCATENATE /sew/cl_mig_utils=>merge
                worker
                src_id
                sys_id
                '' "<p0002>-pernr JMB20210408 D - personNumber equals Oracle PERNR
                begda_tmp
                endda_tmp
                sprsl_tmp
                begda_tmp
                gbdat_tmp
                <p0002>-gblnd
                massn
                <p0002>-gbort
                <p0002>-gbdep
    INTO DATA(data_tmp) SEPARATED BY /sew/cl_mig_utils=>separator.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.

    "get actual legal entity
    LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>) WHERE begda LE sy-datum AND
                                                        endda GE sy-datum AND
                                                        pernr EQ <p0002>-pernr.
      create_lcl_pernr( pernr      = <p0002>-pernr
                        begda      = begda_tmp
                        endda      = endda_tmp
                        bukrs      = <p0001>-bukrs
                        src_sys_id = src_id
                        sys_id     = sys_id ).
      EXIT.
    ENDLOOP.
  ENDLOOP.
ENDMETHOD.


METHOD map_cogl_data.

  DATA: language        TYPE string,
        src_id          TYPE string,
        sys_id          TYPE string,
        pernr_tmp       TYPE pernr_d,
        senior_date     TYPE dardt,
        message_handler TYPE REF TO if_hrpa_message_handler.

  CONSTANTS: senior_datar TYPE datar VALUE '99',
             last_date    TYPE datum VALUE '19500101'.
  CREATE OBJECT message_handler TYPE cl_hrpa_message_list.

  CONCATENATE /sew/cl_mig_utils=>sap sy-mandt INTO sys_id.

  LOOP AT p0002 ASSIGNING FIELD-SYMBOL(<p0002>).

    LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000>) WHERE begda LE <p0002>-endda AND
                                                        endda GE <p0002>-begda AND
                                                        pernr EQ <p0002>-pernr.
      EXIT.
    ENDLOOP.

    CHECK sy-subrc EQ 0.

    "get seniority date
    LOOP AT p0041 ASSIGNING FIELD-SYMBOL(<p0041>) WHERE pernr EQ <p0000>-pernr AND
                                                        begda LE <p0000>-endda AND
                                                        endda GE <p0000>-begda.
      CALL FUNCTION 'HR_ECM_READ_IT0041_DATE_TYPE'
        EXPORTING
          datar           = senior_datar
          p0041           = <p0041>
          message_handler = message_handler
        IMPORTING
          date            = senior_date.

      CHECK senior_date IS NOT INITIAL AND
            senior_date GE last_date.

      <p0002>-begda = COND #( WHEN senior_date LT <p0002>-begda
                              THEN senior_date
                              ELSE <p0002>-begda ).

      "relevant for assignment and work terms
      IF senior_date LT <p0000>-begda.
        DATA(p0000_tmp) = <p0000>.
        p0000_tmp-endda = <p0000>-begda - 1.
        p0000_tmp-begda = senior_date.
        APPEND p0000_tmp TO p0000.
        CLEAR p0000_tmp.
      ENDIF.
      EXIT.
    ENDLOOP.

    map_mig_values( EXPORTING p0000 = <p0000>
                              p0002 = <p0002>
                    IMPORTING massn = DATA(massn)
                              sprsl = DATA(sprsl_tmp) ).

    DATA(begda_tmp) = /sew/cl_mig_utils=>convert_date( <p0002>-begda ).
    DATA(endda_tmp) = /sew/cl_mig_utils=>convert_date( <p0002>-endda ).
    DATA(gbdat_tmp) = /sew/cl_mig_utils=>convert_date( <p0002>-gbdat ).

**JMB20210618 start insert, don´t migrate date of birth for USA or CA
*
    IF ( '07'        IN molga                                OR  "CA
         '10'        IN molga )                              AND "USA
       molga         IS NOT INITIAL.
      CLEAR: gbdat_tmp.
    ENDIF.
*JMB20210618 insert end

    CONCATENATE per <p0002>-pernr INTO src_id.

    "add src_id to value pair table
    APPEND VALUE #( name = <p0002>-pernr value = src_id ) TO vp_src_id.

    CONCATENATE /sew/cl_mig_utils=>merge
                worker
                src_id
                sys_id
                '' "<p0002>-pernr JMB20210408 D - personNumber equals Oracle PERNR
                begda_tmp
                endda_tmp
                sprsl_tmp
                begda_tmp
                gbdat_tmp
                massn
    INTO DATA(data_tmp) SEPARATED BY /sew/cl_mig_utils=>separator.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.

    "get actual legal entity
    LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>) WHERE begda LE sy-datum AND
                                                        endda GE sy-datum AND
                                                        pernr EQ <p0002>-pernr.
      create_lcl_pernr( pernr      = <p0002>-pernr
                        begda      = begda_tmp
                        endda      = endda_tmp
                        bukrs      = <p0001>-bukrs
                        src_sys_id = src_id
                        sys_id     = sys_id ).
      EXIT.
    ENDLOOP.
  ENDLOOP.
ENDMETHOD.


METHOD MAP_COGU_DATA.

  DATA: language        TYPE string,
        src_id          TYPE string,
        sys_id          TYPE string,
        pernr_tmp       TYPE pernr_d,
        senior_date     TYPE dardt,
        message_handler TYPE REF TO if_hrpa_message_handler.

  CREATE OBJECT message_handler TYPE cl_hrpa_message_list.

  CONCATENATE /sew/cl_mig_utils=>sap sy-mandt INTO sys_id.

  LOOP AT p0002 ASSIGNING FIELD-SYMBOL(<p0002>).

    LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000>) WHERE begda LE <p0002>-endda AND
                                                        endda GE <p0002>-begda AND
                                                        pernr EQ <p0002>-pernr.
      EXIT.
    ENDLOOP.

    CHECK sy-subrc EQ 0.

    CLEAR senior_date.

    "get seniority date
    LOOP AT p0041 ASSIGNING FIELD-SYMBOL(<p0041>) WHERE pernr EQ <p0000>-pernr AND
                                                        begda LE <p0002>-endda AND
                                                        endda GE <p0002>-begda.

      DATA(senior_datar) = SWITCH datar( sy-mandt
                                         WHEN /sew/cl_int_constants=>cofu_mandant-austria THEN '99'
                                         WHEN /sew/cl_int_constants=>cofu_mandant-italy   THEN '99'
                                         WHEN /sew/cl_int_constants=>cofu_mandant-france  THEN '30' ).

      CALL FUNCTION 'HR_ECM_READ_IT0041_DATE_TYPE'
        EXPORTING
          datar           = senior_datar
          p0041           = <p0041>
          message_handler = message_handler
        IMPORTING
          date            = senior_date.
      EXIT.
    ENDLOOP.

    "get seniority date
    LOOP AT p0016 ASSIGNING FIELD-SYMBOL(<p0016>) WHERE pernr EQ <p0000>-pernr AND
                                                        begda LE <p0002>-endda AND
                                                        endda GE <p0002>-begda AND
                                                        kondt IS NOT INITIAL.
      IF senior_date   IS INITIAL OR
         <p0016>-kondt LT senior_date.
        senior_date = <p0016>-kondt.
      ENDIF.
    ENDLOOP.

    IF senior_date IS NOT INITIAL.

      <p0002>-begda = COND #( WHEN senior_date LT <p0002>-begda                              THEN senior_date
                              WHEN sy-mandt    EQ /sew/cl_int_constants=>cofu_mandant-france THEN senior_date "JMB20211030 I - C400129651-5814
                              ELSE <p0002>-begda ).

      "relevant for assignment and work terms
      IF senior_date LT <p0000>-begda OR
         sy-mandt    EQ /sew/cl_int_constants=>cofu_mandant-france.            "JMB20211030 I

**JMB20211030 start insert - in case of France pass IT0041/0030 - C400129651-5814
*
        DATA(p0000_tmp) = <p0000>.
        CASE sy-mandt.
          WHEN /sew/cl_int_constants=>cofu_mandant-france.
            p0000_tmp-begda = senior_date.
            DELETE p0000 WHERE endda EQ <p0000>-endda AND
                               pernr EQ <p0002>-pernr.
          WHEN OTHERS.
            p0000_tmp-endda = <p0000>-begda - 1.
            p0000_tmp-begda = senior_date.
        ENDCASE.

        APPEND p0000_tmp TO p0000.
        SORT p0000 BY pernr begda.
        CLEAR p0000_tmp.

        IF <p0000> IS NOT ASSIGNED.
          LOOP AT p0000 ASSIGNING <p0000> WHERE begda LE <p0002>-endda AND
                                                endda GE <p0002>-begda AND
                                                pernr EQ <p0002>-pernr.
            EXIT.
          ENDLOOP.
        ENDIF.
*JMB20211030 insert end

      ENDIF.
    ENDIF.

    map_mig_values( EXPORTING p0000 = <p0000>
                              p0002 = <p0002>
                    IMPORTING massn = DATA(massn)
                              sprsl = DATA(sprsl_tmp) ).

    DATA(begda_tmp) = /sew/cl_mig_utils=>convert_date( <p0002>-begda ).
    DATA(endda_tmp) = /sew/cl_mig_utils=>convert_date( <p0002>-endda ).
    DATA(gbdat_tmp) = /sew/cl_mig_utils=>convert_date( <p0002>-gbdat ).

    CONCATENATE per <p0002>-pernr INTO src_id.

    "add src_id to value pair table
    APPEND VALUE #( name = <p0002>-pernr value = src_id ) TO vp_src_id.

    CONCATENATE /sew/cl_mig_utils=>merge
                worker
                src_id
                sys_id
                '' "<p0002>-pernr JMB20210408 D - personNumber equals Oracle PERNR
                begda_tmp
                endda_tmp
                sprsl_tmp
                begda_tmp
                gbdat_tmp
                massn
    INTO DATA(data_tmp) SEPARATED BY /sew/cl_mig_utils=>separator.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.

    "get actual legal entity
    LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>) WHERE begda LE sy-datum AND
                                                        endda GE sy-datum AND
                                                        pernr EQ <p0002>-pernr.
      create_lcl_pernr( pernr      = <p0002>-pernr
                        begda      = begda_tmp
                        endda      = endda_tmp
                        bukrs      = <p0001>-bukrs
                        src_sys_id = src_id
                        sys_id     = sys_id ).
      EXIT.
    ENDLOOP.
  ENDLOOP.
ENDMETHOD.


METHOD map_mig_values.
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

  "Process language mapping
  value_tmp = CONV #( p0002-sprsl ).
  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0002
      field_sap      = /sew/cl_mig_worker=>sprsl
      field_oracle   = correspondencelanguage
      mapping_fields = CONV #( mapping_fields_sprsl )
      mapping_values = CONV #( mapping_values_sprsl )
    CHANGING
      value          = value_tmp ).

  sprsl = value_tmp.
ENDMETHOD.


METHOD PROCEED_COFU_WORKER.
  get_cofu_data( ).
  get_mapping_fields( ).
  get_mapping_values( ).
  /sew/cl_mig_utils=>summarize_it0000_cofu( CHANGING p0000 = p0000 ).
  /sew/cl_mig_utils=>update_begin_date( EXPORTING p0000 = p0000
                                         CHANGING p0001 = p0001
                                                  p0002 = p0002 ).
  /sew/cl_mig_utils=>summarize_it0002( CHANGING p0002 = p0002 ).
  data = map_cofu_data( IMPORTING vp_src_id = vp_src_id ).
  SORT p0000 BY pernr begda.
ENDMETHOD.


METHOD proceed_cogl_worker.
  get_cogl_data( ).
  get_mapping_fields( ).
  get_mapping_values( ).
  /sew/cl_mig_utils=>summarize_it0000_cogl( CHANGING p0000 = p0000 ).
  /sew/cl_mig_utils=>update_begin_date( EXPORTING p0000 = p0000
                                         CHANGING p0001 = p0001
                                                  p0002 = p0002 ).
  /sew/cl_mig_utils=>summarize_it0002( CHANGING p0002 = p0002 ).
  data = map_cogl_data( IMPORTING vp_src_id = vp_src_id ).
  SORT p0000 BY pernr begda.
ENDMETHOD.


METHOD PROCEED_COGU_WORKER.
  get_cofu_data( ).
  get_mapping_fields( ).
  get_mapping_values( ).
  /sew/cl_mig_utils=>summarize_it0000_cofu( CHANGING p0000 = p0000 ).
  /sew/cl_mig_utils=>update_begin_date( EXPORTING p0000 = p0000
                                         CHANGING p0001 = p0001
                                                  p0002 = p0002 ).
  /sew/cl_mig_utils=>summarize_it0002( CHANGING p0002 = p0002 ).
  data = map_cogu_data( IMPORTING vp_src_id = vp_src_id ).
  SORT p0000 BY pernr begda.
ENDMETHOD.
ENDCLASS.
