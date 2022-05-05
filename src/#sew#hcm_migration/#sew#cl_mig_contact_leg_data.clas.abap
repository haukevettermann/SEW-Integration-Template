class /SEW/CL_MIG_CONTACT_LEG_DATA definition
  public
  create public .

public section.

  data PERNR type RSDSSELOPT_T .
  data BEGDA type BEGDA .
  data ENDDA type ENDDA .
  data COFU type BOOLEAN .
  data COGL type BOOLEAN .
  data COGU type BOOLEAN .
  data MOLGA type RSDSSELOPT_T .
  data VP_CON_LEG_DATA_STRUCTURE type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  constants CONTACT_LEG_DATA type STRING value 'ContactLegislativeData' ##NO_TEXT.
  constants PER type STRING value 'PER' ##NO_TEXT.
  constants CONT type STRING value 'CONT' ##NO_TEXT.
  constants LEG type STRING value 'LEG' ##NO_TEXT.
  constants SEX type /SEW/DD_FIELD value 'SEX' ##NO_TEXT.
  constants GESCH type /SEW/DD_FIELD value 'GESCH' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !PERNR type RSDSSELOPT_T
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !COFU type BOOLEAN
      !COGL type BOOLEAN
      !MOLGA type RSDSSELOPT_T
      !COGU type BOOLEAN .
  methods PROCEED_COFU_CON_LEG_DATA
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !WORKER type ref to /SEW/CL_MIG_WORKER
    returning
      value(DATA) type STRING .
  methods CREATE_METADATA
    returning
      value(METADATA) type STRING .
protected section.
private section.

  data P0021 type P0021_TAB .
  data P0001 type P0001_TAB .
  data LAND1_MAP type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  data MAPPING_FIELDS_GESCH type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPP_FI .
  data MAPPING_VALUES_GESCH type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPPING .

  methods GET_COFU_DATA .
  methods MAP_COFU_DATA
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
    returning
      value(DATA) type STRING .
  methods MAP_MIG_VALUES
    importing
      !P0021 type P0021
    exporting
      !GESCH type /SEW/DD_VALUE .
  methods GET_MAPPING_FIELDS .
  methods GET_MAPPING_VALUES .
ENDCLASS.



CLASS /SEW/CL_MIG_CONTACT_LEG_DATA IMPLEMENTATION.


  METHOD constructor.
    me->pernr = pernr.
    me->begda = begda.
    me->endda = endda.
    me->cofu  = cofu.
    me->cogl  = cogl.
    me->cogu  = cogu.
    me->molga = molga.

    IF cogu EQ abap_true.

    ELSEIF cofu EQ abap_true.
      vp_con_leg_data_structure = VALUE #( ( name = 1  value = /sew/cl_mig_utils=>merge )
                                           ( name = 2  value = contact_leg_data )
                                           ( name = 3  value = 'EffectiveStartDate' )
                                           ( name = 4  value = 'EffectiveEndDate' )
                                           ( name = 5  value = 'PersonId(SourceSystemId)' )
                                           ( name = 6  value = 'LegislationCode' )
                                           ( name = 7  value = 'Sex' )
                                           ( name = 8  value = 'SourceSystemOwner' )
                                           ( name = 9  value = 'SourceSystemId' ) ).
    ELSEIF cogl EQ abap_true.

    ENDIF.


  ENDMETHOD.


  METHOD create_metadata.

    DESCRIBE TABLE vp_con_leg_data_structure LINES DATA(length).

    LOOP AT vp_con_leg_data_structure ASSIGNING FIELD-SYMBOL(<contact_leg_data>).

      "set METADATA title
      CASE <contact_leg_data>-name.
        WHEN 1.
          CONCATENATE /sew/cl_mig_utils=>metadata /sew/cl_mig_utils=>separator INTO metadata.
          CONTINUE.
      ENDCASE.

      CONCATENATE metadata <contact_leg_data>-value INTO metadata.

      "set separator
      CHECK length NE sy-tabix.
      CONCATENATE metadata /sew/cl_mig_utils=>separator INTO metadata.
    ENDLOOP.


  ENDMETHOD.


METHOD get_cofu_data.
**JMB20211312 start insert - select only specific subtypes
*
  DATA: famsa TYPE rsdsselopt_t.
  CASE sy-mandt.
    WHEN /sew/cl_int_constants=>cofu_mandant-netherlands.
      famsa = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '1' )
                                  ( sign = 'I' option = 'EQ' low = '13' )
                                  ( sign = 'I' option = 'EQ' low = '15' ) ).
  ENDCASE.
*JMB20211312 insert end

  "Get IT0021
  SELECT pernr,
         begda,
         endda,
         fasex
  INTO CORRESPONDING FIELDS OF TABLE @p0021 FROM pa0021 WHERE pernr IN @pernr
                                                          AND famsa IN @famsa
                                                          AND begda LE @endda
                                                          AND endda GE @begda.

  "Get BUKRS for LegislationCode
  SELECT pernr,
         begda,
         endda,
         bukrs INTO CORRESPONDING FIELDS OF TABLE @p0001 FROM pa0001 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.



  DATA(bukrs) = VALUE rsdsselopt_t( FOR <p0001> IN p0001 ( sign = 'I' option = 'EQ' low = <p0001>-bukrs ) ).
  SORT bukrs BY low.
  DELETE ADJACENT DUPLICATES FROM bukrs COMPARING low.
  land1_map = /sew/cl_mig_utils=>get_legislation_codes( bukrs ).

ENDMETHOD.


  method GET_MAPPING_FIELDS.

  "get mapping fields for sex
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0002
                                                   sap_field    = gesch
                                                   oracle_field = sex
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_gesch ).

  endmethod.


  METHOD get_mapping_values.
    "get mapping values for sex
    /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = /sew/cl_mig_utils=>it0002
                                                     sap_field    = gesch
                                                     oracle_field = sex
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_gesch ).

  ENDMETHOD.


METHOD map_cofu_data.

  DATA: src_id TYPE string,
        sys_id TYPE string,
        land1  TYPE /iwbep/s_mgw_name_value_pair.

  CHECK p0021 IS NOT INITIAL. "IFT20211207 I, due to dump
  SORT p0021 BY pernr ASCENDING begda ASCENDING.

  DATA(check_pernr) = p0021[ 1 ]-pernr.
  DATA(count) = 0.

  LOOP AT p0021 ASSIGNING FIELD-SYMBOL(<p0021>).

    IF <p0021>-pernr NE check_pernr.
      count = 1.
      check_pernr = <p0021>-pernr.
    ELSE.
      count = count + 1.
    ENDIF.

    "get legislationcode
    CLEAR land1.
    LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>) WHERE pernr EQ <p0021>-pernr AND
                                                        begda LE <p0021>-endda AND
                                                        endda GE <p0021>-begda.
      READ TABLE land1_map INTO land1 WITH KEY name = <p0001>-bukrs.
      EXIT.
    ENDLOOP.

    DATA(eff_start_date) = /sew/cl_mig_utils=>convert_date( <p0021>-begda ).

    DATA(count_str) = CONV string( count ).
    CONDENSE count_str.

    CONCATENATE per
                cont
                <p0021>-pernr
                count_str
    INTO DATA(per_id) SEPARATED BY '_'.

    sys_id = 'SAP_' && sy-mandt.

    CONCATENATE per
                cont
                leg
                <p0021>-pernr
                count_str
    INTO src_id SEPARATED BY '_'.
    map_mig_values( EXPORTING p0021 = <p0021>
                    IMPORTING gesch = DATA(gesch_tmp) ).

    CONCATENATE /sew/cl_mig_utils=>merge
                contact_leg_data
                eff_start_date
                ''
                per_id
                land1-value
                gesch_tmp
                sys_id
                src_id
    INTO DATA(data_tmp) SEPARATED BY /sew/cl_mig_utils=>separator.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
  ENDLOOP.
ENDMETHOD.


  METHOD map_mig_values.
    DATA: value_tmp TYPE /sew/dd_value.

    "Process FASEX/GESCH mapping
    value_tmp = CONV #( p0021-fasex ).
    /sew/cl_int_mapping=>process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = /sew/cl_mig_utils=>it0002
        field_sap      = /sew/cl_mig_person_leg_data=>gesch
        field_oracle   = sex
        mapping_fields = CONV #( mapping_fields_gesch )
        mapping_values = CONV #( mapping_values_gesch )
     CHANGING
       value           = value_tmp ).

    gesch              = value_tmp.


  ENDMETHOD.


  METHOD proceed_cofu_con_leg_data.

    get_cofu_data( ).
    get_mapping_fields( ).
    get_mapping_values( ).
    data =  map_cofu_data( vp_src_id ).

  ENDMETHOD.
ENDCLASS.
