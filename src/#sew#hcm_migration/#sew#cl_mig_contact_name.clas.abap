class /SEW/CL_MIG_CONTACT_NAME definition
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
  data VP_CONTACT_NAME_STRUCTURE type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  constants CONTACT_NAME type STRING value 'ContactName' ##NO_TEXT.
  constants GLOBAL type STRING value 'GLOBAL' ##NO_TEXT.
  constants PER type STRING value 'PER' ##NO_TEXT.
  constants CONT type STRING value 'CONT' ##NO_TEXT.
  constants NAME type STRING value 'NAME' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !PERNR type RSDSSELOPT_T
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !COFU type BOOLEAN
      !COGL type BOOLEAN
      !MOLGA type RSDSSELOPT_T
      !COGU type BOOLEAN .
  methods PROCEED_COFU_CON_NAME
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
  data MAPPING_FIELDS_NAMEINFO7 type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPP_FI .
  data MAPPING_VALUES_NAMEINFO7 type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPPING .
  data MAPPING_FIELDS_NAMEINFO1 type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPP_FI .
  data MAPPING_VALUES_NAMEINFO1 type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPPING .

  methods MAP_MIG_VALUES
    importing
      !P0021 type P0021
    exporting
      !TITLE type /SEW/DD_VALUE
      !NAMEINFO7 type /SEW/DD_VALUE .
  methods GET_MAPPING_FIELDS .
  methods GET_MAPPING_VALUES .
  methods GET_COFU_DATA .
  methods MAP_COFU_DATA
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
    returning
      value(DATA) type STRING .
ENDCLASS.



CLASS /SEW/CL_MIG_CONTACT_NAME IMPLEMENTATION.


  METHOD constructor.
    me->pernr = pernr.
    me->begda = begda.
    me->endda = endda.
    me->cofu  = cofu.
    me->cogu  = cogu.
    me->cogl  = cogl.
    me->molga = molga.

    IF cogl EQ abap_true.

    ELSEIF cofu EQ abap_true.
      vp_contact_name_structure = VALUE #( ( name = 1  value = /sew/cl_mig_utils=>merge )
                                           ( name = 2  value = contact_name )
                                           ( name = 3  value = 'PersonId(SourceSystemId)' )
                                           ( name = 4  value = 'EffectiveStartDate' )
                                           ( name = 5  value = 'EffectiveEndDate' )
                                           ( name = 6  value = 'LegislationCode' )
                                           ( name = 7  value = 'NameType' )
                                           ( name = 8  value = 'FirstName' )
                                           ( name = 9  value = 'MiddleNames' )
                                           ( name = 10 value = 'LastName' )
                                           ( name = 11 value = 'SourceSystemOwner' )
                                           ( name = 12 value = 'SourceSystemId' )
                                           ( name = 16 value = 'NameInformation1' )
                                           ( name = 17 value = 'NameInformation2' )
                                           ( name = 18 value = 'NameInformation3' )
                                           ( name = 19 value = 'NameInformation4' )
                                           ( name = 20 value = 'NameInformation5' )
                                           ( name = 21 value = 'NameInformation6' )
                                           ( name = 22 value = 'NameInformation7' )
                                           ( name = 23 value = 'PreNameAdjunct' )  ).
    ELSEIF cogu EQ abap_true.

    ENDIF.
  ENDMETHOD.


  method CREATE_METADATA.

    DESCRIBE TABLE vp_contact_name_structure LINES DATA(length).

    LOOP AT vp_contact_name_structure ASSIGNING FIELD-SYMBOL(<contact_name_struc>).

      "set METADATA title
      CASE <contact_name_struc>-name.
        WHEN 1.
          CONCATENATE /sew/cl_mig_utils=>metadata /sew/cl_mig_utils=>separator INTO metadata.
          CONTINUE.
      ENDCASE.

      CONCATENATE metadata <contact_name_struc>-value INTO metadata.

      "set separator
      CHECK length NE sy-tabix.
      CONCATENATE metadata /sew/cl_mig_utils=>separator INTO metadata.
    ENDLOOP.


  endmethod.


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

  SELECT pernr,
         begda,
         endda,
         favor,
         fanam,
         fnac2,
         fknzn,
         finit,
         fvrsw,
         fvrs2,
         titel
  INTO CORRESPONDING FIELDS OF TABLE @p0021 FROM pa0021 WHERE pernr IN @pernr
                                                          AND famsa IN @famsa
                                                          AND begda LE @endda
                                                          AND endda GE @begda.

  "get BUKRS for LegislationCode
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


METHOD GET_MAPPING_FIELDS.

  "get mapping fields for anred
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0002
                                                   sap_field    = 'TITEL'
                                                   oracle_field = 'NAMEINFORMATION1'
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_nameinfo1 ).

  "get mapping fields for anred
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0002
                                                   sap_field    = 'KNZNM'
                                                   oracle_field = 'NAMEINFORMATION7'
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_nameinfo7 ).
ENDMETHOD.


METHOD GET_MAPPING_VALUES.

  "get mapping values for titel
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0002
                                                   sap_field    = 'TITEL'
                                                   oracle_field = 'NAMEINFORMATION1'
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_nameinfo1 ).

  "get mapping fields for anred
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0002
                                                   sap_field    = 'KNZNM'
                                                   oracle_field = 'NAMEINFORMATION7'
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_nameinfo7 ).
ENDMETHOD.


METHOD map_cofu_data.

  DATA: src_id      TYPE string,
        sys_id      TYPE string,
        nameinfo1   TYPE string,
        nameinfo2   TYPE string,
        nameinfo3   TYPE string,
        nameinfo4   TYPE string,
        nameinfo5   TYPE string,
        nameinfo6   TYPE string,
        nameinfo7   TYPE string,
        nameadjunct TYPE string,
        land1       TYPE  /iwbep/s_mgw_name_value_pair.

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
                name
                <p0021>-pernr
                count_str
    INTO src_id  SEPARATED BY '_'.

    map_mig_values( EXPORTING p0021     = <p0021>
                    IMPORTING title     = DATA(title_tmp)
                              nameinfo7 = DATA(nameinfo7_tmp) ).

    CASE sy-mandt.
      WHEN /sew/cl_int_constants=>cofu_mandant-netherlands.
        nameadjunct = <p0021>-fvrsw.
        nameinfo1   = <p0021>-finit.
        nameinfo5   = <p0021>-fnac2.
        nameinfo6   = <p0021>-fvrs2.
        nameinfo7   = nameinfo7_tmp.
        nameinfo2   = title_tmp.
        TRANSLATE nameadjunct TO UPPER CASE.
        TRANSLATE nameinfo6   TO UPPER CASE.
    ENDCASE.

    DATA(eff_start_date) = /sew/cl_mig_utils=>convert_date( <p0021>-begda ).

    CONCATENATE /sew/cl_mig_utils=>merge
                contact_name
                per_id
                eff_start_date
                ''
                land1-value
                global
                <p0021>-favor
                ''
                <p0021>-fanam
                sys_id
                src_id
                nameinfo1
                nameinfo2
                nameinfo3
                nameinfo4
                nameinfo5
                nameinfo6
                nameinfo7
                nameadjunct
    INTO DATA(data_tmp) SEPARATED BY /sew/cl_mig_utils=>separator.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.

  ENDLOOP.
ENDMETHOD.


METHOD MAP_MIG_VALUES.
  DATA: value_tmp TYPE /sew/dd_value.

  "Process ANRED mapping
  value_tmp = CONV #( p0021-titel ).
  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0002
      field_sap      = /sew/cl_mig_person_name=>sap_titel
      field_oracle   = 'NAMEINFORMATION1'
      mapping_fields = CONV #( mapping_fields_nameinfo1 )
      mapping_values = CONV #( mapping_values_nameinfo1 )
    CHANGING
      value          = value_tmp ).

  title              = value_tmp.

  "Process ANRED mapping
  value_tmp = CONV #( p0021-fknzn ).
  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0002
      field_sap      = 'KNZNM'
      field_oracle   = 'NAMEINFORMATION7'
      mapping_fields = CONV #( mapping_fields_nameinfo7 )
      mapping_values = CONV #( mapping_values_nameinfo7 )
    CHANGING
      value          = value_tmp ).

  nameinfo7 = value_tmp.
ENDMETHOD.


METHOD proceed_cofu_con_name.

  get_cofu_data( ).
  get_mapping_fields( ).
  get_mapping_values( ).

  data = map_cofu_data( vp_src_id ).
ENDMETHOD.
ENDCLASS.
