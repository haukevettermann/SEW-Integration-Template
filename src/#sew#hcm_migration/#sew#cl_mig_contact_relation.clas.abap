class /SEW/CL_MIG_CONTACT_RELATION definition
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
  data VP_CON_RELATIONSHIP_STRUCTURE type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  constants CONTACT_RELATIONSHIP type STRING value 'ContactRelationship' ##NO_TEXT.
  data P0021 type P0021_TAB .
  constants PER type STRING value 'PER' ##NO_TEXT.
  constants CONT type STRING value 'CONT' ##NO_TEXT.
  constants REL type STRING value 'REL' ##NO_TEXT.
  constants FAMSA type /SEW/DD_FIELD value 'FAMSA' ##NO_TEXT.
  data MAPPING_FIELDS_FAMSA type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPP_FI .
  data MAPPING_VALUES_FAMSA type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPPING .

  methods CONSTRUCTOR
    importing
      !PERNR type RSDSSELOPT_T
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !COFU type BOOLEAN
      !COGL type BOOLEAN
      !MOLGA type RSDSSELOPT_T
      !COGU type BOOLEAN .
  methods PROCEED_COFU_CON_RELATIONSHIP
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

  methods GET_COFU_DATA .
  methods MAP_COFU_DATA
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
    returning
      value(DATA) type STRING .
  methods GET_MAPPING_COFU_FIELDS .
  methods GET_MAPPING_COFU_VALUES .
  methods MAP_MIG_VALUES
    importing
      !P0021 type P0021
    exporting
      !FAMSA type /SEW/DD_VALUE .
ENDCLASS.



CLASS /SEW/CL_MIG_CONTACT_RELATION IMPLEMENTATION.


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
      vp_con_relationship_structure = VALUE #( ( name = 1  value = /sew/cl_mig_utils=>merge )
                                               ( name = 2  value = contact_relationship )
                                               ( name = 3  value = 'EffectiveStartDate' )
                                               ( name = 4  value = 'EffectiveEndDate' )
                                               ( name = 5  value = 'PersonId(SourceSystemId)' )
                                               ( name = 6  value = 'PersonNumber' )
                                               ( name = 7  value = 'RelatedPersonId(SourceSystemId)' )
                                               ( name = 8  value = 'RelatedPersonNumber' )
                                               ( name = 9  value = 'ContactType' )
                                               ( name = 10 value = 'StatutoryDependent' )
                                               ( name = 11 value = 'EmergencyContactFlag' )
                                               ( name = 12 value = 'ExistingPerson' )
                                               ( name = 13 value = 'PersonalFlag' )
                                               ( name = 14 value = 'PrimaryContactFlag' )
                                               ( name = 15 value = 'SequenceNumber' )
                                               ( name = 16 value = 'SourceSystemOwner' )
                                               ( name = 17 value = 'SourceSystemId' ) ).
    ELSEIF cogl EQ abap_true.

    ENDIF.

  ENDMETHOD.


  METHOD create_metadata.

    DESCRIBE TABLE vp_con_relationship_structure LINES DATA(length).

    LOOP AT vp_con_relationship_structure ASSIGNING FIELD-SYMBOL(<contact_relation_data>).

      "set METADATA title
      CASE <contact_relation_data>-name.
        WHEN 1.
          CONCATENATE /sew/cl_mig_utils=>metadata /sew/cl_mig_utils=>separator INTO metadata.
          CONTINUE.
      ENDCASE.

      CONCATENATE metadata <contact_relation_data>-value INTO metadata.

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

  " Read Infotype 0021
  SELECT pernr,
         begda,
         endda,
         famsa,
         objps,
         fgbdt
  INTO CORRESPONDING FIELDS OF TABLE @p0021 FROM pa0021 WHERE pernr IN @pernr
                                                          AND famsa IN @famsa
                                                          AND begda LE @endda
                                                          AND endda GE @begda.

ENDMETHOD.


  METHOD get_mapping_cofu_fields.


    " get mapping fields for ContactRelation
    /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga          = molga
                                                     infty          = /sew/cl_mig_utils=>it0021
                                                     sap_field      = /sew/cl_mig_contact_relation=>famsa
                                                     oracle_field   = /sew/cl_mig_utils=>contact_type
                                                     export         = abap_true
                                           IMPORTING mapping_fields = mapping_fields_famsa ).


  ENDMETHOD.


  METHOD get_mapping_cofu_values.

    /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga          = molga
                                                     infty          = /sew/cl_mig_utils=>it0021
                                                     sap_field      = /sew/cl_mig_contact_relation=>famsa
                                                     oracle_field   = /sew/cl_mig_utils=>contact_type
                                                     export         = abap_true
                                           IMPORTING mapping_values = mapping_values_famsa ).


  ENDMETHOD.


  METHOD map_cofu_data.

    DATA: src_id TYPE string,
          sys_id TYPE string.



    CHECK p0021 IS NOT INITIAL.

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

      DATA(count_str) = CONV string( count ).
      CONDENSE count_str.

      CONCATENATE per
                  cont
                  <p0021>-pernr
                  count_str
      INTO DATA(per_id) SEPARATED BY '_'.

      DATA(eff_start_date) = /sew/cl_mig_utils=>convert_date( <p0021>-begda ).
      DATA(rel_per_id) = per && '_' && <p0021>-pernr.

      DATA(emergency_contact) = COND #( WHEN <p0021>-emrgn IS NOT INITIAL
                                        THEN 'Y'
                                        ELSE 'N' ).

      DATA(existing_person) = 'Y'.
      map_mig_values( EXPORTING p0021 = <p0021>
                      IMPORTING famsa = DATA(contact_type) ).

      CHECK contact_type IS NOT INITIAL.

      sys_id = 'SAP_' && sy-mandt.
      CONCATENATE per
                  cont
                  rel
                  <p0021>-pernr
                  count_str
                  INTO src_id SEPARATED BY '_'.

      CONCATENATE /sew/cl_mig_utils=>merge
                  contact_relationship
                  eff_start_date
                  ''
                  per_id
                  ''
                  rel_per_id
                  ''
                  contact_type
                  ''
                  emergency_contact
                  existing_person
                  'Y'
                  'N'
                  count_str "IFT20211116 I
                  sys_id
                  src_id
      INTO DATA(data_tmp) SEPARATED BY /sew/cl_mig_utils=>separator.

      CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.

    ENDLOOP.

  ENDMETHOD.


  METHOD map_mig_values.

    DATA: value_tmp TYPE /sew/dd_value.

    " Process FAMSA mapping
    value_tmp = CONV #( p0021-famsa ).
    /sew/cl_int_mapping=>process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = /sew/cl_mig_utils=>it0021
        field_sap      = /sew/cl_mig_contact_relation=>famsa
        field_oracle   = /sew/cl_mig_utils=>contact_type
        mapping_fields = CONV #( mapping_fields_famsa )
        mapping_values = CONV #( mapping_values_famsa )
      CHANGING
        value          = value_tmp ).

    famsa = value_tmp.

  ENDMETHOD.


  METHOD proceed_cofu_con_relationship.

    get_cofu_data( ).
    get_mapping_cofu_fields( ).
    get_mapping_cofu_values( ).
    data = map_cofu_data( vp_src_id ).

  ENDMETHOD.
ENDCLASS.
