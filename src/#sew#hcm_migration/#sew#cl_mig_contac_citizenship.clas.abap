class /SEW/CL_MIG_CONTAC_CITIZENSHIP definition
  public
  create public .

public section.

  data PERNR type RSDSSELOPT_T .
  data BEGDA type BEGDA .
  data ENDDA type ENDDA .
  data COFU type BOOLEAN .
  data COGL type BOOLEAN .
  data MOLGA type RSDSSELOPT_T .
  data P0021 type P0021_TAB .
  data VP_PER_CITIZENSHIP_STRUCTURE type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  constants CONTACT_CITIZENSHIP type STRING value 'ContactCitizenship' ##NO_TEXT.
  constants PER type STRING value 'PER' ##NO_TEXT.
  constants CITIZENSHIP type STRING value 'Citizenship' ##NO_TEXT.
  constants CON_CITIZEN type STRING value 'ConCitizen' ##NO_TEXT.

  class-methods GET_SRC_ID
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !PERNR type PERNR_D
      !BEGDA type BEGDA
      !ENDDA type ENDDA
    returning
      value(SRC_ID) type STRING .
  methods PROCEED_COFU_PER_CITIZENSHIP
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !WORKER type ref to /SEW/CL_MIG_WORKER
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

  data COGU type BOOLEAN .

  methods GET_COFU_DATA .
  methods MAP_COFU_DATA
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
    returning
      value(DATA) type STRING .
ENDCLASS.



CLASS /SEW/CL_MIG_CONTAC_CITIZENSHIP IMPLEMENTATION.


METHOD CONSTRUCTOR.

  me->pernr = pernr.
  me->begda = begda.
  me->endda = endda.
  me->cofu  = cofu.
  me->cogl  = cogl.
  me->cogu  = cogu.
  me->molga = molga.

  IF cogl EQ abap_true OR
     cogu EQ abap_true.
    vp_per_citizenship_structure = VALUE #( ( name = 1  value = /sew/cl_mig_utils=>merge )
                                        ( name = 2  value = contact_citizenship )
                                        ( name = 3  value = 'SourceSystemId' )
                                        ( name = 4  value = 'SourceSystemOwner' )
                                        ( name = 5  value = 'PersonId(SourceSystemId)' )
                                        ( name = 6  value = 'LegislationCode' )
                                        ( name = 7  value = 'CitizenshipStatus' )
                                        ( name = 8  value = 'DateFrom' )
                                        ( name = 9  value = 'DateTo' ) ).
  ELSEIF cofu EQ abap_true.
   vp_per_citizenship_structure = VALUE #( ( name = 1  value = /sew/cl_mig_utils=>merge )
                                           ( name = 2  value = contact_citizenship )
                                           ( name = 3  value = 'SourceSystemId' )
                                           ( name = 4  value = 'SourceSystemOwner' )
                                           ( name = 5  value = 'PersonId(SourceSystemId)' )
                                           ( name = 6  value = 'LegislationCode' )
                                           ( name = 7  value = 'CitizenshipStatus' )
                                           ( name = 8  value = 'DateFrom' )
                                           ( name = 9  value = 'DateTo' ) ).
  ENDIF.
ENDMETHOD.


  METHOD CREATE_METADATA.

    DESCRIBE TABLE vp_per_citizenship_structure LINES DATA(length).

    LOOP AT vp_per_citizenship_structure ASSIGNING FIELD-SYMBOL(<person_citizenship_struc>).

      "set METADATA title
      CASE <person_citizenship_struc>-name.
        WHEN 1.
          CONCATENATE /sew/cl_mig_utils=>metadata /sew/cl_mig_utils=>separator INTO metadata.
          CONTINUE.
      ENDCASE.

      CONCATENATE metadata <person_citizenship_struc>-value INTO metadata.

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
         fanat
  INTO CORRESPONDING FIELDS OF TABLE @p0021 FROM pa0021 WHERE pernr IN @pernr
                                                          AND famsa IN @famsa
                                                          AND begda LE @endda
                                                          AND endda GE @begda.
ENDMETHOD.


  METHOD GET_SRC_ID.

    LOOP AT vp_src_id ASSIGNING FIELD-SYMBOL(<src_id>) WHERE name EQ pernr.

      "get date
      DATA(length) = strlen( <src_id>-value ).
      DATA(start)  = length - 8.
      DATA(datum)  = CONV datum( <src_id>-value+start(8) ).

      "default
      src_id = <src_id>-value.
      EXIT.

    ENDLOOP.
  ENDMETHOD.


METHOD map_cofu_data.

  DATA: language    TYPE string,
        src_id      TYPE string,
        sys_id      TYPE string,
        check_pernr TYPE pernr_d,
        land1       TYPE /iwbep/s_mgw_name_value_pair,
        count       TYPE i.

  CHECK p0021 IS NOT INITIAL.
  CONCATENATE /sew/cl_mig_utils=>sap sy-mandt INTO sys_id.

  SORT p0021 BY pernr ASCENDING begda ASCENDING.

  LOOP AT p0021 ASSIGNING FIELD-SYMBOL(<p0021>).

    IF <p0021>-pernr NE check_pernr OR
       check_pernr IS INITIAL.
      count = 1.
      check_pernr = <p0021>-pernr.
    ELSE.
      count = count + 1.
    ENDIF.

    CHECK <p0021>-fanat IS NOT INITIAL.

    DATA(begda_tmp) = /sew/cl_mig_utils=>convert_date( <p0021>-begda ).
    DATA(endda_tmp) = /sew/cl_mig_utils=>convert_date( <p0021>-endda ).

    DATA(count_str) = CONV string( count ).
    CONDENSE count_str.

    CONCATENATE per
                /sew/cl_mig_contact=>cont
                <p0021>-pernr
                count_str
    INTO DATA(per_id) SEPARATED BY '_'.

    CONCATENATE per
                con_citizen
                <p0021>-pernr
                count_str
    INTO src_id SEPARATED BY '_'.

    CONCATENATE /sew/cl_mig_utils=>merge
                contact_citizenship
                src_id
                sys_id
                per_id
                <p0021>-fanat
                'Active'
                begda_tmp
                endda_tmp
    INTO DATA(data_tmp) SEPARATED BY /sew/cl_mig_utils=>separator.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.

  ENDLOOP.
ENDMETHOD.


METHOD proceed_cofu_per_citizenship.
  get_cofu_data( ).
  data = map_cofu_data( vp_src_id ).
ENDMETHOD.
ENDCLASS.
