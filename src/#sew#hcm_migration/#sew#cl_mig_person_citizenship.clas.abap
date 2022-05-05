class /SEW/CL_MIG_PERSON_CITIZENSHIP definition
  public
  create public .

public section.

  data PERNR type RSDSSELOPT_T .
  data BEGDA type BEGDA .
  data ENDDA type ENDDA .
  data COFU type BOOLEAN .
  data COGL type BOOLEAN .
  data MOLGA type RSDSSELOPT_T .
  data P0002 type P0002_TAB .
  data VP_PER_CITIZENSHIP_STRUCTURE type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  constants PERSON_CITIZENSHIP type STRING value 'PersonCitizenship' ##NO_TEXT.
  constants PER type STRING value 'PER_' ##NO_TEXT.
  constants CITIZENSHIP type STRING value '_Citizenship' ##NO_TEXT.
  constants CITIZEN type STRING value 'Citizen' ##NO_TEXT.

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
  methods PROCEED_COGL_PER_CITIZENSHIP
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
  methods GET_COGL_DATA .
  methods MAP_COFU_DATA
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
    returning
      value(DATA) type STRING .
  methods MAP_COGL_DATA
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
    returning
      value(DATA) type STRING .
ENDCLASS.



CLASS /SEW/CL_MIG_PERSON_CITIZENSHIP IMPLEMENTATION.


METHOD constructor.

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
                                        ( name = 2  value = person_citizenship )
                                        ( name = 3  value = 'SourceSystemId' )
                                        ( name = 4  value = 'SourceSystemOwner' )
                                        ( name = 5  value = 'PersonId(SourceSystemId)' )
                                        ( name = 6  value = 'LegislationCode' )
                                        ( name = 7  value = 'CitizenshipStatus' )
                                        ( name = 8  value = 'DateFrom' )
                                        ( name = 9  value = 'DateTo' ) ).
  ELSEIF cofu EQ abap_true.
   vp_per_citizenship_structure = VALUE #( ( name = 1  value = /sew/cl_mig_utils=>merge )
                                        ( name = 2  value = person_citizenship )
                                        ( name = 3  value = 'SourceSystemId' )
                                        ( name = 4  value = 'SourceSystemOwner' )
                                        ( name = 5  value = 'PersonId(SourceSystemId)' )
                                        ( name = 6  value = 'LegislationCode' )
                                        ( name = 7  value = 'CitizenshipStatus' )
                                        ( name = 8  value = 'DateFrom' )
                                        ( name = 9  value = 'DateTo' ) ).
  ENDIF.
ENDMETHOD.


  METHOD create_metadata.

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


METHOD GET_COFU_DATA.
  "Get IT0002
  SELECT pernr,
         begda,
         endda,
         natio INTO CORRESPONDING FIELDS OF TABLE @p0002 FROM pa0002 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda AND
                                                                           natio IS NOT NULL.
ENDMETHOD.


METHOD get_cogl_data.
  "Get IT0002
  SELECT pernr,
         begda,
         endda,
         natio INTO CORRESPONDING FIELDS OF TABLE @p0002 FROM pa0002 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.
ENDMETHOD.


  METHOD get_src_id.

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

  DATA: language TYPE string,
        src_id   TYPE string,
        sys_id   TYPE string,
        land1    TYPE /iwbep/s_mgw_name_value_pair,
        count    TYPE i.

  CONCATENATE /sew/cl_mig_utils=>sap sy-mandt INTO sys_id.

  LOOP AT p0002 ASSIGNING FIELD-SYMBOL(<p0002>) WHERE begda LE endda AND
                                                      endda GE begda AND
                                                      natio IS NOT INITIAL.

    DATA(begda_tmp) = /sew/cl_mig_utils=>convert_date( <p0002>-begda ).
    DATA(endda_tmp) = /sew/cl_mig_utils=>convert_date( <p0002>-endda ).

    CONCATENATE citizen <p0002>-pernr INTO src_id SEPARATED BY '_'." IFT 20211026 I

    DATA(src_sys_id) = /sew/cl_mig_person_citizenship=>get_src_id( pernr     = <p0002>-pernr
                                                                   begda     = <p0002>-begda
                                                                   endda     = <p0002>-endda
                                                                   vp_src_id = vp_src_id ).



    CONCATENATE /sew/cl_mig_utils=>merge
                person_citizenship
                src_id
                sys_id
                src_sys_id
                <p0002>-natio
                'Active'
                begda_tmp
                endda_tmp
    INTO DATA(data_tmp) SEPARATED BY /sew/cl_mig_utils=>separator.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.

  ENDLOOP.
ENDMETHOD.


METHOD map_cogl_data.

  DATA: language TYPE string,
        src_id   TYPE string,
        sys_id   TYPE string,
        land1    TYPE /iwbep/s_mgw_name_value_pair.

  CONCATENATE /sew/cl_mig_utils=>sap sy-mandt INTO sys_id.

  LOOP AT p0002 ASSIGNING FIELD-SYMBOL(<p0002>).

    DATA(begda_tmp) = /sew/cl_mig_utils=>convert_date( <p0002>-begda ).
    DATA(endda_tmp) = /sew/cl_mig_utils=>convert_date( <p0002>-endda ).

    CONCATENATE per <p0002>-pernr citizenship INTO src_id.

    "get source id
    DATA(src_sys_id) = /sew/cl_mig_utils=>get_src_id( pernr = <p0002>-pernr
                                                      begda = <p0002>-begda
                                                      endda = <p0002>-endda
                                                      vp_src_id = vp_src_id ).

    CONCATENATE /sew/cl_mig_utils=>merge
                person_citizenship
                src_id
                sys_id
                src_sys_id
                <p0002>-natio
                'Active'
                begda_tmp
                endda_tmp
    INTO DATA(data_tmp) SEPARATED BY /sew/cl_mig_utils=>separator.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
  ENDLOOP.
ENDMETHOD.


METHOD proceed_cofu_per_citizenship.
  get_cofu_data( ).
  /sew/cl_mig_utils=>update_begin_date( EXPORTING p0000 = worker->p0000
                                         CHANGING p0002 = p0002 ).
  /sew/cl_mig_utils=>summarize_it0002( CHANGING p0002 = p0002 ).
  data = map_cofu_data( vp_src_id ).
ENDMETHOD.


METHOD proceed_cogl_per_citizenship.
  get_cogl_data( ).
  /sew/cl_mig_utils=>update_begin_date( EXPORTING p0000 = worker->p0000
                                         CHANGING p0002 = p0002 ).
  data = map_cogl_data( vp_src_id ).
ENDMETHOD.
ENDCLASS.
