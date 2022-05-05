class /SEW/CL_MIG_COST_ALLOCATION definition
  public
  create public .

public section.

  data PERNR type RSDSSELOPT_T .
  data BEGDA type BEGDA .
  data ENDDA type ENDDA .
  data COFU type BOOLEAN .
  data COGL type BOOLEAN .
  data MOLGA type RSDSSELOPT_T .
  data VP_COST_ALLOCATION_DATA type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  constants COST_ALLOCATION_DATA type STRING value 'CostAllocationV3' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !PERNR type RSDSSELOPT_T
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !COFU type BOOLEAN
      !COGL type BOOLEAN
      !MOLGA type RSDSSELOPT_T
      !COGU type BOOLEAN .
  methods PROCEED_COFU_COST_ALLOCATION
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !WORKER type ref to /SEW/CL_MIG_WORKER
    returning
      value(DATA) type STRING .
  methods PROCEED_COGL_COST_ALLOCATION
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

  data MAPPING_FIELDS_BUKRS type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPP_FI .
  data MAPPING_VALUES_BUKRS type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPPING .
  constants LEG_DATA_GROUP_NAME type STRING value 'LDG' ##NO_TEXT.
  constants SOURCE_TYPE type STRING value 'ASG' ##NO_TEXT.
  constants ALLOCATION type STRING value 'Allocation' ##NO_TEXT.
  data P0001 type P0001_TAB .
  data LAND1_MAP type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  data COGU type BOOLEAN .
  data P0027 type P0027_TAB .
  data P0000 type P0000_TAB .

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



CLASS /SEW/CL_MIG_COST_ALLOCATION IMPLEMENTATION.


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
    vp_cost_allocation_data = VALUE #( ( name = 1  value = /sew/cl_mig_utils=>merge )
                                       ( name = 2  value = cost_allocation_data )
                                       ( name = 3  value = 'EffectiveEndDate' )
                                       ( name = 4  value = 'EffectiveStartDate' )
                                       ( name = 5  value = 'SourceSystemId' )
                                       ( name = 6  value = 'SourceSystemOwner' )
                                       ( name = 7  value = 'SourceType' )
                                       ( name = 8  value = 'LegislativeDataGroupName' )
                                       ( name = 9  value = 'AssignmentId(SourceSystemId)' ) ).
  ELSEIF cofu EQ abap_true.
    vp_cost_allocation_data = VALUE #( ( name = 1  value = /sew/cl_mig_utils=>merge )
                                       ( name = 2  value = cost_allocation_data )
                                       ( name = 3  value = 'EffectiveEndDate' )
                                       ( name = 4  value = 'EffectiveStartDate' )
                                       ( name = 5  value = 'SourceSystemId' )
                                       ( name = 6  value = 'SourceSystemOwner' )
                                       ( name = 7  value = 'SourceType' )
                                       ( name = 8  value = 'LegislativeDataGroupName' )
                                       ( name = 9  value = 'AssignmentId(SourceSystemId)' ) ).
  ENDIF.

ENDMETHOD.


METHOD CREATE_METADATA.
  DESCRIBE TABLE vp_cost_allocation_data LINES DATA(length).

  LOOP AT vp_cost_allocation_data ASSIGNING FIELD-SYMBOL(<cost_allocation_data>).

    "set METADATA title
    CASE <cost_allocation_data>-name.
      WHEN 1.
        CONCATENATE /sew/cl_mig_utils=>metadata /sew/cl_mig_utils=>separator INTO metadata.
        CONTINUE.
    ENDCASE.

    CONCATENATE metadata <cost_allocation_data>-value INTO metadata.

    "set separator
    CHECK length NE sy-tabix.
    CONCATENATE metadata /sew/cl_mig_utils=>separator INTO metadata.
  ENDLOOP.
ENDMETHOD.


METHOD get_cofu_data.

  "Get BUKRS for LegislationCode
  SELECT pernr,
         begda,
         endda,
         bukrs INTO CORRESPONDING FIELDS OF TABLE @p0001 FROM pa0001 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.

  IF sy-mandt EQ /sew/cl_int_constants=>cofu_mandant-netherlands.
    "Get cost account distribution for Netherlands
    SELECT * INTO CORRESPONDING FIELDS OF TABLE @p0027 FROM pa0027 WHERE pernr IN @pernr AND
                                                                         begda LE @endda AND
                                                                         endda GE @begda.
  ENDIF.

  DATA(bukrs) = VALUE rsdsselopt_t( FOR <p0001> IN p0001 ( sign = 'I' option = 'EQ' low = <p0001>-bukrs ) ).
  SORT bukrs BY low.
  DELETE ADJACENT DUPLICATES FROM bukrs COMPARING low.
  land1_map = /sew/cl_mig_utils=>get_legislation_codes( bukrs ).
ENDMETHOD.


METHOD GET_COGL_DATA.

  "Get BUKRS for LegislationCode
  SELECT pernr,
         begda,
         endda,
         bukrs INTO CORRESPONDING FIELDS OF TABLE @p0001 FROM pa0001 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.

  DATA(bukrs) = VALUE rsdsselopt_t( FOR <p0001> IN p0001 ( sign = 'I' option = 'EQ' low = <p0001>-bukrs ) ).
  SORT bukrs by low.
  DELETE ADJACENT DUPLICATES FROM bukrs COMPARING low.
  land1_map = /sew/cl_mig_utils=>get_legislation_codes( bukrs ).
ENDMETHOD.


METHOD map_cofu_data.

  DATA: src_id       TYPE string,
        sys_id       TYPE string,
        leg_grp_name TYPE string,
        src_asn_id   TYPE string,
        src_sys_id   TYPE string,
        begda_tmp    TYPE string,
        endda_tmp    TYPE string,
        count        TYPE int8,
        pernr_old    TYPE rsdsselopt_t,
        land1        TYPE /iwbep/s_mgw_name_value_pair.

  FIELD-SYMBOLS: <p0000> TYPE p0000.

  CONCATENATE /sew/cl_mig_utils=>sap sy-mandt INTO sys_id.

  LOOP AT p0027 ASSIGNING FIELD-SYMBOL(<p0027>).
    CHECK <p0027>-pernr NOT IN pernr_old OR
    pernr_old IS INITIAL.

    APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0027>-pernr ) TO pernr_old.

    <p0027>-endda = cl_hcp_global_constants=>c_highdate. "JMB20220211 I - CostAllocatin isn´t date effective, so only one entry can be passed

**JMB20210811 start insert - check for worker entry
*
    "get source id
    src_sys_id = /sew/cl_mig_utils=>get_src_id( pernr = <p0027>-pernr
                                                begda = <p0027>-begda
                                                endda = <p0027>-endda
                                                vp_src_id = vp_src_id ).

    CHECK src_sys_id IS NOT INITIAL.  "JMB20210811 I
*JMB20210811 end insert

    LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001_tmp>) WHERE pernr = <p0027>-pernr AND
                                                            begda = <p0027>-begda AND
                                                            endda = <p0027>-endda.
      "get legislationcode
      CLEAR: land1, leg_grp_name.
      READ TABLE land1_map INTO land1 WITH KEY name = <p0001_tmp>-bukrs.
      CONCATENATE land1-value leg_data_group_name INTO leg_grp_name SEPARATED BY space.
      EXIT.
    ENDLOOP.

    "set start date to migration date
    LOOP AT p0000 ASSIGNING <p0000> WHERE pernr EQ <p0027>-pernr.
      IF <p0027>-begda LT <p0000>-begda.
        <p0027>-begda = <p0000>-begda.
      ENDIF.
    ENDLOOP.

    begda_tmp = /sew/cl_mig_utils=>convert_date( <p0027>-begda ).
    endda_tmp = /sew/cl_mig_utils=>convert_date( <p0027>-endda ).

    "get source id
    CONCATENATE /sew/cl_mig_utils=>assign <p0027>-pernr INTO src_asn_id.

    CONCATENATE src_asn_id '_' allocation INTO src_id.

    CONCATENATE /sew/cl_mig_utils=>merge
                cost_allocation_data
                endda_tmp
                begda_tmp
                src_id
                sys_id
                source_type
                leg_grp_name
                src_asn_id
    INTO DATA(data_0027) SEPARATED BY /sew/cl_mig_utils=>separator.

    CONCATENATE data cl_abap_char_utilities=>newline data_0027 INTO data.
  ENDLOOP.

  IF pernr_old IS NOT INITIAL.
    DELETE p0001 WHERE pernr IN pernr_old.
  ENDIF.

  LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>).
    CHECK <p0001>-pernr NOT IN pernr_old OR
          pernr_old     IS INITIAL.

    APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0001>-pernr ) TO pernr_old.

    <p0001>-endda = cl_hcp_global_constants=>c_highdate. "JMB20220211 I - CostAllocatin isn´t date effective, so only one entry can be passed

**JMB20210811 start insert - check for worker entry
*
    "get source id
    src_sys_id = /sew/cl_mig_utils=>get_src_id( pernr = <p0001>-pernr
                                                begda = <p0001>-begda
                                                endda = <p0001>-endda
                                                vp_src_id = vp_src_id ).

    CHECK src_sys_id IS NOT INITIAL.  "JMB20210811 I
*JMB20210811 end insert

    "get legislationcode
    CLEAR: land1, leg_grp_name.
    READ TABLE land1_map INTO land1 WITH KEY name = <p0001>-bukrs.

    CONCATENATE land1-value leg_data_group_name INTO leg_grp_name SEPARATED BY space.

    "set start date to migration date
    LOOP AT p0000 ASSIGNING <p0000> WHERE pernr EQ <p0001>-pernr.
      IF <p0001>-begda LT <p0000>-begda.
        <p0001>-begda = <p0000>-begda.
      ENDIF.
    ENDLOOP.

    begda_tmp = /sew/cl_mig_utils=>convert_date( <p0001>-begda ).
    endda_tmp = /sew/cl_mig_utils=>convert_date( <p0001>-endda ).

    "get source id
    CONCATENATE /sew/cl_mig_utils=>assign <p0001>-pernr INTO src_asn_id.

    CONCATENATE src_asn_id '_' allocation INTO src_id.

    CONCATENATE /sew/cl_mig_utils=>merge
                cost_allocation_data
                endda_tmp
                begda_tmp
                src_id
                sys_id
                source_type
                leg_grp_name
                src_asn_id
    INTO DATA(data_tmp) SEPARATED BY /sew/cl_mig_utils=>separator.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
  ENDLOOP.
ENDMETHOD.


METHOD MAP_COGL_DATA.

  DATA: src_id         TYPE string,
        sys_id         TYPE string,
        leg_grp_name   TYPE string,
        src_asn_id     type string,
        count          TYPE int8,
        land1          TYPE /iwbep/s_mgw_name_value_pair.

  CONCATENATE /sew/cl_mig_utils=>sap sy-mandt INTO sys_id.

  LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>).

    "get legislationcode
    CLEAR: land1, leg_grp_name.
    READ TABLE land1_map INTO land1 WITH KEY name = <p0001>-bukrs.

    CONCATENATE land1-value leg_data_group_name INTO leg_grp_name SEPARATED BY space.

    DATA(begda_tmp) = /sew/cl_mig_utils=>convert_date( <p0001>-begda ).
    DATA(endda_tmp) = /sew/cl_mig_utils=>convert_date( <p0001>-endda ).

    "get source id
    CONCATENATE /sew/cl_mig_utils=>assign <p0001>-pernr INTO src_asn_id.

    CONCATENATE src_asn_id '_' allocation INTO src_id.

    CONCATENATE /sew/cl_mig_utils=>merge
                cost_allocation_data
                endda_tmp
                begda_tmp
                src_id
                sys_id
                source_type
                leg_grp_name
                src_asn_id
    INTO DATA(data_tmp) SEPARATED BY /sew/cl_mig_utils=>separator.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
  ENDLOOP.
ENDMETHOD.


METHOD PROCEED_COFU_COST_ALLOCATION.
  get_cofu_data( ).
  p0000 = worker->p0000.
  DELETE p0000 WHERE massn NE 'ZO'. "Keep only migration action to set start date
  data = map_cofu_data( vp_src_id ).
ENDMETHOD.


METHOD proceed_cogl_cost_allocation.
  get_cogl_data( ).
  data = map_cogl_data( vp_src_id ).
ENDMETHOD.
ENDCLASS.
