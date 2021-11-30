class /SEW/CL_MIG_ASSIGN_EXTRA_INFO definition
  public
  create public .

public section.

  data PERNR type RSDSSELOPT_T .
  data BEGDA type BEGDA .
  data ENDDA type ENDDA .
  data COFU type BOOLEAN .
  data COGL type BOOLEAN .
  data MOLGA type RSDSSELOPT_T .
  data VP_EXTRA_INFO type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  constants ASSIGN_EXTRA_INFO type STRING value 'AssignmentExtraInfo' ##NO_TEXT.
  constants NL_TMGT type STRING value 'SEW_NL_TMGT' ##NO_TEXT.
  data P0050 type PTT_P0050 .
  constants SRC_ID_NL_PREFIX type STRING value 'PER_ASG_EIT' ##NO_TEXT.

  methods PROCEED_COFU_EXTRA_INFO
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

  data VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  data COGU type BOOLEAN .
  data DE_TMGT type STRING value 'SEW_DE_TMGT' ##NO_TEXT.

  methods GET_COFU_DATA .
  methods MAP_COFU_DATA
    returning
      value(DATA) type STRING .
ENDCLASS.



CLASS /SEW/CL_MIG_ASSIGN_EXTRA_INFO IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    me->pernr = pernr.
    me->begda = begda.
    me->endda = endda.
    me->cofu  = cofu.
    me->cogu  = cogu.
    me->cogl  = cogl.
    me->molga = molga.

    IF cofu EQ abap_true.
      vp_extra_info = VALUE #( ( name = 1  value = /sew/cl_mig_utils=>merge )
                               ( name = 2  value = assign_extra_info )
                               ( name = 3  value = 'AssignmentId(SourceSystemId)' )
                               ( name = 6  value = 'FLEX:PER_ASSIGNMENT_EIT_EFF' )
                               ( name = 7  value = 'EFF_CATEGORY_CODE' )
                               ( name = 8  value = 'AeiInformationCategory' )
                               ( name = 9  value = 'EffectiveStartDate' )
                               ( name = 10 value = 'EffectiveEndDate' )
                               ( name = 11 value = 'EffectiveLatestChange' )
                               ( name = 11 value = 'EffectiveSequence' )
                               ( name = 11 value = 'InformationType' )
                               ( name = 11 value = 'sewNlTmgtStartDt(PER_ASSIGNMENT_EIT_EFF=SEW_NL_TMGT)' )
                               ( name = 11 value = 'sewNlTmgtEndDt(PER_ASSIGNMENT_EIT_EFF=SEW_NL_TMGT)' )
                               ( name = 11 value = 'sewNlTmgtRole(PER_ASSIGNMENT_EIT_EFF=SEW_NL_TMGT)' )
                               ( name = 11 value = 'sewDeTmgtStartDt(PER_ASSIGNMENT_EIT_EFF=SEW_DE_TMGT)' )
                               ( name = 11 value = 'sewDeTmgtEndDt(PER_ASSIGNMENT_EIT_EFF=SEW_DE_TMGT)' )
                               ( name = 11 value = 'sewDeTmgtRole(PER_ASSIGNMENT_EIT_EFF=SEW_DE_TMGT)' )
                               ( name = 4  value = 'SourceSystemId' )
                               ( name = 4  value = 'SourceSystemOwner' ) ).
    ENDIF.
  ENDMETHOD.


METHOD CREATE_METADATA.
  DESCRIBE TABLE vp_extra_info LINES DATA(length).

  LOOP AT vp_extra_info ASSIGNING FIELD-SYMBOL(<ext_info_data>).

    "set METADATA title
    CASE <ext_info_data>-name.
      WHEN 1.
        CONCATENATE /sew/cl_mig_utils=>metadata /sew/cl_mig_utils=>separator INTO metadata.
        CONTINUE.
    ENDCASE.

    CONCATENATE metadata <ext_info_data>-value INTO metadata.

    "set separator
    CHECK length NE sy-tabix.
    CONCATENATE metadata /sew/cl_mig_utils=>separator INTO metadata.
  ENDLOOP.
ENDMETHOD.


METHOD GET_COFU_DATA.

  "Get IT0050
  SELECT pernr,
         begda,
         endda,
         bdegr FROM pa0050 INTO CORRESPONDING FIELDS OF TABLE @p0050 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.
ENDMETHOD.


METHOD map_cofu_data.

  DATA: src_id        TYPE string,
        sys_id        TYPE string,
        pernr_counter TYPE i VALUE 0,
        pernr_old     TYPE pernr_d,
        src_sys_id    TYPE string,
        begda_tmp_nl  TYPE string,
        endda_tmp_nl  TYPE string,
        bdegr_nl      TYPE string,
        begda_tmp_de  TYPE string,
        endda_tmp_de  TYPE string,
        bdegr_de      TYPE string,
        data_tmp      TYPE string,
        old_0050      TYPE rsdsselopt_t.

  CONCATENATE /sew/cl_mig_utils=>sap sy-mandt INTO sys_id.

  LOOP AT p0050 ASSIGNING FIELD-SYMBOL(<p0050>).
    CHECK <p0050>-bdegr IS NOT INITIAL.

    IF pernr_old NE <p0050>-pernr.
      pernr_old = <p0050>-pernr.
      pernr_counter = 0.
    ENDIF.

    pernr_counter = pernr_counter + 1.

*    CHECK <p0050>-pernr NOT IN old_0050 OR
*          old_0050      IS INITIAL.

    DATA(xx_tmgt) = COND string( WHEN sy-mandt EQ /sew/cl_int_constants=>cofu_mandant-germany     THEN de_tmgt
                                 WHEN sy-mandt EQ /sew/cl_int_constants=>cofu_mandant-netherlands THEN nl_tmgt
                                 ELSE '' ).

    CHECK xx_tmgt IS NOT INITIAL.

    DATA(begda_tmp) = /sew/cl_mig_utils=>convert_date( <p0050>-begda ).
    DATA(endda_tmp) = /sew/cl_mig_utils=>convert_date( <p0050>-endda ).

    CLEAR: begda_tmp_nl, endda_tmp_nl, bdegr_nl, begda_tmp_de, endda_tmp_de, bdegr_de.

    CASE sy-mandt.
      WHEN /sew/cl_int_constants=>cofu_mandant-netherlands.
        begda_tmp_nl = begda_tmp.
        endda_tmp_nl = endda_tmp.
        bdegr_nl     = 'PR00000' && <p0050>-bdegr.
      WHEN /sew/cl_int_constants=>cofu_mandant-germany.
        begda_tmp_de = begda_tmp.
        endda_tmp_de = endda_tmp.
        bdegr_de     = 'PR00000' && <p0050>-bdegr.
    ENDCASE.

    DATA(latestchange) = COND string( WHEN sy-datum BETWEEN <p0050>-begda AND <p0050>-endda THEN /sew/cl_mig_utils=>yes
                                      ELSE '' ).

    src_id = CONV #( pernr_counter ).
    CONCATENATE /sew/cl_mig_utils=>assign <p0050>-pernr INTO DATA(asn_id).

    "get source id
    src_sys_id = /sew/cl_mig_utils=>get_src_id( pernr = <p0050>-pernr
                                                begda = <p0050>-begda
                                                endda = <p0050>-endda
                                                vp_src_id = vp_src_id ).
    DATA(counter) = src_id.
    CONDENSE counter.

    "ID needs to be unique
    CONCATENATE xx_tmgt '_' asn_id '_' src_id INTO src_id.
    CONDENSE src_id.

    CONCATENATE /sew/cl_mig_utils=>merge
                assign_extra_info
                asn_id
                xx_tmgt
                'PER_ASG_EIT'
                xx_tmgt
                begda_tmp
                endda_tmp
                latestchange
                counter
                xx_tmgt
                begda_tmp_nl
                endda_tmp_nl
                bdegr_nl
                begda_tmp_de
                endda_tmp_de
                bdegr_de
                src_id
                sys_id
    INTO data_tmp SEPARATED BY /sew/cl_mig_utils=>separator.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.

*    APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0050>-pernr ) TO old_0050.
  ENDLOOP.
ENDMETHOD.


METHOD PROCEED_COFU_EXTRA_INFO.
  me->vp_src_id = vp_src_id.
  get_cofu_data( ).
  data = map_cofu_data( ).
ENDMETHOD.
ENDCLASS.
