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
  data P0001 type P0001_TAB .

  methods PROCEED_COFU_EXTRA_INFO
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !WORKER type ref to /SEW/CL_MIG_WORKER .
  methods CONSTRUCTOR
    importing
      !PERNR type RSDSSELOPT_T
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !COFU type BOOLEAN
      !COGL type BOOLEAN
      !MOLGA type RSDSSELOPT_T
      !COGU type BOOLEAN .
  methods MAP_COFU_DATA
    importing
      !PERIODS type HRPERIODS_TAB
      !PERNR type PERNR_D
    returning
      value(DATA) type STRING .
  methods CREATE_METADATA
    returning
      value(METADATA) type STRING .
  PROTECTED SECTION.
private section.

  data VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  data COGU type BOOLEAN .
  data DE_TMGT type STRING value 'SEW_DE_TMGT' ##NO_TEXT.
  data AT_TMGT type STRING value 'SEW_AT_TMGT' ##NO_TEXT.
  data IT_TMGT type STRING value 'SEW_IT_TMGT' ##NO_TEXT.

  methods GET_COFU_DATA .
  methods MAP_0001
    importing
      !PERIODS type HRPERIODS_TAB
      !PERNR type PERNR_D
    returning
      value(DATA) type STRING .
  methods MAP_0008
    importing
      !PERIODS type HRPERIODS_TAB
      !PERNR type PERNR_D
    returning
      value(DATA) type STRING .
  methods MAP_0050
    importing
      !PERIODS type HRPERIODS_TAB
      !PERNR type PERNR_D
    returning
      value(DATA) type STRING .
ENDCLASS.



CLASS /SEW/CL_MIG_ASSIGN_EXTRA_INFO IMPLEMENTATION.


  METHOD constructor.

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
*                               ( name = 11 value = 'sewNlTmgtEndDt(PER_ASSIGNMENT_EIT_EFF=SEW_NL_TMGT)' )
                               ( name = 11 value = 'sewNlTmgtRole(PER_ASSIGNMENT_EIT_EFF=SEW_NL_TMGT)' )
*                               ( name = 11 value = 'sewDeTmgtStartDt(PER_ASSIGNMENT_EIT_EFF=SEW_DE_TMGT)' )
*                               ( name = 11 value = 'sewDeTmgtEndDt(PER_ASSIGNMENT_EIT_EFF=SEW_DE_TMGT)' )
*                               ( name = 11 value = 'sewDeTmgtRole(PER_ASSIGNMENT_EIT_EFF=SEW_DE_TMGT)' )
                               ( name = 11 value = 'sewNlPayrollStartDt(PER_ASSIGNMENT_EIT_EFF=SEW_NL_PAYROLL)' )
*                               ( name = 11 value = 'sewNlPayrollEndDt(PER_ASSIGNMENT_EIT_EFF=SEW_NL_PAYROLL)' )
                               ( name = 11 value = 'sewNlPayrollArea(PER_ASSIGNMENT_EIT_EFF=SEW_NL_PAYROLL)' )
                               ( name = 55 value = 'swjLastGradeChange(PER_ASSIGNMENT_EIT_EFF=SEW_NL_LAST_GRADE_CHANGE)' )
                               ( name = 11 value = 'sewItPayrollStartDt(PER_ASSIGNMENT_EIT_EFF=SEW_IT_PAYROLL)' )
*                               ( name = 11 value = 'sewItPayrollEndDt(PER_ASSIGNMENT_EIT_EFF=SEW_IT_PAYROLL)' )
                               ( name = 11 value = 'sewItPayrollArea(PER_ASSIGNMENT_EIT_EFF=SEW_IT_PAYROLL)' )
                               ( name = 11 value = 'sewItSapPay(PER_ASSIGNMENT_EIT_EFF=SEW_IT_PAYROLL)' )
*                               ( name = 11 value = 'sewItSapTime(PER_ASSIGNMENT_EIT_EFF=SEW_IT_PAYROLL)' )
                               ( name = 11 value = 'sewAtPayrollStartDt(PER_ASSIGNMENT_EIT_EFF=SEW_AT_PAYROLL)' )
                               ( name = 11 value = 'sewAtPayrollArea(PER_ASSIGNMENT_EIT_EFF=SEW_AT_PAYROLL)' )
                               ( name = 11 value = 'sewAtSapPay(PER_ASSIGNMENT_EIT_EFF=SEW_AT_PAYROLL)' )
                               ( name = 11 value = 'sewAtSapTime(PER_ASSIGNMENT_EIT_EFF=SEW_AT_PAYROLL)' )
                               ( name = 11 value = 'sewAtTmgtStartDt(PER_ASSIGNMENT_EIT_EFF=SEW_AT_TMGT)' )
                               ( name = 11 value = 'sewAtTmgtRole(PER_ASSIGNMENT_EIT_EFF=SEW_AT_TMGT)' )
                               ( name = 11 value = 'sewItTmgtStartDt(PER_ASSIGNMENT_EIT_EFF=SEW_IT_TMGT)' )
                               ( name = 11 value = 'sewItTmgtRole(PER_ASSIGNMENT_EIT_EFF=SEW_IT_TMGT)' )
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


METHOD get_cofu_data.

  "Get IT0050
  SELECT pernr,
         begda,
         endda,
         bdegr FROM pa0050 INTO CORRESPONDING FIELDS OF TABLE @p0050 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.

  "Get IT0001
  SELECT pernr,
         begda,
         endda,
         abkrs,
         sachp,
         sachz FROM pa0001 INTO CORRESPONDING FIELDS OF TABLE @p0001 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.
ENDMETHOD.


METHOD map_0001.

  DATA: src_id          TYPE string,
        sys_id          TYPE string,
        pernr_counter   TYPE i VALUE 0,
        pernr_old       TYPE pernr_d,
        src_sys_id      TYPE string,
        begda_tmp_nl    TYPE string,
        endda_tmp_nl    TYPE string,
        bdegr_nl        TYPE string,
        begda_tmp_de    TYPE string,
        endda_tmp_de    TYPE string,
        bdegr_de        TYPE string,
        begda_py_tmp_nl TYPE string,
        endda_py_tmp_nl TYPE string,
        payroll_nl      TYPE string,
        begda_py_tmp_it TYPE string,
        endda_py_tmp_it TYPE string,
        payroll_it      TYPE string,
        sacha_it        TYPE string,
        sachz_it        TYPE string,
        begda_py_tmp_at TYPE string,
        endda_py_tmp_at TYPE string,
        payroll_at      TYPE string,
        sacha_at        TYPE string,
        sachz_at        TYPE string,
        data_tmp        TYPE string,
        old_0001        TYPE rsdsselopt_t.

  CONCATENATE /sew/cl_mig_utils=>sap sy-mandt INTO sys_id.

  LOOP AT periods ASSIGNING FIELD-SYMBOL(<period>).
    LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>) WHERE pernr EQ pernr          AND
                                                        begda LE <period>-endda AND
                                                        endda GE <period>-begda.
      EXIT.
    ENDLOOP.

    CHECK <p0001> IS ASSIGNED.

    CHECK <p0001>-abkrs IS NOT INITIAL.

    IF pernr_old NE pernr.
      pernr_old = pernr.
      pernr_counter = 0.
    ENDIF.

    pernr_counter = pernr_counter + 1.

    DATA(xx_payroll) = COND string( WHEN sy-mandt EQ /sew/cl_int_constants=>cofu_mandant-germany     THEN 'SEW_DE_PAYROLL'
                                    WHEN sy-mandt EQ /sew/cl_int_constants=>cofu_mandant-netherlands THEN 'SEW_NL_PAYROLL'
                                    WHEN sy-mandt EQ /sew/cl_int_constants=>cofu_mandant-france      THEN 'SEW_FR_PAYROLL'  ).

    IF '15' IN molga.
      xx_payroll = 'SEW_IT_PAYROLL'.
    ELSEIF '03' IN molga.
      xx_payroll = 'SEW_AT_PAYROLL'.
    ENDIF.

    CHECK xx_payroll IS NOT INITIAL.

    DATA(begda_tmp) = /sew/cl_mig_utils=>convert_date( <period>-begda ).
    DATA(endda_tmp) = /sew/cl_mig_utils=>convert_date( <period>-endda ).

    CLEAR: begda_tmp_nl, endda_tmp_nl, bdegr_nl, begda_tmp_de, endda_tmp_de, bdegr_de.

    CASE sy-mandt.
      WHEN /sew/cl_int_constants=>cofu_mandant-netherlands.
        begda_py_tmp_nl = begda_tmp.
        endda_py_tmp_nl = endda_tmp.
        payroll_nl      = <p0001>-abkrs.
      WHEN /sew/cl_int_constants=>cofu_mandant-italy.
        IF '15' IN molga.
          begda_py_tmp_it = begda_tmp.
          endda_py_tmp_it = endda_tmp.
          payroll_it      = <p0001>-abkrs.
          sacha_it        = <p0001>-sachp.
          sachz_it        = <p0001>-sachz.
        ELSEIF '03' IN molga.
          begda_py_tmp_at = begda_tmp.
          endda_py_tmp_at = endda_tmp.
          payroll_at      = <p0001>-abkrs.
          sacha_at        = <p0001>-sachp.
          sachz_at        = <p0001>-sachz.
        ENDIF.
      WHEN /sew/cl_int_constants=>cofu_mandant-germany.

    ENDCASE.

    DATA(latestchange) = COND string( WHEN sy-datum BETWEEN <p0001>-begda AND <p0001>-endda THEN /sew/cl_mig_utils=>yes
                                      ELSE '' ).

    src_id = CONV #( pernr_counter ).
    CONCATENATE /sew/cl_mig_utils=>assign pernr INTO DATA(asn_id).

    "get source id
    src_sys_id = /sew/cl_mig_utils=>get_src_id( pernr = pernr
                                                begda = <period>-begda
                                                endda = <period>-endda
                                                vp_src_id = vp_src_id ).
    DATA(counter) = src_id.
    CONDENSE counter.

    "ID needs to be unique
    CONCATENATE xx_payroll '_' asn_id '_' src_id INTO src_id.
    CONDENSE src_id.

    CONCATENATE /sew/cl_mig_utils=>merge
                assign_extra_info
                asn_id
                xx_payroll
                'PER_ASG_EIT'
                xx_payroll
                begda_tmp
                endda_tmp
                latestchange
                counter
                xx_payroll
                begda_tmp_nl
*                endda_tmp_nl
                bdegr_nl
*                begda_tmp_de
*                endda_tmp_de
*                bdegr_de
                begda_py_tmp_nl
*                endda_py_tmp_nl
                payroll_nl
                ''
                begda_py_tmp_it
*                endda_py_tmp_it
                payroll_it
                sacha_it
*                sachz_it
                begda_py_tmp_at
                payroll_at
                sacha_at
                sachz_at
                ''
                ''
                ''
                ''
                src_id
                sys_id
    INTO data_tmp SEPARATED BY /sew/cl_mig_utils=>separator.

    IF data IS INITIAL.
      data = data_tmp.
      CONTINUE.
    ENDIF.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.

  ENDLOOP.
ENDMETHOD.


METHOD map_0008.

  DATA: src_id            TYPE string,
        sys_id            TYPE string,
        pernr_counter     TYPE i VALUE 0,
        pernr_old         TYPE pernr_d,
        src_sys_id        TYPE string,
        begda_tmp_nl      TYPE string,
        endda_tmp_nl      TYPE string,
        bdegr_nl          TYPE string,
        begda_tmp_de      TYPE string,
        endda_tmp_de      TYPE string,
        bdegr_de          TYPE string,
        begda_py_tmp_nl   TYPE string,
        endda_py_tmp_nl   TYPE string,
        payroll_nl        TYPE string,
        begda_py_tmp_it   TYPE string,
        endda_py_tmp_it   TYPE string,
        payroll_it        TYPE string,
        sacha_it          TYPE string,
        sachz_it          TYPE string,
        begda_py_tmp_at   TYPE string,
        endda_py_tmp_at   TYPE string,
        payroll_at        TYPE string,
        sacha_at          TYPE string,
        sachz_at          TYPE string,
        data_tmp          TYPE string,
        last_grade        TYPE datum,
        last_grade_change TYPE string,
        old_0001          TYPE rsdsselopt_t.

  CONCATENATE /sew/cl_mig_utils=>sap sy-mandt INTO sys_id.
  DATA(xx_last_grade) = 'SEW_NL_LAST_GRADE_CHANGE'.

  LOOP AT periods ASSIGNING FIELD-SYMBOL(<period>) WHERE begda LE sy-datum AND
                                                         endda GE sy-datum.
    IF pernr_old NE pernr.
      pernr_old = pernr.
      pernr_counter = 0.

      CLEAR: last_grade.
      CALL FUNCTION '/SEW/HR_GET_LAST_GRADECHANGE'
        EXPORTING
          im_pernr = pernr
        IMPORTING
          ex_date  = last_grade.
    ENDIF.

    pernr_counter = pernr_counter + 1.

    CHECK last_grade IS NOT INITIAL.
    last_grade_change = /sew/cl_mig_utils=>convert_date( last_grade ).

    src_id = CONV #( pernr_counter ).
    DATA(counter) = src_id.
    CONDENSE counter.

    DATA(begda_tmp) = /sew/cl_mig_utils=>convert_date( <period>-begda ).
    DATA(endda_tmp) = /sew/cl_mig_utils=>convert_date( <period>-endda ).

    DATA(latestchange) = COND string( WHEN sy-datum BETWEEN <period>-begda AND <period>-endda THEN /sew/cl_mig_utils=>yes
                                      ELSE '' ).

    CONCATENATE /sew/cl_mig_utils=>assign pernr INTO DATA(asn_id).

    "get source id
    src_sys_id = /sew/cl_mig_utils=>get_src_id( pernr = pernr
                                                begda = <period>-begda
                                                endda = <period>-endda
                                                vp_src_id = vp_src_id ).

    "ID needs to be unique
    CONCATENATE xx_last_grade '_' asn_id '_' src_id INTO src_id.
    CONDENSE src_id.

    CONCATENATE /sew/cl_mig_utils=>merge
                assign_extra_info
                asn_id
                xx_last_grade
                'PER_ASG_EIT'
                xx_last_grade
                begda_tmp
                endda_tmp
                latestchange
                counter
                xx_last_grade
                begda_tmp_nl
*                endda_tmp_nl
                bdegr_nl
*                begda_tmp_de
*                endda_tmp_de
*                bdegr_de
                begda_py_tmp_nl
*                endda_py_tmp_nl
                payroll_nl
                last_grade_change
                begda_py_tmp_it
*                endda_py_tmp_it
                payroll_it
                sacha_it
*                sachz_it
                begda_py_tmp_at
                payroll_at
                sacha_at
                sachz_at
                ''
                ''
                ''
                ''
                src_id
                sys_id
    INTO data_tmp SEPARATED BY /sew/cl_mig_utils=>separator.

    IF data IS INITIAL.
      data = data_tmp.
      CONTINUE.
    ENDIF.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.

  ENDLOOP.
ENDMETHOD.


METHOD map_0050.

  DATA: src_id          TYPE string,
        sys_id          TYPE string,
        pernr_counter   TYPE i VALUE 0,
        pernr_old       TYPE pernr_d,
        src_sys_id      TYPE string,
        begda_tmp_nl    TYPE string,
        endda_tmp_nl    TYPE string,
        bdegr_nl        TYPE string,
        begda_tmp_de    TYPE string,
        endda_tmp_de    TYPE string,
        bdegr_de        TYPE string,
        begda_py_tmp_nl TYPE string,
        endda_py_tmp_nl TYPE string,
        payroll_nl      TYPE string,
        begda_py_tmp_it TYPE string,
        endda_py_tmp_it TYPE string,
        payroll_it      TYPE string,
        begda_tmp_at    TYPE string,
        bdegr_at        TYPE string,
        begda_tmp_it    TYPE string,
        bdegr_it        TYPE string,
        data_tmp        TYPE string,
        old_0050        TYPE rsdsselopt_t.

  CONCATENATE /sew/cl_mig_utils=>sap sy-mandt INTO sys_id.

  LOOP AT periods ASSIGNING FIELD-SYMBOL(<period>).
    LOOP AT p0050 ASSIGNING FIELD-SYMBOL(<p0050>) WHERE pernr EQ pernr          AND
                                                        begda LE <period>-endda AND
                                                        endda GE <period>-begda.
      EXIT.
    ENDLOOP.

    CHECK <p0050> IS ASSIGNED.

    CHECK <p0050>-bdegr IS NOT INITIAL.

    IF pernr_old NE pernr.
      pernr_old = pernr.
      pernr_counter = 0.
    ENDIF.

    data(xx_tmgt) = COND string( WHEN sy-mandt EQ /sew/cl_int_constants=>cofu_mandant-germany     THEN de_tmgt
                                 WHEN sy-mandt EQ /sew/cl_int_constants=>cofu_mandant-netherlands THEN nl_tmgt
                                 ELSE '' ).

    IF sy-mandt EQ /sew/cl_int_constants=>cofu_mandant-italy.
      IF '03' IN molga.
        xx_tmgt = at_tmgt.
      ELSEIF '15' IN molga.
        xx_tmgt = it_tmgt.
      ENDIF.
    ENDIF.

    CHECK xx_tmgt IS NOT INITIAL.

    pernr_counter = pernr_counter + 1.

    DATA(begda_tmp) = /sew/cl_mig_utils=>convert_date( <period>-begda ).
    DATA(endda_tmp) = /sew/cl_mig_utils=>convert_date( <period>-endda ).

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
      WHEN /sew/cl_int_constants=>cofu_mandant-italy.
        IF '03' IN molga.
          begda_tmp_at = begda_tmp.
          bdegr_at     = 'PR00000' && <p0050>-bdegr.
        ELSEIF '15' IN molga.
          begda_tmp_it = begda_tmp.
          bdegr_it     = 'PR00000' && <p0050>-bdegr.
        ENDIF.
    ENDCASE.

    DATA(latestchange) = COND string( WHEN sy-datum BETWEEN <p0050>-begda AND <p0050>-endda THEN /sew/cl_mig_utils=>yes
                                      ELSE '' ).

    src_id = CONV #( pernr_counter ).
    CONCATENATE /sew/cl_mig_utils=>assign pernr INTO DATA(asn_id).

    "get source id
    src_sys_id = /sew/cl_mig_utils=>get_src_id( pernr = pernr
                                                begda = <period>-begda
                                                endda = <period>-endda
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
*                endda_tmp_nl
                bdegr_nl
*                begda_tmp_de
*                endda_tmp_de
*                bdegr_de
                begda_py_tmp_nl
*                endda_py_tmp_nl
                payroll_nl
                ''
                begda_py_tmp_it
*                endda_py_tmp_it
                payroll_it
                ''
                ''
*                ''
                ''
                ''
                ''
                begda_tmp_at
                bdegr_at
                begda_tmp_it
                bdegr_it
                src_id
                sys_id
    INTO data_tmp SEPARATED BY /sew/cl_mig_utils=>separator.

    IF data IS INITIAL.
      data = data_tmp.
      CONTINUE.
    ENDIF.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.

  ENDLOOP.
ENDMETHOD.


METHOD map_cofu_data.
  DATA(data_0050) = map_0050( EXPORTING periods = periods
                                        pernr   = pernr ).
  DATA(data_0001) = map_0001( EXPORTING periods = periods
                                        pernr   = pernr ).
  CONCATENATE data_0050
              cl_abap_char_utilities=>newline
              data_0001
              cl_abap_char_utilities=>newline INTO data.
  CLEAR: data_0050, data_0001.

  IF sy-mandt EQ /sew/cl_int_constants=>cofu_mandant-netherlands.
    DATA(data_0008) = map_0008( EXPORTING periods = periods
                                          pernr   = pernr ).
    IF data_0008 IS NOT INITIAL.
      CONCATENATE data
                  cl_abap_char_utilities=>newline
                  data_0008
                  cl_abap_char_utilities=>newline INTO data.
      CLEAR: data_0008.
    ENDIF.
  ENDIF.
ENDMETHOD.


METHOD PROCEED_COFU_EXTRA_INFO.
  me->vp_src_id = vp_src_id.
  get_cofu_data( ).
ENDMETHOD.
ENDCLASS.
