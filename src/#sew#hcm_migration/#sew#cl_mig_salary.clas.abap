class /SEW/CL_MIG_SALARY definition
  public
  create public .

public section.

  types:
    BEGIN OF vp_wkr_id ,
            pernr TYPE pernr ,
            begda TYPE begda ,
            endda TYPE endda ,
            wkr_id TYPE string,
           END OF vp_wkr_id .
  types:
    vp_wkr_id_t type STANDARD TABLE OF vp_wkr_id .

  data PERNR type RSDSSELOPT_T .
  data BEGDA type BEGDA .
  data ENDDA type ENDDA .
  data COGL type BOOLEAN .
  data COFU type BOOLEAN .
  data MOLGA type RSDSSELOPT_T .
  data P0001 type P0001_TAB .
  data VP_SALARY_STRUCTURE type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  constants SALARY type STRING value 'Salary' ##NO_TEXT.
  data P0008 type P0008_TAB .
  data VP_WRK_ID type /SEW/CL_MIG_WORK_RELATION=>VP_WKR_ID_T .
  data PN_BEGDA type BEGDA .
  data VP_SALARY_COMP_STRUCTURE type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  constants SALARY_COMP type STRING value 'SalarySimpleComponent' ##NO_TEXT.
  data HR_PERIODS type HRPERIODS_TAB .

  methods PROCEED_COFU_SALARY
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
      !COGU type BOOLEAN
      !PN_BEGDA type BEGDA optional .
  methods CREATE_METADATA
    returning
      value(METADATA) type STRING .
  PROTECTED SECTION.
private section.

  data MAPPING_VALUES_LGART type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPPING .
  data MAPPING_FIELDS_LGART type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPP_FI .
  data COGU type BOOLEAN .

  methods COLLECT_HR_PERIODS
    importing
      !PERNR type PERNR_D .
  methods MAP_COFU_DATA
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
    exporting
      !DATA_COMP type STRING
    returning
      value(DATA) type STRING .
  methods MAP_MIG_VALUES
    importing
      !P0008 type P0008
    exporting
      !COMPONENTCODE type /SEW/DD_VALUE .
  methods GET_COFU_DATA .
  methods GET_MAPPING_FIELDS .
  methods GET_MAPPING_VALUES .
ENDCLASS.



CLASS /SEW/CL_MIG_SALARY IMPLEMENTATION.


METHOD collect_hr_periods.

  DATA(p0001_pernr) = p0001.
  DATA(p0008_pernr) = p0008.

  DELETE p0008_pernr WHERE pernr NE pernr.
  DELETE p0001_pernr WHERE pernr NE pernr.

  /sew/cl_mig_utils=>get_hr_periods( EXPORTING table      = p0001_pernr
                                     CHANGING  hr_periods = hr_periods ).

  /sew/cl_mig_utils=>get_hr_periods( EXPORTING table      = p0008_pernr
                                     CHANGING  hr_periods = hr_periods ).

ENDMETHOD.


METHOD constructor.

  DATA: mig_date TYPE datum.

  me->pernr = pernr.
  me->begda = begda.

**JMB20220211 start insert - Only the last 5 years will be migrated
*
  DATA(first_of_year) = CONV datum( sy-datum(4) && '01' && '01' ).

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      date      = first_of_year
      days      = 0
      months    = 0
      signum    = '-'
      years     = 5
    IMPORTING
      calc_date = mig_date.

  me->begda = mig_date.
*JMB20220211 insert end

  me->pn_begda = pn_begda. "JMB20211011 I - C400129651-5882
  me->endda = endda.
  me->cofu = cofu.
  me->cogu = cogu.
  me->cogl = cogl.
  me->molga = molga.

  IF cogl EQ abap_true OR
     cogu EQ abap_true.
    vp_salary_structure = VALUE #( ).
  ELSEIF cofu EQ abap_true.
    vp_salary_structure = VALUE #( ( name = 1  value = /sew/cl_mig_utils=>merge )
                                     ( name = 2  value = salary )
                                     ( name = 3  value = 'AssignmentId(SourceSystemId)' )
                                     ( name = 4  value = 'DateFrom' )
                                     ( name = 5  value = 'DateTo' )
                                     ( name = 6  value = 'SalaryAmount' )
                                     ( name = 7  value = 'MultipleComponents' )
                                     ( name = 8  value = 'AssignmentNumber' )
                                     ( name = 9  value = 'SalaryBasisName' )
                                     ( name = 10 value = 'ActionCode' )
                                     ( name = 16 value = 'SourceSystemId' )
                                     ( name = 17 value = 'SourceSystemOwner' ) ).

    vp_salary_comp_structure = VALUE #( ( name = 1  value = /sew/cl_mig_utils=>merge )
                                        ( name = 2  value = salary_comp )
                                        ( name = 3  value = 'ComponentCode' )
                                        ( name = 4  value = 'Amount' )
                                        ( name = 5  value = 'Percentage' )
                                        ( name = 6  value = 'SalaryDateFrom' )
                                        ( name = 8  value = 'AssignmentNumber' ) ).
  ENDIF.
ENDMETHOD.


METHOD create_metadata.

  DESCRIBE TABLE vp_salary_structure LINES DATA(length).

  LOOP AT vp_salary_structure ASSIGNING FIELD-SYMBOL(<salary_struc>).

    "set METADATA title
    CASE <salary_struc>-name.
      WHEN 1.
        CONCATENATE /sew/cl_mig_utils=>metadata /sew/cl_mig_utils=>separator INTO metadata.
        CONTINUE.
    ENDCASE.

    CONCATENATE metadata <salary_struc>-value INTO metadata.

    "set separator
    CHECK length NE sy-tabix.
    CONCATENATE metadata /sew/cl_mig_utils=>separator INTO metadata.
  ENDLOOP.

  DESCRIBE TABLE vp_salary_comp_structure LINES length.

  LOOP AT vp_salary_comp_structure ASSIGNING FIELD-SYMBOL(<salary_comp_struc>).

    "set METADATA title
    CASE <salary_comp_struc>-name.
      WHEN 1.
        CONCATENATE metadata
                    cl_abap_char_utilities=>newline
                    /sew/cl_mig_utils=>metadata
                    /sew/cl_mig_utils=>separator INTO metadata.
        CONTINUE.
    ENDCASE.

    CONCATENATE metadata <salary_comp_struc>-value INTO metadata.

    "set separator
    CHECK length NE sy-tabix.
    CONCATENATE metadata /sew/cl_mig_utils=>separator INTO metadata.
  ENDLOOP.

ENDMETHOD.


METHOD get_cofu_data.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE @p0008 FROM pa0008 WHERE pernr IN @pernr AND
                                                                       begda LE @endda AND
                                                                       endda GE @begda.

  SELECT pernr,
         begda,
         endda,
         persk INTO CORRESPONDING FIELDS OF TABLE @p0001 FROM pa0001 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.
ENDMETHOD.


METHOD get_mapping_fields.

  "get mapping fields for actioncode
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = '0008'
                                                   sap_field    = 'LGART'
                                                   oracle_field = /sew/cl_mig_utils=>componentcode
                                                   export       = abap_true
                                        IMPORTING mapping_fields = mapping_fields_lgart ).
ENDMETHOD.


METHOD GET_MAPPING_VALUES.

  "get mapping values for actioncode
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = '0008'
                                                   sap_field    = 'LGART'
                                                   oracle_field = /sew/cl_mig_utils=>componentcode
                                                   export       = abap_true
                                        IMPORTING mapping_values = mapping_values_lgart ).
ENDMETHOD.


METHOD map_cofu_data.

  DATA: src_id           TYPE string,
        sys_id           TYPE string,
        salary_base_name TYPE string,
        count            TYPE i,
        betrg            TYPE maxbt,
        ppbwla           TYPE hreg_t_pbwla.

  CONCATENATE /sew/cl_mig_utils=>sap sy-mandt INTO sys_id.

  LOOP AT pernr ASSIGNING FIELD-SYMBOL(<pernr>).
    CLEAR: hr_periods.
    collect_hr_periods( CONV #( <pernr>-low ) ).
    count = 1.

    LOOP AT hr_periods ASSIGNING FIELD-SYMBOL(<period>).
      LOOP AT p0008 ASSIGNING FIELD-SYMBOL(<p0008>) WHERE pernr EQ <pernr>-low    AND
                                                          begda LE <period>-endda AND
                                                          endda GE <period>-begda.
        EXIT.
      ENDLOOP.

      CHECK sy-subrc IS INITIAL.

      LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>) WHERE pernr EQ <pernr>-low    AND
                                                          begda LE <period>-endda AND
                                                          endda GE <period>-begda.
        EXIT.
      ENDLOOP.

      CHECK sy-subrc IS INITIAL.

      CALL FUNCTION 'RP_FILL_WAGE_TYPE_TABLE'
        EXPORTING
          begda                        = <period>-begda
          endda                        = <period>-endda
          pernr                        = <p0008>-pernr
        TABLES
          ppbwla                       = ppbwla
        EXCEPTIONS
          error_at_indirect_evaluation = 1.

      CHECK sy-subrc IS INITIAL.

      CONCATENATE 'E' <p0008>-pernr INTO DATA(assign_number).

      DATA(comp_num) = lines( ppbwla ).
      DATA(multiple) = 'N'.
      IF comp_num GT 1.
        multiple = 'Y'.
      ENDIF.

      "build components structure
      LOOP AT ppbwla ASSIGNING FIELD-SYMBOL(<ppbwla>).
        "For Italy only overall sum code is necessary
        IF '15'           IN molga AND
           <ppbwla>-lgart NE '1250'.
          CONTINUE.
        ENDIF.

        <p0008>-lga01 = <ppbwla>-lgart.

        map_mig_values( EXPORTING p0008 = <p0008>
                        IMPORTING componentcode  = DATA(componentcode) ).

        betrg = betrg + ( <ppbwla>-betrg * 13 ).

        DATA(begda_tmp) = /sew/cl_mig_utils=>convert_date( <ppbwla>-begda ).
        DATA(endda_tmp) = /sew/cl_mig_utils=>convert_date( <ppbwla>-endda ).

        DATA(amount) = CONV string( <ppbwla>-betrg ).

        CONDENSE: amount.

        CONCATENATE /sew/cl_mig_utils=>merge
                    salary_comp
                    componentcode
                    amount
                    '100'
                    begda_tmp
                    assign_number
                    INTO DATA(data_comp_tmp) SEPARATED BY /sew/cl_mig_utils=>separator.
        CONCATENATE data_comp cl_abap_char_utilities=>newline data_comp_tmp INTO data_comp.

        CASE sy-mandt.
          WHEN /sew/cl_int_constants=>cofu_mandant-netherlands.

            salary_base_name = SWITCH #( <p0001>-persk
                                         WHEN '10' THEN 'NL_Employee'
                                         WHEN '12' THEN 'NL_Trainee/Stagiaires' ).

            salary_base_name = SWITCH #( <p0001>-pernr
                                         WHEN '00200307' THEN 'NL_Director'
                                         ELSE salary_base_name ).

            IF sy-sysid EQ 'D02'.
              salary_base_name = 'NL_Employee_New1'.
            ENDIF.

          WHEN /sew/cl_int_constants=>cofu_mandant-austria.
            IF '03' IN molga.
              salary_base_name = SWITCH #( <p0001>-persk
                                            WHEN '1B' THEN 'AT_Wage_New'
                                            WHEN 'A3' THEN 'AT_Pension'
                                            ELSE 'AT_Salary' ).
            ELSEIF '15' IN molga.
              salary_base_name = 'IT_Salary'.
            ENDIF.

        ENDCASE.
      ENDLOOP.

      begda_tmp = /sew/cl_mig_utils=>convert_date( <period>-begda ).
      endda_tmp = /sew/cl_mig_utils=>convert_date( <period>-endda ).

      DATA(count_s) = CONV string( count ).
      CONDENSE count_s.
      src_id = salary_base_name && '_' && assign_number && '_' && count_s.
      DATA(sum) = CONV string( betrg ).
      CONDENSE sum.

      CONCATENATE /sew/cl_mig_utils=>merge
                  salary
                  ''
                  begda_tmp
                  endda_tmp
                  sum
                  multiple
                  assign_number
                  salary_base_name
                  'SEW_MIGRATION' "JMB20220311 D - 'HIRE'
                  src_id
                  sys_id
      INTO DATA(data_tmp) SEPARATED BY /sew/cl_mig_utils=>separator.
      CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
      CLEAR: sum, betrg.
      count = count + 1.

    ENDLOOP.
  ENDLOOP.
ENDMETHOD.


METHOD map_mig_values.
  DATA: value_tmp TYPE /sew/dd_value.

  value_tmp = p0008-lga01.
  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = '0008'
      field_sap      = 'LGART'
      field_oracle   = /sew/cl_mig_utils=>componentcode
      mapping_fields = CONV #( mapping_fields_lgart )
      mapping_values = CONV #( mapping_values_lgart )
    CHANGING
      value          = value_tmp ).

  componentcode = value_tmp.

**JMB20211207 start insert - due to configuration issues in DEV1 use own componentCode
*
  IF sy-sysid EQ 'D02' AND
     sy-mandt EQ /sew/cl_int_constants=>cofu_mandant-netherlands.
    componentcode = '1000-Salary'.
  ENDIF.
*JMB20211207 insert end
ENDMETHOD.


METHOD proceed_cofu_salary.
  DATA: data_comp TYPE string.

  get_cofu_data( ).
  get_mapping_fields( ).
  get_mapping_values( ).

  data = map_cofu_data( EXPORTING vp_src_id = vp_src_id
                        IMPORTING data_comp = data_comp ).
  CONCATENATE data
              cl_abap_char_utilities=>newline
              data_comp
              INTO data.
ENDMETHOD.
ENDCLASS.
