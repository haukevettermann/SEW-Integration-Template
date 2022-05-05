class /SEW/CL_MIG_WORK_TERMS definition
  public
  create public .

public section.

  types:
    BEGIN OF vp_wterm_id ,
            pernr TYPE pernr ,
            begda TYPE begda ,
            endda TYPE endda ,
            wterm_id TYPE string,
    END OF vp_wterm_id .
  types:
    vp_wterm_id_t type STANDARD TABLE OF vp_wterm_id .

  data PERNR type RSDSSELOPT_T .
  data BEGDA type BEGDA .
  data ENDDA type ENDDA .
  data COFU type BOOLEAN .
  data COGL type BOOLEAN .
  data P0001 type P0001_TAB .
  data MOLGA type RSDSSELOPT_T .
  data ASSIGNMENT_STRUCTURE type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  data VP_WTERM_COL type /SEW/CL_MIG_WORK_TERMS=>VP_WTERM_ID_T .
  constants WORKTERMS type STRING value 'WorkTerms' ##NO_TEXT.
  data P0000 type P0000_TAB .
  constants WT type STRING value 'WT_' ##NO_TEXT.
  constants ET type STRING value 'ET' ##NO_TEXT.
  data P0105 type P0105_TB .
  constants STAT2 type /SEW/DD_FIELD value 'STAT2' ##NO_TEXT.
  constants ASSIGNMENTSTATUSTYPECODE type /SEW/DD_FIELD value 'ASSIGNMENTSTATUSTYPECODE' ##NO_TEXT.
  constants ASSIGNMENTTYPE type /SEW/DD_FIELD value 'ASSIGNMENTTYPE' ##NO_TEXT.
  constants WORKERCATEGORY type /SEW/DD_FIELD value 'WORKERCATEGORY' ##NO_TEXT.
  constants ASSIGNMENTCATEGORY type /SEW/DD_FIELD value 'ASSIGNMENTCATEGORY' ##NO_TEXT.
  constants PERSONTYPECODE type /SEW/DD_FIELD value 'PERSONTYPECODE' ##NO_TEXT.
  constants SYSTEMPERSONTYPE type /SEW/DD_FIELD value 'SYSTEMPERSONTYPE' ##NO_TEXT.
  constants PERSK type /SEW/DD_FIELD value 'PERSK' ##NO_TEXT.
  constants ASSIGNMENTPERSONTYPE type /SEW/DD_FIELD value 'ASSIGNMENTPERSONTYPE' ##NO_TEXT.
  data P0016 type P0016_TAB .

  methods CREATE_HIRE_COFU_ENTRY
    importing
      !PERIOD type HRPERIODS
      !PERNR type PERNR_D
    returning
      value(DATA) type STRING .
  methods PROCEED_COGU_WORK_TERMS
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !VP_WKR_ID type /SEW/CL_MIG_WORK_RELATION=>VP_WKR_ID_T
      !WORKER type ref to /SEW/CL_MIG_WORKER
    exporting
      !VP_WTERM_ID type /SEW/CL_MIG_WORK_TERMS=>VP_WTERM_ID_T
      !DATA_CONTRACT type STRING
    returning
      value(DATA) type STRING .
  methods PROCEED_COGL_WORK_TERMS
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !VP_WKR_ID type /SEW/CL_MIG_WORK_RELATION=>VP_WKR_ID_T
      !WORKER type ref to /SEW/CL_MIG_WORKER
    exporting
      !VP_WTERM_ID type /SEW/CL_MIG_WORK_TERMS=>VP_WTERM_ID_T
    returning
      value(DATA) type STRING .
  methods PROCEED_COFU_WORK_TERMS
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !VP_WKR_ID type /SEW/CL_MIG_WORK_RELATION=>VP_WKR_ID_T
      !WORKER type ref to /SEW/CL_MIG_WORKER
      !ASSIGNMENT type ref to /SEW/CL_MIG_ASSIGNMENT
    exporting
      !VP_WTERM_ID type /SEW/CL_MIG_WORK_TERMS=>VP_WTERM_ID_T
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
  methods MAP_COFU_DATA
    importing
      !PERNR type PERNR_D
      !PERIODS type HRPERIODS_TAB
      !MIG_DATE type DATUM optional
    returning
      value(DATA) type STRING .
  methods CREATE_METADATA
    returning
      value(METADATA) type STRING .
  PROTECTED SECTION.
private section.

  data MAPPING_FIELDS_PERSG_WT type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPP_FI .
  data MAPPING_VALUES_PERSG_WT type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPPING .
  data MAPPING_FIELDS_MASSN type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPP_FI .
  data MAPPING_VALUES_MASSN type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPPING .
  data MAPPING_FIELDS_MASSG type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPP_FI .
  data MAPPING_VALUES_MASSG type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPPING .
  data MAPPING_FIELDS_STAT2 type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPP_FI .
  data MAPPING_VALUES_STAT2 type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPPING .
  data MAPPING_FIELDS_PERSG_AT type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPP_FI .
  data MAPPING_VALUES_PERSG_AT type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPPING .
  data MAPPING_FIELDS_PERSK_WC type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPP_FI .
  data MAPPING_VALUES_PERSK_WC type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPPING .
  data MAPPING_FIELDS_PERSK_AC type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPP_FI .
  data MAPPING_VALUES_PERSK_AC type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPPING .
  data MAPPING_FIELDS_PERSK_PTC type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPP_FI .
  data MAPPING_VALUES_PERSK_PTC type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPPING .
  data MAPPING_FIELDS_PERSK_SPT type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPP_FI .
  data MAPPING_VALUES_PERSK_SPT type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPPING .
  data HRP1000_ORGEH type HRTNM_P1000_TAB .
  data BUKRS_TXT type T_T001 .
  data HRP1000_STELL type HRTNM_P1000_TAB .
  data MAPPING_FIELDS_BTRTL type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPP_FI .
  data MAPPING_VALUES_BTRTL type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPPING .
  data BU_COUNTRY type PIQ_T005T_T .
  data MAPPING_FIELDS_BUKRS type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPP_FI .
  data MAPPING_VALUES_BUKRS type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPPING .
  data VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  data VP_WKR_ID type /SEW/CL_MIG_WORK_RELATION=>VP_WKR_ID_T .
  data PERNR_HISTORY type RSDSSELOPT_T .
  data MAPPING_FIELDS_BUKRS_BU type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPP_FI .
  data MAPPING_VALUES_BUKRS_BU type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPPING .
  data MAPPING_FIELDS_DEPARTMENT type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPP_FI .
  data MAPPING_VALUES_DEPARTMENT type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPPING .
  data COGU type BOOLEAN .
  data MAPPING_FIELDS_PERSK_APT type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPP_FI .
  data MAPPING_VALUES_PERSK_APT type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPPING .
  data DATA_CONTRACT type STRING .
  data MAPPING_FIELDS_JOB type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPP_FI .
  data MAPPING_VALUES_JOB type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPPING .
  data MAPPING_FIELDS_PRBEH type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPP_FI .
  data MAPPING_VALUES_PRBEH type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPPING .
  data MAPPING_FIELDS_KDGF2 type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPP_FI .
  data MAPPING_VALUES_KDGF2 type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPPING .

  methods CREATE_HIRE_COGU_ENTRY
    returning
      value(DATA) type STRING .
  methods CREATE_HIRE_ENTRY
    returning
      value(DATA) type STRING .
  methods GET_BUSINESS_CODE
    importing
      !T001 type T001
      !ENDDA type ENDDA
    returning
      value(BUSINESS_CODE) type STRING .
  methods MAP_MIG_VALUES
    importing
      !P0001 type P0001
      !P0000 type P0000
      !HIRE type BOOLEAN optional
    exporting
      !MASSN type /SEW/DD_VALUE
      !MASSG type /SEW/DD_VALUE
      !WORKER_TYPE type /SEW/DD_VALUE
      !ASSIGNMENT_TYPE type /SEW/DD_VALUE
      !PERSON_TYPE_CODE type /SEW/DD_VALUE
      !SYSTEM_PERSON_TYPE type /SEW/DD_VALUE
      !ASSIGN_STATUS type /SEW/DD_VALUE
      !WORKER_CATEGORY type /SEW/DD_VALUE
      !ASSIGNMENT_CATEGORY type /SEW/DD_VALUE
      !LOCATION_CODE type /SEW/DD_VALUE
      !LEGAL_EMPLOYER_NAME type /SEW/DD_VALUE
      !BUSINESS_UNIT_CODE type /SEW/DD_VALUE
      !DEPARTMENT_NAME type /SEW/DD_VALUE .
  methods MAP_MIG_COGU_VALUES
    importing
      !P0001 type P0001
      !P0000 type P0000
      !HIRE type BOOLEAN optional
      !JOB type STRING
    exporting
      !MASSN type /SEW/DD_VALUE
      !MASSG type /SEW/DD_VALUE
      !WORKER_TYPE type /SEW/DD_VALUE
      !ASSIGNMENT_TYPE type /SEW/DD_VALUE
      !PERSON_TYPE_CODE type /SEW/DD_VALUE
      !SYSTEM_PERSON_TYPE type /SEW/DD_VALUE
      !ASSIGN_STATUS type /SEW/DD_VALUE
      !WORKER_CATEGORY type /SEW/DD_VALUE
      !ASSIGNMENT_CATEGORY type /SEW/DD_VALUE
      !LOCATION_CODE type /SEW/DD_VALUE
      !LEGAL_EMPLOYER_NAME type /SEW/DD_VALUE
      !BUSINESS_UNIT_CODE type /SEW/DD_VALUE
      !DEPARTMENT_NAME type /SEW/DD_VALUE
      !ASSIGNMENT_PERSON_TYPE_CODE type /SEW/DD_VALUE
      !JOB_CODE type /SEW/DD_VALUE .
  methods MAP_MIG_COFU_VALUES
    importing
      !P0001 type P0001
      !P0000 type P0000
      !HIRE type BOOLEAN optional
      !JOB type STRING
      !P0016 type P0016
    exporting
      !MASSN type /SEW/DD_VALUE
      !MASSG type /SEW/DD_VALUE
      !WORKER_TYPE type /SEW/DD_VALUE
      !ASSIGNMENT_TYPE type /SEW/DD_VALUE
      !PERSON_TYPE_CODE type /SEW/DD_VALUE
      !SYSTEM_PERSON_TYPE type /SEW/DD_VALUE
      !ASSIGN_STATUS type /SEW/DD_VALUE
      !WORKER_CATEGORY type /SEW/DD_VALUE
      !ASSIGNMENT_CATEGORY type /SEW/DD_VALUE
      !LOCATION_CODE type /SEW/DD_VALUE
      !LEGAL_EMPLOYER_NAME type /SEW/DD_VALUE
      !BUSINESS_UNIT_CODE type /SEW/DD_VALUE
      !DEPARTMENT_NAME type /SEW/DD_VALUE
      !ASSIGNMENT_PERSON_TYPE_CODE type /SEW/DD_VALUE
      !JOB_CODE type /SEW/DD_VALUE
      !PROBATIONUNIT type /SEW/DD_VALUE
      !NOTICEUOM type /SEW/DD_VALUE .
  methods GET_MAPPING_VALUES .
  methods GET_MAPPING_FIELDS .
  methods GET_MAPPING_COGU_VALUES .
  methods GET_MAPPING_COGU_FIELDS .
  methods GET_MAPPING_COFU_VALUES .
  methods GET_MAPPING_COFU_FIELDS .
  methods GET_COGL_DATA .
  methods GET_COFU_DATA .
  methods MAP_COGL_DATA
    returning
      value(DATA) type STRING .
  methods MAP_COGU_DATA
    returning
      value(DATA) type STRING .
ENDCLASS.



CLASS /SEW/CL_MIG_WORK_TERMS IMPLEMENTATION.


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
      assignment_structure = VALUE #( ( name = 1  value = /sew/cl_mig_utils=>merge )
                                      ( name = 2  value = workterms )
                                      ( name = 3  value = 'SourceSystemOwner' )
                                      ( name = 4  value = 'SourceSystemId' )
                                      ( name = 5  value = 'PersonId(SourceSystemId)' )
                                      ( name = 6  value = 'ActionCode' )
                                      ( name = 7  value = 'EffectiveStartDate' )
                                      ( name = 8  value = 'EffectiveEndDate' )
                                      ( name = 9  value = 'EffectiveSequence' )
                                      ( name = 10 value = 'EffectiveLatestChange' )
                                      ( name = 11 value = 'AssignmentName' )
                                      ( name = 12 value = 'AssignmentNumber' )
                                      ( name = 13 value = 'AssignmentStatusTypeCode' )
                                      ( name = 15 value = 'AssignmentType' )
                                      ( name = 16 value = 'BusinessUnitShortCode' )
                                      ( name = 17 value = 'WorkerCategory' )
                                      ( name = 18 value = 'AssignmentCategory' )
                                      ( name = 19 value = 'JobCode' )
                                      ( name = 20 value = 'LocationCode' )
                                      ( name = 21 value = 'ManagerFlag' )
                                      ( name = 22 value = 'DepartmentName' )
                                      ( name = 23 value = 'PeriodOfServiceId(SourceSystemId)' )
                                      ( name = 24 value = 'DateStart' )
                                      ( name = 25 value = 'WorkerType' )
                                      ( name = 32 value = 'LegalEmployerName' )
                                      ( name = 26 value = 'PersonTypeCode' )
                                      ( name = 27 value = 'SystemPersonType' )
                                      ( name = 30 value = 'ReasonCode' )
                                      ( name = 35 value = 'InternalOfficeNumber' ) ).

    ELSEIF cofu EQ abap_true.
      assignment_structure = VALUE #( ( name = 1  value = /sew/cl_mig_utils=>merge )
                                      ( name = 2  value = workterms )
                                      ( name = 3  value = 'SourceSystemOwner' )
                                      ( name = 4  value = 'SourceSystemId' )
                                      ( name = 5  value = 'PersonId(SourceSystemId)' )
                                      ( name = 6  value = 'ActionCode' )
                                      ( name = 7  value = 'EffectiveStartDate' )
                                      ( name = 8  value = 'EffectiveEndDate' )
                                      ( name = 9  value = 'EffectiveSequence' )
                                      ( name = 10 value = 'EffectiveLatestChange' )
                                      ( name = 11 value = 'AssignmentName' )
                                      ( name = 12 value = 'AssignmentNumber' )
                                      ( name = 13 value = 'AssignmentStatusTypeCode' )
                                      ( name = 15 value = 'AssignmentType' )
                                      ( name = 16 value = 'BusinessUnitShortCode' )
                                      ( name = 17 value = 'WorkerCategory' )
                                      ( name = 18 value = 'AssignmentCategory' )
                                      ( name = 19 value = 'JobCode' )
                                      ( name = 20 value = 'LocationCode' )
                                      ( name = 21 value = 'ManagerFlag' )
                                      ( name = 22 value = 'DepartmentName' )
                                      ( name = 23 value = 'PeriodOfServiceId(SourceSystemId)' )
                                      ( name = 24 value = 'DateStart' )
                                      ( name = 25 value = 'WorkerType' )
                                      ( name = 32 value = 'LegalEmployerName' )
                                      ( name = 26 value = 'PersonTypeCode' )
                                      ( name = 27 value = 'SystemPersonType' )
                                      ( name = 30 value = 'ReasonCode' )
                                      ( name = 31 value = 'InternalOfficeNumber' )
                                      ( name = 34 value = 'DateProbationEnd' )
                                      ( name = 35 value = 'ReportingEstablishment' )
                                      ( name = 36 value = 'GradeCode' )
                                      ( name = 39 value = 'PositionCode' )
                                      ( name = 40 value = 'ProbationPeriod' )
                                      ( name = 41 value = 'ProbationUnit' )  ).
    ENDIF.
  ENDMETHOD.


METHOD create_hire_cofu_entry.

  DATA: plvar           TYPE plvar,
        job_short       TYPE string,
        internal_number TYPE string,
        department_name TYPE string,
        period_of_ser   TYPE string,
        dateprob_end    TYPE string,
        rpt_est         TYPE string,
        grade_code      TYPE string,
        prob_unit       TYPE string,
        notice_per      TYPE string,
        notice_per_uom  TYPE string,
        pos_code        TYPE string,
        prob_period     TYPE string,
        normalhours     TYPE string.

  "collect all hired employees
  APPEND VALUE #( sign = 'I' option = 'EQ' low = pernr ) TO pernr_history.
  CONCATENATE /sew/cl_mig_utils=>sap sy-mandt INTO DATA(sys_id).

  "get active plvar
  CALL FUNCTION 'RH_GET_PLVAR'
    IMPORTING
      plvar = plvar.

  "get relevant P0000
  LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000>) WHERE begda LE period-endda AND
                                                      endda GE period-begda AND
                                                      pernr EQ pernr.
    EXIT.
  ENDLOOP.

  CHECK sy-subrc EQ 0.

  "get actual P0001
  LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001_actual>) WHERE begda LE sy-datum AND
                                                      endda GE sy-datum AND
                                                      pernr EQ pernr.
    EXIT.
  ENDLOOP.

  CHECK sy-subrc EQ 0.

  "get relevant P0001
  LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>) WHERE begda LE period-endda AND
                                                      endda GE period-begda AND
                                                      pernr EQ pernr.
    "Decision 06.08.2021: pass only actual legal employer due to import problems in Oracle
    <p0001>-bukrs = <p0001_actual>-bukrs. "JMB20210806 I

    "pass only actual worker/assignment type due to worker type in work relationship
    <p0001>-persg = <p0001_actual>-persg.
    <p0001>-persk = <p0001_actual>-persk.

    "get job text
    LOOP AT hrp1000_stell ASSIGNING FIELD-SYMBOL(<stell>) WHERE objid EQ <p0001>-stell AND
                                                                begda LE <p0001>-endda AND
                                                                endda GE <p0001>-begda.
      job_short = <stell>-short.
      EXIT.
    ENDLOOP.

    EXIT.
  ENDLOOP.

  CHECK sy-subrc EQ 0.

  "get P0016
  LOOP AT p0016 INTO DATA(p0016_entry) WHERE begda LE period-endda AND
                                             endda GE period-begda AND
                                             pernr EQ pernr.
    EXIT.
  ENDLOOP.

  map_mig_cofu_values( EXPORTING p0001 = <p0001>
                                 p0000 = <p0000>
                                 p0016 = p0016_entry
                                 hire  = abap_true
                                 job   = job_short
                       IMPORTING massn               = DATA(massn)
                                 massg               = DATA(massg)
                                 worker_type         = DATA(worker_type)
                                 assignment_type     = DATA(assignment_type)
                                 person_type_code    = DATA(person_type_code)
                                 system_person_type  = DATA(system_person_type)
                                 assign_status       = DATA(assign_status)
                                 worker_category     = DATA(worker_category)
                                 assignment_person_type_code = DATA(assignment_person_type_code)
                                 assignment_category = DATA(assignment_category)
                                 location_code       = DATA(location_code)
                                 legal_employer_name = DATA(legal_employer_name)
                                 business_unit_code  = DATA(business_code)
                                 job_code            = DATA(job) ).

  "in case of work terms assigment type -> add T in case of E and C
  assignment_type = SWITCH #( assignment_type
                              WHEN 'E' OR 'C' THEN assignment_type && 'T' ).

  DATA(begda_tmp) = /sew/cl_mig_utils=>convert_date( period-begda ).
  DATA(endda_tmp) = /sew/cl_mig_utils=>convert_date( period-endda ).

  CONCATENATE wt pernr INTO DATA(src_id).
  CONCATENATE et pernr  INTO DATA(assign_num).

  "get source id
  DATA(src_sys_id) = /sew/cl_mig_utils=>get_src_id( pernr = pernr
                                                    begda = period-begda
                                                    endda = period-endda
                                                    vp_src_id = vp_src_id ).

  DATA(is_manager) = /sew/cl_mig_utils=>is_manager( <p0001> ).

  "store work terms for assignment entity
  APPEND VALUE #( pernr = pernr
                  begda = period-begda
                  endda = period-endda
                  wterm_id = src_id ) TO vp_wterm_col.

  IF business_code NE /sew/cl_mig_utils=>default_business_unit.
    "get orgunit text
    LOOP AT hrp1000_orgeh ASSIGNING FIELD-SYMBOL(<hrp1000>) WHERE objid EQ <p0001>-orgeh AND
                                                                  begda LE <p0001>-endda AND
                                                                  endda GE <p0001>-begda.
      DATA(stext_length) = strlen( <hrp1000>-stext ) - 2.
      CHECK <hrp1000>-stext+stext_length(2) NE business_code+0(2).
      CONCATENATE <hrp1000>-stext '-' business_code+0(2) INTO department_name.
      EXIT.
    ENDLOOP.
  ENDIF.

  "Get relevant work relationship
  LOOP AT vp_wkr_id ASSIGNING FIELD-SYMBOL(<vp_wkr_id>) WHERE pernr EQ pernr        AND
                                                              begda LE period-endda AND
                                                              endda GE period-begda.
    period_of_ser = <vp_wkr_id>-wkr_id.
    EXIT.
  ENDLOOP.

**JMB20210928 start insert - set room number in hire dummy entry
*
  "get P0105
  LOOP AT p0105 ASSIGNING FIELD-SYMBOL(<p0105>) WHERE pernr EQ pernr        AND
                                                      begda LE period-endda AND
                                                      endda GE period-begda.
    internal_number = <p0105>-usrid.
    EXIT.
  ENDLOOP.
*JMB20210928 insert end

  CONCATENATE /sew/cl_mig_utils=>merge
              workterms
              sys_id
              src_id
              src_sys_id
              massn
              begda_tmp
              endda_tmp
              '1'
              /sew/cl_mig_utils=>yes
              job
              assign_num
              assign_status
              assignment_type
              business_code
              worker_category
              assignment_category
              '#NULL' "JobCode
              location_code
              is_manager
              '#NULL' "department_name
              period_of_ser
              begda_tmp
              worker_type
              legal_employer_name
              person_type_code
              system_person_type
              massg
              internal_number
              dateprob_end
              rpt_est
              '#NULL' "grade_code
              pos_code
              prob_period
              prob_unit
  INTO DATA(data_tmp) SEPARATED BY /sew/cl_mig_utils=>separator.

  CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
  CLEAR: internal_number, job, department_name.

**JMB20210802 start insert - build contract information
*
*    /sew/cl_mig_contract=>map_cogl_data( EXPORTING pernr                = <p0001>-pernr
*                                                   begda                = begda_tmp
*                                                   assignment_source_id = src_id
*                                                   source_system_owner  = sys_id
*                                                   person_source_id     = src_sys_id
*                                          CHANGING contract             = data_contract ).
*JMB20210802 end insert
ENDMETHOD.


METHOD create_hire_cogu_entry.

  DATA: plvar           TYPE plvar,
        job_short       TYPE string,
        internal_number TYPE string,
        department_name TYPE string,
        period_of_ser   TYPE string.

    DATA(massn_term) = SWITCH rsdsselopt_t( sy-mandt
                                          WHEN /sew/cl_int_constants=>cofu_mandant-germany     THEN VALUE #( ( sign = 'I' option = 'EQ' low = '10' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '11' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '13' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '18' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '35' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '99' )
                                                                                                             ( sign = 'I' option = 'EQ' low = 'ZZ' ) )
                                          WHEN /sew/cl_int_constants=>cofu_mandant-france      THEN VALUE #( ( sign = 'I' option = 'EQ' low = '09' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '10' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '11' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '18' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '32' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '34' ) )
                                          WHEN /sew/cl_int_constants=>cofu_mandant-netherlands THEN VALUE #( ( sign = 'I' option = 'EQ' low = 'Z4' ) )
                                          WHEN /sew/cl_int_constants=>cofu_mandant-australia   THEN VALUE #( ( sign = 'I' option = 'EQ' low = '10' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '20' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '25' ) )
                                          WHEN /sew/cl_int_constants=>cofu_mandant-newzealand  THEN VALUE #( ( sign = 'I' option = 'EQ' low = '25' ) )
                                          "default Italy/Austria
                                          ELSE VALUE #( ( sign = 'I' option = 'EQ' low = '03' )
                                                        ( sign = 'I' option = 'EQ' low = 'ZZ' ) ) ).

  "get history assignment
  DATA(p0001_history) = p0001.

  DELETE p0001_history WHERE begda GT sy-datum.
  DELETE p0001_history WHERE begda LE sy-datum AND
                             endda GE sy-datum.

  "delete history from main table
  LOOP AT p0001_history ASSIGNING FIELD-SYMBOL(<p0001_historxy>).
    DELETE TABLE p0001 FROM <p0001_historxy>.
  ENDLOOP.

  "only first assignment relevant
  SORT p0001_history BY pernr begda ASCENDING.
  DELETE ADJACENT DUPLICATES FROM p0001_history COMPARING pernr.

  "get actual assignment
  DATA(p0001_actual) = p0001.
  DELETE p0001_actual WHERE endda LT sy-datum OR
                            begda GT sy-datum.

  "get active plvar
  CALL FUNCTION 'RH_GET_PLVAR'
    IMPORTING
      plvar = plvar.

  CONCATENATE /sew/cl_mig_utils=>sap sy-mandt INTO DATA(sys_id).

  LOOP AT p0001_history ASSIGNING FIELD-SYMBOL(<p0001>).
    "collect all hired employees
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0001>-pernr ) TO pernr_history.

    "get relevant P0000
    LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000>) WHERE begda LE <p0001>-endda AND
                                                        endda GE <p0001>-begda AND
                                                        pernr EQ <p0001>-pernr.
      EXIT.
    ENDLOOP.

    CHECK sy-subrc EQ 0.

    "get actual assignment and set enddate of history entry to actual begda - 1
    LOOP AT p0001_actual ASSIGNING FIELD-SYMBOL(<p0001_actual>) WHERE pernr EQ <p0001>-pernr.
      <p0001>-endda = <p0001_actual>-begda - 1.

      "Decision 06.08.2021: pass only actual legal employer due to import problems in Oracle
      <p0001>-bukrs = <p0001_actual>-bukrs. "JMB20210806 I

      "pass only actual worker/assignment type due to worker type in work relationship
      <p0001>-persg = <p0001_actual>-persg.
      <p0001>-persk = <p0001_actual>-persk.
      EXIT.
    ENDLOOP.

    CHECK sy-subrc EQ 0.

    "in case of termination ignore entry
*    CHECK <p0000>-massn NOT IN massn_term. "JMB20210312 D: Provide entry even if it´s provided in WorkRelationship

    "get job text
    LOOP AT hrp1000_stell ASSIGNING FIELD-SYMBOL(<stell>) WHERE objid EQ <p0001>-stell AND
                                                                begda LE <p0001>-endda AND
                                                                endda GE <p0001>-begda.
      job_short = <stell>-short.
      EXIT.
    ENDLOOP.

    map_mig_cogu_values( EXPORTING p0001 = <p0001>
                                   p0000 = <p0000>
                                   hire  = abap_true
                                   job   = job_short
                         IMPORTING massn               = DATA(massn)
                                   massg               = DATA(massg)
                                   worker_type         = DATA(worker_type)
                                   assignment_type     = DATA(assignment_type)
                                   person_type_code    = DATA(person_type_code)
                                   system_person_type  = DATA(system_person_type)
                                   assign_status       = DATA(assign_status)
                                   worker_category     = DATA(worker_category)
                                   assignment_person_type_code = DATA(assignment_person_type_code)
                                   assignment_category = DATA(assignment_category)
                                   location_code       = DATA(location_code)
                                   legal_employer_name = DATA(legal_employer_name)
                                   business_unit_code  = DATA(business_code)
                                   job_code            = DATA(job) ).

    "in case of work terms assigment type -> add T in case of E and C
    CASE assignment_type.
      WHEN 'E' OR 'C'.
        CONCATENATE assignment_type 'T' INTO assignment_type.
    ENDCASE.

    DATA(begda_tmp) = /sew/cl_mig_utils=>convert_date( <p0001>-begda ).
    DATA(endda_tmp) = /sew/cl_mig_utils=>convert_date( <p0001>-endda ).
    CONCATENATE wt <p0001>-pernr INTO DATA(src_id).
    CONCATENATE et <p0001>-pernr  INTO DATA(assign_num).

    "get source id
    DATA(src_sys_id) = /sew/cl_mig_utils=>get_src_id( pernr = <p0001>-pernr
                                                      begda = <p0001>-begda
                                                      endda = <p0001>-endda
                                                      vp_src_id = vp_src_id ).

    DATA(is_manager) = /sew/cl_mig_utils=>is_manager( <p0001> ).

    "store work terms for assignment entity
    APPEND VALUE #( pernr  = <p0001>-pernr
                    begda  = <p0001>-begda
                    endda  = <p0001>-endda
                    wterm_id = src_id ) TO vp_wterm_col.

    IF business_code NE /sew/cl_mig_utils=>default_business_unit.
      "get orgunit text
      LOOP AT hrp1000_orgeh ASSIGNING FIELD-SYMBOL(<hrp1000>) WHERE objid EQ <p0001>-orgeh AND
                                                                    begda LE <p0001>-endda AND
                                                                    endda GE <p0001>-begda.
        DATA(stext_length) = strlen( <hrp1000>-stext ) - 2.
        CHECK <hrp1000>-stext+stext_length(2) NE business_code+0(2).
        CONCATENATE <hrp1000>-stext '-' business_code+0(2) INTO department_name.
        EXIT.
      ENDLOOP.

    ENDIF.

    "Get relevant work relationship
    LOOP AT vp_wkr_id ASSIGNING FIELD-SYMBOL(<vp_wkr_id>) WHERE pernr EQ <p0001>-pernr AND
                                                                begda LE <p0001>-endda AND
                                                                endda GE <p0001>-begda.
      period_of_ser = <vp_wkr_id>-wkr_id.
      EXIT.
    ENDLOOP.

**JMB20210928 start insert - set room number in hire dummy entry
*
    "get P0105
    LOOP AT p0105 ASSIGNING FIELD-SYMBOL(<p0105>) WHERE begda LE <p0001>-endda AND
                                                        endda GE <p0001>-begda AND
                                                        pernr EQ <p0001>-pernr.
      internal_number = <p0105>-usrid.
      EXIT.
    ENDLOOP.
*JMB20210928 insert end

    CONCATENATE /sew/cl_mig_utils=>merge
                workterms
                sys_id
                src_id
                src_sys_id
                massn
                begda_tmp
                endda_tmp
                '1'
                /sew/cl_mig_utils=>yes
                job
                assign_num
                assign_status
                assignment_type
                business_code
                worker_category
                assignment_category
                ''
                location_code
                is_manager
                department_name
                period_of_ser
                begda_tmp
                worker_type
                legal_employer_name
                person_type_code
                system_person_type
                massg
                internal_number
    INTO DATA(data_tmp) SEPARATED BY /sew/cl_mig_utils=>separator.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
    CLEAR: internal_number, job, department_name.

**JMB20210802 start insert - build contract information
*
    /sew/cl_mig_contract=>map_cogl_data( EXPORTING pernr                = <p0001>-pernr
                                                   begda                = begda_tmp
                                                   assignment_source_id = src_id
                                                   source_system_owner  = sys_id
                                                   person_source_id     = src_sys_id
                                          CHANGING contract             = data_contract ).
*JMB20210802 end insert

  ENDLOOP.
ENDMETHOD.


METHOD create_hire_entry.

  DATA: plvar           TYPE plvar,
        job             TYPE string,
        internal_number TYPE string,
        department_name TYPE string,
        period_of_ser   TYPE string.

  "get history assignment
  DATA(p0001_history) = p0001.

  DELETE p0001_history WHERE begda GT sy-datum.
  DELETE p0001_history WHERE begda LE sy-datum AND
                             endda GE sy-datum.

  "delete history from main table
  LOOP AT p0001_history ASSIGNING FIELD-SYMBOL(<p0001_historxy>).
    DELETE TABLE p0001 FROM <p0001_historxy>.
  ENDLOOP.

  "only first assignment relevant
  SORT p0001_history BY pernr begda ASCENDING.
  DELETE ADJACENT DUPLICATES FROM p0001_history COMPARING pernr.

  "get actual assignment
  DATA(p0001_actual) = p0001.
  DELETE p0001_actual WHERE endda LT sy-datum OR
                            begda GT sy-datum.

  DATA(massn_term) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '03' ) ).

  "get active plvar
  CALL FUNCTION 'RH_GET_PLVAR'
    IMPORTING
      plvar = plvar.

  CONCATENATE /sew/cl_mig_utils=>sap sy-mandt INTO DATA(sys_id).

  LOOP AT p0001_history ASSIGNING FIELD-SYMBOL(<p0001>).
    "collect all hired employees
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0001>-pernr ) TO pernr_history.

    "get relevant P0000
    LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000>) WHERE begda LE <p0001>-endda AND
                                                        endda GE <p0001>-begda AND
                                                        pernr EQ <p0001>-pernr.
      EXIT.
    ENDLOOP.

    CHECK sy-subrc EQ 0.

    "get actual assignment and set enddate of history entry to actual begda - 1
    LOOP AT p0001_actual ASSIGNING FIELD-SYMBOL(<p0001_actual>) WHERE pernr EQ <p0001>-pernr.
      <p0001>-endda = <p0001_actual>-begda - 1.

      "pass only actual worker/assignment type due to worker type in work relationship
      <p0001>-persg = <p0001_actual>-persg.
      <p0001>-persk = <p0001_actual>-persk.
      EXIT.
    ENDLOOP.

    CHECK sy-subrc EQ 0.

    "in case of termination ignore entry
*    CHECK <p0000>-massn NOT IN massn_term. "JMB20210312 D: Provide entry even if it´s provided in WorkRelationship

    map_mig_values( EXPORTING p0001 = <p0001>
                              p0000 = <p0000>
                              hire  = abap_true
                    IMPORTING massn               = DATA(massn)
                              massg               = DATA(massg)
                              worker_type         = DATA(worker_type)
                              assignment_type     = DATA(assignment_type)
                              person_type_code    = DATA(person_type_code)
                              system_person_type  = DATA(system_person_type)
                              assign_status       = DATA(assign_status)
                              worker_category     = DATA(worker_category)
                              assignment_category = DATA(assignment_category)
                              location_code       = DATA(location_code)
                              legal_employer_name = DATA(legal_employer_name)
                              business_unit_code  = DATA(business_code) ).

    "in case of work terms assigment type -> add T in case of E and C
    CASE assignment_type.
      WHEN 'E' OR 'C'.
        CONCATENATE assignment_type 'T' INTO assignment_type.
    ENDCASE.

    DATA(begda_tmp) = /sew/cl_mig_utils=>convert_date( <p0001>-begda ).
    DATA(endda_tmp) = /sew/cl_mig_utils=>convert_date( <p0001>-endda ).
    CONCATENATE wt <p0001>-pernr INTO DATA(src_id).
    CONCATENATE et <p0001>-pernr  INTO DATA(assign_num).

    "get source id
    DATA(src_sys_id) = /sew/cl_mig_utils=>get_src_id( pernr = <p0001>-pernr
                                                      begda = <p0001>-begda
                                                      endda = <p0001>-endda
                                                      vp_src_id = vp_src_id ).

    DATA(is_manager) = /sew/cl_mig_utils=>is_manager( <p0001> ).

    "store work terms for assignment entity
    APPEND VALUE #( pernr  = <p0001>-pernr
                    begda  = <p0001>-begda
                    endda  = <p0001>-endda
                    wterm_id = src_id ) TO vp_wterm_col.

    IF business_code NE /sew/cl_mig_utils=>default_business_unit.
      "get orgunit text
      LOOP AT hrp1000_orgeh ASSIGNING FIELD-SYMBOL(<hrp1000>) WHERE objid EQ <p0001>-orgeh AND
                                                                    begda LE <p0001>-endda AND
                                                                    endda GE <p0001>-begda.
        DATA(stext_length) = strlen( <hrp1000>-stext ) - 2.
        CHECK <hrp1000>-stext+stext_length(2) NE business_code+0(2).
        CONCATENATE <hrp1000>-stext '-' business_code+0(2) INTO department_name.
        EXIT.
      ENDLOOP.

      "get job text
      LOOP AT hrp1000_stell ASSIGNING <hrp1000> WHERE objid EQ <p0001>-stell AND
                                                      begda LE <p0001>-endda AND
                                                      endda GE <p0001>-begda.
        job = <hrp1000>-stext.
        EXIT.
      ENDLOOP.
    ENDIF.

    "Get relevant work relationship
    LOOP AT vp_wkr_id ASSIGNING FIELD-SYMBOL(<vp_wkr_id>) WHERE pernr EQ <p0001>-pernr AND
                                                                begda LE <p0001>-endda AND
                                                                endda GE <p0001>-begda.
      period_of_ser = <vp_wkr_id>-wkr_id.
      EXIT.
    ENDLOOP.

    CONCATENATE /sew/cl_mig_utils=>merge
                workterms
                sys_id
                src_id
                src_sys_id
                massn
                begda_tmp
                endda_tmp
                '1'
                /sew/cl_mig_utils=>yes
                job
                assign_num
                assign_status
                assignment_type
                business_code
                worker_category
                '' "assignment_category "JMB20210408 D - Only pass space as AssignmentCategory
                ''
                location_code
                is_manager
                department_name
                period_of_ser
                begda_tmp
                worker_type
                legal_employer_name
                person_type_code
                system_person_type
                massg
                internal_number
    INTO DATA(data_tmp) SEPARATED BY /sew/cl_mig_utils=>separator.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
    CLEAR: internal_number, job, department_name.
  ENDLOOP.
ENDMETHOD.


  METHOD CREATE_METADATA.

    DESCRIBE TABLE assignment_structure LINES DATA(length).

    LOOP AT assignment_structure ASSIGNING FIELD-SYMBOL(<assign_struc>).

      "set METADATA title
      CASE <assign_struc>-name.
        WHEN 1.
          CONCATENATE /sew/cl_mig_utils=>metadata /sew/cl_mig_utils=>separator INTO metadata.
          CONTINUE.
      ENDCASE.

      CONCATENATE metadata <assign_struc>-value INTO metadata.

      "set separator
      CHECK length NE sy-tabix.
      CONCATENATE metadata /sew/cl_mig_utils=>separator INTO metadata.
    ENDLOOP.

  ENDMETHOD.


METHOD get_business_code.

  "get country to bukrs
  READ TABLE bu_country INTO DATA(country) WITH KEY land1 = t001-land1.

  "concatenate BusinessUnitShortCode
  CONCATENATE t001-land1 t001-bukrs country-landx INTO business_code SEPARATED BY space.

  "01.03.2021 - Final decision: for history provide default value
  CHECK endda LT sy-datum.

  business_code = 'Data Migration BU'.

ENDMETHOD.


METHOD get_cofu_data.

  "Get IT0000
  IF p0000 IS INITIAL.
    SELECT pernr,
           begda,
           endda,
           massn,
           massg,
           stat1,
           stat2 INTO CORRESPONDING FIELDS OF TABLE @p0000 FROM pa0000 WHERE pernr IN @pernr AND
                                                                             begda LE @endda AND
                                                                             endda GE @begda.
  ENDIF.

  IF p0001 IS INITIAL.
    "Get IT0001
    SELECT pernr,
           begda,
           endda,
           bukrs,
           abkrs,
           persg,
           persk,
           plans,
           stell,
           werks,
           btrtl,
           orgeh,
           ansvh,
           kostl INTO CORRESPONDING FIELDS OF TABLE @p0001 FROM pa0001 WHERE pernr IN @pernr AND
                                                                             begda LE @endda AND
                                                                             endda GE @begda.
  ENDIF.

  IF p0105 IS INITIAL.
    "Get IT0105
    SELECT pernr,
           begda,
           endda,
           subty,
           usrid FROM pa0105 INTO CORRESPONDING FIELDS OF TABLE @p0105 WHERE pernr IN @pernr AND
                                                                             begda LE @endda AND
                                                                             endda GE @begda AND
                                                                             subty EQ '9001'. "JMB20210928 I - C400129651-5704

  ENDIF.

  IF p0016 IS INITIAL.
    "Get IT0016
    SELECT pernr,
           begda,
           endda,
           prbzt,
           prbeh,
           kdgf2
           FROM pa0016 INTO CORRESPONDING FIELDS OF TABLE @p0016 WHERE pernr IN @pernr AND
                                                                       begda LE @endda AND
                                                                       endda GE @begda.

  ENDIF.

  "get orgunit text
  DATA(orgeh) = VALUE rsdsselopt_t( FOR <p0001> IN p0001 ( sign = 'I' option = 'EQ' low = <p0001>-orgeh ) ).
  SORT orgeh BY low.
  DELETE ADJACENT DUPLICATES FROM orgeh COMPARING low.

  "retrieve actual department name
  SELECT objid, begda, endda, stext FROM hrp1000 INTO CORRESPONDING FIELDS OF TABLE @hrp1000_orgeh WHERE objid IN @orgeh    AND
                                                                                                         otype EQ 'O'       AND
                                                                                                         langu EQ @sy-langu AND
                                                                                                         begda LE @sy-datum AND
                                                                                                         begda LE @sy-datum.
  "begda LE @endda AND "JMB20211014 D
  "endda GE @begda.    "JMB20211014 D

  "get job name
  DATA(stell) = VALUE rsdsselopt_t( FOR <p0001> IN p0001 ( sign = 'I' option = 'EQ' low = <p0001>-stell ) ).
  SORT stell BY low.
  DELETE ADJACENT DUPLICATES FROM stell COMPARING low.

  SELECT objid, begda, endda, short FROM hrp1000 INTO CORRESPONDING FIELDS OF TABLE @hrp1000_stell WHERE objid IN @stell    AND
                                                                                                         otype EQ 'C'       AND
                                                                                                         langu EQ @sy-langu AND "JMB20211014 I
                                                                                                         begda LE @endda    AND
                                                                                                         endda GE @begda.

  "get bukrs text
  DATA(bukrs) = VALUE rsdsselopt_t( FOR <p0001> IN p0001 ( sign = 'I' option = 'EQ' low = <p0001>-bukrs ) ).
  SORT bukrs BY low.
  DELETE ADJACENT DUPLICATES FROM bukrs COMPARING low.

  SELECT bukrs, butxt, land1 FROM t001 INTO CORRESPONDING FIELDS OF TABLE @bukrs_txt WHERE bukrs IN @bukrs.

  "get country to bukrs
  DATA(land1) = VALUE rsdsselopt_t( FOR <t001> IN bukrs_txt ( sign = 'I' option = 'EQ' low = <t001>-land1 ) ).
  SORT land1 BY low.
  DELETE ADJACENT DUPLICATES FROM land1 COMPARING low.

  SELECT land1, landx FROM t005t INTO CORRESPONDING FIELDS OF TABLE @bu_country WHERE land1 IN @land1    AND
                                                                                      spras EQ @sy-langu.
ENDMETHOD.


METHOD get_cogl_data.

  "Get IT0000
  IF p0000 IS INITIAL.
    SELECT pernr,
           begda,
           endda,
           massn,
           massg,
           stat2 INTO CORRESPONDING FIELDS OF TABLE @p0000 FROM pa0000 WHERE pernr IN @pernr AND
                                                                             begda LE @endda AND
                                                                             endda GE @begda.
  ENDIF.

  "Get IT0001
  SELECT pernr,
         begda,
         endda,
         bukrs,
         abkrs,
         persg,
         persk,
         plans,
         stell,
         werks,
         btrtl,
         orgeh INTO CORRESPONDING FIELDS OF TABLE @p0001 FROM pa0001 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.
  "Get IT0105
  SELECT pernr,
         begda,
         endda,
         subty,
         usrid FROM pa0105 INTO CORRESPONDING FIELDS OF TABLE @p0105 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda AND
                                                                           subty EQ @/sew/cl_mig_utils=>it0105_9901.

  "get orgunit text
  DATA(orgeh) = VALUE rsdsselopt_t( FOR <p0001> IN p0001 ( sign = 'I' option = 'EQ' low = <p0001>-orgeh ) ).
  SORT orgeh BY low.
  DELETE ADJACENT DUPLICATES FROM orgeh COMPARING low.

  SELECT objid, begda, endda, stext FROM hrp1000 INTO CORRESPONDING FIELDS OF TABLE @hrp1000_orgeh WHERE objid IN @orgeh AND
                                                                                                         otype EQ 'O'    AND
                                                                                                         begda LE @endda AND
                                                                                                         endda GE @begda.

  "get job name
  DATA(stell) = VALUE rsdsselopt_t( FOR <p0001> IN p0001 ( sign = 'I' option = 'EQ' low = <p0001>-stell ) ).
  SORT stell BY low.
  DELETE ADJACENT DUPLICATES FROM stell COMPARING low.

  SELECT objid, begda, endda, stext FROM hrp1000 INTO CORRESPONDING FIELDS OF TABLE @hrp1000_stell WHERE objid IN @stell AND
                                                                                                         otype EQ 'C'    AND
                                                                                                         begda LE @endda AND
                                                                                                         endda GE @begda.

  "get bukrs text
  DATA(bukrs) = VALUE rsdsselopt_t( FOR <p0001> IN p0001 ( sign = 'I' option = 'EQ' low = <p0001>-bukrs ) ).
  SORT bukrs BY low.
  DELETE ADJACENT DUPLICATES FROM bukrs COMPARING low.

  SELECT bukrs, butxt, land1 FROM t001 INTO CORRESPONDING FIELDS OF TABLE @bukrs_txt WHERE bukrs IN @bukrs.

  "get country to bukrs
  DATA(land1) = VALUE rsdsselopt_t( FOR <t001> IN bukrs_txt ( sign = 'I' option = 'EQ' low = <t001>-land1 ) ).
  SORT land1 BY low.
  DELETE ADJACENT DUPLICATES FROM land1 COMPARING low.

  SELECT land1, landx FROM t005t INTO CORRESPONDING FIELDS OF TABLE @bu_country WHERE land1 IN @land1    AND
                                                                                      spras EQ @sy-langu.
ENDMETHOD.


METHOD get_mapping_cofu_fields.

  DATA(worker_sap) = persk.
  "for Austria employee group is relevant for workertype
  IF '03' IN molga OR
     sy-mandt = /sew/cl_int_constants=>cofu_mandant-australia OR
     sy-mandt = /sew/cl_int_constants=>cofu_mandant-newzealand.
    worker_sap = /sew/cl_mig_utils=>persg.
  ENDIF.

  "get mapping fields for workertype
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = worker_sap
                                                   oracle_field = /sew/cl_mig_utils=>workertype
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_persg_wt ).

  "get mapping fields for locationcode
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = /sew/cl_mig_utils=>btrtl
                                                   oracle_field = /sew/cl_mig_utils=>locationcode
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_btrtl ).

  worker_sap = COND #( WHEN sy-mandt EQ /sew/cl_int_constants=>cofu_mandant-newzealand THEN persk
                       ELSE worker_sap ).

  "get mapping fields for assignmenttype
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = worker_sap
                                                   oracle_field = assignmenttype
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_persg_at ).


  "get mapping fields for actioncode
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0000
                                                   sap_field    = /sew/cl_mig_utils=>massn
                                                   oracle_field = /sew/cl_mig_utils=>actioncode
                                                   export       = abap_true
                                        IMPORTING mapping_fields = mapping_fields_massn ).

  "get mapping fields for reasoncode
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0000
                                                   sap_field    = /sew/cl_mig_utils=>massg
                                                   oracle_field = /sew/cl_mig_utils=>reasoncode
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_massg ).

  "get mapping fields for AssignmentStatusTypeCode
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0000
                                                   sap_field    = stat2
                                                   oracle_field = assignmentstatustypecode
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_stat2 ).

  DATA(worker_cat) = persk.

  DATA(abkrs_clients) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = /sew/cl_int_constants=>cofu_mandant-germany )
                                            ( sign = 'I' option = 'EQ' low = /sew/cl_int_constants=>cofu_mandant-australia )
                                            ( sign = 'I' option = 'EQ' low = /sew/cl_int_constants=>cofu_mandant-newzealand ) ).
  "in case of Germany pass ABKRS
  IF sy-mandt IN abkrs_clients .
    worker_cat = 'ABKRS'.
  ENDIF.

  "get mapping fields for workercategory
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = worker_cat
                                                   oracle_field = workercategory
                                                   export       = abap_true
                                          IMPORTING mapping_fields = mapping_fields_persk_wc ).

  "get mapping fields for assignmentcategory
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = persk
                                                   oracle_field = assignmentcategory
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_persk_ac ).

  "get mapping fields for assignmentpersontype
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = persk
                                                   oracle_field = assignmentpersontype
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_persk_apt ).

  "get mapping fields for persontypecode
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = persk
                                                   oracle_field = persontypecode
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_persk_ptc ).

  "get mapping fields for systempersontype
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = persk
                                                   oracle_field = systempersontype
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_persk_spt ).

  "get mapping fields for legal employer
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = /sew/cl_mig_utils=>bukrs
                                                   oracle_field = /sew/cl_mig_utils=>legalemployername
                                                   export       = abap_true
                                          IMPORTING mapping_fields = mapping_fields_bukrs ).

  "get mapping fields for business unit short
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = /sew/cl_mig_utils=>bukrs
                                                   oracle_field = /sew/cl_mig_utils=>businessunitshortcode
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_bukrs_bu ).

  "get mapping fields for departmentname
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = /sew/cl_mig_utils=>orgeh
                                                   oracle_field = /sew/cl_mig_utils=>departmentname
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_department ).

  "get mapping fields for jobcode
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = 'STELL'
                                                   oracle_field = 'JOBCODE'
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_job ).

  "get mapping fields for PRBEH
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = '0016'
                                                   sap_field    = 'PRBEH'
                                                   oracle_field = 'PROBATIONUNIT'
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_prbeh ).

  "get mapping fields for KDGF2
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = '0016'
                                                   sap_field    = 'KDGF2'
                                                   oracle_field = 'NOTICEPERIODUOM'
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_kdgf2 ).

ENDMETHOD.


METHOD get_mapping_cofu_values.

  "check customizing for Austria
  DATA(worker_sap) = persk.
  IF '03' IN molga OR
     sy-mandt = /sew/cl_int_constants=>cofu_mandant-australia  OR
     sy-mandt = /sew/cl_int_constants=>cofu_mandant-newzealand.
    worker_sap = /sew/cl_mig_utils=>persg.
  ENDIF.

  "get mapping values for workertype
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = worker_sap
                                                   oracle_field = /sew/cl_mig_utils=>workertype
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_persg_wt ).

  "get mapping values for locationcode
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = /sew/cl_mig_utils=>btrtl
                                                   oracle_field = /sew/cl_mig_utils=>locationcode
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_btrtl ).

  worker_sap = COND #( WHEN sy-mandt EQ /sew/cl_int_constants=>cofu_mandant-newzealand THEN persk
                       ELSE worker_sap ).

  "get mapping values for assignmenttype
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = worker_sap
                                                   oracle_field = assignmenttype
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_persg_at ).

  "get mapping values for actioncode
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0000
                                                   sap_field    = /sew/cl_mig_utils=>massn
                                                   oracle_field = /sew/cl_mig_utils=>actioncode
                                                   export       = abap_true
                                        IMPORTING mapping_values = mapping_values_massn ).

  "get mapping values for reasoncode
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0000
                                                   sap_field    = /sew/cl_mig_utils=>massg
                                                   oracle_field = /sew/cl_mig_utils=>reasoncode
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_massg ).

  "get mapping values for AssignmentStatusTypeCode
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0000
                                                   sap_field    = stat2
                                                   oracle_field = assignmentstatustypecode
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_stat2 ).

  DATA(worker_cat) = persk.

  DATA(abkrs_clients) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = /sew/cl_int_constants=>cofu_mandant-germany )
                                            ( sign = 'I' option = 'EQ' low = /sew/cl_int_constants=>cofu_mandant-australia )
                                            ( sign = 'I' option = 'EQ' low = /sew/cl_int_constants=>cofu_mandant-newzealand ) ).
  "in case of Germany pass ABKRS
  IF sy-mandt IN abkrs_clients .
    worker_cat = 'ABKRS'.
  ENDIF.

  "get mapping values for workercategory
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = worker_cat
                                                   oracle_field = workercategory
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_persk_wc ).

  "get mapping values for assignmentcategory
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = persk
                                                   oracle_field = assignmentcategory
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_persk_ac ).

  "get mapping fields for assignmentpersontype
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = persk
                                                   oracle_field = assignmentpersontype
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_persk_apt ).

  "get mapping values for persontypecode
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = persk
                                                   oracle_field = persontypecode
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_persk_ptc ).

  "get mapping values for systempersontype
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = persk
                                                   oracle_field = systempersontype
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_persk_spt ).

  "get mapping values for legal employer
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = /sew/cl_mig_utils=>bukrs
                                                   oracle_field = /sew/cl_mig_utils=>legalemployername
                                                   export       = abap_true
                                          IMPORTING mapping_values = mapping_values_bukrs ).

  "get mapping values for business unit short
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = /sew/cl_mig_utils=>bukrs
                                                   oracle_field = /sew/cl_mig_utils=>businessunitshortcode
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_bukrs_bu ).

  "get mapping values for departmentname
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = /sew/cl_mig_utils=>orgeh
                                                   oracle_field = /sew/cl_mig_utils=>departmentname
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_department ).

  "get mapping values for jobcode
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = 'STELL'
                                                   oracle_field = 'JOBCODE'
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_job ).

  "get mapping fields for PRBEH
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = '0016'
                                                   sap_field    = 'PRBEH'
                                                   oracle_field = 'PROBATIONUNIT'
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_prbeh ).

  "get mapping fields for KDGF2
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = '0016'
                                                   sap_field    = 'KDGF2'
                                                   oracle_field = 'NOTICEPERIODUOM'
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_kdgf2 ).
ENDMETHOD.


METHOD get_mapping_cogu_fields.

  DATA(worker_sap) = persk.
  "for Austria employee group is relevant for workertype
  IF '03' IN molga OR
     sy-mandt = /sew/cl_int_constants=>cofu_mandant-australia OR
     sy-mandt = /sew/cl_int_constants=>cofu_mandant-newzealand.
    worker_sap = /sew/cl_mig_utils=>persg.
  ENDIF.

  "get mapping fields for workertype
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = worker_sap
                                                   oracle_field = /sew/cl_mig_utils=>workertype
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_persg_wt ).

  "get mapping fields for locationcode
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = /sew/cl_mig_utils=>btrtl
                                                   oracle_field = /sew/cl_mig_utils=>locationcode
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_btrtl ).

  worker_sap = COND #( WHEN sy-mandt EQ /sew/cl_int_constants=>cofu_mandant-newzealand THEN persk
                       ELSE worker_sap ).

  "get mapping fields for assignmenttype
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = worker_sap
                                                   oracle_field = assignmenttype
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_persg_at ).


  "get mapping fields for actioncode
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0000
                                                   sap_field    = /sew/cl_mig_utils=>massn
                                                   oracle_field = /sew/cl_mig_utils=>actioncode
                                                   export       = abap_true
                                        IMPORTING mapping_fields = mapping_fields_massn ).

  "get mapping fields for reasoncode
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0000
                                                   sap_field    = /sew/cl_mig_utils=>massg
                                                   oracle_field = /sew/cl_mig_utils=>reasoncode
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_massg ).

  "get mapping fields for AssignmentStatusTypeCode
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0000
                                                   sap_field    = stat2
                                                   oracle_field = assignmentstatustypecode
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_stat2 ).

  DATA(worker_cat) = persk.

  DATA(abkrs_clients) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = /sew/cl_int_constants=>cofu_mandant-germany )
                                            ( sign = 'I' option = 'EQ' low = /sew/cl_int_constants=>cofu_mandant-australia )
                                            ( sign = 'I' option = 'EQ' low = /sew/cl_int_constants=>cofu_mandant-newzealand ) ).
  "in case of Germany pass ABKRS
  IF sy-mandt IN abkrs_clients .
    worker_cat = 'ABKRS'.
  ENDIF.

  "get mapping fields for workercategory
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = worker_cat
                                                   oracle_field = workercategory
                                                   export       = abap_true
                                          IMPORTING mapping_fields = mapping_fields_persk_wc ).

  "get mapping fields for assignmentcategory
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = persk
                                                   oracle_field = assignmentcategory
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_persk_ac ).

  "get mapping fields for assignmentpersontype
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = persk
                                                   oracle_field = assignmentpersontype
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_persk_apt ).

  "get mapping fields for persontypecode
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = persk
                                                   oracle_field = persontypecode
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_persk_ptc ).

  "get mapping fields for systempersontype
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = persk
                                                   oracle_field = systempersontype
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_persk_spt ).

  "get mapping fields for legal employer
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = /sew/cl_mig_utils=>bukrs
                                                   oracle_field = /sew/cl_mig_utils=>legalemployername
                                                   export       = abap_true
                                          IMPORTING mapping_fields = mapping_fields_bukrs ).

  "get mapping fields for business unit short
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = /sew/cl_mig_utils=>bukrs
                                                   oracle_field = /sew/cl_mig_utils=>businessunitshortcode
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_bukrs_bu ).

  "get mapping fields for departmentname
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = /sew/cl_mig_utils=>orgeh
                                                   oracle_field = /sew/cl_mig_utils=>departmentname
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_department ).

  "get mapping fields for jobcode
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = 'STELL'
                                                   oracle_field = 'JOBCODE'
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_job ).
ENDMETHOD.


METHOD get_mapping_cogu_values.

  "check customizing for Austria
  DATA(worker_sap) = persk.
  IF '03' IN molga OR
     sy-mandt = /sew/cl_int_constants=>cofu_mandant-australia  OR
     sy-mandt = /sew/cl_int_constants=>cofu_mandant-newzealand.
    worker_sap = /sew/cl_mig_utils=>persg.
  ENDIF.

  "get mapping values for workertype
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = worker_sap
                                                   oracle_field = /sew/cl_mig_utils=>workertype
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_persg_wt ).

  "get mapping values for locationcode
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = /sew/cl_mig_utils=>btrtl
                                                   oracle_field = /sew/cl_mig_utils=>locationcode
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_btrtl ).

  worker_sap = COND #( WHEN sy-mandt EQ /sew/cl_int_constants=>cofu_mandant-newzealand THEN persk
                       ELSE worker_sap ).

  "get mapping values for assignmenttype
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = worker_sap
                                                   oracle_field = assignmenttype
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_persg_at ).

  "get mapping values for actioncode
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0000
                                                   sap_field    = /sew/cl_mig_utils=>massn
                                                   oracle_field = /sew/cl_mig_utils=>actioncode
                                                   export       = abap_true
                                        IMPORTING mapping_values = mapping_values_massn ).

  "get mapping values for reasoncode
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0000
                                                   sap_field    = /sew/cl_mig_utils=>massg
                                                   oracle_field = /sew/cl_mig_utils=>reasoncode
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_massg ).

  "get mapping values for AssignmentStatusTypeCode
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0000
                                                   sap_field    = stat2
                                                   oracle_field = assignmentstatustypecode
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_stat2 ).

  DATA(worker_cat) = persk.

  DATA(abkrs_clients) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = /sew/cl_int_constants=>cofu_mandant-germany )
                                            ( sign = 'I' option = 'EQ' low = /sew/cl_int_constants=>cofu_mandant-australia )
                                            ( sign = 'I' option = 'EQ' low = /sew/cl_int_constants=>cofu_mandant-newzealand ) ).
  "in case of Germany pass ABKRS
  IF sy-mandt IN abkrs_clients .
    worker_cat = 'ABKRS'.
  ENDIF.

  "get mapping values for workercategory
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = worker_cat
                                                   oracle_field = workercategory
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_persk_wc ).

  "get mapping values for assignmentcategory
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = persk
                                                   oracle_field = assignmentcategory
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_persk_ac ).

  "get mapping fields for assignmentpersontype
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = persk
                                                   oracle_field = assignmentpersontype
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_persk_apt ).

  "get mapping values for persontypecode
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = persk
                                                   oracle_field = persontypecode
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_persk_ptc ).

  "get mapping values for systempersontype
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = persk
                                                   oracle_field = systempersontype
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_persk_spt ).

  "get mapping values for legal employer
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = /sew/cl_mig_utils=>bukrs
                                                   oracle_field = /sew/cl_mig_utils=>legalemployername
                                                   export       = abap_true
                                          IMPORTING mapping_values = mapping_values_bukrs ).

  "get mapping values for business unit short
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = /sew/cl_mig_utils=>bukrs
                                                   oracle_field = /sew/cl_mig_utils=>businessunitshortcode
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_bukrs_bu ).

  "get mapping values for departmentname
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = /sew/cl_mig_utils=>orgeh
                                                   oracle_field = /sew/cl_mig_utils=>departmentname
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_department ).

  "get mapping values for jobcode
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = 'STELL'
                                                   oracle_field = 'JOBCODE'
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_job ).
ENDMETHOD.


METHOD get_mapping_fields.
  "get mapping fields for workertype
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = /sew/cl_mig_utils=>persg
                                                   oracle_field = /sew/cl_mig_utils=>workertype
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_persg_wt ).

  "get mapping fields for locationcode
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = /sew/cl_mig_utils=>btrtl
                                                   oracle_field = /sew/cl_mig_utils=>locationcode
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_btrtl ).

  "get mapping fields for assignmenttype
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = /sew/cl_mig_utils=>persg
                                                   oracle_field = assignmenttype
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_persg_at ).

  "get mapping fields for actioncode
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0000
                                                   sap_field    = /sew/cl_mig_utils=>massn
                                                   oracle_field = /sew/cl_mig_utils=>actioncode
                                                   export       = abap_true
                                        IMPORTING mapping_fields = mapping_fields_massn ).


  "get mapping fields for reasoncode
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0000
                                                   sap_field    = /sew/cl_mig_utils=>massg
                                                   oracle_field = /sew/cl_mig_utils=>reasoncode
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_massg ).

  "get mapping fields for AssignmentStatusTypeCode
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0000
                                                   sap_field    = stat2
                                                   oracle_field = assignmentstatustypecode
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_stat2 ).

  "get mapping fields for workercategory
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = persk
                                                   oracle_field = workercategory
                                                   export       = abap_true
                                          IMPORTING mapping_fields = mapping_fields_persk_wc ).

  "get mapping fields for assignmentcategory
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = persk
                                                   oracle_field = assignmentcategory
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_persk_ac ).

  "get mapping fields for persontypecode
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = persk
                                                   oracle_field = persontypecode
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_persk_ptc ).

  "get mapping fields for systempersontype
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = persk
                                                   oracle_field = systempersontype
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_persk_spt ).

  "get mapping fields for legal employer
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = /sew/cl_mig_utils=>bukrs
                                                   oracle_field = /sew/cl_mig_utils=>legalemployername
                                                   export       = abap_true
                                          IMPORTING mapping_fields = mapping_fields_bukrs ).

  "get mapping fields for business unit short
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = /sew/cl_mig_utils=>bukrs
                                                   oracle_field = /sew/cl_mig_utils=>businessunitshortcode
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_bukrs_bu ).

  "get mapping fields for departmentname
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = /sew/cl_mig_utils=>orgeh
                                                   oracle_field = /sew/cl_mig_utils=>departmentname
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_department ).
ENDMETHOD.


METHOD get_mapping_values.
  "get mapping values for workertype
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = /sew/cl_mig_utils=>persg
                                                   oracle_field = /sew/cl_mig_utils=>workertype
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_persg_wt ).

  "get mapping values for locationcode
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = /sew/cl_mig_utils=>btrtl
                                                   oracle_field = /sew/cl_mig_utils=>locationcode
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_btrtl ).

  "get mapping values for assignmenttype
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = /sew/cl_mig_utils=>persg
                                                   oracle_field = assignmenttype
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_persg_at ).

  "get mapping values for actioncode
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0000
                                                   sap_field    = /sew/cl_mig_utils=>massn
                                                   oracle_field = /sew/cl_mig_utils=>actioncode
                                                   export       = abap_true
                                        IMPORTING mapping_values = mapping_values_massn ).


  "get mapping values for reasoncode
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0000
                                                   sap_field    = /sew/cl_mig_utils=>massg
                                                   oracle_field = /sew/cl_mig_utils=>reasoncode
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_massg ).

  "get mapping values for AssignmentStatusTypeCode
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0000
                                                   sap_field    = stat2
                                                   oracle_field = assignmentstatustypecode
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_stat2 ).

  "get mapping values for workercategory
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = persk
                                                   oracle_field = workercategory
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_persk_wc ).

  "get mapping values for assignmentcategory
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = persk
                                                   oracle_field = assignmentcategory
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_persk_ac ).

  "get mapping values for persontypecode
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = persk
                                                   oracle_field = persontypecode
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_persk_ptc ).

  "get mapping values for systempersontype
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = persk
                                                   oracle_field = systempersontype
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_persk_spt ).

  "get mapping values for legal employer
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = /sew/cl_mig_utils=>bukrs
                                                   oracle_field = /sew/cl_mig_utils=>legalemployername
                                                   export       = abap_true
                                          IMPORTING mapping_values = mapping_values_bukrs ).

  "get mapping values for business unit short
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = /sew/cl_mig_utils=>bukrs
                                                   oracle_field = /sew/cl_mig_utils=>businessunitshortcode
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_bukrs_bu ).

  "get mapping values for departmentname
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = /sew/cl_mig_utils=>orgeh
                                                   oracle_field = /sew/cl_mig_utils=>departmentname
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_department ).
ENDMETHOD.


METHOD map_cofu_data.

  DATA: plvar           TYPE plvar,
        massn_tmp       TYPE massn,
        pernr_tmp       TYPE pernr_d,
        period_del      TYPE hrperiods_tab,
        period_of_ser   TYPE string,
        job             TYPE string,
        dateprob_end    TYPE string,
        rpt_est         TYPE string,
        grade_code      TYPE string,
        notice_per      TYPE string,
        pos_code        TYPE string,
        job_short       TYPE string,
        prob_period     TYPE string,
        normalhours     TYPE string,
        internal_number TYPE string.

  DATA(massn_term) = SWITCH rsdsselopt_t( sy-mandt
                                       WHEN /sew/cl_int_constants=>cofu_mandant-germany     THEN VALUE #( ( sign = 'I' option = 'EQ' low = '10' )
                                                                                                          ( sign = 'I' option = 'EQ' low = '11' )
                                                                                                          ( sign = 'I' option = 'EQ' low = '13' )
                                                                                                          ( sign = 'I' option = 'EQ' low = '18' )
                                                                                                          ( sign = 'I' option = 'EQ' low = '35' )
                                                                                                          ( sign = 'I' option = 'EQ' low = '99' )
                                                                                                          ( sign = 'I' option = 'EQ' low = 'ZZ' ) )
                                       WHEN /sew/cl_int_constants=>cofu_mandant-france      THEN VALUE #( ( sign = 'I' option = 'EQ' low = '09' )
                                                                                                          ( sign = 'I' option = 'EQ' low = '10' )
                                                                                                          ( sign = 'I' option = 'EQ' low = '11' )
                                                                                                          ( sign = 'I' option = 'EQ' low = '18' )
                                                                                                          ( sign = 'I' option = 'EQ' low = '32' )
                                                                                                          ( sign = 'I' option = 'EQ' low = '34' ) )
                                       WHEN /sew/cl_int_constants=>cofu_mandant-netherlands THEN VALUE #( ( sign = 'I' option = 'EQ' low = 'Z4' ) )
                                       "default Italy/Austria
                                       ELSE VALUE #( ( sign = 'I' option = 'EQ' low = '03' )
                                                     ( sign = 'I' option = 'EQ' low = 'ZZ' ) ) ).

  "get active plvar
  CALL FUNCTION 'RH_GET_PLVAR'
    IMPORTING
      plvar = plvar.

  CONCATENATE /sew/cl_mig_utils=>sap sy-mandt INTO DATA(sys_id).

  DATA(periods_lcl) = periods.
  LOOP AT periods_lcl ASSIGNING FIELD-SYMBOL(<period>).

    "get relevant P0000
    LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000>) WHERE begda LE <period>-endda AND
                                                        endda GE <period>-begda AND
                                                        pernr EQ pernr.
      EXIT.
    ENDLOOP.

    CHECK sy-subrc EQ 0.

    "get relevant P0001
    LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>) WHERE begda LE <period>-endda AND
                                                        endda GE <period>-begda AND
                                                        pernr EQ pernr.
      EXIT.
    ENDLOOP.

    CHECK sy-subrc EQ 0.

    "in case of termination in future
    CHECK <p0000>-massn NOT IN massn_term.

    "check for terminations right after actual record
    DATA(datum) = CONV datum( <period>-endda + 1 ).
    LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000_term>) WHERE begda LE datum         AND
                                                             endda GE datum         AND
                                                             pernr EQ pernr         AND
                                                             massn IN massn_term.
      <period>-endda = <p0000_term>-endda.
    ENDLOOP.

    "get P0105
    LOOP AT p0105 ASSIGNING FIELD-SYMBOL(<p0105>) WHERE begda LE <period>-endda AND
                                                        endda GE <period>-begda AND
                                                        pernr EQ pernr.
      internal_number = <p0105>-usrid.
      EXIT.
    ENDLOOP.

    LOOP AT p0016 INTO DATA(p0016_entry) WHERE begda LE <period>-endda AND
                                               endda GE <period>-begda AND
                                               pernr EQ pernr          AND
                                               prbzt NE 0.
      notice_per = prob_period = p0016_entry-prbzt.
      CONDENSE: notice_per, prob_period.
      EXIT.
    ENDLOOP.

    map_mig_cofu_values( EXPORTING p0001 = <p0001>
                                   p0000 = <p0000>
                                   p0016 = p0016_entry
                                   job   = job
                         IMPORTING massn               = DATA(massn)
                                   massg               = DATA(massg)
                                   worker_type         = DATA(worker_type)
                                   assignment_type     = DATA(assignment_type)
                                   person_type_code    = DATA(person_type_code)
                                   system_person_type  = DATA(system_person_type)
                                   assign_status       = DATA(assign_status)
                                   worker_category     = DATA(worker_category)
                                   assignment_category = DATA(assignment_category)
                                   location_code       = DATA(location_code)
                                   legal_employer_name = DATA(legal_employer_name)
                                   business_unit_code  = DATA(business_code)
                                   department_name     = DATA(department_name)
                                   noticeuom           = DATA(notice_per_uom)
                                   probationunit       = DATA(prob_unit) ).

    "in case of work terms assigment type -> add T in case of E and C
    assignment_type = SWITCH #( assignment_type
                                WHEN 'E' OR 'C' THEN assignment_type && 'T' ).

    "check hire entry for employee
    IF pernr          IN pernr_history AND
       <period>-begda NE mig_date      AND
       pernr_history  IS NOT INITIAL.
      "actioncode for these entries will be ASG_CHANGE
      massn = 'ASG_CHANGE'.
    ELSE.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = pernr ) TO pernr_history.
    ENDIF.

    DATA(begda_tmp) = /sew/cl_mig_utils=>convert_date( <period>-begda ).
    DATA(endda_tmp) = /sew/cl_mig_utils=>convert_date( <period>-endda ).

    CONCATENATE wt pernr INTO DATA(src_id).

    CONCATENATE et pernr  INTO DATA(assign_num).

    IF massn_tmp     IN     massn_term    AND
       <p0000>-massn NOT IN massn_term    AND
       pernr_tmp     EQ     <p0001>-pernr.
      CONCATENATE assign_num '_' massn INTO assign_num.
      CONCATENATE src_id     '_' massn INTO src_id.
    ENDIF.

    pernr_tmp = pernr.
    massn_tmp = <p0000>-massn.

    "get source id
    DATA(src_sys_id) = /sew/cl_mig_utils=>get_src_id( pernr = <p0001>-pernr
                                                      begda = <p0001>-begda
                                                      endda = <p0001>-endda
                                                      vp_src_id = vp_src_id ).

    DATA(is_manager) = /sew/cl_mig_utils=>is_manager( <p0001> ).

    "store work terms for assignment entity
    APPEND VALUE #( pernr  = pernr
                    begda  = <period>-begda
                    endda  = <period>-endda
                    wterm_id = src_id ) TO vp_wterm_col.

    IF business_code NE /sew/cl_mig_utils=>default_business_unit.
      "get job text
      LOOP AT hrp1000_stell ASSIGNING FIELD-SYMBOL(<hrp1000>) WHERE objid EQ <p0001>-stell AND
                                                                    begda LE <p0001>-endda AND
                                                                    endda GE <p0001>-begda.
*        job = <hrp1000>-short. "JMB20211202 D - Need to be discussed
        EXIT.
      ENDLOOP.
    ENDIF.

    "Get relevant work relationship
    LOOP AT vp_wkr_id ASSIGNING FIELD-SYMBOL(<vp_wkr_id>) WHERE pernr EQ pernr          AND
                                                                begda LE <period>-endda AND
                                                                endda GE <period>-begda.
      period_of_ser = <vp_wkr_id>-wkr_id.
      EXIT.
    ENDLOOP.

    CONCATENATE /sew/cl_mig_utils=>merge
                workterms
                sys_id
                src_id
                src_sys_id
                massn
                begda_tmp
                endda_tmp
                '1'
                /sew/cl_mig_utils=>yes
                job
                assign_num
                assign_status
                assignment_type
                business_code
                worker_category
                assignment_category
                ''
                location_code
                is_manager
                department_name
                period_of_ser
                begda_tmp
                worker_type
                legal_employer_name
                person_type_code
                system_person_type
                massg
                internal_number
                dateprob_end
                rpt_est
                grade_code
                pos_code
                prob_period
                prob_unit
    INTO DATA(data_tmp) SEPARATED BY /sew/cl_mig_utils=>separator.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
    CLEAR: internal_number, job, department_name.
  ENDLOOP.
ENDMETHOD.


METHOD map_cogl_data.

  DATA: plvar           TYPE plvar,
        massn_tmp       TYPE massn,
        pernr_tmp       TYPE pernr_d,
        period_of_ser   TYPE string,
        job             TYPE string,
        internal_number TYPE string.

  DATA(massn_term) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '03' ) ).

  "get active plvar
  CALL FUNCTION 'RH_GET_PLVAR'
    IMPORTING
      plvar = plvar.

  CONCATENATE /sew/cl_mig_utils=>sap sy-mandt INTO DATA(sys_id).

  LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>).

    "get relevant P0000
    LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000>) WHERE begda LE <p0001>-endda AND
                                                        endda GE <p0001>-begda AND
                                                        pernr EQ <p0001>-pernr.
      EXIT.
    ENDLOOP.

    CHECK sy-subrc EQ 0.

    "in case of termination
    CHECK <p0000>-massn NOT IN massn_term.

    "check for terminations right after actual record
    DATA(datum) = CONV datum( <p0001>-endda + 1 ).
    LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000_term>) WHERE begda LE datum         AND
                                                             endda GE datum         AND
                                                             pernr EQ <p0001>-pernr AND
                                                             massn IN massn_term.
      <p0001>-endda = <p0000_term>-endda.
    ENDLOOP.

    "get P0105
    LOOP AT p0105 ASSIGNING FIELD-SYMBOL(<p0105>) WHERE begda LE <p0001>-endda AND
                                                        endda GE <p0001>-begda AND
                                                        pernr EQ <p0001>-pernr.
      internal_number = <p0105>-usrid.
      EXIT.
    ENDLOOP.

    LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001_actual>) WHERE pernr EQ <p0001>-pernr AND
                                                               begda LE sy-datum      AND
                                                               endda GE sy-datum.
      "pass only actual worker/assignment type due to worker type in work relationship
      <p0001>-persg = <p0001_actual>-persg.
      <p0001>-persk = <p0001_actual>-persk.
      EXIT.
    ENDLOOP.

    map_mig_values( EXPORTING p0001 = <p0001>
                              p0000 = <p0000>
                    IMPORTING massn               = DATA(massn)
                              massg               = DATA(massg)
                              worker_type         = DATA(worker_type)
                              assignment_type     = DATA(assignment_type)
                              person_type_code    = DATA(person_type_code)
                              system_person_type  = DATA(system_person_type)
                              assign_status       = DATA(assign_status)
                              worker_category     = DATA(worker_category)
                              assignment_category = DATA(assignment_category)
                              location_code       = DATA(location_code)
                              legal_employer_name = DATA(legal_employer_name)
                              business_unit_code  = DATA(business_code)
                              department_name     = DATA(department_name) ).

    "in case of work terms assigment type -> add T in case of E and C
    CASE assignment_type.
      WHEN 'E' OR 'C'.
        CONCATENATE assignment_type 'T' INTO assignment_type.
    ENDCASE.

    "check dummy hire entry for employee
    IF <p0001>-pernr  IN pernr_history AND
        pernr_history IS NOT INITIAL.
      "actioncode for these entries will be ASC_CHANGE
      massn = 'ASG_CHANGE'.
    ELSE.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0001>-pernr ) TO pernr_history.
    ENDIF.

    DATA(begda_tmp) = /sew/cl_mig_utils=>convert_date( <p0001>-begda ).
    DATA(endda_tmp) = /sew/cl_mig_utils=>convert_date( COND #( WHEN <p0000>-massn IN massn_term
                                                               THEN /sew/cl_mig_utils=>oracle_hd
                                                               ELSE <p0001>-endda ) ).
    CONCATENATE wt <p0001>-pernr INTO DATA(src_id).
    CONCATENATE et <p0001>-pernr  INTO DATA(assign_num).

    "get source id
    DATA(src_sys_id) = /sew/cl_mig_utils=>get_src_id( pernr = <p0001>-pernr
                                                      begda = <p0001>-begda
                                                      endda = <p0001>-endda
                                                      vp_src_id = vp_src_id ).

    DATA(is_manager) = /sew/cl_mig_utils=>is_manager( <p0001> ).

    "store work terms for assignment entity
    APPEND VALUE #( pernr  = <p0001>-pernr
                    begda  = <p0001>-begda
                    endda  = <p0001>-endda
                    wterm_id = src_id ) TO vp_wterm_col.

    IF business_code NE /sew/cl_mig_utils=>default_business_unit.
      "get job text
      LOOP AT hrp1000_stell ASSIGNING FIELD-SYMBOL(<hrp1000>) WHERE objid EQ <p0001>-stell AND
                                                                    begda LE <p0001>-endda AND
                                                                    endda GE <p0001>-begda.
        job = <hrp1000>-stext.
        EXIT.
      ENDLOOP.
    ENDIF.

    "Get relevant work relationship
    LOOP AT vp_wkr_id ASSIGNING FIELD-SYMBOL(<vp_wkr_id>) WHERE pernr EQ <p0001>-pernr AND
                                                                begda LE <p0001>-endda AND
                                                                endda GE <p0001>-begda.
      period_of_ser = <vp_wkr_id>-wkr_id.
      EXIT.
    ENDLOOP.

    CONCATENATE /sew/cl_mig_utils=>merge
                workterms
                sys_id
                src_id
                src_sys_id
                massn
                begda_tmp
                endda_tmp
                '1'
                /sew/cl_mig_utils=>yes
                job
                assign_num
                assign_status
                assignment_type
                business_code
                worker_category
                '' "assignment_category "JMB20210408 D - Only pass space as AssignmentCategory
                ''
                location_code
                is_manager
                department_name
                period_of_ser
                begda_tmp
                worker_type
                legal_employer_name
                person_type_code
                system_person_type
                massg
                internal_number
    INTO DATA(data_tmp) SEPARATED BY /sew/cl_mig_utils=>separator.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
    CLEAR: internal_number, job, department_name.
  ENDLOOP.
ENDMETHOD.


METHOD map_cogu_data.

  DATA: plvar           TYPE plvar,
        massn_tmp       TYPE massn,
        pernr_tmp       TYPE pernr_d,
        period_of_ser   TYPE string,
        job_short       TYPE string,
        internal_number TYPE string.

  DATA(massn_term) = SWITCH rsdsselopt_t( sy-mandt
                                          WHEN /sew/cl_int_constants=>cofu_mandant-germany     THEN VALUE #( ( sign = 'I' option = 'EQ' low = '10' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '11' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '13' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '18' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '35' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '99' )
                                                                                                             ( sign = 'I' option = 'EQ' low = 'ZZ' ) )
                                          WHEN /sew/cl_int_constants=>cofu_mandant-france      THEN VALUE #( ( sign = 'I' option = 'EQ' low = '09' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '10' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '11' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '18' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '32' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '34' ) )
                                          WHEN /sew/cl_int_constants=>cofu_mandant-netherlands THEN VALUE #( ( sign = 'I' option = 'EQ' low = 'Z4' ) )
                                          WHEN /sew/cl_int_constants=>cofu_mandant-australia   THEN VALUE #( ( sign = 'I' option = 'EQ' low = '10' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '20' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '25' ) )
                                          WHEN /sew/cl_int_constants=>cofu_mandant-newzealand  THEN VALUE #( ( sign = 'I' option = 'EQ' low = '25' ) )
                                          "default Italy/Austria
                                          ELSE VALUE #( ( sign = 'I' option = 'EQ' low = '03' )
                                                        ( sign = 'I' option = 'EQ' low = 'ZZ' ) ) ).

  "get active plvar
  CALL FUNCTION 'RH_GET_PLVAR'
    IMPORTING
      plvar = plvar.

  CONCATENATE /sew/cl_mig_utils=>sap sy-mandt INTO DATA(sys_id).

  LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>).

    "get relevant P0000
    LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000>) WHERE begda LE <p0001>-endda AND
                                                        endda GE <p0001>-begda AND
                                                        pernr EQ <p0001>-pernr.
      EXIT.
    ENDLOOP.

    CHECK sy-subrc EQ 0.

    "in case of termination
    CHECK <p0000>-massn   NOT IN massn_term.

    "check for terminations right after actual record
    DATA(datum) = CONV datum( <p0001>-endda + 1 ).
    LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000_term>) WHERE begda LE datum         AND
                                                             endda GE datum         AND
                                                             pernr EQ <p0001>-pernr AND
                                                             massn IN massn_term.
      <p0001>-endda = <p0000_term>-endda.
    ENDLOOP.

    "get P0105
    LOOP AT p0105 ASSIGNING FIELD-SYMBOL(<p0105>) WHERE begda LE <p0001>-endda AND
                                                        endda GE <p0001>-begda AND
                                                        pernr EQ <p0001>-pernr.
      internal_number = <p0105>-usrid.
      EXIT.
    ENDLOOP.

    LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001_actual>) WHERE pernr EQ <p0001>-pernr AND
                                                               begda LE sy-datum      AND
                                                               endda GE sy-datum.

      "Decision 06.08.2021: pass only actual legal employer due to import problems in Oracle
      <p0001>-bukrs = <p0001_actual>-bukrs. "JMB20210806 I

      "pass only actual worker/assignment type due to worker type in work relationship
      <p0001>-persg = <p0001_actual>-persg.
      <p0001>-persk = <p0001_actual>-persk.
      EXIT.
    ENDLOOP.

    CLEAR job_short.
**JMB20210811 start delete - JobCode not needed for CoGu
*
*    "get job text
*    LOOP AT hrp1000_stell ASSIGNING FIELD-SYMBOL(<hrp1000>) WHERE objid EQ <p0001>-stell AND
*                                                                  begda LE <p0001>-endda AND
*                                                                  endda GE <p0001>-begda.
*      job_short = <hrp1000>-short.
*      EXIT.
*    ENDLOOP.
*JMB20210811 end delete

    map_mig_cogu_values( EXPORTING p0001 = <p0001>
                                   p0000 = <p0000>
                                   job   = job_short
                         IMPORTING massn               = DATA(massn)
                                   massg               = DATA(massg)
                                   worker_type         = DATA(worker_type)
                                   assignment_type     = DATA(assignment_type)
                                   person_type_code    = DATA(person_type_code)
                                   system_person_type  = DATA(system_person_type)
                                   assignment_person_type_code = DATA(assignment_person_type_code)
                                   assign_status       = DATA(assign_status)
                                   worker_category     = DATA(worker_category)
                                   assignment_category = DATA(assignment_category)
                                   location_code       = DATA(location_code)
                                   legal_employer_name = DATA(legal_employer_name)
                                   business_unit_code  = DATA(business_code)
                                   department_name     = DATA(department_name)
                                   job_code            = DATA(job) ).

    "in case of work terms assigment type -> add T in case of E and C
    CASE assignment_type.
      WHEN 'E' OR 'C'.
        CONCATENATE assignment_type 'T' INTO assignment_type.
    ENDCASE.

    "check dummy hire entry for employee
    IF <p0001>-pernr  IN pernr_history AND
        pernr_history IS NOT INITIAL.
      "actioncode for these entries will be ASC_CHANGE
      massn = 'ASG_CHANGE'.
    ELSE.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0001>-pernr ) TO pernr_history.
    ENDIF.

    DATA(begda_tmp) = /sew/cl_mig_utils=>convert_date( <p0001>-begda ).
    DATA(endda_tmp) = /sew/cl_mig_utils=>convert_date( COND #( WHEN <p0000>-massn IN massn_term AND
                                                                    <p0000>-stat2 NE '1'   "JMB20210911 I
                                                               THEN /sew/cl_mig_utils=>oracle_hd
                                                               ELSE <p0001>-endda ) ).
    CONCATENATE wt <p0001>-pernr INTO DATA(src_id).
    CONCATENATE et <p0001>-pernr  INTO DATA(assign_num).

    "get source id
    DATA(src_sys_id) = /sew/cl_mig_utils=>get_src_id( pernr = <p0001>-pernr
                                                      begda = <p0001>-begda
                                                      endda = <p0001>-endda
                                                      vp_src_id = vp_src_id ).

    DATA(is_manager) = /sew/cl_mig_utils=>is_manager( <p0001> ).

    "store work terms for assignment entity
    APPEND VALUE #( pernr  = <p0001>-pernr
                    begda  = <p0001>-begda
                    endda  = <p0001>-endda
                    wterm_id = src_id ) TO vp_wterm_col.

    "Get relevant work relationship
    LOOP AT vp_wkr_id ASSIGNING FIELD-SYMBOL(<vp_wkr_id>) WHERE pernr EQ <p0001>-pernr AND
                                                                begda LE <p0001>-endda AND
                                                                endda GE <p0001>-begda.
      period_of_ser = <vp_wkr_id>-wkr_id.
      EXIT.
    ENDLOOP.

**JMB20220207 start insert - in case of 21945, clear department due to department set issue in Oracle
*
    IF <p0001>-pernr EQ '00021945'.
      CLEAR: department_name.
    ENDIF.
*JMB20220207 insert end

    CONCATENATE /sew/cl_mig_utils=>merge
                workterms
                sys_id
                src_id
                src_sys_id
                massn
                begda_tmp
                endda_tmp
                '1'
                /sew/cl_mig_utils=>yes
                job
                assign_num
                assign_status
                assignment_type
                business_code
                worker_category
                assignment_category
                ''
                location_code
                is_manager
                department_name
                period_of_ser
                begda_tmp
                worker_type
                legal_employer_name
                person_type_code
                system_person_type
                massg
                internal_number
    INTO DATA(data_tmp) SEPARATED BY /sew/cl_mig_utils=>separator.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
    CLEAR: internal_number, job, department_name.

**JMB20210802 start insert - build contract information, only for Hire entry
*
    CHECK massn NE 'ASG_CHANGE'.
    /sew/cl_mig_contract=>map_cogl_data( EXPORTING pernr                = <p0001>-pernr
                                                   begda                = begda_tmp
                                                   assignment_source_id = src_id
                                                   source_system_owner  = sys_id
                                                   person_source_id     = src_sys_id
                                          CHANGING contract             = data_contract ).
*JMB20210802 end insert

  ENDLOOP.
ENDMETHOD.


METHOD map_mig_cofu_values.
  DATA: value_tmp TYPE /sew/dd_value.
  DATA(fields) = VALUE /sew/cl_int_infty_proc_xml=>t_fields( ( infty = /sew/cl_mig_utils=>it0001
                                                               field_sap = /sew/cl_mig_utils=>btrtl
                                                               value = p0001-btrtl )
                                                             ( infty = /sew/cl_mig_utils=>it0001
                                                               field_sap = /sew/cl_mig_utils=>werks
                                                               value = p0001-werks ) ).
  IF sy-mandt EQ /sew/cl_int_constants=>cofu_mandant-italy.
    "Process MASSN mapping
    value_tmp       = CONV #( p0001-btrtl ).
    /sew/cl_int_mapping=>process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = /sew/cl_mig_utils=>it0001
        field_sap      = /sew/cl_mig_utils=>btrtl
        field_oracle   = /sew/cl_mig_utils=>locationcode
        mapping_fields = CONV #( mapping_fields_btrtl )
        mapping_values = CONV #( mapping_values_btrtl )
      CHANGING
        value          = value_tmp ).
    location_code = value_tmp.
  ELSE.
    "Process WERKS/BTRTL mapping (LocationCode)
  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0001
      field_sap      = /sew/cl_mig_utils=>btrtl
      field_oracle   = /sew/cl_mig_utils=>locationcode
      mapping_fields = CONV #( mapping_fields_btrtl )
      fields         = fields
    CHANGING
      value          = location_code ).
    ENDIF.

  "Process MASSN mapping
  value_tmp       = CONV #( p0000-massn ).
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

  "Process MASSG mapping
  value_tmp = CONV #( p0000-massg ).
  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0000
      field_sap      = /sew/cl_mig_utils=>massg
      field_oracle   = /sew/cl_mig_utils=>reasoncode
      mapping_fields = CONV #( mapping_fields_massg )
      mapping_values = CONV #( mapping_values_massg )
    CHANGING
      value          = value_tmp ).

  massg = value_tmp.

  READ TABLE molga INTO DATA(molga_entry) INDEX 1.
  DATA(fields_gk) = VALUE /sew/cl_int_infty_proc_xml=>t_fields( ( infty     = /sew/cl_mig_utils=>it0001
                                                                  field_sap = /sew/cl_mig_utils=>persg
                                                                  value     = p0001-persg )
                                                                ( infty     = /sew/cl_mig_utils=>it0001
                                                                  field_sap = persk
                                                                  value     = p0001-persk )
                                                                ( infty     = /sew/cl_mig_utils=>it0001
                                                                  field_sap = 'MOLGA'
                                                                  value     = molga_entry-low )
                                                                ( infty     = /sew/cl_mig_utils=>it0001
                                                                  field_sap = 'ANSVH'
                                                                  value     = p0001-ansvh ) ).
  "Process workertype mapping
  "for Austria/Australia employee group is relevant for workertype
  DATA(worker_sap) = persk.
  value_tmp = CONV #( p0001-persk ).

  IF '03'    IN molga OR
    sy-mandt EQ /sew/cl_int_constants=>cofu_mandant-australia  OR
     sy-mandt = /sew/cl_int_constants=>cofu_mandant-newzealand.
    worker_sap = /sew/cl_mig_utils=>persg.
    value_tmp = CONV #( p0001-persg ).
  ENDIF.

  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0001
      field_sap      = worker_sap
      field_oracle   = /sew/cl_mig_utils=>workertype
      mapping_fields = CONV #( mapping_fields_persg_wt )
      mapping_values = CONV #( mapping_values_persg_wt )
      fields         = fields_gk
    CHANGING
      value          = value_tmp ).

  worker_type = value_tmp.

  "Process assignmenttype mapping
  "for Austria/Australie employee group is relevant for assignmenttype
  value_tmp = COND #( WHEN '03'     IN molga                                         THEN p0001-persg
                      WHEN sy-mandt EQ /sew/cl_int_constants=>cofu_mandant-australia THEN p0001-persg
                      ELSE p0001-persk ).

  worker_sap = COND #( WHEN sy-mandt EQ /sew/cl_int_constants=>cofu_mandant-newzealand THEN persk
                       ELSE worker_sap ).

  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0001
      field_sap      = worker_sap
      field_oracle   = assignmenttype
      mapping_fields = CONV #( mapping_fields_persg_at )
      mapping_values = CONV #( mapping_values_persg_at )
      fields         = fields_gk
    CHANGING
      value          = value_tmp ).
  assignment_type = value_tmp.

  CLEAR: value_tmp, worker_sap.
  "Process workercategory mapping
  value_tmp = SWITCH #( sy-mandt
                        WHEN /sew/cl_int_constants=>cofu_mandant-germany THEN p0001-abkrs
                        WHEN /sew/cl_int_constants=>cofu_mandant-austria THEN p0001-persk
                        WHEN /sew/cl_int_constants=>cofu_mandant-italy   THEN p0001-persk ).

  worker_sap = SWITCH /sew/dd_field( sy-mandt
                                     WHEN /sew/cl_int_constants=>cofu_mandant-germany    THEN 'ABKRS'
                                     WHEN /sew/cl_int_constants=>cofu_mandant-australia  THEN 'ABKRS'
                                     WHEN /sew/cl_int_constants=>cofu_mandant-newzealand THEN 'ABKRS'
                                     WHEN /sew/cl_int_constants=>cofu_mandant-austria    THEN persk
                                     WHEN /sew/cl_int_constants=>cofu_mandant-italy      THEN persk ).
  IF value_tmp  IS NOT INITIAL AND
     worker_sap IS NOT INITIAL.
    /sew/cl_int_mapping=>process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = /sew/cl_mig_utils=>it0001
        field_sap      = worker_sap
        field_oracle   = workercategory
        mapping_fields = CONV #( mapping_fields_persk_wc )
        mapping_values = CONV #( mapping_values_persk_wc )
      CHANGING
        value          = value_tmp ).

    worker_category    = value_tmp.
  ENDIF.

  "Process assignmentcategory mapping
  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0001
      field_sap      = persk
      field_oracle   = assignmentcategory
      mapping_fields = CONV #( mapping_fields_persk_ac )
      fields         = fields_gk
    CHANGING
      value          = assignment_category ).

  "Process assignmentpersontype mapping
  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0001
      field_sap      = persk
      field_oracle   = assignmentpersontype
      mapping_fields = CONV #( mapping_fields_persk_apt )
      fields         = fields_gk
    CHANGING
      value          = assignment_person_type_code ).

  "Process persontypecode mapping
  value_tmp = CONV #( p0001-persk ).
  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0001
      field_sap      = persk
      field_oracle   = persontypecode
      mapping_fields = CONV #( mapping_fields_persk_ptc )
      mapping_values = CONV #( mapping_values_persk_ptc )
      fields         = fields_gk
    CHANGING
      value          = value_tmp ).

  person_type_code = value_tmp.

**JMB20211031 start insert - pass assignment person type instead of person type code (C400129651-5748)
*
  IF assignment_person_type_code IS NOT INITIAL.
    person_type_code = assignment_person_type_code.
  ENDIF.
*JMB20211031 insert end

  "Process systempersontype mapping
  value_tmp = CONV #( p0001-persk ).
  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0001
      field_sap      = persk
      field_oracle   = systempersontype
      mapping_fields = CONV #( mapping_fields_persk_spt )
      mapping_values = CONV #( mapping_values_persk_spt )
      fields         = fields_gk
   CHANGING
      value          = value_tmp ).

  system_person_type = value_tmp.

  fields = VALUE /sew/cl_int_infty_proc_xml=>t_fields( ( infty     = /sew/cl_mig_utils=>it0000
                                                         field_sap = stat2
                                                         value     = p0000-stat2 )
                                                       ( infty     = /sew/cl_mig_utils=>it0001
                                                         field_sap = 'KOSTL'
                                                         value     = p0001-kostl )
                                                       ( infty     = /sew/cl_mig_utils=>it0000
                                                         field_sap = 'STAT1'
                                                         value     = p0000-stat1 )  ).

  "Process STAT2 mapping
  value_tmp = CONV #( p0000-stat2 ).
  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0000
      field_sap      = stat2
      field_oracle   = assignmentstatustypecode
      mapping_fields = CONV #( mapping_fields_stat2 )
      mapping_values = CONV #( mapping_values_stat2 )
      fields         = fields
    CHANGING
      value          = value_tmp ).

  assign_status      = value_tmp.

  "Process BUKRS Mapping
  value_tmp = CONV #( p0001-bukrs ).
  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0001
      field_sap      = /sew/cl_mig_utils=>bukrs
      field_oracle   = /sew/cl_mig_utils=>legalemployername
      mapping_fields = CONV #( mapping_fields_bukrs )
      mapping_values = CONV #( mapping_values_bukrs )
    CHANGING
      value          = value_tmp ).

  legal_employer_name = value_tmp.

**JMB20211011 start delete - Dominik have changed name of OrgUnits on Prod, so no mapping is needed
*
*  "Process department Mapping
*  value_tmp = CONV #( p0001-orgeh ).
*  /sew/cl_int_mapping=>process_mapping(
*    EXPORTING
*      import         = abap_false
*      export         = abap_true
*      infty          = /sew/cl_mig_utils=>it0001
*      field_sap      = /sew/cl_mig_utils=>orgeh
*      field_oracle   = /sew/cl_mig_utils=>departmentname
*      mapping_fields = CONV #( mapping_fields_department )
*      mapping_values = CONV #( mapping_values_department )
*    CHANGING
*      value          = value_tmp ).
*
*  department_name = value_tmp.
*JMB20211011 delete end, start insert
  LOOP AT hrp1000_orgeh ASSIGNING FIELD-SYMBOL(<hrp1000>) WHERE begda LE sy-datum AND
                                                                endda GE sy-datum AND
                                                                objid EQ p0001-orgeh.
    department_name = <hrp1000>-stext.
    EXIT.
  ENDLOOP.
*JMB20211011 insert end

  "Process BUKRS Mapping
  value_tmp = CONV #( p0001-bukrs ).
  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0001
      field_sap      = /sew/cl_mig_utils=>bukrs
      field_oracle   = /sew/cl_mig_utils=>businessunitshortcode
      mapping_fields = CONV #( mapping_fields_bukrs_bu )
      mapping_values = CONV #( mapping_values_bukrs_bu )
    CHANGING
      value          = value_tmp ).

  business_unit_code = value_tmp.

  CLEAR probationunit.

  IF p0016-prbzt NE 0.
    value_tmp       = CONV #( p0016-prbeh ).
    /sew/cl_int_mapping=>process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = '0016'
        field_sap      = 'PRBEH'
        field_oracle   = 'PROBATIONUNIT'
        mapping_fields = CONV #( mapping_fields_prbeh )
        mapping_values = CONV #( mapping_values_prbeh )
      CHANGING
        value          = value_tmp ).

    probationunit = value_tmp.
  ENDIF.

  value_tmp       = CONV #( p0016-kdgf2 ).
  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = '0016'
      field_sap      = 'KDGF2'
      field_oracle   = 'NOTICEPERIODUOM'
      mapping_fields = CONV #( mapping_fields_kdgf2 )
      mapping_values = CONV #( mapping_values_kdgf2 )
    CHANGING
      value          = value_tmp ).

  noticeuom = value_tmp.

**JMB20210811 start insert - check jobCode mapping
*
  IF job IS NOT INITIAL.
    value_tmp       = CONV #( job ).
    /sew/cl_int_mapping=>process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = /sew/cl_mig_utils=>it0001
        field_sap      = 'STELL'
        field_oracle   = 'JOBCODE'
        mapping_fields = CONV #( mapping_fields_job )
        mapping_values = CONV #( mapping_values_job )
      CHANGING
        value          = value_tmp ).
    job_code = value_tmp.
  ENDIF.
*JMB20210811 end insert

  "01.03.2021 - Final decision: for history provide default value
*  CHECK p0001-endda LT sy-datum.
  CHECK hire EQ abap_true.

  business_unit_code = /sew/cl_mig_utils=>default_business_unit.
  CLEAR: location_code, department_name, job_code.

  location_code = '#NULL'. "JMB20211213 I - To force update to empty value in Oracle
ENDMETHOD.


METHOD map_mig_cogu_values.
  DATA: value_tmp TYPE /sew/dd_value.
  DATA(fields) = VALUE /sew/cl_int_infty_proc_xml=>t_fields( ( infty = /sew/cl_mig_utils=>it0001
                                                               field_sap = /sew/cl_mig_utils=>btrtl
                                                               value = p0001-btrtl )
                                                             ( infty = /sew/cl_mig_utils=>it0001
                                                               field_sap = /sew/cl_mig_utils=>werks
                                                               value = p0001-werks ) ).
  "Process WERKS/BTRTL mapping (LocationCode)
  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0001
      field_sap      = /sew/cl_mig_utils=>btrtl
      field_oracle   = /sew/cl_mig_utils=>locationcode
      mapping_fields = CONV #( mapping_fields_btrtl )
      fields         = fields
    CHANGING
      value          = location_code ).

  "Process MASSN mapping
  value_tmp       = CONV #( p0000-massn ).
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

  "Process MASSG mapping
  value_tmp = CONV #( p0000-massg ).
  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0000
      field_sap      = /sew/cl_mig_utils=>massg
      field_oracle   = /sew/cl_mig_utils=>reasoncode
      mapping_fields = CONV #( mapping_fields_massg )
      mapping_values = CONV #( mapping_values_massg )
    CHANGING
      value          = value_tmp ).

  massg = value_tmp.

  READ TABLE molga INTO DATA(molga_entry) INDEX 1.
  DATA(fields_gk) = VALUE /sew/cl_int_infty_proc_xml=>t_fields( ( infty     = /sew/cl_mig_utils=>it0001
                                                                  field_sap = /sew/cl_mig_utils=>persg
                                                                  value     = p0001-persg )
                                                                ( infty     = /sew/cl_mig_utils=>it0001
                                                                  field_sap = persk
                                                                  value     = p0001-persk )
                                                                ( infty     = /sew/cl_mig_utils=>it0001
                                                                  field_sap = 'MOLGA'
                                                                  value     = molga_entry-low )
                                                                ( infty     = /sew/cl_mig_utils=>it0001
                                                                  field_sap = 'ANSVH'
                                                                  value     = p0001-ansvh ) ).
  "Process workertype mapping
  "for Austria/Australia employee group is relevant for workertype
  DATA(worker_sap) = persk.
  value_tmp = CONV #( p0001-persk ).

  IF '03'    IN molga OR
    sy-mandt EQ /sew/cl_int_constants=>cofu_mandant-australia  OR
     sy-mandt = /sew/cl_int_constants=>cofu_mandant-newzealand.
    worker_sap = /sew/cl_mig_utils=>persg.
    value_tmp = CONV #( p0001-persg ).
  ENDIF.

  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0001
      field_sap      = worker_sap
      field_oracle   = /sew/cl_mig_utils=>workertype
      mapping_fields = CONV #( mapping_fields_persg_wt )
      mapping_values = CONV #( mapping_values_persg_wt )
      fields         = fields_gk
    CHANGING
      value          = value_tmp ).

  worker_type = value_tmp.

"Process assignmenttype mapping
  "for Austria/Australie employee group is relevant for assignmenttype
  value_tmp = COND #( WHEN '03'     IN molga                                         THEN p0001-persg
                      WHEN sy-mandt EQ /sew/cl_int_constants=>cofu_mandant-australia THEN p0001-persg
                      ELSE p0001-persk ).

  worker_sap = COND #( WHEN sy-mandt EQ /sew/cl_int_constants=>cofu_mandant-newzealand THEN persk
                       ELSE worker_sap ).

  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0001
      field_sap      = worker_sap
      field_oracle   = assignmenttype
      mapping_fields = CONV #( mapping_fields_persg_at )
      mapping_values = CONV #( mapping_values_persg_at )
      fields         = fields_gk
    CHANGING
      value          = value_tmp ).
  assignment_type = value_tmp.

  CLEAR: value_tmp, worker_sap.
  "Process workercategory mapping
  value_tmp = SWITCH #( sy-mandt
                        WHEN /sew/cl_int_constants=>cofu_mandant-germany THEN p0001-abkrs
                        WHEN /sew/cl_int_constants=>cofu_mandant-austria THEN p0001-persk
                        WHEN /sew/cl_int_constants=>cofu_mandant-italy   THEN p0001-persk ).

  worker_sap = SWITCH /sew/dd_field( sy-mandt
                                     WHEN /sew/cl_int_constants=>cofu_mandant-germany    THEN 'ABKRS'
                                     WHEN /sew/cl_int_constants=>cofu_mandant-australia  THEN 'ABKRS'
                                     WHEN /sew/cl_int_constants=>cofu_mandant-newzealand THEN 'ABKRS'
                                     WHEN /sew/cl_int_constants=>cofu_mandant-austria    THEN persk
                                     WHEN /sew/cl_int_constants=>cofu_mandant-italy      THEN persk ).
  IF value_tmp  IS NOT INITIAL AND
     worker_sap IS NOT INITIAL.
    /sew/cl_int_mapping=>process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = /sew/cl_mig_utils=>it0001
        field_sap      = worker_sap
        field_oracle   = workercategory
        mapping_fields = CONV #( mapping_fields_persk_wc )
        mapping_values = CONV #( mapping_values_persk_wc )
      CHANGING
        value          = value_tmp ).

    worker_category    = value_tmp.
  ENDIF.

  "Process assignmentcategory mapping
  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0001
      field_sap      = persk
      field_oracle   = assignmentcategory
      mapping_fields = CONV #( mapping_fields_persk_ac )
      fields         = fields_gk
    CHANGING
      value          = assignment_category ).

  "Process assignmentpersontype mapping
  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0001
      field_sap      = persk
      field_oracle   = assignmentpersontype
      mapping_fields = CONV #( mapping_fields_persk_apt )
      fields         = fields_gk
    CHANGING
      value          = assignment_person_type_code ).

  "Process persontypecode mapping
  value_tmp = CONV #( p0001-persk ).
  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0001
      field_sap      = persk
      field_oracle   = persontypecode
      mapping_fields = CONV #( mapping_fields_persk_ptc )
      mapping_values = CONV #( mapping_values_persk_ptc )
      fields         = fields_gk
    CHANGING
      value          = value_tmp ).

  person_type_code = value_tmp.

**JMB20211031 start insert - pass assignment person type instead of person type code (C400129651-5748)
*
  IF assignment_person_type_code IS NOT INITIAL.
    person_type_code = assignment_person_type_code.
  ENDIF.
*JMB20211031 insert end

  "Process systempersontype mapping
  value_tmp = CONV #( p0001-persk ).
  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0001
      field_sap      = persk
      field_oracle   = systempersontype
      mapping_fields = CONV #( mapping_fields_persk_spt )
      mapping_values = CONV #( mapping_values_persk_spt )
      fields         = fields_gk
   CHANGING
      value          = value_tmp ).

  system_person_type = value_tmp.

  fields = VALUE /sew/cl_int_infty_proc_xml=>t_fields( ( infty     = /sew/cl_mig_utils=>it0000
                                                         field_sap = stat2
                                                         value     = p0000-stat2 )
                                                       ( infty     = /sew/cl_mig_utils=>it0001
                                                         field_sap = 'KOSTL'
                                                         value     = p0001-kostl )
                                                       ( infty     = /sew/cl_mig_utils=>it0000
                                                         field_sap = 'STAT1'
                                                         value     = p0000-stat1 )  ).

  "Process STAT2 mapping
  value_tmp = CONV #( p0000-stat2 ).
  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0000
      field_sap      = stat2
      field_oracle   = assignmentstatustypecode
      mapping_fields = CONV #( mapping_fields_stat2 )
      mapping_values = CONV #( mapping_values_stat2 )
      fields         = fields
    CHANGING
      value          = value_tmp ).

  assign_status      = value_tmp.

  "Process BUKRS Mapping
  value_tmp = CONV #( p0001-bukrs ).
  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0001
      field_sap      = /sew/cl_mig_utils=>bukrs
      field_oracle   = /sew/cl_mig_utils=>legalemployername
      mapping_fields = CONV #( mapping_fields_bukrs )
      mapping_values = CONV #( mapping_values_bukrs )
    CHANGING
      value          = value_tmp ).

  legal_employer_name = value_tmp.

**JMB20211011 start delete - Dominik have changed name of OrgUnits on Prod, so no mapping is needed
*
*  "Process department Mapping
*  value_tmp = CONV #( p0001-orgeh ).
*  /sew/cl_int_mapping=>process_mapping(
*    EXPORTING
*      import         = abap_false
*      export         = abap_true
*      infty          = /sew/cl_mig_utils=>it0001
*      field_sap      = /sew/cl_mig_utils=>orgeh
*      field_oracle   = /sew/cl_mig_utils=>departmentname
*      mapping_fields = CONV #( mapping_fields_department )
*      mapping_values = CONV #( mapping_values_department )
*    CHANGING
*      value          = value_tmp ).
*
*  department_name = value_tmp.
*JMB20211011 delete end, start insert
  LOOP AT hrp1000_orgeh ASSIGNING FIELD-SYMBOL(<hrp1000>) WHERE begda LE sy-datum AND
                                                                endda GE sy-datum AND
                                                                objid EQ p0001-orgeh.
    department_name = <hrp1000>-stext.
    EXIT.
  ENDLOOP.
*JMB20211011 insert end

  "Process BUKRS Mapping
  value_tmp = CONV #( p0001-bukrs ).
  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0001
      field_sap      = /sew/cl_mig_utils=>bukrs
      field_oracle   = /sew/cl_mig_utils=>businessunitshortcode
      mapping_fields = CONV #( mapping_fields_bukrs_bu )
      mapping_values = CONV #( mapping_values_bukrs_bu )
    CHANGING
      value          = value_tmp ).

  business_unit_code = value_tmp.

**JMB20210811 start insert - check jobCode mapping
*
  IF job IS NOT INITIAL.
    value_tmp       = CONV #( job ).
    /sew/cl_int_mapping=>process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = /sew/cl_mig_utils=>it0001
        field_sap      = 'STELL'
        field_oracle   = 'JOBCODE'
        mapping_fields = CONV #( mapping_fields_job )
        mapping_values = CONV #( mapping_values_job )
      CHANGING
        value          = value_tmp ).
    job_code = value_tmp.
  ENDIF.
*JMB20210811 end insert

  "01.03.2021 - Final decision: for history provide default value
*  CHECK p0001-endda LT sy-datum.
  CHECK hire EQ abap_true.

  business_unit_code = /sew/cl_mig_utils=>default_business_unit.
  CLEAR: location_code, department_name, job_code.
  location_code = '#NULL'. "JMB20220112 I - To force update to empty value in Oracle
ENDMETHOD.


METHOD map_mig_values.
  DATA: value_tmp TYPE /sew/dd_value.
  DATA(fields) = VALUE /sew/cl_int_infty_proc_xml=>t_fields( ( infty = /sew/cl_mig_utils=>it0001
                                                               field_sap = /sew/cl_mig_utils=>btrtl
                                                               value = p0001-btrtl )
                                                             ( infty = /sew/cl_mig_utils=>it0001
                                                               field_sap = /sew/cl_mig_utils=>werks
                                                               value = p0001-werks ) ).
  "Process WERKS/BTRTL mapping (LocationCode)
  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0001
      field_sap      = /sew/cl_mig_utils=>btrtl
      field_oracle   = /sew/cl_mig_utils=>locationcode
      mapping_fields = CONV #( mapping_fields_btrtl )
      fields         = fields
    CHANGING
      value          = location_code ).

  "Process MASSN mapping
  value_tmp       = CONV #( p0000-massn ).
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

  "Process MASSG mapping
  value_tmp = CONV #( p0000-massg ).
  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0000
      field_sap      = /sew/cl_mig_utils=>massg
      field_oracle   = /sew/cl_mig_utils=>reasoncode
      mapping_fields = CONV #( mapping_fields_massg )
      mapping_values = CONV #( mapping_values_massg )
    CHANGING
      value          = value_tmp ).

  massg = value_tmp.

  "Process workertype mapping
  value_tmp = CONV #( p0001-persg ).
  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0001
      field_sap      = /sew/cl_mig_utils=>persg
      field_oracle   = /sew/cl_mig_utils=>workertype
      mapping_fields = CONV #( mapping_fields_persg_wt )
      mapping_values = CONV #( mapping_values_persg_wt )
    CHANGING
      value          = value_tmp ).

  worker_type = value_tmp.

  "Process assignmenttype mapping
  value_tmp = CONV #( p0001-persg ).
  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0001
      field_sap      = /sew/cl_mig_utils=>persg
      field_oracle   = assignmenttype
      mapping_fields = CONV #( mapping_fields_persg_at )
      mapping_values = CONV #( mapping_values_persg_at )
    CHANGING
      value          = value_tmp ).

  assignment_type    = value_tmp.

  "Process workercategory mapping
  value_tmp = CONV #( p0001-persk ).
  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0001
      field_sap      = persk
      field_oracle   = workercategory
      mapping_fields = CONV #( mapping_fields_persk_wc )
      mapping_values = CONV #( mapping_values_persk_wc )
    CHANGING
      value          = value_tmp ).

  worker_category    = value_tmp.

  "Process assignmentcategory mapping
  value_tmp = CONV #( p0001-persk ).
  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0001
      field_sap      = persk
      field_oracle   = assignmentcategory
      mapping_fields = CONV #( mapping_fields_persk_ac )
      mapping_values = CONV #( mapping_values_persk_ac )
    CHANGING
      value          = value_tmp ).

  assignment_category = value_tmp.

  "Process assignmentcategory mapping
  value_tmp = CONV #( p0001-persk ).
  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0001
      field_sap      = persk
      field_oracle   = persontypecode
      mapping_fields = CONV #( mapping_fields_persk_ptc )
      mapping_values = CONV #( mapping_values_persk_ptc )
    CHANGING
      value          = value_tmp ).

  person_type_code = value_tmp.

  "Process assignmentcategory mapping
  value_tmp = CONV #( p0001-persk ).
  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0001
      field_sap      = persk
      field_oracle   = systempersontype
      mapping_fields = CONV #( mapping_fields_persk_spt )
      mapping_values = CONV #( mapping_values_persk_spt )
   CHANGING
      value          = value_tmp ).

  system_person_type = value_tmp.

  "Process STAT2 mapping
  value_tmp = CONV #( p0000-stat2 ).
  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0000
      field_sap      = stat2
      field_oracle   = assignmentstatustypecode
      mapping_fields = CONV #( mapping_fields_stat2 )
      mapping_values = CONV #( mapping_values_stat2 )
    CHANGING
      value          = value_tmp ).

  assign_status      = value_tmp.

  "Process BUKRS Mapping
  value_tmp = CONV #( p0001-bukrs ).
  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0001
      field_sap      = /sew/cl_mig_utils=>bukrs
      field_oracle   = /sew/cl_mig_utils=>legalemployername
      mapping_fields = CONV #( mapping_fields_bukrs )
      mapping_values = CONV #( mapping_values_bukrs )
    CHANGING
      value          = value_tmp ).

  legal_employer_name = value_tmp.

  "Process department Mapping
  value_tmp = CONV #( p0001-orgeh ).
  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0001
      field_sap      = /sew/cl_mig_utils=>orgeh
      field_oracle   = /sew/cl_mig_utils=>departmentname
      mapping_fields = CONV #( mapping_fields_department )
      mapping_values = CONV #( mapping_values_department )
    CHANGING
      value          = value_tmp ).

  department_name = value_tmp.

  "Process BUKRS Mapping
  value_tmp = CONV #( p0001-bukrs ).
  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = /sew/cl_mig_utils=>it0001
      field_sap      = /sew/cl_mig_utils=>bukrs
      field_oracle   = /sew/cl_mig_utils=>businessunitshortcode
      mapping_fields = CONV #( mapping_fields_bukrs_bu )
      mapping_values = CONV #( mapping_values_bukrs_bu )
    CHANGING
      value          = value_tmp ).

  business_unit_code = value_tmp.

  "01.03.2021 - Final decision: for history provide default value
*  CHECK p0001-endda LT sy-datum.
  CHECK hire EQ abap_true.

  business_unit_code = /sew/cl_mig_utils=>default_business_unit.
  CLEAR: location_code, department_name.
ENDMETHOD.


METHOD proceed_cofu_work_terms.
  p0000           = worker->p0000.
  p0001           = assignment->p0001.
  p0016           = assignment->p0016.
  p0105           = assignment->p0105.
  hrp1000_orgeh   = assignment->hrp1000_orgeh.
  hrp1000_stell   = assignment->hrp1000_stell.
  bu_country      = assignment->bu_country.
  bukrs_txt       = assignment->bukrs_txt.
  me->vp_src_id   = vp_src_id.
  me->vp_wkr_id   = vp_wkr_id.
  get_mapping_cofu_fields( ).
  get_mapping_cofu_values( ).
ENDMETHOD.


METHOD proceed_cogl_work_terms.
  me->vp_src_id = vp_src_id.
  me->vp_wkr_id = vp_wkr_id.
  p0000 = worker->p0000.

  get_cogl_data( ).
  get_mapping_fields( ).
  get_mapping_values( ).

  /sew/cl_mig_utils=>update_begin_date( EXPORTING p0000       = p0000
                                                  create_hire = abap_true
                                         CHANGING p0001 = p0001 ).

  DATA(hire_data) = create_hire_entry( ).
  /sew/cl_mig_utils=>check_assign_supervisor( EXPORTING all_periods = abap_true
                                               CHANGING p0001       = p0001 ).
  data = map_cogl_data( ).
  vp_wterm_id = me->vp_wterm_col.

  CONCATENATE hire_data data INTO data.
ENDMETHOD.


METHOD proceed_cogu_work_terms.
  me->vp_src_id = vp_src_id.
  me->vp_wkr_id = vp_wkr_id.
  p0000 = worker->p0000.

  get_cofu_data( ).
  get_mapping_cogu_fields( ).
  get_mapping_cogu_values( ).

  /sew/cl_mig_utils=>update_begin_date( EXPORTING p0000       = p0000
                                                  create_hire = abap_true
                                         CHANGING p0001 = p0001 ).

  /sew/cl_mig_utils=>check_assign_supervisor( EXPORTING all_periods = abap_true
                                               CHANGING p0001       = p0001 ).
  DATA(hire_data) = create_hire_cogu_entry( ).
  data = map_cogu_data( ).
  vp_wterm_id = me->vp_wterm_col.

  CONCATENATE hire_data data INTO data.
  data_contract = me->data_contract.
ENDMETHOD.
ENDCLASS.
