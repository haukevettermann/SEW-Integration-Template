class /SEW/CL_MIG_POSITION_DATA definition
  public
  create public .

public section.

  data OBJECTS type OBJEC_T .
  data BEGDA type BEGDA .
  data ENDDA type ENDDA .
  constants ORG_INFO type STRING value 'OrgInformation' ##NO_TEXT.
  constants PER_ORG_INFO type STRING value 'PER_ORG_MANAGER_INFO' ##NO_TEXT.
  constants DEPARTMENT_CODE type STRING value 'DEPARTMENT' ##NO_TEXT.
  constants DEPARTMENT_CLASS type STRING value 'Department' ##NO_TEXT.
  constants SET type STRING value '_SET' ##NO_TEXT.
  constants DE_FILE type CHAR255 value 'POSITION_DE.zip' ##NO_TEXT.
  constants AT_FILE type CHAR255 value 'POSITION_AT.zip' ##NO_TEXT.
  constants IT_FILE type CHAR255 value 'POSITION_IT.zip' ##NO_TEXT.
  constants FR_FILE type CHAR255 value 'POSITION_FR.zip' ##NO_TEXT.
  constants NL_FILE type CHAR255 value 'POSITION_NL.zip' ##NO_TEXT.

  methods DOWNLOAD_FILES
    importing
      !FILES type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  methods CONSTRUCTOR
    importing
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !OBJEC type OBJEC_T
      !AL11 type BOOLEAN .
  methods PROCEED_POSITION_DATA
    exporting
      !POSITION_DATA_MIG type STRING
      !POSITION_HIER_DATA_MIG type STRING .
  PROTECTED SECTION.
private section.

  data MOLGA_COUNTRY_CODES type T77SFEC_T500L_TAB .
  data COST_CENTERS type HRBPREP_COST_DIST_IT .
  data MAPPING_VALUES_BUKRS type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPPING .
  data MAPPING_FIELDS_BUKRS type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPP_FI .
  data ORGID_COUNTRY_CODE type /SEW/TT_MIG_ORGID_CC .
  data MOLGA type RSDSSELOPT_T .
  data BUKRS_TXT type T_T001 .
  data COST_CENTERS_TMP type HRBPREP_COST_DIST_IT .
  constants ORG_INFO_PREFIX type STRING value '_OrgInfo' ##NO_TEXT.
  data MAPPING_FIELDS_POSITION type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPP_FI .
  data MAPPING_VALUES_POSITION type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPPING .
  constants ORG_SOURCE_TYPE type STRING value 'ORG' ##NO_TEXT.
  constants COST type STRING value 'COST' ##NO_TEXT.
  constants LEG_DATA_GROUP_NAME type STRING value 'LDG' ##NO_TEXT.
  data PA0001 type P0001_TAB .
  data AL11 type BOOLEAN .
  data PARENT_ORGUNIT type HRP1001_T .
  data PA9400_PERNR type /SEW/P9400_TAB .
  data MANAGER_ASSIGNMENT type /SEW/TT_POSITION_MANAGER_DATA .

  methods GET_JOB_CODE .
  methods MAP_HR_PERIODS
    importing
      value(HR_PERIODS) type HRPERIODS_TAB
    exporting
      !OM_DATA_FUT type /SEW/TT_MIG_OM_DATA
    changing
      !OM_DATA_ENTRY type /SEW/S_MIG_OM_DATA
    returning
      value(OM_DATA) type /SEW/TT_MIG_OM_DATA .
  methods GET_KOSTL_OF_FIRST_EMP
    importing
      !OBJID type OBJID
      !BEGDA type BEGDA
      !ENDDA type ENDDA
    returning
      value(KOSTL) type KOSTL .
  methods GET_HR_PERIODS
    importing
      !OBJID type HROBJID
      !BEGDA type BEGDA
      !ENDDA type ENDDA
    returning
      value(HR_PERIODS) type HRPERIODS_TAB .
  methods GET_ACCOUNT_ASSIGNMENT .
  methods CREATE_MIG_FILE
    importing
      !DATA type /SEW/TT_MIG_OM_DATA
    returning
      value(OM_DATA) type STRING .
  methods CREATE_METADATA
    returning
      value(METADATA) type STRING .
  methods CREATE_DATA
    importing
      !DATA type /SEW/TT_MIG_OM_DATA
    returning
      value(OM_DATA) type STRING .
  methods GET_MAPPING_VALUES .
  methods GET_MAPPING_FIELDS .
  methods GET_HEADER_LINE
    returning
      value(HEADER) type STRING .
  methods GET_COUNTRY_TO_ORG_UNIT .
  methods GET_POSITION_DATA .
  methods MAP_POSITION_DATA
    exporting
      !POSITION_DATA_MIG type STRING .
  methods GET_BUKRS_TXT .
ENDCLASS.



CLASS /SEW/CL_MIG_POSITION_DATA IMPLEMENTATION.


METHOD CONSTRUCTOR.
  me->begda = begda.
  me->endda = endda.
  me->objects = objec.
  me->al11  = al11.
ENDMETHOD.


METHOD CREATE_DATA.

  DATA: seq_number      TYPE string,
        src_id          TYPE string,
        count_s         TYPE string,
        department_name TYPE string,
        leg_code        TYPE string,
        value_tmp       TYPE /sew/dd_value,
        count           TYPE i VALUE 1,
        sys_id          TYPE string.

  sys_id = 'EBS-HR'.

  LOOP AT data ASSIGNING FIELD-SYMBOL(<data>).

    count_s = CONV #( count ).
    CONDENSE count_s.
    CONCATENATE <data>-integration_key org_info_prefix INTO src_id.

    "Process department Mapping
    value_tmp = CONV #( <data>-integration_key ).
    /sew/cl_int_mapping=>process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = /sew/cl_mig_utils=>it0001
        field_sap      = /sew/cl_mig_utils=>orgeh
        field_oracle   = /sew/cl_mig_utils=>departmentname
        mapping_fields = CONV #( mapping_fields_position )
        mapping_values = CONV #( mapping_values_position )
      CHANGING
        value          = value_tmp ).

    department_name = value_tmp.

    CHECK department_name IS NOT INITIAL.

    CONCATENATE /sew/cl_mig_utils=>merge
                org_info
                per_org_info
                department_code
                <data>-effective_start_date
                <data>-effective_end_date
                department_name
                department_code
                per_org_info
                department_class
                leg_code
                seq_number
                sys_id
                src_id
                <data>-oracleid
    INTO DATA(data_tmp) SEPARATED BY /sew/cl_mig_utils=>separator.

    ADD 1 TO count.

    CONCATENATE om_data cl_abap_char_utilities=>newline data_tmp INTO om_data.

  ENDLOOP.
ENDMETHOD.


METHOD CREATE_METADATA.

  CONCATENATE /sew/cl_mig_utils=>metadata
              'OrgInformation'
              'FLEX:PER_ORGANIZATION_INFORMATION_EFF'
              'EFF_CATEGORY_CODE'
              'EffectiveStartDate'
              'EffectiveEndDate'
              'OrganizationName'
              'ClassificationCode'
              'OrgInformationContext'
              'ClassificationName'
              'LegislationCode'
              'SequenceNumber'
              'SourceSystemOwner'
              'SourceSystemId'
              '_MANAGER(PER_ORGANIZATION_INFORMATION_EFF=PER_ORG_MANAGER_INFO)'
   INTO metadata SEPARATED BY /sew/cl_mig_utils=>separator.

ENDMETHOD.


METHOD CREATE_MIG_FILE.
  DATA(metadata)  = create_metadata( ).
  DATA(data_conv) = create_data( data ).

  CONCATENATE metadata cl_abap_char_utilities=>newline data_conv INTO om_data.
ENDMETHOD.


METHOD DOWNLOAD_FILES.

  DATA: content   TYPE stringtab,
        filename  TYPE string,
        content_x TYPE xstring,
        zip_file  TYPE string,
        zip_tab   TYPE swxmlcont.

  CASE al11.
    WHEN abap_true.
      DATA(zip) = NEW cl_abap_zip( ).

      LOOP AT files ASSIGNING FIELD-SYMBOL(<files_zip>).
        CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
          EXPORTING
            text   = <files_zip>-value
          IMPORTING
            buffer = content_x.

        zip->add( name    = <files_zip>-name
                  content = content_x ).
      ENDLOOP.

      DATA(zip_xstring) =  zip->save( ).

      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer     = zip_xstring
        TABLES
          binary_tab = zip_tab.

      zip_file = SWITCH #( sy-mandt
                           WHEN /sew/cl_int_constants=>cofu_mandant-france      THEN fr_file
                           WHEN /sew/cl_int_constants=>cofu_mandant-germany     THEN de_file
                           WHEN /sew/cl_int_constants=>cofu_mandant-netherlands THEN nl_file ).
      "Austria and Italy are on the same mandant
      CASE sy-mandt.
        WHEN /sew/cl_int_constants=>cofu_mandant-italy.
          IF molga IS NOT INITIAL.
            "Austria
            IF     '03' IN molga.
              zip_file = at_file.
            "Italy
            ELSEIF '15' IN molga.
              zip_file = it_file.
            ENDIF.
          ENDIF.
      ENDCASE.

      CHECK zip_file IS NOT INITIAL.

      DATA(logical_filename) = CONV filename-fileintern('/SEW/HCM_ORACLE_EXTRACTS_COGU').
      CALL FUNCTION 'FILE_GET_NAME'
        EXPORTING
          logical_filename = logical_filename
          parameter_1      = zip_file
        IMPORTING
          file_name        = filename
        EXCEPTIONS
          file_not_found   = 1
          OTHERS           = 2.

      CHECK filename IS NOT INITIAL.

      OPEN DATASET filename FOR OUTPUT IN BINARY MODE.
      CHECK sy-subrc IS INITIAL.

      LOOP AT zip_tab ASSIGNING FIELD-SYMBOL(<zip_tab>).
        TRANSFER <zip_tab> TO filename.
      ENDLOOP.
      CLOSE DATASET filename.
    WHEN abap_false.

      LOOP AT files ASSIGNING FIELD-SYMBOL(<files>).
        APPEND <files>-value TO content.
        CALL FUNCTION 'GUI_DOWNLOAD'
          EXPORTING
            filename = <files>-name
            filetype = 'DAT'
            codepage = '4110' "UTF-8
          TABLES
            data_tab = content.
        CLEAR: content.
      ENDLOOP.
  ENDCASE.

ENDMETHOD.


METHOD GET_ACCOUNT_ASSIGNMENT.

*  DATA: struc     TYPE struc_t.
*
*  "relevant otypes
*  DATA(otype_rel) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = 'IC' )
*                                        ( sign = 'I' option = 'EQ' low = 'I1' ) ).
*
*  LOOP AT objec ASSIGNING FIELD-SYMBOL(<objec>).
*    "get account assignment of orgunit
*    CALL FUNCTION 'RH_STRUC_GET'
*      EXPORTING
*        act_otype      = <objec>-otype
*        act_objid      = <objec>-objid
*        act_wegid      = 'OMACC_U'
*        act_plvar      = <objec>-plvar
*        act_begda      = <objec>-begda
*        act_endda      = <objec>-endda
*      TABLES
*        result_struc   = struc
*      EXCEPTIONS
*        no_entry_found = 1
*        no_plvar_found = 2.
*
*    "get company code and personnel subareas
*    DELETE struc WHERE otype NOT IN otype_rel.
*    SORT struc BY otype objid vbegda vendda.
*    DELETE ADJACENT DUPLICATES FROM struc COMPARING otype objid vbegda vendda.
*
*    "collect all personnel areas
*    APPEND LINES OF VALUE p1008_tab( FOR <struc_br> IN struc WHERE ( otype EQ 'I1' ) ( objid = <objec>-objid
*                                                                                       begda = <struc_br>-vbegda
*                                                                                       endda = <struc_br>-vendda
*                                                                                       btrtl = <struc_br>-objid+4(4) ) ) TO hrp1008_br.
*
*    "collect all company codes
*    APPEND LINES OF VALUE p1008_tab( FOR <struc_br> IN struc WHERE ( otype EQ 'IC' ) ( objid = <objec>-objid
*                                                                                       begda = <struc_br>-vbegda
*                                                                                       endda = <struc_br>-vendda
*                                                                                       bukrs = <struc_br>-objid ) ) to hrp1008_com.
*
*    CLEAR: struc.
*  ENDLOOP.

ENDMETHOD.


METHOD GET_BUKRS_TXT.
  DATA(bukrs) = VALUE rsdsselopt_t( FOR <bukrs> IN bukrs_txt ( sign = 'I' option = 'EQ' low = <bukrs>-bukrs ) ).

  CLEAR bukrs_txt.

  SELECT bukrs, butxt FROM t001 INTO CORRESPONDING FIELDS OF TABLE @bukrs_txt WHERE bukrs IN @bukrs.
ENDMETHOD.


METHOD GET_COUNTRY_TO_ORG_UNIT.

  DATA: org_unit_id           TYPE pd_objid_r,
        results               TYPE tswhactor,
        main_org_country_code TYPE /sew/tt_mig_orgid_cc,
        country_code          TYPE intca,
        country_codes         TYPE rsdsselopt_t.

  LOOP AT objects ASSIGNING FIELD-SYMBOL(<object>).
    DATA(objid) = <object>-objid.
    CALL FUNCTION '/SEW/HROMFB_GET_ABTEILUNG_NEW'
      EXPORTING
        plvar          = <object>-plvar
        otype          = <object>-otype
        objid          = <object>-objid
        begda          = <object>-begda
        endda          = <object>-endda
      IMPORTING
        geschaeftsfuehrung = org_unit_id
      EXCEPTIONS
        objid_initial  = 1
        not_found      = 2
        err_infty1003  = 3
        internal_error = 4
        no_plvar       = 5.

    "check if main org unit already was readed
    LOOP AT main_org_country_code ASSIGNING FIELD-SYMBOL(<main_org_unit>) WHERE objid EQ org_unit_id   AND
                                                                                begda LE <object>-endda AND
                                                                                endda GE <object>-begda.
      country_code = <main_org_unit>-land1.
      EXIT.
    ENDLOOP.

    IF <main_org_unit> IS NOT ASSIGNED.

      "get account assignment details of main orgunit
      CALL FUNCTION 'RH_STRUC_GET'
        EXPORTING
          act_otype      = <object>-otype
          act_objid      = org_unit_id
          act_wegid      = 'OMACC_U'
          act_plvar      = <object>-plvar
          act_begda      = <object>-begda
          act_endda      = <object>-endda
        TABLES
          result_tab     = results
        EXCEPTIONS
          no_entry_found = 1
          no_plvar_found = 2.

      "get company code
      READ TABLE results INTO DATA(result) WITH KEY otype = 'IC'.

      CALL FUNCTION 'BBP_GET_COMPANY_COUNTRY'
        EXPORTING
          iv_company = CONV bukrs( result-objid )
        IMPORTING
          ev_country = country_code.

      APPEND VALUE #( objid = org_unit_id
                      begda = <object>-begda
                      endda = <object>-endda
                      land1 = CONV #( country_code ) ) TO main_org_country_code.

      APPEND VALUE #( sign = 'I' option = 'EQ' low = country_code ) TO country_codes.
      APPEND VALUE #( bukrs = result-objid ) TO bukrs_txt.

    ENDIF.

    APPEND VALUE #( objid = <object>-objid
                    begda = <object>-begda
                    endda = <object>-endda
                    land1 = CONV #( country_code ) ) TO orgid_country_code.

    CLEAR: country_code.
    UNASSIGN: <main_org_unit>.
  ENDLOOP.

  "get molga according to country code
  SORT country_codes BY low.
  DELETE ADJACENT DUPLICATES FROM country_codes COMPARING low.

  SELECT molga, intca FROM t500l INTO TABLE @molga_country_codes WHERE intca IN @country_codes.

  me->molga = VALUE rsdsselopt_t( FOR <molga> IN molga_country_codes ( sign = 'I' option = 'EQ' low = <molga>-molga ) ).
ENDMETHOD.


  METHOD GET_HEADER_LINE.

    CONCATENATE   'Integration Key'
                  'Effective Start Date'
                  'Effective End Date'
                  'Department Set'
                  'Name'
                  'Status'
                  'Action Reason'
                  'Internal Address Line'
                  'Location'
                  'Name Short'
                  'Manager'
                  'OracleId'
                  'Company'
                  'Company Name'
                  'Cost Center'
                  'Parent Integration Key'
                  'Country'
    INTO header SEPARATED BY ';'.

  ENDMETHOD.


METHOD GET_HR_PERIODS.
  "default time range
  APPEND VALUE #( begda = begda
                  endda = endda ) TO hr_periods.

  LOOP AT pa0001 ASSIGNING FIELD-SYMBOL(<pa0001>) WHERE orgeh EQ objid.
    APPEND VALUE #( begda = <pa0001>-begda
                    endda = <pa0001>-endda ) TO hr_periods.
*    APPEND <pa0001> TO pa0001_tmp.
  ENDLOOP.

*  LOOP AT hrp1001_up ASSIGNING FIELD-SYMBOL(<hrp1001_up>) WHERE objid EQ objid.
*    <hrp1001_up>-endda = COND #( WHEN <hrp1001_up>-endda GT endda
*                                 THEN endda
*                                 ELSE <hrp1001_up>-endda ).
*    APPEND VALUE #( begda = <hrp1001_up>-begda
*                    endda = <hrp1001_up>-endda ) TO hr_periods.
*    APPEND <hrp1001_up> TO hrp1001_up_tmp.
*  ENDLOOP.

*  LOOP AT hrp1008_br ASSIGNING FIELD-SYMBOL(<hrp1008_br>) WHERE objid EQ objid.
*    <hrp1008_br>-endda = COND #( WHEN <hrp1008_br>-endda GT endda
*                                 THEN endda
*                                 ELSE <hrp1008_br>-endda ).
*    APPEND VALUE #( begda = <hrp1008_br>-begda
*                    endda = <hrp1008_br>-endda ) TO hr_periods.
*    APPEND <hrp1008_br> TO hrp1008_br_tmp.
*  ENDLOOP.

*  LOOP AT hrp1008_com ASSIGNING FIELD-SYMBOL(<hrp1008_com>) WHERE objid EQ objid.
*    <hrp1008_com>-endda = COND #( WHEN <hrp1008_com>-endda GT endda
*                                  THEN endda
*                                  ELSE <hrp1008_com>-endda ).
*    APPEND VALUE #( begda = <hrp1008_com>-begda
*                    endda = <hrp1008_com>-endda ) TO hr_periods.
*    APPEND <hrp1008_com> TO hrp1008_com_tmp.
*  ENDLOOP.

  LOOP AT cost_centers ASSIGNING FIELD-SYMBOL(<cost_center>) WHERE objid EQ objid.
    <cost_center>-endda = COND #( WHEN <cost_center>-endda GT endda
                                  THEN endda
                                  ELSE <cost_center>-endda ).
    APPEND VALUE #( begda = <cost_center>-begda
                    endda = <cost_center>-endda ) TO hr_periods.
    APPEND <cost_center> TO cost_centers_tmp.
  ENDLOOP.

  SORT hr_periods.
  DELETE ADJACENT DUPLICATES FROM hr_periods.

  CALL FUNCTION 'RHXPROVIDE_PERIODS'
    TABLES
      provide_tab = hr_periods.

  DELETE hr_periods WHERE endda LT me->begda. "JMB20210706 I - Keep only the actual and future entries

ENDMETHOD.


  METHOD get_job_code.
    "Build range for all objects
    DATA(objids) = VALUE rsdsselopt_t( FOR <objec> IN objects ( sign = 'I' option = 'EQ' low = <objec>-objid ) ).
    SELECT * FROM hrp1001 INTO TABLE @DATA(job_code_relat) WHERE objid IN @objids
                                                      AND rsign = 'B'
                                                      AND relat = '007'
                                                      AND sclas = 'C'
                                                      AND begda <= @me->endda
                                                      AND endda >= @me->begda.
    "Build range for all job codes objects
    DATA(job_code_objids) = VALUE rsdsselopt_t( FOR <job_code> IN job_code_relat ( sign = 'I' option = 'EQ' low = <job_code>-sobid ) ).
    SELECT * FROM hrp1000 INTO TABLE @DATA(job_codes) WHERE objid IN @job_code_objids
                                                      AND otype = 'C'
                                                      AND begda <= @me->endda
                                                      AND endda >= @me->begda
                                                      AND langu = @sy-langu.

  ENDMETHOD.


METHOD GET_KOSTL_OF_FIRST_EMP.
  DATA: plvar        TYPE plvar,
        result_objec TYPE objec_t.

  CALL FUNCTION 'RH_GET_PLVAR'
    IMPORTING
      plvar = plvar.

  "get only employees in same OrgUnit
  CALL FUNCTION 'RH_STRUC_GET'
    EXPORTING
      act_otype      = 'O'
      act_objid      = objid
      act_wegid      = 'O-P'
      act_plvar      = plvar
      act_begda      = begda
      act_endda      = endda
    TABLES
      result_objec   = result_objec
    EXCEPTIONS
      no_plvar_found = 1
      no_entry_found = 2
      OTHERS         = 3.

  "keep only employees
  DELETE result_objec WHERE otype NE 'P'.

  CHECK result_objec IS NOT INITIAL.

  "get first employee
  READ TABLE result_objec ASSIGNING FIELD-SYMBOL(<employee>) INDEX 1.

  CHECK <employee> IS ASSIGNED.

  "get cost center of employee
  SELECT SINGLE kostl INTO @kostl FROM pa0001 WHERE pernr EQ @<employee>-objid AND
                                                    begda LE @endda            AND
                                                    endda GE @begda.
  ENDMETHOD.


METHOD GET_MAPPING_FIELDS.

  "get mapping fields for company code
  SELECT * FROM /sew/int_mapp_fi INTO TABLE @mapping_fields_bukrs WHERE molga        IN @molga   AND
                                                                        infty        = '1008'    AND
                                                                        field_sap    = 'BUKRS'   AND
                                                                        field_oracle = 'COMPANY' AND
                                                                        export       = @abap_true.

  "get mapping fields for departmentname
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = /sew/cl_mig_utils=>orgeh
                                                   oracle_field = /sew/cl_mig_utils=>departmentname
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_position ).

ENDMETHOD.


METHOD GET_MAPPING_VALUES.

  SELECT * FROM /sew/int_mapping INTO TABLE @mapping_values_bukrs WHERE molga        IN @molga   AND
                                                                        infty        = '1008'    AND
                                                                        field_sap    = 'BUKRS'   AND
                                                                        field_oracle = 'COMPANY' AND
                                                                        export       = @abap_true.

  "get mapping values for departmentname
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = /sew/cl_mig_utils=>orgeh
                                                   oracle_field = /sew/cl_mig_utils=>departmentname
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_position ).
ENDMETHOD.


METHOD get_position_data.
  DATA: plvar        TYPE plvar.

  "get all objid
  DATA(objid) = VALUE rsdsselopt_t( FOR <objec> IN objects ( sign = 'I' option = 'EQ' low = <objec>-objid ) ).

  "get upper assigned orgunit
  SELECT objid, begda, endda, sobid INTO CORRESPONDING FIELDS OF TABLE @parent_orgunit FROM hrp1001 WHERE objid IN @objid AND
                                                                                                      otype EQ 'O'    AND
                                                                                                      rsign EQ 'A'    AND
                                                                                                      relat EQ '002'  AND
                                                                                                      sclas EQ 'O'    AND
                                                                                                      begda LE @endda AND
                                                                                                      endda GE @begda.

  "get active plvar
  CALL FUNCTION 'RH_GET_PLVAR'
    IMPORTING
      plvar = plvar.
  DATA(orgid) = VALUE hrrootob_t( FOR <objid> IN objid ( otype = 'O' objid = <objid>-low ) ).

  "get managers in time range of object
  manager_assignment = /sew/cl_mig_utils=>check_assign_supervisor_v2( EXPORTING objects = objects ).
  IF manager_assignment IS NOT INITIAL.
  DATA(manager_pernr) = VALUE rsdsselopt_t( FOR <pernr> IN manager_assignment ( sign = 'I' option = 'EQ' low = <pernr>-manager_id ) ).
    SELECT pernr, begda, endda, oracleid INTO CORRESPONDING FIELDS OF TABLE @pa9400_pernr FROM pa9400 WHERE pernr IN @manager_pernr AND
                                                                                                            begda LE @endda         AND
                                                                                                            endda GE @begda.
  ENDIF.


  "get molga to org unit
  get_country_to_org_unit( ).

  "get mapping values
  get_mapping_fields( ).

  "get mapping values
  get_mapping_values( ).

  "get bukrs text
  get_bukrs_txt( ).
ENDMETHOD.


METHOD map_hr_periods.
  DATA: value      TYPE /sew/dd_value,
        plvar      TYPE plvar,
        manager_id TYPE realo.

  "get mapping fields for locationcode
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = CONV #( VALUE #( ( sign = 'I' option = 'EQ' low = '*' ) ) )
                                                   infty        = /sew/cl_mig_utils=>it0001
                                                   sap_field    = /sew/cl_mig_utils=>btrtl
                                                   oracle_field = /sew/cl_mig_utils=>locationcode
                                                   export       = abap_true
                                         IMPORTING mapping_fields = DATA(mapping_fields_btrtl) ).

**JMB20210706 start insert - store exception departments
*
  DATA(company_0021) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '00050379' )
                                           ( sign = 'I' option = 'EQ' low = '00000003' )
                                           ( sign = 'I' option = 'EQ' low = '00000002' )
                                           ( sign = 'I' option = 'EQ' low = '00000004' )
                                           ( sign = 'I' option = 'EQ' low = '00071312' )
                                           ( sign = 'I' option = 'EQ' low = '00071313' )
                                           ( sign = 'I' option = 'EQ' low = '00000005' )
                                           ( sign = 'I' option = 'EQ' low = '00000011' )
                                           ( sign = 'I' option = 'EQ' low = '00074183' ) ).

  DATA(company_0028) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '00000012' )
                                           ( sign = 'I' option = 'EQ' low = '00069298' )
                                           ( sign = 'I' option = 'EQ' low = '00069299' ) ).
*JMB20210706 end insert

  CALL FUNCTION 'RH_GET_PLVAR'
    IMPORTING
      plvar = plvar.

  LOOP AT hr_periods ASSIGNING FIELD-SYMBOL(<hr_periods>).
    om_data_entry-effective_start_date = /sew/cl_mig_utils=>convert_date( <hr_periods>-begda ).
    om_data_entry-effective_end_date   = /sew/cl_mig_utils=>convert_date( <hr_periods>-endda ).

    LOOP AT pa0001 ASSIGNING FIELD-SYMBOL(<pa0001>) WHERE begda LE <hr_periods>-endda AND
                                                              endda GE <hr_periods>-begda.

      CALL FUNCTION 'RH_GET_LEADER'
        EXPORTING
          plvar                     = plvar
          keydate                   = <hr_periods>-begda
          otype                     = 'O'
          objid                     = CONV realo( <pa0001>-orgeh )
          consider_vac_pos          = 'X'
        IMPORTING
          leader_id                 = manager_id
        EXCEPTIONS
          no_leader_found           = 1
          no_leading_position_found = 2
          OTHERS                    = 3.

      IF manager_id IS NOT INITIAL.
        om_data_entry-manager              = manager_id.
        LOOP AT pa9400_pernr ASSIGNING FIELD-SYMBOL(<p9400>) WHERE pernr EQ manager_id         AND
                                                                   begda LE <hr_periods>-endda AND
                                                                   endda GE <hr_periods>-begda.
          om_data_entry-oracleid = <p9400>-oracleid.
          EXIT.
        ENDLOOP.
      ENDIF.
      CLEAR: manager_id.
      EXIT.
    ENDLOOP.

*    LOOP AT hrp1001_tab ASSIGNING FIELD-SYMBOL(<hrp1001_up>) WHERE begda LE <hr_periods>-endda AND
*                                                                      endda GE <hr_periods>-begda.
*      om_data_entry-parent_integration_key = <hrp1001_up>-sobid.
*      EXIT.
*    ENDLOOP.

**JMB20210819 start insert - in case no parent integration key was found, pass entry
*
    IF om_data_entry-parent_integration_key IS NOT INITIAL.
*JMB20210819 end insert

**JMB20210706 start insert - check mandant and departments for CoFu countries and set Department_SET
*
      CASE sy-mandt.
        WHEN '005'. "France
          om_data_entry-company_name = '0005'.
          om_data_entry-company      = '0005'.
        WHEN '122'. "New Zealand
          om_data_entry-company_name = '2200'.
          om_data_entry-company      = '2200'.
        WHEN '120'. "Australia
          om_data_entry-company_name = '2000'.
          om_data_entry-company      = '2000'.
        WHEN '190'. "Netherlands
          om_data_entry-company_name = '9000'.
          om_data_entry-company      = '9000'.
        WHEN '102'. "Germany
          om_data_entry-company_name = '0001'.
          om_data_entry-company      = '0001'.
      ENDCASE.

      IF om_data_entry-integration_key IN company_0021.
        om_data_entry-company_name = '0021'.
        om_data_entry-company      = '0021'.
      ENDIF.

      IF om_data_entry-integration_key IN company_0028.
        om_data_entry-company_name = '0028'.
        om_data_entry-company      = '0028'.
      ENDIF.

*JMB20210706 end insert

      IF om_data_entry-company IS INITIAL.
*        LOOP AT hrp1008_com_tmp ASSIGNING FIELD-SYMBOL(<hrp1008_com>) WHERE begda LE <hr_periods>-endda AND
*                                                                            endda GE <hr_periods>-begda.
*          "get bukrs text
*          READ TABLE bukrs_txt INTO DATA(butxt) WITH KEY bukrs = <hrp1008_com>-bukrs.
*          om_data_entry-company_name = butxt-butxt.
*          om_data_entry-company = <hrp1008_com>-bukrs.    "JMB20210526 I
*
*          EXIT.
*        ENDLOOP.
      ENDIF.

      LOOP AT cost_centers_tmp ASSIGNING FIELD-SYMBOL(<cost_center>) WHERE begda LE <hr_periods>-endda AND
                                                                           endda GE <hr_periods>-begda.

**JMB20210939 start insert - in case of cost center 0018070000, pass company code 0021
*
        IF <cost_center>-sobid EQ '0018070000'.
          om_data_entry-company_name = '0021'.
          om_data_entry-company      = '0021'.
        ENDIF.
*JMB20210939 insert end

        om_data_entry-cost_center = om_data_entry-company && '-' && <cost_center>-sobid.
        EXIT.
      ENDLOOP.

**JMB20210526 start insert - in case no cost center was found
*
      IF  om_data_entry-cost_center IS INITIAL.

        DATA(kostl) = get_kostl_of_first_emp( objid = om_data_entry-integration_key
                                              begda = <hr_periods>-begda
                                              endda = <hr_periods>-endda ).

        IF kostl IS NOT INITIAL.
**JMB20210939 start insert - in case of cost center 0018070000, pass company code 0021
*
          IF kostl EQ '0018070000'.
            om_data_entry-company_name = '0021'.
            om_data_entry-company      = '0021'.
          ENDIF.
*JMB20210939 insert end

          om_data_entry-cost_center = om_data_entry-company && '-' && kostl.
          CLEAR: kostl.
        ENDIF.
      ENDIF.
*JMB20210526 end insert
*
*      LOOP AT hrp1008_br_tmp ASSIGNING FIELD-SYMBOL(<hrp1008_br>) WHERE begda LE <hr_periods>-endda AND
*                                                                        endda GE <hr_periods>-begda.
***JMB20210805 start insert - check complex mapping
**
*        DATA(fields) = VALUE /sew/cl_int_infty_proc_xml=>t_fields( ( infty = /sew/cl_mig_utils=>it0001
*                                                                     field_sap = /sew/cl_mig_utils=>btrtl
*                                                                     value = <hrp1008_br>-btrtl )
*                                                                   ( infty = /sew/cl_mig_utils=>it0001
*                                                                     field_sap = /sew/cl_mig_utils=>werks
*                                                                     value = om_data_entry-company ) ).
*        "Process WERKS/BTRTL mapping (LocationCode)
*        /sew/cl_int_mapping=>process_mapping(
*          EXPORTING
*            import         = abap_false
*            export         = abap_true
*            infty          = /sew/cl_mig_utils=>it0001
*            field_sap      = /sew/cl_mig_utils=>btrtl
*            field_oracle   = /sew/cl_mig_utils=>locationcode
*            mapping_fields = CONV #( mapping_fields_btrtl )
*            fields         = fields
*          CHANGING
*            value          = value ).
*
*        CHECK value IS NOT INITIAL.
*        om_data_entry-location = value.
*        CLEAR: value.
***JMB20210805 insert end
**      om_data_entry-location               = <hrp1008_br>-btrtl. "JMB20210805 D
*        EXIT.
*      ENDLOOP.

      APPEND om_data_entry TO om_data.
      APPEND om_data_entry TO om_data_fut.
    ENDIF.

    CLEAR: om_data_entry-manager,
           om_data_entry-parent_integration_key,
           om_data_entry-location,
           om_data_entry-company_name,
           om_data_entry-company,
           om_data_entry-cost_center.
  ENDLOOP.
  CLEAR: cost_centers_tmp.
ENDMETHOD.


METHOD MAP_POSITION_DATA.

  DATA: value_tmp        TYPE /sew/dd_value,
        om_data          TYPE /sew/tt_mig_om_data,
        om_data_imp      TYPE /sew/tt_mig_om_data,
        om_data_ca_tmp   TYPE /sew/tt_mig_om_data,
        om_data_fut      TYPE /sew/tt_mig_om_data,
        om_entry         TYPE /sew/s_mig_om_data.

  LOOP AT objects ASSIGNING FIELD-SYMBOL(<object>).

    READ TABLE orgid_country_code INTO DATA(orgid_cc_entry) WITH KEY objid = <object>-objid
                                                                     begda = <object>-begda
                                                                     endda = <object>-endda.

**JMB20210706 start insert - check mandant for CoFu countries and set Department_SET
*
    CASE sy-mandt.
      WHEN '005'. "France
        data(land1) = 'FR'.
      WHEN '122'. "New Zealand
        land1 = 'NZ'.
      WHEN '120'. "Australia
        land1 = 'AU'.
      WHEN '190'. "Netherlands
        land1 = 'NL'.
      WHEN '102'. "Germany
        land1 = 'DE'.
    ENDCASE.
*JMB20210706 end insert


**JMB20211008 start deletion - pass names from HRP1000 for CoGu
*
*    "Process department Mapping
*    CLEAR value_tmp.
*    value_tmp = CONV #( <object>-objid ).
*    /sew/cl_int_mapping=>process_mapping(
*      EXPORTING
*        import         = abap_false
*        export         = abap_true
*        infty          = /sew/cl_mig_utils=>it0001
*        field_sap      = /sew/cl_mig_utils=>orgeh
*        field_oracle   = /sew/cl_mig_utils=>departmentname
*        mapping_fields = CONV #( mapping_fields_department )
*        mapping_values = CONV #( mapping_values_department )
*      CHANGING
*        value          = value_tmp ).
*
*    om_entry-name = value_tmp.
**JMB20211008 start deletion - pass names from HRP1000
*
*    IF om_entry-name IS INITIAL OR
*       om_entry-name EQ CONV string( <object>-objid ).
      om_entry-name            = <object>-stext.
*      COND #( WHEN om_entry-country IS NOT INITIAL
*              THEN <object>-stext    && '-' && om_entry-country
*              ELSE <object>-stext ).
*    ENDIF.
*JMB20211008 deletion end

    DATA(om_data_tmp) = map_hr_periods( EXPORTING hr_periods    = get_hr_periods( EXPORTING objid = <object>-objid
                                                                                            begda = <object>-begda
                                                                                            endda = <object>-endda )
                                        IMPORTING om_data_fut   = om_data_fut
                                        CHANGING  om_data_entry = om_entry ).

    APPEND LINES OF om_data_tmp TO om_data.

    "pass only actual and future sentence to mig file
    APPEND LINES OF om_data_fut TO om_data_imp.

*JMB20210528 end insert

    CLEAR: om_entry, om_data_tmp.
  ENDLOOP.

  position_data_mig = create_mig_file( om_data_fut ).

ENDMETHOD.


METHOD PROCEED_POSITION_DATA.
  get_position_data( ).
  map_position_data( IMPORTING position_data_mig = position_data_mig ).
ENDMETHOD.
ENDCLASS.
