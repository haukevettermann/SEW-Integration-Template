class /SEW/CL_MIG_POS_DATA definition
  public
  create public .

public section.

  data OBJECTS type OBJEC_T .
  data BEGDA type BEGDA .
  data ENDDA type ENDDA .
  constants POSITION type STRING value 'Position' ##NO_TEXT.
  constants POS type STRING value 'POS_' ##NO_TEXT.
  data MOLGA type RSDSSELOPT_T .

  methods CONSTRUCTOR
    importing
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !OBJEC type OBJEC_T
      !PLVAR type PLVAR .
  methods PROCEED_POS_DATA
    returning
      value(DATA) type STRING .
  methods CREATE_METADATA
    returning
      value(METADATA) type STRING .
  methods DOWNLOAD_FILES
    importing
      !FILES type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !ZIP_FILES type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  methods PROCEED_FILE_CONSTRUCTION
    importing
      !METADATA type STRING
      !DATA type STRING
    returning
      value(CONTENT) type STRING .
protected section.
private section.

  data IT1008 type /SEW/HRP1008_T .
  data ORGUNITS type /SEW/HRP1000_T .
  data CONNECTIONS_TO_JOBS type HRP1001_T .
  data CONNECTIONS_TO_ORGUNITS type HRP1001_T .
  data POSITIONS type /SEW/HRP1000_T .
  data JOBS type /SEW/HRP1000_T .
  data BUKRS_TEXTS type T_T001 .
  data MAPPING_FIELDS_BUKRS_NWH type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPP_FI .
  data MAPPING_VALUES_BUKRS_NWH type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPPING .
  data MAPPING_FIELDS_BUKRS_BUSC type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPP_FI .
  data MAPPING_VALUES_BUKRS_BUSC type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPPING .
  data PLVAR type PLVAR .

  methods GET_INHERITED_BUKRS
    importing
      !POS type HRP1000
      !OTYPE type HRP1000-OTYPE
    returning
      value(BUKRS) type BUKRS .
  methods GET_ENTRY_GRADE_CODE
    returning
      value(ENTRY_GRADE_CODE) type STRING .
  methods GET_ACCOUNT_ASSIGNMENT .
  methods MAP_POS_DATA
    returning
      value(DATA) type STRING .
  methods GET_POS_DATA .
  methods GET_BUKRS
    importing
      !ORGUNIT_OBJECT_ID type HROBJID
      !POSITION_OBJECT_ID type HROBJID
      !POS type HRP1000 optional
    returning
      value(BUKRS) type BUKRS .
  methods GET_ORGUNIT_DATA
    importing
      !OBJECT_ID type HROBJID
    exporting
      value(ORGUNIT_NAME) type STEXT
      value(ORGUNIT_OBJECT_ID) type HROBJID .
  methods GET_SET_CODE
    returning
      value(SET_CODE) type STRING .
  methods GET_JOB_DATA
    importing
      !OBJECT_ID type HROBJID
    returning
      value(JOB_NAME) type STEXT .
  methods GET_EFFECTIVE_START_DATE
    importing
      !OBJECT_ID type HROBJID
    returning
      value(EFFECTIVE_START_DATE) type BEGDA .
  methods GET_WERKS
    importing
      !ORGUNIT_OBJECT_ID type HROBJID
    returning
      value(WERKS) type WERKS_D .
  methods GET_BUKRS_TEXT
    importing
      !BUKRS type BUKRS
    returning
      value(BUKRS_TEXT) type BUTXT .
  methods GET_MAPPING_FIELDS .
  methods GET_MAPPING_VALUES .
  methods MAP_MIG_VALUES
    importing
      !BUKRS type BUKRS
    exporting
      !BUSINESS_UNIT_NAME type /SEW/DD_VALUE
      !WORKING_HOURS type /SEW/DD_VALUE .
ENDCLASS.



CLASS /SEW/CL_MIG_POS_DATA IMPLEMENTATION.


  METHOD constructor.

    me->begda = begda.
    me->endda = endda.
    me->objects = objec.
    me->plvar = plvar.

  ENDMETHOD.


  method CREATE_METADATA.

    CONCATENATE /sew/cl_mig_utils=>metadata
                position
                'SourceSystemOwner'
                'EffectiveStartDate'
                'EffectiveEndDate'
                'BusinessUnitName'
                'Name'
                'PositionCode'
                'ActionReasonCode'
                'ActiveStatus'
                'DepartmentName'
                'JobCode'
                'JobSetCode'
                'LocationCode'
                'LocationSetCode'
                'SupervisorPersonNumber'
                'FullPartTime'
                'RegularTemporary'
                'HiringStatus'
                'PositionType'
                'FTE'
                'HeadCount'
                'SecurityClearance'
                'ProbationPeriod'
                'ProbationPeriodUnitCd'
                'BargainingUnitCd'
                'CollectiveAgreementCode'
                'OverlapAllowedFlag'
                'SeasonalFlag'
                'GradeLadderName'
                'EntryGradeCode'
                'EntryGradeSetCode'
                'EntryStepName'
                'StandardWorkingHours'
                'StandardWorkingFrequency'
                'WorkingHours'
                'Frequency'
                'StartTime'
                'EndTime'
                'SourceSystemId'
                'UnionName'
                'UnionClassificationCode'
                'SeasonalStartDate'
                'SeasonalEndDate'
                'RequisitionNumber'
                'AssignmentCategory'
                'BudgetAmount'
                'BudgetAmountCurrency'
                'BudgetedPositionFlag'
                'CostCenterName'
                'DelegatePositionCode'
                'DelegatePositionBusinessUnitName'
                'FundedByExistingPositionFlag'
   INTO metadata SEPARATED BY /sew/cl_mig_utils=>separator.

  endmethod.


  METHOD download_files.

    DATA: content     TYPE stringtab,
          filename    TYPE string,
          content_x   TYPE xstring,
          zip_file    TYPE string,
          zip_tab     TYPE swxmlcont,
          zip_xstring TYPE xstring.

    IF zip_files IS INITIAL.
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
    ELSE.
      DATA: components TYPE TABLE OF string.
      DATA(zip) = NEW cl_abap_zip( ).
      LOOP AT files ASSIGNING <files>.
        SPLIT <files>-name AT '\' INTO TABLE components.
        LOOP AT components ASSIGNING FIELD-SYMBOL(<components>).
        ENDLOOP.
        CHECK sy-subrc IS INITIAL.

        CASE <components>.
          WHEN 'Position.dat'.
            CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
              EXPORTING
                text   = <files>-value
              IMPORTING
                buffer = content_x.

            zip->add( name    = <components>
                      content = content_x    ).
          WHEN OTHERS. "not needed yet
            CONTINUE.
        ENDCASE.
      ENDLOOP.

      LOOP AT zip_files ASSIGNING FIELD-SYMBOL(<zip_file>).
        CASE <zip_file>-name.
          WHEN 1.
            zip_xstring = zip->save( ).
          WHEN OTHERS. "not needed yet
            CONTINUE.
        ENDCASE.
        CHECK zip_xstring IS NOT INITIAL.

        CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
          EXPORTING
            buffer     = zip_xstring
          TABLES
            binary_tab = zip_tab.

        CALL METHOD cl_gui_frontend_services=>gui_download
          EXPORTING
            filename = <zip_file>-value
            filetype = 'BIN'
          CHANGING
            data_tab = zip_tab
          EXCEPTIONS
            OTHERS   = 1.

        CLEAR zip_xstring.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD get_account_assignment.

    DATA: struc      TYPE struc_t,
          it1008_tmp TYPE /sew/hrp1008_t.

    DATA(otype_rel) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = 'IC' )    " Buchungskreis
                                          ( sign = 'I' option = 'EQ' low = 'I1' )    " Personalteilbereich
                                          ( sign = 'I' option = 'EQ' low = 'S'  )
                                          ( sign = 'I' option = 'EQ' low = 'O'  ) ).

    LOOP AT objects ASSIGNING FIELD-SYMBOL(<objec>).

      CALL FUNCTION 'RH_STRUC_GET'
        EXPORTING
          act_otype      = <objec>-otype
          act_objid      = <objec>-objid
          act_wegid      = 'OMACC_U'
          act_plvar      = <objec>-plvar
          act_begda      = <objec>-begda
          act_endda      = <objec>-endda
        TABLES
          result_struc   = struc
        EXCEPTIONS
          no_entry_found = 1
          no_plvar_found = 2.

      DELETE struc WHERE otype NOT IN otype_rel.
      SORT struc BY otype objid vbegda vendda.
      DELETE ADJACENT DUPLICATES FROM struc COMPARING otype objid vbegda vendda.

      LOOP AT struc ASSIGNING FIELD-SYMBOL(<struc>) WHERE otype IN otype_rel.
        APPEND INITIAL LINE TO it1008_tmp ASSIGNING FIELD-SYMBOL(<it1008>).
        CASE <struc>-otype.
          WHEN 'IC'.
            <it1008>-bukrs = <struc>-objid.
          WHEN 'I1'.
            <it1008>-btrtl = <struc>-objid+4(4).
          WHEN OTHERS.
            CONTINUE.
        ENDCASE.
        <it1008>-begda = <struc>-vbegda.
        <it1008>-endda = <struc>-vendda.
      ENDLOOP.

      LOOP AT it1008_tmp ASSIGNING <it1008> WHERE begda IS NOT INITIAL
                                            AND   endda IS NOT INITIAL.
        APPEND <it1008> TO it1008.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_bukrs.
    DATA: struc     TYPE struc_t,
          p1008_tab TYPE TABLE OF hrp1008.

    LOOP AT it1008 ASSIGNING FIELD-SYMBOL(<it1008_position>) WHERE objid = position_object_id AND otype = 'S'.
      bukrs = <it1008_position>-bukrs.
    ENDLOOP.

    IF sy-subrc <> 0.
      LOOP AT it1008 ASSIGNING FIELD-SYMBOL(<it1008_orgunit>) WHERE objid = orgunit_object_id AND otype = 'O'.
        bukrs = <it1008_orgunit>-bukrs.
      ENDLOOP.
    ENDIF.

    CHECK bukrs IS INITIAL.

    CASE sy-mandt.
      WHEN /sew/cl_int_constants=>cofu_mandant-france.
        bukrs = '0005'.
      WHEN /sew/cl_int_constants=>cofu_mandant-newzealand.
        bukrs = '2200'.
      WHEN /sew/cl_int_constants=>cofu_mandant-australia.
        bukrs = '2000'.
      WHEN /sew/cl_int_constants=>cofu_mandant-netherlands.
        bukrs = '9000'.
      WHEN /sew/cl_int_constants=>cofu_mandant-germany.
        bukrs = '0001'.
    ENDCASE.

  ENDMETHOD.


  METHOD get_bukrs_text.

    LOOP AT bukrs_texts ASSIGNING FIELD-SYMBOL(<bukrs>) WHERE bukrs = bukrs.
      bukrs_text = <bukrs>-butxt.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_effective_start_date.

    DATA: position_begda   TYPE begda,
          connection_begda TYPE begda.

    LOOP AT positions ASSIGNING FIELD-SYMBOL(<position>) WHERE objid = object_id.
      position_begda = <position>-begda.
    ENDLOOP.

    LOOP AT connections_to_orgunits ASSIGNING FIELD-SYMBOL(<connection_to_orgunit>) WHERE objid = object_id.
      connection_begda = <connection_to_orgunit>-begda.
    ENDLOOP.

    IF connection_begda > position_begda.
      effective_start_date = connection_begda.
    ELSE.
      effective_start_date = position_begda.
    ENDIF.

  ENDMETHOD.


  METHOD get_entry_grade_code.

    entry_grade_code = COND  string(  WHEN  sy-mandt = /sew/cl_int_constants=>cofu_mandant-germany     THEN 'DE_A'
                                      WHEN  sy-mandt = /sew/cl_int_constants=>cofu_mandant-france      THEN 'FR_A'
                                      WHEN  sy-mandt = /sew/cl_int_constants=>cofu_mandant-netherlands THEN 'NL_A'
                                      WHEN  sy-mandt = /sew/cl_int_constants=>cofu_mandant-australia   THEN 'AU_A'
                                      WHEN  sy-mandt = /sew/cl_int_constants=>cofu_mandant-newzealand  THEN 'NZ_A'
                                      ELSE '').

  ENDMETHOD.


  METHOD get_inherited_bukrs.
    DATA: in_objects       TYPE TABLE OF hrrootob,
          main_costcenters TYPE TABLE OF hri1001_cost.

    DATA(otype_loc) = COND #( WHEN pos-otype IS INITIAL
                              THEN otype
                              ELSE pos-otype ).

    APPEND VALUE hrrootob( objid = pos-objid otype = otype_loc ) TO in_objects.

    CALL FUNCTION 'RH_COSTCENTER_OF_OBJECT_GET'
      EXPORTING
        plvar                        = me->plvar
        begda                        = pos-begda
        endda                        = pos-endda
        buffered_access              = space
      TABLES
        in_objects                   = in_objects
        main_costcenters             = main_costcenters
      EXCEPTIONS
        given_p0001_tab_not_complete = 1
        OTHERS                       = 2.

    CHECK sy-subrc IS INITIAL.

    LOOP AT main_costcenters ASSIGNING FIELD-SYMBOL(<main_costcenter>).
      bukrs = <main_costcenter>-bukrs.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_job_data.

    DATA: job_object_id TYPE hrobjid.

    DELETE connections_to_jobs WHERE objid = object_id AND endda LT sy-datum. "Keep only the actual and future entries

    LOOP AT connections_to_jobs ASSIGNING FIELD-SYMBOL(<connection_to_job>) WHERE objid = object_id.
      job_object_id = <connection_to_job>-sobid.
    ENDLOOP.

    DELETE jobs WHERE objid = job_object_id AND endda LT sy-datum. "Keep only the actual and future entries

    LOOP AT jobs ASSIGNING FIELD-SYMBOL(<job>) WHERE objid = job_object_id.
      job_name = <job>-short.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_mapping_fields.

    "get mapping fields for business unit short
    /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = /sew/cl_mig_utils=>it0001
                                                     sap_field    = /sew/cl_mig_utils=>bukrs
                                                     oracle_field = /sew/cl_mig_utils=>businessunitshortcode
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_bukrs_busc ).




    /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = '0001'
                                                     sap_field    = 'BUKRS'
                                                     oracle_field = 'NORMALHOURS'
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_bukrs_nwh ).

  ENDMETHOD.


  METHOD get_mapping_values.

    /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = '0001'
                                                   sap_field    = 'BUKRS'
                                                   oracle_field = 'BUSINESSUNITSHORTCODE'
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_bukrs_busc ).




    /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                  infty        = '0001'
                                                  sap_field    = 'BUKRS'
                                                  oracle_field = 'NORMALHOURS'
                                                  export       = abap_true
                                        IMPORTING mapping_values = mapping_values_bukrs_nwh ).

  ENDMETHOD.


  METHOD get_orgunit_data.

    DELETE connections_to_orgunits WHERE objid = object_id AND endda LT sy-datum. "Keep only the actual and future entries

    LOOP AT connections_to_orgunits ASSIGNING FIELD-SYMBOL(<connection_to_orgunit>) WHERE objid = object_id.
      orgunit_object_id = <connection_to_orgunit>-sobid.
    ENDLOOP.

    DELETE orgunits WHERE objid = orgunit_object_id AND endda LT sy-datum. "Keep only the actual and future entries

    LOOP AT orgunits ASSIGNING FIELD-SYMBOL(<orgunit>) WHERE objid = orgunit_object_id.
      orgunit_name = <orgunit>-stext.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_pos_data.


    "get all objid from class attribute
    DATA(objid) = VALUE rsdsselopt_t( FOR <object> IN objects ( sign = 'I' option = 'EQ' low = <object>-objid ) ).

    "get all revelant positions from IT1000, the language should be equal to the system language
    SELECT objid,
           begda,
           endda,
           langu,
           stext INTO CORRESPONDING FIELDS OF TABLE @positions FROM hrp1000 WHERE objid IN @objid AND
                                                                                  begda LE @endda AND
                                                                                  endda GE @begda AND
                                                                                  otype EQ 'S'    AND
                                                                                  plvar EQ '01'   AND
                                                                                  langu EQ @sy-langu.
    "get connections to orgunits
    SELECT objid,
           begda,
           endda,
           sobid INTO CORRESPONDING FIELDS OF TABLE @connections_to_orgunits FROM hrp1001 WHERE objid IN @objid AND
                                                                                                begda LE @endda AND
                                                                                                endda GE @begda AND
                                                                                                otype EQ 'S'    AND
                                                                                                plvar EQ '01'   AND
                                                                                                relat EQ '003'  AND
                                                                                                sclas EQ 'O'.
    DATA(sobid_o) = VALUE rsdsselopt_t( FOR <sobid> IN connections_to_orgunits ( sign = 'I' option = 'EQ' low = <sobid>-sobid ) ).
    "get orgunits
    SELECT objid,
           plvar,
           otype,
           begda,
           endda,
           stext INTO CORRESPONDING FIELDS OF TABLE @orgunits FROM hrp1000 WHERE begda LE @endda   AND
                                                                                 endda GE @begda   AND
                                                                                 otype EQ 'O'      AND
                                                                                 plvar EQ '01'     AND
                                                                                 objid IN @sobid_o AND
                                                                                 langu EQ @sy-langu.
    "get connections to jobs
    SELECT objid,
           begda,
           endda,
           sobid INTO CORRESPONDING FIELDS OF TABLE @connections_to_jobs FROM hrp1001 WHERE objid IN @objid AND
                                                                                            begda LE @endda AND
                                                                                            endda GE @begda AND
                                                                                            otype EQ 'S'    AND
                                                                                            plvar EQ '01'   AND
                                                                                            relat EQ '007'  AND
                                                                                            sclas EQ 'C'.
    DATA(sobid_c) = VALUE rsdsselopt_t( FOR <sobid_c> IN connections_to_jobs ( sign = 'I' option = 'EQ' low = <sobid_c>-sobid ) ).
    "get jobs
    SELECT objid,
           begda,
           endda,
           short INTO CORRESPONDING FIELDS OF TABLE @jobs FROM hrp1000 WHERE begda LE @sy-datum AND
                                                                             endda GE @sy-datum AND
                                                                             otype EQ 'C'       AND
                                                                             plvar EQ '01'      AND
                                                                             objid IN @sobid_c  AND
                                                                             langu EQ @sy-langu.

    "get IT1008 for bukrs
    SELECT objid,
           otype,
           begda,
           endda,
           bukrs,
           werks INTO CORRESPONDING FIELDS OF TABLE @it1008 FROM hrp1008 WHERE objid IN @objid AND " IFT20211214 I
                                                                               begda LE @endda AND
                                                                               endda GE @begda.

    "get bukrs text
    SELECT bukrs,
           butxt INTO CORRESPONDING FIELDS OF TABLE @bukrs_texts FROM t001.


  ENDMETHOD.


  method GET_SET_CODE.

     CASE sy-mandt.
          WHEN '102'.
            set_code = 'DE_SET'.
          WHEN '005'.
            set_code = 'FR_SET'.
          WHEN OTHERS.
            set_code = ''.
        ENDCASE.

  endmethod.


  method GET_WERKS.

    LOOP AT it1008 ASSIGNING FIELD-SYMBOL(<it1008>) WHERE objid = orgunit_object_id.
      werks = <it1008>-werks.
    ENDLOOP.

  endmethod.


  METHOD map_mig_values.

    DATA: value_tmp TYPE /sew/dd_value.

    value_tmp = CONV #( bukrs ).
    /sew/cl_int_mapping=>process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = '0001'
        field_sap      = 'BUKRS'
        field_oracle   = 'BUSINESSUNITSHORTCODE'
        mapping_fields = CONV #( mapping_fields_bukrs_busc )
        mapping_values = CONV #( mapping_values_bukrs_busc )
     CHANGING
       value           = value_tmp ).

    business_unit_name             = value_tmp.



    value_tmp = CONV #( bukrs ).
    /sew/cl_int_mapping=>process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = '0001'
        field_sap      = 'BUKRS'
        field_oracle   = 'NORMALHOURS'
        mapping_fields = CONV #( mapping_fields_bukrs_nwh )
        mapping_values = CONV #( mapping_values_bukrs_nwh )
     CHANGING
       value           = value_tmp ).

    working_hours             = value_tmp.



  ENDMETHOD.


  METHOD map_pos_data.

    DATA: src_id               TYPE string,
          sys_id               TYPE string,
          hr_periods           TYPE hrperiods_tab,
          hr_periods_line      TYPE hrperiods,
          bukrs                TYPE bukrs,
          werks                TYPE werks_d,
          orgunit_name         TYPE stext,
          orgunit_object_id    TYPE hrobjid,
          job_name             TYPE stext,
          oldest_dataset_begda TYPE begda,
          count                TYPE i,
          is_deleted           TYPE bool,
          location_set_code    TYPE string,
          job_set_code         TYPE string,
          set_code             TYPE string.

    CONCATENATE /sew/cl_mig_utils=>sap sy-mandt INTO sys_id.

    SORT positions ASCENDING.

    LOOP AT positions ASSIGNING FIELD-SYMBOL(<position>).

      IF is_deleted = abap_true.
        is_deleted = abap_false.
        CONTINUE.
      ENDIF.

      LOOP AT positions ASSIGNING FIELD-SYMBOL(<pos>) WHERE objid = <position>-objid.

        count = count + 1.
        IF count = 1.
          oldest_dataset_begda = <pos>-begda.
          DELETE positions WHERE objid = <pos>-objid AND endda LT sy-datum. "Keep only the actual and future entries
          IF <pos> IS NOT ASSIGNED.
            is_deleted = abap_true.
            CONTINUE.
          ENDIF.
        ELSE.
          <pos>-begda = oldest_dataset_begda.
        ENDIF.

        CONCATENATE pos <pos>-objid INTO src_id.

        get_orgunit_data( EXPORTING object_id = <pos>-objid
                          IMPORTING orgunit_name = orgunit_name
                                    orgunit_object_id = orgunit_object_id ).

        DELETE it1008 WHERE objid = <pos>-objid AND endda LT sy-datum. "Keep only the actual and future entries

        bukrs = get_bukrs( position_object_id = <pos>-objid
                           orgunit_object_id = orgunit_object_id
                           pos = <pos> ).
**IFT20220104 Start Insert
*
        IF bukrs IS INITIAL.
          bukrs = get_inherited_bukrs( pos = <pos>
                                       otype = 'S' ).

        ENDIF.
*IFT20220104 End Insert
        werks = get_werks( orgunit_object_id = orgunit_object_id ).
        CONCATENATE bukrs werks INTO DATA(location_code).

        map_mig_values( EXPORTING bukrs = bukrs
                        IMPORTING business_unit_name = DATA(business_unit_name)
                                  working_hours = DATA(working_hours) ).

        job_name = get_job_data( object_id = <pos>-objid ).

        location_set_code = COND string( WHEN location_code IS NOT INITIAL
                                         THEN get_set_code( )
                                         ELSE '' ).
        job_set_code = COND string( WHEN job_name IS NOT INITIAL
                                    THEN get_set_code( )
                                    ELSE '' ).
        set_code = get_set_code( ).

        DATA(effective_start_date) = /sew/cl_mig_utils=>convert_date( get_effective_start_date( <pos>-objid ) ).
        DATA(effective_end_date) = /sew/cl_mig_utils=>convert_date( <pos>-endda ).
**IFT20211221 start insert
*
        DATA(entry_grade_code) = get_entry_grade_code( ).
        DATA(entry_set_code) = COND string( WHEN entry_grade_code IS NOT INITIAL
                                            THEN get_set_code( )
                                            ELSE '' ).
*IFT20211221 end insert
        CONCATENATE /sew/cl_mig_utils=>merge
                          position
                          sys_id
                          effective_start_date
                          effective_end_date
                          business_unit_name
                          <pos>-stext "Name
                          <pos>-objid "PositionCode
                          '' "ActionReasonCode
                          'Active' "ActiveStatus
                          orgunit_name "DepartmentName
                          job_name "JobCode
                          job_set_code "JobSetCode
                          location_code "LocationCode
                          location_set_code "LocationSetCode
                          '' "SupervisorPersonNumber
                          '' "FullPartTime
                          '' "RegularTemporary
                          'Approved' "HiringStatus
                          'Single Incumbent' "PositionType
                          '' "FTE
                          '1' "HeadCount
                          '' "SecurityClearance
                          '' "ProbationPeriod
                          '' "ProbationPeriodUnitCd
                          '' "BargainingUnitCd
                          '' "CollectiveAgreementCode
                          'N' "OverlapAllowedFlag
                          '' "SeasonalFlag
                          '' "GradeLadderName
                          entry_grade_code"'' "EntryGradeCode
                          entry_set_code  "'' "EntryGradeSetCode
                          '' "set_code "EntryStepName
                          '' "StandardWorkingHours
                          '' "StandardWorkingFrequency
                          working_hours "WorkingHours
                          'W' "Frequency
                          '' "StartTime
                          '' "EndTime
                          src_id "SourceSystemId
                          '' "UnionName
                          '' "UnionClassificationCode
                          '' "SeasonalStartDate
                          '' "SeasonalEndDate
                          '' "RequisitionNumber
                          '' "AssignmentCategory
                          '' "BudgetAmount
                          '' "BudgetAmountCurrency
                          '' "BudgetedPositionFlag
                          '' "CostCenterName
                          '' "DelegatePositionCode
                          '' "DelegatePositionBusinessUnitName
                          '' "FundedByExistingPositionFlag
              INTO DATA(data_tmp) SEPARATED BY /sew/cl_mig_utils=>separator.

        CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.

      ENDLOOP.
      count = 0.

    ENDLOOP.

  ENDMETHOD.


  METHOD proceed_file_construction.

    CONCATENATE metadata data INTO content SEPARATED BY cl_abap_char_utilities=>newline.

  ENDMETHOD.


  METHOD proceed_pos_data.

    get_pos_data( ).
    get_mapping_fields( ).
    get_mapping_values( ).
    data = map_pos_data( ).

  ENDMETHOD.
ENDCLASS.
