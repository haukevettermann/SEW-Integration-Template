class /SEW/CL_INT_INFTY_PROC_XML_UP definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF s_folder.
    TYPES folder TYPE /sew/dd_folder.
    TYPES begda TYPE dats.
    TYPES endda TYPE dats.
    TYPES hierarchy TYPE char2.
    TYPES prev_folder TYPE /sew/dd_folder.
    TYPES has_elements TYPE boole_d.
    TYPES node TYPE REF TO if_ixml_node.
    TYPES END OF s_folder .
  types:
    t_folders TYPE TABLE OF s_folder WITH KEY folder begda endda .
  types T_DATA type ref to DATA .
  types:
    BEGIN OF s_field.
    TYPES infty TYPE infty.
    TYPES folder TYPE /sew/dd_folder.
    TYPES element TYPE /sew/dd_element.
    TYPES begda TYPE dats.
    TYPES endda TYPE dats.
    TYPES field TYPE /sew/dd_field.
    TYPES value TYPE /sew/dd_value.
    TYPES processed type boole_d.
    TYPES END OF s_field .
  types:
    t_fields TYPE TABLE OF s_field with key infty folder element begda endda field value .

  data FIELD_CUSTOMIZING type /SEW/CL_INT_CUSTOMIZING_XML=>T_CUSTOMIZING_FIELDS .
  data FOLDERS type T_FOLDERS .
  data FIELDS type T_FIELDS .
  data TIME_SLICES type HRPERIODS_TAB .
  data INFTY type INFTY .
  data CHANGED_DATA_IT type /SEW/CL_INT_EMPLOYEE_XML_UP=>T_ITAENDUP .
  data CHANGED_DATA_OM type /SEW/CL_INT_OM_XML_UP=>T_OMAENDUP .
  data GO_CUSTOMIZING type ref to /SEW/CL_INT_CUSTOMIZING_XML .

  methods BUILD_FOLDERS
    importing
      !PERNR type PERNR_D
    exporting
      !MESSAGE type BAPIRET1 .
  methods DETERMINE_TIME_SLICES
    exporting
      !MESSAGE type BAPIRET1 .
  methods CONSTRUCTOR
    importing
      !CUSTOMIZING type ref to /SEW/CL_INT_CUSTOMIZING_XML
      !CHANGED_DATA_IT type /SEW/CL_INT_EMPLOYEE_XML_UP=>T_ITAENDUP optional
      !CHANGED_DATA_OM type /SEW/CL_INT_OM_XML_UP=>T_OMAENDUP optional .
  methods GET_FIELDS
    exporting
      !MESSAGE type BAPIRET1 .
  methods GET_FOLDERS
    exporting
      !MESSAGE type BAPIRET1 .
  PROTECTED SECTION.
private section.

  methods ADD_FIELDS_2006
    importing
      !CHANGED_DATA_IT type ANY
      !CURRENT_FIELD type S_FIELD
    returning
      value(NEW_FIELDS) type T_FIELDS .
  methods GET_FIELDS_2006
    importing
      !STRUCTURE type ANY
    returning
      value(FIELDS) type /SEW/CL_INT_INFTY_PROC_XML=>T_FIELDS .
  methods ADD_FIELDS_2001
    importing
      !CHANGED_DATA_IT type ANY
      !CURRENT_FIELD type S_FIELD
    returning
      value(NEW_FIELDS) type T_FIELDS .
ENDCLASS.



CLASS /SEW/CL_INT_INFTY_PROC_XML_UP IMPLEMENTATION.


METHOD add_fields_2001.

  DATA: is_ok    TYPE sy-subrc,
        it       TYPE REF TO data,
        prelp    TYPE prelp,
        ssid_end TYPE string.

  FIELD-SYMBOLS: <struk>    TYPE any.
  CONSTANTS: no_auth TYPE boole_d VALUE abap_true.

**JMB20211026 start insert - read infotype structure
*
  ASSIGN COMPONENT 'INFTY' OF STRUCTURE changed_data_it TO FIELD-SYMBOL(<infty>).
  "create infotype structure
  CONCATENATE 'P' <infty> INTO DATA(it_name).
  CREATE DATA it TYPE (it_name).
  ASSIGN it->* TO <struk>.

  IF <struk> IS ASSIGNED.
    "get infotype data
    MOVE-CORRESPONDING changed_data_it TO prelp.
    cl_hr_pnnnn_type_cast=>prelp_to_pnnnn(
      EXPORTING
        prelp = prelp
      IMPORTING
        pnnnn = <struk> ).
  ENDIF.
*JMB20211026 insert end

  DATA(new_field) = current_field.

  DATA(fields) = VALUE /iwbep/t_mgw_name_value_pair( ( name = '/AbsStartTime'      value = 'BEGUZ'     )
                                                     ( name = '/AbsEndTime'        value = 'ENDUZ'     )
                                                     ( name = '/AbsenceType'       value = 'ABTYP'     )
                                                     ( name = '/SourceSystemOwner' value = 'SSOWN'     )
                                                     ( name = '/SourceSystemId'    value = 'SSID'      )
                                                     ( name = '/Employer'          value = 'EMPLY'     )
                                                     ( name = '/AbsenceStatus'     value = 'SUBMITTED' )
                                                     ( name = '/ApprovalStatus'    value = 'APPROVED'  )
                                                     ( name = '/Operation'         value = /sew/cl_forms_utils=>delete    ) ).

  ASSIGN COMPONENT 'BEGDA' OF STRUCTURE changed_data_it TO FIELD-SYMBOL(<begda>).
  ASSIGN COMPONENT 'ENDDA' OF STRUCTURE changed_data_it TO FIELD-SYMBOL(<endda>).
  ASSIGN COMPONENT 'PERNR' OF STRUCTURE changed_data_it TO FIELD-SYMBOL(<pernr>).
  ASSIGN COMPONENT 'SUBTY' OF STRUCTURE changed_data_it TO FIELD-SYMBOL(<subty>).

  LOOP AT fields ASSIGNING FIELD-SYMBOL(<fields>).

    new_field-element = <fields>-name.
    new_field-field   = <fields>-name.

    CASE <fields>-value.

      WHEN 'BEGUZ'.
        ASSIGN COMPONENT 'BEGUZ' OF STRUCTURE <struk> TO FIELD-SYMBOL(<beguz>). "JMB20211025 I
        new_field-value = <beguz>.

      WHEN 'ENDUZ'.
        ASSIGN COMPONENT 'ENDUZ' OF STRUCTURE <struk> TO FIELD-SYMBOL(<enduz>). "JMB20211025 I
        new_field-value = <enduz>.

      WHEN 'ABTYP'.
        CHECK <begda> IS ASSIGNED AND <begda> IS NOT INITIAL.
        CHECK <endda> IS ASSIGNED AND <endda> IS NOT INITIAL.
        CHECK <subty> IS ASSIGNED AND <subty> IS NOT INITIAL.

        DATA(molga) = /sew/cl_forms_utils=>get_molga( pernr = <pernr> ).

        SELECT SINGLE oracle_awart FROM /sew/int_awa_map WHERE molga EQ @molga
                                                           AND infty EQ '2001'
                                                           AND awart EQ @<subty>
                                                           AND begda <= @<endda>
                                                           AND endda >= @<begda>
                                                          INTO @DATA(oracle_awart).

        CHECK oracle_awart IS NOT INITIAL.

        new_field-value = oracle_awart.

      WHEN 'SSOWN'.
        new_field-value = 'SAP_' && sy-mandt.

      WHEN 'SSID'.
        CHECK <begda> IS ASSIGNED AND <begda> IS NOT INITIAL.
        CHECK <endda> IS ASSIGNED AND <endda> IS NOT INITIAL.
        CHECK <subty> IS ASSIGNED AND <subty> IS NOT INITIAL.

        ASSIGN COMPONENT 'DOCNR' OF STRUCTURE <struk> TO FIELD-SYMBOL(<docnr>). "JMB20211025 I
        ASSIGN COMPONENT 'DOCSY' OF STRUCTURE <struk> TO FIELD-SYMBOL(<docsy>). "JMB20211025 I

        DATA(oracle_pernr) = /sew/cl_forms_utils=>get_oraclepernr( EXPORTING pernr = <pernr> ).

        SELECT SINGLE oracle_awart FROM /sew/int_awa_map WHERE molga EQ @molga
                                                           AND infty EQ '2001'
                                                           AND awart EQ @<subty>
                                                           AND begda <= @<endda>
                                                           AND endda >= @<begda>
                                                          INTO @oracle_awart.

        new_field-value = oracle_pernr && '_' && <docsy> && '_' && <docnr>.

      WHEN 'EMPLY'.

        SELECT SINGLE bukrs FROM pa0001 WHERE pernr  = @<pernr>
                                        AND   begda <= @<endda>
                                        AND   endda >= @<begda>
                                        INTO  @DATA(bukrs).

        CHECK sy-subrc EQ 0.

        molga = /sew/cl_int_utility=>get_molga( pernr = <pernr>
                                                begda = <begda>
                                                endda = <endda> ).

        /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga          = VALUE #( ( sign = 'I' option = 'EQ' low = molga ) )
                                                         infty          = /sew/cl_mig_utils=>it0001
                                                         sap_field      = /sew/cl_mig_utils=>bukrs
                                                         oracle_field   = /sew/cl_mig_utils=>legalemployername
                                                         export         = abap_true
                                               IMPORTING mapping_fields = DATA(mapping_fields_bukrs) ).

        /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga          = VALUE #( ( sign = 'I' option = 'EQ' low = molga ) )
                                                         infty          = /sew/cl_mig_utils=>it0001
                                                         sap_field      = /sew/cl_mig_utils=>bukrs
                                                         oracle_field   = /sew/cl_mig_utils=>legalemployername
                                                         export         = abap_true
                                               IMPORTING mapping_values = DATA(mapping_values_bukrs) ).

        DATA(value_tmp) = CONV /sew/dd_value( bukrs ).

        /sew/cl_int_mapping=>process_mapping( EXPORTING import         = abap_false
                                                        export         = abap_true
                                                        infty          = /sew/cl_mig_utils=>it0001
                                                        field_sap      = /sew/cl_mig_utils=>bukrs
                                                        field_oracle   = /sew/cl_mig_utils=>legalemployername
                                                        mapping_fields = CONV #( mapping_fields_bukrs )
                                                        mapping_values = CONV #( mapping_values_bukrs )
                                               CHANGING value          = value_tmp ).

        CHECK value_tmp IS NOT INITIAL.

        new_field-value = value_tmp.

      WHEN /sew/cl_forms_utils=>delete.
        new_field-value   = /sew/cl_forms_utils=>merge.
        ASSIGN COMPONENT 'OPERATION' OF STRUCTURE changed_data_it TO FIELD-SYMBOL(<operation>).
        CHECK <operation> IS ASSIGNED.
        IF <operation> EQ 'DEL'.
          new_field-value   = <fields>-value.
        ENDIF.
      WHEN OTHERS.
        new_field-value   = <fields>-value.
    ENDCASE.

    APPEND new_field TO new_fields.

  ENDLOOP.

ENDMETHOD.


METHOD add_fields_2006.

  DATA: it    TYPE REF TO data,
        prelp TYPE prelp.

  DATA(new_field) = current_field.
  DATA(fields) = VALUE /iwbep/t_mgw_name_value_pair( ( name = '/AccrualType'     value = 'OPERATION' )
                                                     ( name = '/Enrollment'      value = 'OPERATION' )
                                                     ( name = '/WorkTermsNumber' value = 'PERNR'     )
                                                     ( name = '/SourceSystemId'  value = 'SSID'      )
                                                     ( name = '/Operation'       value = /sew/cl_forms_utils=>delete  ) ).

  FIELD-SYMBOLS: <struk>    TYPE any.

**JMB20211026 start insert - read infotype structure
*
  ASSIGN COMPONENT 'INFTY' OF STRUCTURE changed_data_it TO FIELD-SYMBOL(<infty>).
  "create infotype structure
  CONCATENATE 'P' <infty> INTO DATA(it_name).
  CREATE DATA it TYPE (it_name).
  ASSIGN it->* TO <struk>.

  IF <struk> IS ASSIGNED.
    "get infotype data
    MOVE-CORRESPONDING changed_data_it TO prelp.
    cl_hr_pnnnn_type_cast=>prelp_to_pnnnn(
      EXPORTING
        prelp = prelp
      IMPORTING
        pnnnn = <struk> ).
  ENDIF.
*JMB20211026 insert end

  LOOP AT fields ASSIGNING FIELD-SYMBOL(<fields>).
    ASSIGN COMPONENT <fields>-value OF STRUCTURE changed_data_it TO FIELD-SYMBOL(<field>).

    new_field-element = <fields>-name.
    new_field-field   = <fields>-name.

    CASE <fields>-value.
      WHEN 'SSID'.
        ASSIGN COMPONENT 'PERNR' OF STRUCTURE <struk> TO FIELD-SYMBOL(<pernr>).
        ASSIGN COMPONENT 'QUONR' OF STRUCTURE <struk> TO FIELD-SYMBOL(<quonr>).
        new_field-value = <pernr> && '_' && <quonr>.
      WHEN 'OPERATION'.

        CASE <fields>-name.
          WHEN '/Enrollment'.
            new_field-value = abap_false.
            IF <field> EQ 'INS'.
              new_field-value = abap_true.
            ENDIF.
          WHEN OTHERS.
            new_field-value = 'INIT'.
            IF <field> EQ 'UPD'.
              new_field-value = 'ADJOTH'.
            ENDIF.
        ENDCASE.

      WHEN 'PERNR'.
        new_field-value   = <field>.

        IF <fields>-name EQ '/WorkTermsNumber'.
          new_field-value   = 'ET' && <field>.
        ENDIF.

      WHEN /sew/cl_forms_utils=>delete.
        new_field-value   = /sew/cl_forms_utils=>merge.
        ASSIGN COMPONENT 'OPERATION' OF STRUCTURE changed_data_it TO FIELD-SYMBOL(<operation>).
        CHECK <operation> IS ASSIGNED.
        IF <operation> EQ 'DEL'.
          new_field-value   = <fields>-value.
        ENDIF.
      WHEN OTHERS.
        new_field-value   = <fields>-value.
    ENDCASE.

    APPEND new_field TO new_fields.
  ENDLOOP.

  GET TIME STAMP FIELD DATA(ts).
  ts = ts + 1.

  "get time stamp for person accrual details
  new_field-element = '/Timestamp'.
  new_field-field   = '/Timestamp'.
  new_field-value   = ts.
  CONDENSE new_field-value.
  APPEND new_field TO new_fields.

ENDMETHOD.


  METHOD build_folders.

  ENDMETHOD.


METHOD constructor.
  me->field_customizing = customizing->custom_fields.
  me->go_customizing    = customizing.
  me->changed_data_it = changed_data_it.
  me->changed_data_om = changed_data_om.
ENDMETHOD.


  METHOD determine_time_slices.
    READ TABLE me->fields WITH KEY field = /sew/cl_int_constants=>begda ASSIGNING FIELD-SYMBOL(<begda>).
*      time_slices = CORRESPONDING #( BASE ( time_slices ) VALUE hrperiods_tab( FOR field IN me->fields ( begda = field-begda endda = field-endda ) ) ).
    CALL FUNCTION 'RHXPROVIDE_PERIODS'
      TABLES
        provide_tab = me->time_slices.
  ENDMETHOD.


METHOD get_fields.
  DATA: lr_structdescr TYPE REF TO cl_abap_structdescr,
        classdescr     TYPE REF TO cl_abap_classdescr,
        new_fields     TYPE t_fields,
        fields_infty   TYPE /sew/cl_int_infty_proc_xml=>t_fields,
        lr_structure   TYPE REF TO data..

  classdescr ?= cl_abap_typedescr=>describe_by_name( cl_abap_classdescr=>get_class_name( me ) ).

  LOOP AT me->changed_data_it ASSIGNING FIELD-SYMBOL(<changed_data_it>).
    DATA(fields) = VALUE /sew/cl_int_infty_proc_xml=>t_fields( ( infty     = '0001'
                                                                 field_sap = 'MOLGA'
                                                                 value     = go_customizing->molga ) ).

    me->infty = <changed_data_it>-infty. "JMB20210914 I

*   Create dynamic structure
    lr_structdescr ?= cl_abap_typedescr=>describe_by_name( 'P' && <changed_data_it>-infty ).
    CREATE DATA lr_structure TYPE HANDLE lr_structdescr.
    ASSIGN lr_structure->* TO FIELD-SYMBOL(<fs_structure>).
    cl_hr_pnnnn_type_cast=>prelp_to_pnnnn( EXPORTING prelp = CORRESPONDING #( <changed_data_it> )
                                           IMPORTING pnnnn = <fs_structure> ).

    ASSIGN COMPONENT /sew/cl_int_constants=>begda OF STRUCTURE <changed_data_it> TO FIELD-SYMBOL(<begda>).
    ASSIGN COMPONENT /sew/cl_int_constants=>endda OF STRUCTURE <changed_data_it> TO FIELD-SYMBOL(<endda>).

**JMB20210908 start insert - check for infotype specific fields
*
    DATA(method) = 'GET_FIELDS_' && <changed_data_it>-infty.

    READ TABLE classdescr->methods WITH KEY name = method TRANSPORTING NO FIELDS.

    IF sy-subrc EQ 0.

      CALL METHOD (method)
        EXPORTING
          structure = <fs_structure>
        RECEIVING
          fields    = fields_infty.

      APPEND LINES OF  fields_infty TO fields.
    ENDIF.
*JMB20210908 end insert

    DATA(add_fields) = abap_false. "JMB20210909 I
    LOOP AT me->field_customizing ASSIGNING FIELD-SYMBOL(<field>) WHERE infty = <changed_data_it>-infty.
      CHECK <field>-folder IS NOT INITIAL.

      ASSIGN COMPONENT <field>-field OF STRUCTURE <fs_structure> TO FIELD-SYMBOL(<value>).

      IF <value> IS ASSIGNED AND <value> IS NOT INITIAL.
        APPEND INITIAL LINE TO me->fields ASSIGNING FIELD-SYMBOL(<new_field>).
        <new_field>-infty   = <field>-infty.
        <new_field>-folder  = <field>-folder.
        <new_field>-element = <field>-element.
        <new_field>-field   = <field>-field.
        <new_field>-begda   = <begda>.
        <new_field>-endda   = <endda>.

**JMB20210907 start insert - check for export mapping
*
        DATA(field_oracle) = CONV /sew/dd_field( <field>-element ).
        REPLACE ALL OCCURRENCES OF '/' IN field_oracle WITH space.
        TRANSLATE field_oracle TO UPPER CASE.

        DATA(value_tmp) = CONV /sew/dd_value( <value> ).

        APPEND LINES OF VALUE /sew/cl_int_infty_proc_xml=>t_fields( ( infty     = infty
                                                                      field_sap = <field>-field
                                                                      value     = value_tmp     )  ) TO fields.

        /sew/cl_int_mapping=>process_mapping( EXPORTING infty          = <field>-infty
                                                        field_sap      = <field>-field
                                                        field_oracle   = field_oracle
                                                        begda          = <begda>
                                                        endda          = <endda>
                                                        mapping_fields = go_customizing->custom_mapping_fields
                                                        mapping_values = go_customizing->custom_mapping_values
                                                        import         = abap_false
                                                        export         = abap_true
                                                        fields         = fields
                                               CHANGING value          = value_tmp ).
        <new_field>-value = value_tmp.
        CLEAR: value_tmp.
        CONDENSE <new_field>-value.

**JMB20210909 start insert - add fields to export xml
*
        IF add_fields EQ abap_false.
          add_fields = abap_true.

          method = 'ADD_FIELDS_' && <changed_data_it>-infty.
          READ TABLE classdescr->methods WITH KEY name = method TRANSPORTING NO FIELDS.

          IF sy-subrc EQ 0.

            CALL METHOD (method)
              EXPORTING
                changed_data_it = <changed_data_it>
                current_field   = <new_field>
              RECEIVING
                new_fields      = new_fields.

            APPEND LINES OF new_fields TO me->fields.
            CLEAR: new_fields.
          ENDIF.
        ENDIF.
*JMB20210909 end insert
*JMB20210907 end insert
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  LOOP AT me->changed_data_om ASSIGNING FIELD-SYMBOL(<changed_data_om>).
*   Create dynamic structure
    lr_structdescr ?= cl_abap_typedescr=>describe_by_name( 'P' && <changed_data_om>-infty ).
    CREATE DATA lr_structure TYPE HANDLE lr_structdescr.
    ASSIGN lr_structure->* TO FIELD-SYMBOL(<fs_structure_om>).
    cl_hr_pnnnn_type_cast=>prelp_to_pnnnn(
      EXPORTING
        prelp = CORRESPONDING #( <changed_data_om> )
      IMPORTING
        pnnnn = <fs_structure_om> ).
    LOOP AT me->field_customizing ASSIGNING FIELD-SYMBOL(<field_om>) WHERE infty = <changed_data_om>-infty.
      CHECK <field_om>-folder IS NOT INITIAL.
      ASSIGN COMPONENT /sew/cl_int_constants=>begda OF STRUCTURE <changed_data_om> TO FIELD-SYMBOL(<begda_om>).
      ASSIGN COMPONENT /sew/cl_int_constants=>endda OF STRUCTURE <changed_data_om> TO FIELD-SYMBOL(<endda_om>).
      ASSIGN COMPONENT <field_om>-field OF STRUCTURE <fs_structure_om> TO FIELD-SYMBOL(<value_om>).
      IF <value> IS ASSIGNED AND <value> IS NOT INITIAL.
        APPEND INITIAL LINE TO me->fields ASSIGNING FIELD-SYMBOL(<new_field_om>).
        <new_field_om>-infty = <field_om>-infty.
        <new_field_om>-folder = <field_om>-folder.
        <new_field_om>-element = <field_om>-element.
        <new_field_om>-field = <field_om>-field.
        <new_field_om>-begda = <begda_om>.
        <new_field_om>-endda = <endda_om>.
        <new_field_om>-value = <value_om>.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDMETHOD.


METHOD get_fields_2006.

  ASSIGN COMPONENT 'DESTA' OF STRUCTURE structure TO FIELD-SYMBOL(<desta>).
  ASSIGN COMPONENT 'DEEND' OF STRUCTURE structure TO FIELD-SYMBOL(<deend>).

  fields = VALUE #( ( infty     = '2006'
                      field_sap = 'DESTA'
                      value     = <desta> )
                    ( infty     = '2006'
                      field_sap = 'DEEND'
                      value     = <deend> ) ).

ENDMETHOD.


METHOD get_folders.
  CLEAR me->folders.
  DATA(temp_folder) = me->folders.
  DATA(temp_folder_date) = me->folders.

  temp_folder = VALUE #( FOR field IN me->fields ( folder = field-folder begda = field-begda endda = field-endda ) ).
  DELETE temp_folder WHERE folder IS INITIAL.

  SORT temp_folder BY folder begda endda ASCENDING.
  DELETE ADJACENT DUPLICATES FROM temp_folder COMPARING folder begda endda.

  LOOP AT temp_folder ASSIGNING FIELD-SYMBOL(<folder>).
    AT NEW folder.
*        temp_folder_date = VALUE #( FOR folder IN temp_folder ( folder = folder-folder ) ).
      me->time_slices = VALUE #( FOR folder IN temp_folder ( begda = folder-begda endda = folder-endda  ) ).
      CALL FUNCTION 'RHXPROVIDE_PERIODS'
        TABLES
          provide_tab = me->time_slices.
      temp_folder_date = CORRESPONDING #( BASE ( temp_folder_date ) VALUE t_folders( FOR time_slice IN me->time_slices ( folder = <folder>-folder begda = time_slice-begda endda = time_slice-endda ) ) ).
    ENDAT.
  ENDLOOP.

  LOOP AT temp_folder_date ASSIGNING <folder>.
    REPLACE ALL OCCURRENCES OF '//' IN <folder>-folder WITH ''.
    SPLIT <folder>-folder AT '/' INTO TABLE DATA(split_folders).

    DATA(prev_folder) = <folder>-folder.
    CLEAR prev_folder.
    DATA(index) = 01.

    WHILE index <= lines( split_folders ).
      READ TABLE split_folders INDEX index ASSIGNING FIELD-SYMBOL(<split_folder>).
      APPEND INITIAL LINE TO me->folders ASSIGNING FIELD-SYMBOL(<new_folder>).
      <new_folder>-has_elements = COND #( WHEN index = lines( split_folders ) THEN abap_true ELSE abap_false ).
      <new_folder>-begda = COND #( WHEN <new_folder>-has_elements = abap_true THEN <folder>-begda ELSE '' ).
      <new_folder>-endda = COND #( WHEN <new_folder>-has_elements = abap_true THEN <folder>-endda ELSE '' ).
      <new_folder>-folder = <split_folder>.
      <new_folder>-hierarchy = index.
      <new_folder>-prev_folder = prev_folder.
      prev_folder = <split_folder>.
      index = index + 1.
    ENDWHILE.
    CLEAR: prev_folder, split_folders.
  ENDLOOP.
ENDMETHOD.
ENDCLASS.
