class /SEW/CL_INT_INFTY_HANDLER definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF s_payment .
    TYPES oracle_wagetype TYPE string.
    TYPES wagetype TYPE p0008-lga01.
    TYPES amount TYPE p0008-bet01.
    TYPES seqnr TYPE seqnr.
    TYPES END OF s_payment .
  types:
    t_payments TYPE TABLE OF s_payment WITH DEFAULT KEY .
  types:
    BEGIN OF s_costing .
    TYPES company_code TYPE p0027-kbu01.
    TYPES cost_center TYPE p0027-kst01.
    TYPES proportion TYPE p0027-kpr01.
    TYPES seqnr TYPE seqnr.
    TYPES END OF s_costing .
  types:
    t_costing TYPE TABLE OF s_costing WITH DEFAULT KEY .
  types:
    BEGIN OF s_time_slices .
        INCLUDE TYPE hrperiods.
        TYPES seqnr TYPE seqnr.
    TYPES folder TYPE /sew/dd_folder.
    TYPES parent_folder TYPE /sew/dd_folder.
    TYPES parent_folder_seqnr TYPE seqnr.
    TYPES END OF s_time_slices .
  types:
    t_time_slices TYPE TABLE OF s_time_slices .
  types:
    BEGIN OF s_folder.
    TYPES folder TYPE /sew/dd_folder.
    TYPES begda TYPE begda.
    TYPES endda TYPE endda.
    TYPES node TYPE REF TO if_ixml_node.
    TYPES cust_seqnr TYPE seqnr.
    TYPES seqnr TYPE seqnr.
    TYPES ignore_timeslices TYPE boole_d.
    TYPES parent_folder TYPE /sew/dd_folder.
    TYPES parent_folder_seqnr TYPE seqnr.
    TYPES END OF s_folder .
  types:
    t_folders TYPE TABLE OF s_folder WITH DEFAULT KEY .
  types T_DATA type ref to DATA .
  types:
    BEGIN OF s_field.
    TYPES infty TYPE infty.
    TYPES field_sap TYPE /sew/dd_field.
    TYPES field_oracle TYPE /sew/dd_field.
    TYPES begda TYPE begda.
    TYPES endda TYPE endda.
    TYPES value TYPE /sew/dd_value.
    TYPES value_mapped TYPE /sew/dd_value.
    TYPES value_converted TYPE /sew/dd_value.
    TYPES folder TYPE /sew/dd_folder.
    TYPES cust_seqnr TYPE seqnr.
    TYPES seqnr TYPE seqnr.
    TYPES parent_folder TYPE /sew/dd_folder.
    TYPES parent_folder_seqnr TYPE seqnr.
    TYPES END OF s_field .
  types:
    t_fields TYPE TABLE OF s_field WITH KEY infty field_sap field_oracle begda endda .
  types:
    BEGIN OF s_folders_aggregated.
    TYPES id TYPE seqnr.
    TYPES folders TYPE t_folders.
    TYPES fields TYPE t_fields.
    TYPES END OF s_folders_aggregated .
  types:
    t_folders_aggregated TYPE TABLE OF s_folders_aggregated WITH DEFAULT KEY .

  data XML_NODE type ref to IF_IXML_NODE .
  data FOLDERS type T_FOLDERS .
  data FOLDERS_AGGREGATED type T_FOLDERS_AGGREGATED .
  data FIELDS type T_FIELDS .
  data TIME_SLICES type T_TIME_SLICES .
  data INFTY type INFTY .
  data SEQNR type SEQNR .
  data ACTION type MASSN .
  data CUSTOMIZING type ref to /SEW/CL_INT_CUSTOMIZING_XML .
  class-data GO_INSTANCE type ref to /SEW/CL_INT_INFTY_PROC_XML .
  data INFOTYPE_CHANGES type ref to /SEW/CL_INT_IT_AEND .
  data OBJECT_HANDLER type ref to /SEW/CL_INT_OBJECT_HANDLER .
  data INT_RUN type GUID_32 .
  data MOLGA type MOLGA .
  data CUSTOMIZING_FOLDERS type /SEW/CL_INT_CUSTOMIZING_XML=>T_CUSTOMIZING_FOLDERS .
  data HAS_MULTIPLE type BOOLE_D .
  data HAS_ERROR type BOOLE_D .

  class-methods PROCESS_0008
    importing
      !BEGDA type DATS
      !ENDDA type DATS
      value(SAP_ID) type /SEW/VALUE
      !CLOUD_ID type /SEW/VALUE
      !AEND_ID type GUID_32
      !HAS_ERROR type BOOLE_D
      !FIELDS type T_FIELDS
      !INFOTYPE_XML_PROCESSOR type ref to /SEW/CL_INT_INFTY_PROC_XML
      value(DATA) type ANY
    exporting
      !IT_AEND type /SEW/INT_IT_AEND .
  class-methods PROCESS_0008_03
    importing
      !BEGDA type DATS
      !ENDDA type DATS
      value(SAP_ID) type /SEW/VALUE
      !CLOUD_ID type /SEW/VALUE
      !AEND_ID type GUID_32
      !HAS_ERROR type BOOLE_D
      !FIELDS type T_FIELDS
      !INFOTYPE_XML_PROCESSOR type ref to /SEW/CL_INT_INFTY_PROC_XML
      value(DATA) type ANY
    exporting
      !IT_AEND type /SEW/INT_IT_AEND .
  class-methods PROCESS_0008_15
    importing
      !BEGDA type DATS
      !ENDDA type DATS
      value(SAP_ID) type /SEW/VALUE
      !CLOUD_ID type /SEW/VALUE
      !AEND_ID type GUID_32
      !HAS_ERROR type BOOLE_D
      !FIELDS type T_FIELDS
      !INFOTYPE_XML_PROCESSOR type ref to /SEW/CL_INT_INFTY_PROC_XML
      value(DATA) type ANY
    exporting
      !IT_AEND type /SEW/INT_IT_AEND .
  class-methods PROCESS_0027
    importing
      !BEGDA type DATS
      !ENDDA type DATS
      value(SAP_ID) type /SEW/VALUE
      !CLOUD_ID type /SEW/VALUE
      !AEND_ID type GUID_32
      !HAS_ERROR type BOOLE_D
      !FIELDS type T_FIELDS
      !INFOTYPE_XML_PROCESSOR type ref to /SEW/CL_INT_INFTY_PROC_XML
      value(DATA) type ANY
    exporting
      !IT_AEND type /SEW/INT_IT_AEND .
  class-methods PROCESS_0000
    importing
      !BEGDA type DATS
      !ENDDA type DATS
      value(SAP_ID) type /SEW/VALUE
      !CLOUD_ID type /SEW/VALUE
      !AEND_ID type GUID_32
      !HAS_ERROR type BOOLE_D
      !FIELDS type T_FIELDS
      !INFOTYPE_XML_PROCESSOR type ref to /SEW/CL_INT_INFTY_PROC_XML
      value(DATA) type ANY
    exporting
      !IT_AEND type /SEW/INT_IT_AEND .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS /SEW/CL_INT_INFTY_HANDLER IMPLEMENTATION.


  METHOD process_0000.
    DATA: classdescr TYPE REF TO cl_abap_classdescr.
*   Check if molga specific method is available
    classdescr ?= cl_abap_typedescr=>describe_by_name( '/SEW/CL_INT_INFTY_PROC_XML' ).
    DATA(method) = |PROCESS_| && infotype_xml_processor->infty && '_' && infotype_xml_processor->molga.
    READ TABLE classdescr->methods WITH KEY name = method TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL."Infty spefic method availlable
      CALL METHOD infotype_xml_processor->(method)
        EXPORTING
          begda     = begda
          endda     = endda
          sap_id    = sap_id
          cloud_id  = cloud_id
          aend_id   = aend_id
          has_error = has_error
          fields    = fields
          data      = data
        IMPORTING
          it_aend   = it_aend.
    ELSE." Logic if not infty specific.
      DATA: prelp_orig TYPE prelp,
            endda_term TYPE dats.
      CLEAR: endda_term.
      ASSIGN data TO FIELD-SYMBOL(<structure>).
*     Check action and set instance attribute.
      IF infotype_xml_processor->infty = /sew/cl_int_constants=>it0000.
        READ TABLE fields WITH KEY field_sap = /sew/cl_int_constants=>massn ASSIGNING FIELD-SYMBOL(<action>).

        "SEW specific Hire and Termination handled in customizing via infotype 0000 seqnr 001. Therefore ignore for seqnr 000
        IF infotype_xml_processor->seqnr = 000 AND <action>-value IN /sew/cl_int_constants=>non_relevant_action( ).    "001 -> 000 changed
          EXIT.
        ENDIF.

        ASSIGN COMPONENT /sew/cl_int_constants=>massn OF STRUCTURE <structure> TO FIELD-SYMBOL(<value>).
        IF <value> = 'RE'.
          EXIT.
        ENDIF.

        IF <value> IN /sew/cl_int_constants=>orgchange_range.
          ASSIGN COMPONENT /sew/cl_int_constants=>massg OF STRUCTURE <structure> TO FIELD-SYMBOL(<massg>).
          IF <value> NE '08'.
            <massg> = '01'.
          ENDIF.
        ENDIF.

        IF <value> IN /sew/cl_int_constants=>rehire_range.
          ASSIGN COMPONENT /sew/cl_int_constants=>massg OF STRUCTURE <structure> TO FIELD-SYMBOL(<massg_rehire>).
          IF <massg_rehire> IS ASSIGNED AND <massg_rehire> NE '02'.
            <massg_rehire> = '01'.
          ENDIF.
        ENDIF.

        CHECK <value> NOT IN /sew/cl_int_constants=>non_relevant_action( ).
        IF <action>-value NE 'ORA_EMPL_REV_TERMINATION'.
          CHECK <value> IS NOT INITIAL.
        ENDIF.

*     Set action for IT0000
        infotype_xml_processor->action = <value>.
        IF <value> IS NOT INITIAL AND <value> NE /sew/cl_int_constants=>hire_date_change.
          IF infotype_xml_processor->object_handler->action NOT IN /sew/cl_int_constants=>termination_range.
            infotype_xml_processor->object_handler->action = <value>.
          ENDIF.
*      Set termination date for delimiting
          IF infotype_xml_processor->object_handler->action IN /sew/cl_int_constants=>termination_range.
            ASSIGN COMPONENT /sew/cl_int_constants=>endda OF STRUCTURE <structure> TO <value>.
            endda_term = <value>.
*          IF <value> = /sew/cl_int_constants=>highdate.
            ASSIGN COMPONENT /sew/cl_int_constants=>begda OF STRUCTURE <structure> TO <value>.
            infotype_xml_processor->object_handler->delimit_date = endda_term + 1.
*          ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
*   Rehire special logic
      IF infotype_xml_processor->infty NE /sew/cl_int_constants=>it0000 AND infotype_xml_processor->object_handler->action = '04'.
        ASSIGN COMPONENT /sew/cl_int_constants=>endda OF STRUCTURE <structure> TO <value>.
        IF <value> NE /sew/cl_int_constants=>highdate.
          CLEAR it_aend.
          EXIT.
        ENDIF.
      ENDIF.
*     Assign pernr
      ASSIGN COMPONENT /sew/cl_int_constants=>pernr OF STRUCTURE <structure> TO <value>.
      <value> = sap_id.
*     Transfer data to prelp
      infotype_xml_processor->pa_to_prelp( EXPORTING infotype = <structure>
                                 aend_id  = aend_id
                       IMPORTING prelp    = DATA(prelp) ).
      cl_hr_pnnnn_type_cast=>pnnnn_to_prelp( EXPORTING pnnnn = <structure>
                                             IMPORTING prelp = prelp_orig ).


      it_aend = CORRESPONDING /sew/int_it_aend( prelp_orig ).
      it_aend-aend_id = aend_id.

*     Set action for infotypes other then IT0000
      it_aend-action = infotype_xml_processor->action.
      "Check for active status.
      READ TABLE fields WITH KEY field_oracle = 'AssStatusType' ASSIGNING FIELD-SYMBOL(<status>).
      IF <status> IS ASSIGNED AND <status>-value CS 'INACTIVE' AND it_aend-action NOT IN /sew/cl_int_constants=>termination_range.
        it_aend-active = 'I'.
      ENDIF.

      IF sap_id IS NOT INITIAL AND it_aend-action IN /sew/cl_int_constants=>termination_range AND <status>-value CS 'INACTIVE'.
        SELECT * FROM pa0000 INTO TABLE @DATA(it0000) WHERE pernr = @it_aend-pernr AND endda GE @begda AND begda LE @endda.
        IF sy-subrc IS INITIAL.
          READ TABLE it0000 INTO DATA(current_0000) INDEX 1.
          IF current_0000-massn IN /sew/cl_int_constants=>termination_range.
            it_aend-active = 'I'.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD process_0008.
    DATA: classdescr  TYPE REF TO cl_abap_classdescr,
          p0008_tab   TYPE TABLE OF p0008,
          t510_single TYPE t510.
*   Check if molga specific method is available
    classdescr ?= cl_abap_typedescr=>describe_by_name( '/SEW/CL_INT_INFTY_HANDLER' ).
    DATA(infotype_handler) = /sew/cl_int_general_settings=>get_infotyphandler( ).
    DATA(method) = |PROCESS_| && infotype_xml_processor->infty && '_' && infotype_xml_processor->molga.
    READ TABLE classdescr->methods WITH KEY name = method TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL."Molga spefic method availlable
      CALL METHOD (infotype_handler)=>(method)
        EXPORTING
          begda                  = begda
          endda                  = endda
          sap_id                 = sap_id
          cloud_id               = cloud_id
          aend_id                = aend_id
          has_error              = has_error
          fields                 = fields
          data                   = data
          infotype_xml_processor = infotype_xml_processor
        IMPORTING
          it_aend                = it_aend.
    ELSE." Logic if not molga specific.

*     Read current infotype with begda = begda of current entry
      DATA(lo_infotype) = NEW /sew/cl_int_it_operation( ).
      lo_infotype->read_paxxxx_dcif(
        EXPORTING
          iv_infty        = '0008'
          iv_pernr        = CONV #( sap_id )
          iv_stidat       = endda
          iv_subty        = '0'
        IMPORTING
          record_tab      = p0008_tab ).
      IF p0008_tab IS NOT INITIAL.
        DATA(p0008) = VALUE #( p0008_tab[ 1 ] OPTIONAL ).
      ELSE.
*     Fill general fields first in case no IT0008 exists
*     Assign pernr
        p0008-pernr = sap_id.
*     Set infotype
        p0008-infty = /sew/cl_int_constants=>it0008.
      ENDIF.
*     Fill other fields
      p0008-begda = begda.
      p0008-endda = endda.
      p0008-subty = '0'.
*     Fill tarif info TRFAR, TRFGB, TRFGR
      READ TABLE fields WITH KEY field_sap = 'TRFAR' begda = begda endda = endda ASSIGNING FIELD-SYMBOL(<trfar>).
      IF <trfar> IS ASSIGNED.
        p0008-trfar = COND #( WHEN <trfar>-value_converted IS NOT INITIAL THEN <trfar>-value_converted
                              "WHEN <trfar>-value_mapped IS NOT INITIAL THEN <trfar>-value_mapped
                              WHEN <trfar>-value_mapped IS NOT INITIAL THEN <trfar>-value_mapped
                              ELSE <trfar>-value ).
      ELSE.
        LOOP AT fields ASSIGNING <trfar> WHERE field_sap = 'TRFAR' AND begda LE begda AND endda = endda.
          IF <trfar> IS ASSIGNED.
            p0008-trfar = COND #( WHEN <trfar>-value_converted IS NOT INITIAL THEN <trfar>-value_converted
                                  "WHEN <trfar>-value_mapped IS NOT INITIAL THEN <trfar>-value_mapped
                                  WHEN <trfar>-value_mapped IS NOT INITIAL THEN <trfar>-value_mapped
                                  ELSE <trfar>-value ).
          ENDIF.
        ENDLOOP.
      ENDIF.
      READ TABLE fields WITH KEY field_sap = 'TRFGB' begda = begda endda = endda ASSIGNING FIELD-SYMBOL(<trfgb>).
      IF <trfgb> IS ASSIGNED.
        p0008-trfgb = COND #( WHEN <trfgb>-value_converted IS NOT INITIAL THEN <trfgb>-value_converted
                              WHEN <trfgb>-value_mapped IS NOT INITIAL THEN <trfgb>-value_mapped
                              "WHEN <trfgb>-value_converted IS NOT INITIAL THEN <trfgb>-value_converted
                              ELSE <trfgb>-value ).
      ELSE.
        LOOP AT fields ASSIGNING <trfgb> WHERE field_sap = 'TRFGB' AND begda LE begda AND endda = endda.
          IF <trfgb> IS ASSIGNED.
            p0008-trfgb = COND #( WHEN <trfgb>-value_converted IS NOT INITIAL THEN <trfgb>-value_converted
                                  "WHEN <trfar>-value_mapped IS NOT INITIAL THEN <trfar>-value_mapped
                                  WHEN <trfgb>-value_mapped IS NOT INITIAL THEN <trfgb>-value_mapped
                                  ELSE <trfgb>-value ).
          ENDIF.
        ENDLOOP.
      ENDIF.
      READ TABLE fields WITH KEY field_sap = 'TRFGR' begda = begda endda = endda ASSIGNING FIELD-SYMBOL(<trfgr>).
      IF <trfgr> IS ASSIGNED AND <trfar> IS ASSIGNED.
        p0008-trfgr = COND #( WHEN <trfgr>-value_converted IS NOT INITIAL THEN <trfgr>-value_converted
                              "WHEN <trfgr>-value_mapped IS NOT INITIAL THEN <trfgr>-value_mapped
                              WHEN <trfar>-value_mapped IS NOT INITIAL THEN <trfar>-value_mapped
                              ELSE <trfgr>-value ).
      ELSE.
        LOOP AT fields ASSIGNING <trfgr> WHERE field_sap = 'TRFGR' AND begda LE begda AND endda = endda.
          IF <trfgr> IS ASSIGNED AND <trfar> IS ASSIGNED.
            p0008-trfgr = COND #( WHEN <trfgr>-value_converted IS NOT INITIAL THEN <trfgr>-value_converted
                                  "WHEN <trfar>-value_mapped IS NOT INITIAL THEN <trfar>-value_mapped
                                  WHEN <trfgr>-value_mapped IS NOT INITIAL THEN <trfgr>-value_mapped
                                  ELSE <trfgr>-value ).
          ENDIF.
        ENDLOOP.
      ENDIF.
      READ TABLE fields WITH KEY field_sap = 'TRFST' begda = begda endda = endda ASSIGNING FIELD-SYMBOL(<trfst>).
      IF <trfst> IS ASSIGNED AND <trfar> IS ASSIGNED.
        IF <trfst>-value_converted NE 'DELETED'.
          p0008-trfst = COND #( WHEN <trfst>-value_converted IS NOT INITIAL THEN <trfst>-value_converted
                                "WHEN <trfst>-value_mapped IS NOT INITIAL THEN <trfst>-value_mapped
                                WHEN <trfar>-value_mapped IS NOT INITIAL THEN <trfar>-value_mapped
                                ELSE <trfst>-value ).
        ENDIF.
      ELSE.
        LOOP AT fields ASSIGNING <trfst> WHERE field_sap = 'TRFST' AND begda LE begda AND endda = endda.
          IF <trfst> IS ASSIGNED AND <trfar> IS ASSIGNED.
            IF <trfst>-value_converted NE 'DELETED'.
              p0008-trfst = COND #( WHEN <trfst>-value_converted IS NOT INITIAL THEN <trfst>-value_converted
                                    "WHEN <trfar>-value_mapped IS NOT INITIAL THEN <trfar>-value_mapped
                                    WHEN <trfst>-value_mapped IS NOT INITIAL THEN <trfst>-value_mapped
                                    ELSE <trfst>-value ).
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
*     Collect payments in table.
      DATA(payments) = VALUE t_payments( FOR payment IN fields WHERE ( field_sap = 'LGART' AND begda LE endda AND endda GE begda ) ( oracle_wagetype = payment-value seqnr = payment-seqnr ) ).
      CHECK payments IS NOT INITIAL.
      LOOP AT payments ASSIGNING FIELD-SYMBOL(<payment>).
        DATA(offset) = strlen( <payment>-oracle_wagetype ) - 4.
        <payment>-wagetype = <payment>-oracle_wagetype+offset(4).
        READ TABLE fields WITH KEY seqnr = <payment>-seqnr field_sap = 'BETRG' ASSIGNING FIELD-SYMBOL(<field>).
        IF <field> IS ASSIGNED.
          <payment>-amount = <field>-value.
        ENDIF.
      ENDLOOP.

      SORT payments ASCENDING BY wagetype.
      DELETE ADJACENT DUPLICATES FROM payments COMPARING wagetype.
*     Start processing of infotype 0008.
      DATA(continue) = abap_true.
      DATA(wage_types_done) = CONV char2( '01' ).
      DATA(comp_wage_type) = 'LGA' && wage_types_done.
      DATA(comp_amount) = 'BET' && wage_types_done.
      DATA(comp_ind) = 'IND' && wage_types_done.
      WHILE continue = abap_true.
        ASSIGN COMPONENT comp_wage_type OF STRUCTURE p0008 TO FIELD-SYMBOL(<wage_type>).
        ASSIGN COMPONENT comp_amount OF STRUCTURE p0008 TO FIELD-SYMBOL(<amount>).
        ASSIGN COMPONENT comp_ind OF STRUCTURE p0008 TO FIELD-SYMBOL(<ind>).
*       Check if IT0008 is filled and
        IF ( <wage_type> IS ASSIGNED AND <wage_type> IS NOT INITIAL ) AND ( <amount> IS ASSIGNED AND <amount> IS NOT INITIAL ).
          READ TABLE payments WITH KEY wagetype = <wage_type> ASSIGNING FIELD-SYMBOL(<new_payment>).
          IF sy-subrc IS NOT INITIAL.
            <amount> = <new_payment>-amount.
            IF p0008-trfgr = 'NVT'.
              SELECT SINGLE * FROM t510 INTO @t510_single WHERE molga = @infotype_xml_processor->molga AND trfar = @p0008-trfar AND trfgb = @p0008-trfgb
                                                                                                     AND trfgr = @p0008-trfgr
                                                                                                     AND begda LE @endda AND endda GE @begda.
            ELSE.
              SELECT SINGLE * FROM t510 INTO @t510_single WHERE molga = @infotype_xml_processor->molga AND trfar = @p0008-trfar AND trfgb = @p0008-trfgb
                                                                                                     AND trfgr = @p0008-trfgr AND trfst = @p0008-trfst
                                                                                                     AND begda LE @endda AND endda GE @begda.
            ENDIF.
            IF t510_single IS NOT INITIAL AND ( t510_single-betrg IS INITIAL OR t510_single-betrg = '0.00' ).
              <ind> = '-'.
            ELSE.
              CLEAR: <amount>.
              <ind> = 'I'.
            ENDIF.
            UNASSIGN <new_payment>.
            DELETE payments WHERE wagetype = <wage_type>.
          ENDIF.
        ELSE.
          READ TABLE payments ASSIGNING <new_payment> INDEX 1.
          IF <new_payment> IS ASSIGNED.
            <wage_type> = <new_payment>-wagetype.
            <amount> = <new_payment>-amount.
            IF p0008-trfgr = 'NVT'.
              SELECT SINGLE * FROM t510 INTO @t510_single WHERE molga = @infotype_xml_processor->molga AND trfar = @p0008-trfar AND trfgb = @p0008-trfgb
                                                                                                     AND trfgr = @p0008-trfgr
                                                                                                     AND begda LE @endda AND endda GE @begda.
            ELSE.
              SELECT SINGLE * FROM t510 INTO @t510_single WHERE molga = @infotype_xml_processor->molga AND trfar = @p0008-trfar AND trfgb = @p0008-trfgb
                                                                                                     AND trfgr = @p0008-trfgr AND trfst = @p0008-trfst
                                                                                                     AND begda LE @endda AND endda GE @begda.
            ENDIF.
            IF t510_single IS NOT INITIAL AND ( t510_single-betrg IS INITIAL OR t510_single-betrg = '0.00' ).
              <ind> = '-'.
            ELSE.
              CLEAR: <amount>.
              <ind> = 'I'.
            ENDIF.
            UNASSIGN <new_payment>.
            DELETE payments INDEX 1.
          ELSE.
            continue = abap_false.
          ENDIF.
        ENDIF.
        wage_types_done = wage_types_done + 1.
        UNPACK wage_types_done TO wage_types_done.
        comp_wage_type = 'LGA' && wage_types_done.
        comp_amount = 'BET' && wage_types_done.
        comp_ind = 'IND' && wage_types_done.
        IF wage_types_done = 41.
          continue = abap_false.
        ENDIF.
      ENDWHILE.
      infotype_xml_processor->pa_to_prelp( EXPORTING infotype = p0008
                                 aend_id  = aend_id
                       IMPORTING prelp    = DATA(prelp) ).
      it_aend = CORRESPONDING /sew/int_it_aend( prelp ).
      it_aend-aend_id = aend_id.
    ENDIF.
*    IF p0008-ind01 IS NOT INITIAL.
*     Transfer data to IT_AEND structure

*    ENDIF.
*    ENDIF.
  ENDMETHOD.


  METHOD PROCESS_0008_03.
    DATA: classdescr  TYPE REF TO cl_abap_classdescr,
          p0008_tab   TYPE TABLE OF p0008,
          t510_single TYPE t510.
*   Check if molga specific method is available
    classdescr ?= cl_abap_typedescr=>describe_by_name( '/SEW/CL_INT_INFTY_HANDLER' ).
    DATA(method) = |PROCESS_| && infotype_xml_processor->infty && '_' && infotype_xml_processor->molga.
    READ TABLE classdescr->methods WITH KEY name = method TRANSPORTING NO FIELDS.
*    IF sy-subrc IS INITIAL."Molga spefic method availlable
*      CALL METHOD infotype_xml_processor->(method)
*        EXPORTING
*          begda     = begda
*          endda     = endda
*          sap_id    = sap_id
*          cloud_id  = cloud_id
*          aend_id   = aend_id
*          has_error = has_error
*          fields    = fields
*          data      = data
*        IMPORTING
*          it_aend   = it_aend.
*    ELSE." Logic if not molga specific.

*     Read current infotype with begda = begda of current entry
      DATA(lo_infotype) = NEW /sew/cl_int_it_operation( ).
      lo_infotype->read_paxxxx_dcif(
        EXPORTING
          iv_infty        = '0008'
          iv_pernr        = CONV #( sap_id )
          iv_stidat       = endda
          iv_subty        = '0'
        IMPORTING
          record_tab      = p0008_tab ).
      IF p0008_tab IS NOT INITIAL.
        DATA(p0008) = VALUE #( p0008_tab[ 1 ] OPTIONAL ).
      ELSE.
*     Fill general fields first in case no IT0008 exists
*     Assign pernr
        p0008-pernr = sap_id.
*     Set infotype
        p0008-infty = /sew/cl_int_constants=>it0008.
      ENDIF.
*     Fill other fields
      p0008-begda = begda.
      p0008-endda = endda.
      p0008-subty = '0'.
*     Fill tarif info TRFAR, TRFGB, TRFGR
      READ TABLE fields WITH KEY field_sap = 'TRFAR' begda = begda endda = endda ASSIGNING FIELD-SYMBOL(<trfar>).
      IF <trfar> IS ASSIGNED.
        p0008-trfar = COND #( WHEN <trfar>-value_converted IS NOT INITIAL THEN <trfar>-value_converted
                              "WHEN <trfar>-value_mapped IS NOT INITIAL THEN <trfar>-value_mapped
                              WHEN <trfar>-value_mapped IS NOT INITIAL THEN <trfar>-value_mapped
                              ELSE <trfar>-value ).
      ELSE.
        LOOP AT fields ASSIGNING <trfar> WHERE field_sap = 'TRFAR' AND begda LE begda AND endda = endda.
          IF <trfar> IS ASSIGNED.
            p0008-trfar = COND #( WHEN <trfar>-value_converted IS NOT INITIAL THEN <trfar>-value_converted
                                  "WHEN <trfar>-value_mapped IS NOT INITIAL THEN <trfar>-value_mapped
                                  WHEN <trfar>-value_mapped IS NOT INITIAL THEN <trfar>-value_mapped
                                  ELSE <trfar>-value ).
          ENDIF.
        ENDLOOP.
      ENDIF.
      READ TABLE fields WITH KEY field_sap = 'TRFGB' begda = begda endda = endda ASSIGNING FIELD-SYMBOL(<trfgb>).
      IF <trfgb> IS ASSIGNED.
        p0008-trfgb = COND #( WHEN <trfgb>-value_converted IS NOT INITIAL THEN <trfgb>-value_converted
                              WHEN <trfgb>-value_mapped IS NOT INITIAL THEN <trfgb>-value_mapped
                              "WHEN <trfgb>-value_converted IS NOT INITIAL THEN <trfgb>-value_converted
                              ELSE <trfgb>-value ).
      ELSE.
        LOOP AT fields ASSIGNING <trfgb> WHERE field_sap = 'TRFGB' AND begda LE begda AND endda = endda.
          IF <trfgb> IS ASSIGNED.
            p0008-trfgb = COND #( WHEN <trfgb>-value_converted IS NOT INITIAL THEN <trfgb>-value_converted
                                  "WHEN <trfar>-value_mapped IS NOT INITIAL THEN <trfar>-value_mapped
                                  WHEN <trfgb>-value_mapped IS NOT INITIAL THEN <trfgb>-value_mapped
                                  ELSE <trfgb>-value ).
          ENDIF.
        ENDLOOP.
      ENDIF.
      READ TABLE fields WITH KEY field_sap = 'TRFGR' begda = begda endda = endda ASSIGNING FIELD-SYMBOL(<trfgr>).
      IF <trfgr> IS ASSIGNED AND <trfar> IS ASSIGNED.
        p0008-trfgr = COND #( WHEN <trfgr>-value_converted IS NOT INITIAL THEN <trfgr>-value_converted
                              "WHEN <trfgr>-value_mapped IS NOT INITIAL THEN <trfgr>-value_mapped
                              WHEN <trfar>-value_mapped IS NOT INITIAL THEN <trfar>-value_mapped
                              ELSE <trfgr>-value ).
      ELSE.
        LOOP AT fields ASSIGNING <trfgr> WHERE field_sap = 'TRFGR' AND begda LE begda AND endda = endda.
          IF <trfgr> IS ASSIGNED AND <trfar> IS ASSIGNED.
            p0008-trfgr = COND #( WHEN <trfgr>-value_converted IS NOT INITIAL THEN <trfgr>-value_converted
                                  "WHEN <trfar>-value_mapped IS NOT INITIAL THEN <trfar>-value_mapped
                                  WHEN <trfgr>-value_mapped IS NOT INITIAL THEN <trfgr>-value_mapped
                                  ELSE <trfgr>-value ).
          ENDIF.
        ENDLOOP.
      ENDIF.
      READ TABLE fields WITH KEY field_sap = 'TRFST' begda = begda endda = endda ASSIGNING FIELD-SYMBOL(<trfst>).
      IF <trfst> IS ASSIGNED AND <trfar> IS ASSIGNED.
        IF <trfst>-value_converted NE 'DELETED'.
          p0008-trfst = COND #( WHEN <trfst>-value_converted IS NOT INITIAL THEN <trfst>-value_converted
                                "WHEN <trfst>-value_mapped IS NOT INITIAL THEN <trfst>-value_mapped
                                WHEN <trfar>-value_mapped IS NOT INITIAL THEN <trfar>-value_mapped
                                ELSE <trfst>-value ).
        ENDIF.
      ELSE.
        LOOP AT fields ASSIGNING <trfst> WHERE field_sap = 'TRFST' AND begda LE begda AND endda = endda.
          IF <trfst> IS ASSIGNED AND <trfar> IS ASSIGNED.
            IF <trfst>-value_converted NE 'DELETED'.
              p0008-trfst = COND #( WHEN <trfst>-value_converted IS NOT INITIAL THEN <trfst>-value_converted
                                    "WHEN <trfar>-value_mapped IS NOT INITIAL THEN <trfar>-value_mapped
                                    WHEN <trfst>-value_mapped IS NOT INITIAL THEN <trfst>-value_mapped
                                    ELSE <trfst>-value ).
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
*     Collect payments in table.
      DATA(payments) = VALUE t_payments( FOR payment IN fields WHERE ( field_sap = 'LGART' AND begda LE endda AND endda GE begda ) ( oracle_wagetype = payment-value seqnr = payment-seqnr ) ).
      CHECK payments IS NOT INITIAL.
      LOOP AT payments ASSIGNING FIELD-SYMBOL(<payment>).
        DATA(offset) = strlen( <payment>-oracle_wagetype ) - 4.
        <payment>-wagetype = <payment>-oracle_wagetype+offset(4).
        READ TABLE fields WITH KEY seqnr = <payment>-seqnr field_sap = 'BETRG' ASSIGNING FIELD-SYMBOL(<field>).
        IF <field> IS ASSIGNED.
          <payment>-amount = <field>-value.
        ENDIF.
      ENDLOOP.

      SORT payments ASCENDING BY wagetype.
      DELETE ADJACENT DUPLICATES FROM payments COMPARING wagetype.
*     Start processing of infotype 0008.
      DATA(continue) = abap_true.
      DATA(wage_types_done) = CONV char2( '01' ).
      DATA(comp_wage_type) = 'LGA' && wage_types_done.
      DATA(comp_amount) = 'BET' && wage_types_done.
      DATA(comp_ind) = 'IND' && wage_types_done.
      WHILE continue = abap_true.
        ASSIGN COMPONENT comp_wage_type OF STRUCTURE p0008 TO FIELD-SYMBOL(<wage_type>).
        ASSIGN COMPONENT comp_amount OF STRUCTURE p0008 TO FIELD-SYMBOL(<amount>).
        ASSIGN COMPONENT comp_ind OF STRUCTURE p0008 TO FIELD-SYMBOL(<ind>).
*       Check if IT0008 is filled and
        IF ( <wage_type> IS ASSIGNED AND <wage_type> IS NOT INITIAL ) AND ( <amount> IS ASSIGNED AND <amount> IS NOT INITIAL ).
          READ TABLE payments WITH KEY wagetype = <wage_type> ASSIGNING FIELD-SYMBOL(<new_payment>).
          IF sy-subrc IS NOT INITIAL.
            <amount> = <new_payment>-amount.
            IF p0008-trfgr = 'NVT'.
              SELECT SINGLE * FROM t510 INTO @t510_single WHERE molga = @infotype_xml_processor->molga AND trfar = @p0008-trfar AND trfgb = @p0008-trfgb
                                                                                                     AND trfgr = @p0008-trfgr
                                                                                                     AND begda LE @endda AND endda GE @begda.
            ELSE.
              SELECT SINGLE * FROM t510 INTO @t510_single WHERE molga = @infotype_xml_processor->molga AND trfar = @p0008-trfar AND trfgb = @p0008-trfgb
                                                                                                     AND trfgr = @p0008-trfgr AND trfst = @p0008-trfst
                                                                                                     AND begda LE @endda AND endda GE @begda.
            ENDIF.
*            IF t510_single IS NOT INITIAL AND ( t510_single-betrg IS INITIAL OR t510_single-betrg = '0.00' ).
*              <ind> = '-'.
*            ELSE.
*              CLEAR: <amount>.
*              <ind> = 'I'.
*            ENDIF.
            UNASSIGN <new_payment>.
            DELETE payments WHERE wagetype = <wage_type>.
          ENDIF.
        ELSE.
          READ TABLE payments ASSIGNING <new_payment> INDEX 1.
          IF <new_payment> IS ASSIGNED.
            <wage_type> = <new_payment>-wagetype.
            <amount> = <new_payment>-amount.
            IF p0008-trfgr = 'NVT'.
              SELECT SINGLE * FROM t510 INTO @t510_single WHERE molga = @infotype_xml_processor->molga AND trfar = @p0008-trfar AND trfgb = @p0008-trfgb
                                                                                                     AND trfgr = @p0008-trfgr
                                                                                                     AND begda LE @endda AND endda GE @begda.
            ELSE.
              SELECT SINGLE * FROM t510 INTO @t510_single WHERE molga = @infotype_xml_processor->molga AND trfar = @p0008-trfar AND trfgb = @p0008-trfgb
                                                                                                     AND trfgr = @p0008-trfgr AND trfst = @p0008-trfst
                                                                                                     AND begda LE @endda AND endda GE @begda.
            ENDIF.
*            IF t510_single IS NOT INITIAL AND ( t510_single-betrg IS INITIAL OR t510_single-betrg = '0.00' ).
*              <ind> = '-'.
*            ELSE.
*              CLEAR: <amount>.
*              <ind> = 'I'.
*            ENDIF.
            UNASSIGN <new_payment>.
            DELETE payments INDEX 1.
          ELSE.
            continue = abap_false.
          ENDIF.
        ENDIF.
        wage_types_done = wage_types_done + 1.
        UNPACK wage_types_done TO wage_types_done.
        comp_wage_type = 'LGA' && wage_types_done.
        comp_amount = 'BET' && wage_types_done.
        comp_ind = 'IND' && wage_types_done.
        IF wage_types_done = 41.
          continue = abap_false.
        ENDIF.
      ENDWHILE.
*    ENDIF.
*    IF p0008-ind01 IS NOT INITIAL.
*     Transfer data to IT_AEND structure
    infotype_xml_processor->pa_to_prelp( EXPORTING infotype = p0008
                               aend_id  = aend_id
                     IMPORTING prelp    = DATA(prelp) ).
    it_aend = CORRESPONDING /sew/int_it_aend( prelp ).
    it_aend-aend_id = aend_id.
*    ENDIF.
*    ENDIF.
  ENDMETHOD.


  METHOD PROCESS_0008_15.
    DATA: classdescr  TYPE REF TO cl_abap_classdescr,
          p0008_tab   TYPE TABLE OF p0008,
          t510_single TYPE t510.
*   Check if molga specific method is available
    classdescr ?= cl_abap_typedescr=>describe_by_name( '/SEW/CL_INT_INFTY_HANDLER' ).
    DATA(method) = |PROCESS_| && infotype_xml_processor->infty && '_' && infotype_xml_processor->molga.
    READ TABLE classdescr->methods WITH KEY name = method TRANSPORTING NO FIELDS.
*    IF sy-subrc IS INITIAL."Molga spefic method availlable
*      CALL METHOD infotype_xml_processor->(method)
*        EXPORTING
*          begda     = begda
*          endda     = endda
*          sap_id    = sap_id
*          cloud_id  = cloud_id
*          aend_id   = aend_id
*          has_error = has_error
*          fields    = fields
*          data      = data
*        IMPORTING
*          it_aend   = it_aend.
*    ELSE." Logic if not molga specific.

*     Read current infotype with begda = begda of current entry
      DATA(lo_infotype) = NEW /sew/cl_int_it_operation( ).
      lo_infotype->read_paxxxx_dcif(
        EXPORTING
          iv_infty        = '0008'
        IMPORTING
          record_tab      = p0008_tab ).
      IF p0008_tab IS NOT INITIAL.
        DATA(p0008) = VALUE #( p0008_tab[ 1 ] OPTIONAL ).
      ELSE.
*     Fill general fields first in case no IT0008 exists
*     Assign pernr
        p0008-pernr = sap_id.
*     Set infotype
        p0008-infty = /sew/cl_int_constants=>it0008.
      ENDIF.
*     Fill other fields
      p0008-begda = begda.
      p0008-endda = endda.
      p0008-subty = '0'.
*     Fill tarif info TRFAR, TRFGB, TRFGR
      READ TABLE fields WITH KEY field_sap = 'TRFAR' begda = begda endda = endda ASSIGNING FIELD-SYMBOL(<trfar>).
      IF <trfar> IS ASSIGNED.
        p0008-trfar = COND #( WHEN <trfar>-value_converted IS NOT INITIAL THEN <trfar>-value_converted
                              "WHEN <trfar>-value_mapped IS NOT INITIAL THEN <trfar>-value_mapped
                              WHEN <trfar>-value_mapped IS NOT INITIAL THEN <trfar>-value_mapped
                              ELSE <trfar>-value ).
      ELSE.
        LOOP AT fields ASSIGNING <trfar> WHERE field_sap = 'TRFAR' AND begda LE begda AND endda = endda.
          IF <trfar> IS ASSIGNED.
            p0008-trfar = COND #( WHEN <trfar>-value_converted IS NOT INITIAL THEN <trfar>-value_converted
                                  "WHEN <trfar>-value_mapped IS NOT INITIAL THEN <trfar>-value_mapped
                                  WHEN <trfar>-value_mapped IS NOT INITIAL THEN <trfar>-value_mapped
                                  ELSE <trfar>-value ).
          ENDIF.
        ENDLOOP.
      ENDIF.
      READ TABLE fields WITH KEY field_sap = 'TRFGB' begda = begda endda = endda ASSIGNING FIELD-SYMBOL(<trfgb>).
      IF <trfgb> IS ASSIGNED.
        p0008-trfgb = COND #( WHEN <trfgb>-value_converted IS NOT INITIAL THEN <trfgb>-value_converted
                              WHEN <trfgb>-value_mapped IS NOT INITIAL THEN <trfgb>-value_mapped
                              "WHEN <trfgb>-value_converted IS NOT INITIAL THEN <trfgb>-value_converted
                              ELSE <trfgb>-value ).
      ELSE.
        LOOP AT fields ASSIGNING <trfgb> WHERE field_sap = 'TRFGB' AND begda LE begda AND endda = endda.
          IF <trfgb> IS ASSIGNED.
            p0008-trfgb = COND #( WHEN <trfgb>-value_converted IS NOT INITIAL THEN <trfgb>-value_converted
                                  "WHEN <trfar>-value_mapped IS NOT INITIAL THEN <trfar>-value_mapped
                                  WHEN <trfgb>-value_mapped IS NOT INITIAL THEN <trfgb>-value_mapped
                                  ELSE <trfgb>-value ).
          ENDIF.
        ENDLOOP.
      ENDIF.
      READ TABLE fields WITH KEY field_sap = 'TRFGR' begda = begda endda = endda ASSIGNING FIELD-SYMBOL(<trfgr>).
      IF <trfgr> IS ASSIGNED AND <trfar> IS ASSIGNED.
        p0008-trfgr = COND #( WHEN <trfgr>-value_converted IS NOT INITIAL THEN <trfgr>-value_converted
                              "WHEN <trfgr>-value_mapped IS NOT INITIAL THEN <trfgr>-value_mapped
                              WHEN <trfar>-value_mapped IS NOT INITIAL THEN <trfar>-value_mapped
                              ELSE <trfgr>-value ).
      ELSE.
        LOOP AT fields ASSIGNING <trfgr> WHERE field_sap = 'TRFGR' AND begda LE begda AND endda = endda.
          IF <trfgr> IS ASSIGNED AND <trfar> IS ASSIGNED.
            p0008-trfgr = COND #( WHEN <trfgr>-value_converted IS NOT INITIAL THEN <trfgr>-value_converted
                                  "WHEN <trfar>-value_mapped IS NOT INITIAL THEN <trfar>-value_mapped
                                  WHEN <trfgr>-value_mapped IS NOT INITIAL THEN <trfgr>-value_mapped
                                  ELSE <trfgr>-value ).
          ENDIF.
        ENDLOOP.
      ENDIF.
      READ TABLE fields WITH KEY field_sap = 'TRFST' begda = begda endda = endda ASSIGNING FIELD-SYMBOL(<trfst>).
      IF <trfst> IS ASSIGNED AND <trfar> IS ASSIGNED.
        IF <trfst>-value_converted NE 'DELETED'.
          p0008-trfst = COND #( WHEN <trfst>-value_converted IS NOT INITIAL THEN <trfst>-value_converted
                                "WHEN <trfst>-value_mapped IS NOT INITIAL THEN <trfst>-value_mapped
                                WHEN <trfar>-value_mapped IS NOT INITIAL THEN <trfar>-value_mapped
                                ELSE <trfst>-value ).
        ENDIF.
      ELSE.
        LOOP AT fields ASSIGNING <trfst> WHERE field_sap = 'TRFST' AND begda LE begda AND endda = endda.
          IF <trfst> IS ASSIGNED AND <trfar> IS ASSIGNED.
            IF <trfst>-value_converted NE 'DELETED'.
              p0008-trfst = COND #( WHEN <trfst>-value_converted IS NOT INITIAL THEN <trfst>-value_converted
                                    "WHEN <trfar>-value_mapped IS NOT INITIAL THEN <trfar>-value_mapped
                                    WHEN <trfst>-value_mapped IS NOT INITIAL THEN <trfst>-value_mapped
                                    ELSE <trfst>-value ).
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
*     Collect payments in table.
      DATA(payments) = VALUE t_payments( FOR payment IN fields WHERE ( field_sap = 'LGART' AND begda LE endda AND endda GE begda ) ( oracle_wagetype = payment-value seqnr = payment-seqnr ) ).
      CHECK payments IS NOT INITIAL.
      LOOP AT payments ASSIGNING FIELD-SYMBOL(<payment>).
        DATA(offset) = strlen( <payment>-oracle_wagetype ) - 4.
        <payment>-wagetype = <payment>-oracle_wagetype+offset(4).
        READ TABLE fields WITH KEY seqnr = <payment>-seqnr field_sap = 'BETRG' ASSIGNING FIELD-SYMBOL(<field>).
        IF <field> IS ASSIGNED.
          <payment>-amount = <field>-value.
        ENDIF.
      ENDLOOP.

      SORT payments DESCENDING BY wagetype.
      DELETE ADJACENT DUPLICATES FROM payments COMPARING wagetype.
*     Start processing of infotype 0008.
      DATA(continue) = abap_true.
      DATA(wage_types_done) = CONV char2( '01' ).
      DATA(comp_wage_type) = 'LGA' && wage_types_done.
      DATA(comp_amount) = 'BET' && wage_types_done.
      DATA(comp_ind) = 'IND' && wage_types_done.
      WHILE continue = abap_true.
        ASSIGN COMPONENT comp_wage_type OF STRUCTURE p0008 TO FIELD-SYMBOL(<wage_type>).
        ASSIGN COMPONENT comp_amount OF STRUCTURE p0008 TO FIELD-SYMBOL(<amount>).
        ASSIGN COMPONENT comp_ind OF STRUCTURE p0008 TO FIELD-SYMBOL(<ind>).
*       Check if IT0008 is filled and
        IF ( <wage_type> IS ASSIGNED AND <wage_type> IS NOT INITIAL ) AND ( <amount> IS ASSIGNED AND <amount> IS NOT INITIAL ).
          READ TABLE payments WITH KEY wagetype = <wage_type> ASSIGNING FIELD-SYMBOL(<new_payment>).
          IF sy-subrc IS NOT INITIAL.
            <amount> = <new_payment>-amount.
            IF p0008-trfgr = 'NVT'.
              SELECT SINGLE * FROM t510 INTO @t510_single WHERE molga = @infotype_xml_processor->molga AND trfar = @p0008-trfar AND trfgb = @p0008-trfgb
                                                                                                     AND trfgr = @p0008-trfgr
                                                                                                     AND begda LE @endda AND endda GE @begda.
            ELSE.
              SELECT SINGLE * FROM t510 INTO @t510_single WHERE molga = @infotype_xml_processor->molga AND trfar = @p0008-trfar AND trfgb = @p0008-trfgb
                                                                                                     AND trfgr = @p0008-trfgr AND trfst = @p0008-trfst
                                                                                                     AND begda LE @endda AND endda GE @begda.
            ENDIF.
*            IF t510_single IS NOT INITIAL AND ( t510_single-betrg IS INITIAL OR t510_single-betrg = '0.00' ).
*              <ind> = '-'.
*            ELSE.
*              CLEAR: <amount>.
*              <ind> = 'I'.
*            ENDIF.
            UNASSIGN <new_payment>.
            DELETE payments WHERE wagetype = <wage_type>.
          ENDIF.
        ELSE.
          READ TABLE payments ASSIGNING <new_payment> INDEX 1.
          IF <new_payment> IS ASSIGNED.
            <wage_type> = <new_payment>-wagetype.
            <amount> = <new_payment>-amount.
            IF p0008-trfgr = 'NVT'.
              SELECT SINGLE * FROM t510 INTO @t510_single WHERE molga = @infotype_xml_processor->molga AND trfar = @p0008-trfar AND trfgb = @p0008-trfgb
                                                                                                     AND trfgr = @p0008-trfgr
                                                                                                     AND begda LE @endda AND endda GE @begda.
            ELSE.
              SELECT SINGLE * FROM t510 INTO @t510_single WHERE molga = @infotype_xml_processor->molga AND trfar = @p0008-trfar AND trfgb = @p0008-trfgb
                                                                                                     AND trfgr = @p0008-trfgr AND trfst = @p0008-trfst
                                                                                                     AND begda LE @endda AND endda GE @begda.
            ENDIF.
*            IF t510_single IS NOT INITIAL AND ( t510_single-betrg IS INITIAL OR t510_single-betrg = '0.00' ).
*              <ind> = '-'.
*            ELSE.
*              CLEAR: <amount>.
*              <ind> = 'I'.
*            ENDIF.
            UNASSIGN <new_payment>.
            DELETE payments INDEX 1.
          ELSE.
            continue = abap_false.
          ENDIF.
        ENDIF.
        wage_types_done = wage_types_done + 1.
        UNPACK wage_types_done TO wage_types_done.
        comp_wage_type = 'LGA' && wage_types_done.
        comp_amount = 'BET' && wage_types_done.
        comp_ind = 'IND' && wage_types_done.
        IF wage_types_done = 41.
          continue = abap_false.
        ENDIF.
      ENDWHILE.
*    ENDIF.
*    IF p0008-ind01 IS NOT INITIAL.
*     Transfer data to IT_AEND structure
    infotype_xml_processor->pa_to_prelp( EXPORTING infotype = p0008
                               aend_id  = aend_id
                     IMPORTING prelp    = DATA(prelp) ).
    it_aend = CORRESPONDING /sew/int_it_aend( prelp ).
    it_aend-aend_id = aend_id.
*    ENDIF.
*    ENDIF.
  ENDMETHOD.


  METHOD process_0027.
    DATA: classdescr TYPE REF TO cl_abap_classdescr,
          p0027_tab  TYPE TABLE OF p0027.
*   Check if molga specific method is available
    classdescr ?= cl_abap_typedescr=>describe_by_name( '/SEW/CL_INT_INFTY_HANDLER' ).
    DATA(method) = |PROCESS_| && infotype_xml_processor->infty && '_' && infotype_xml_processor->molga.
    READ TABLE classdescr->methods WITH KEY name = method TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL."Molga spefic method availlable
      CALL METHOD infotype_xml_processor->(method)
        EXPORTING
          begda     = begda
          endda     = endda
          sap_id    = sap_id
          cloud_id  = cloud_id
          aend_id   = aend_id
          has_error = has_error
          fields    = fields
          data      = data
        IMPORTING
          it_aend   = it_aend.
    ELSE." Logic if not molga specific.

*     Read current infotype with begda = begda of current entry
      DATA(lo_infotype) = NEW /sew/cl_int_it_operation( ).
      lo_infotype->read_paxxxx_dcif(
        EXPORTING
          iv_infty        = '0027'
        IMPORTING
          record_tab      = p0027_tab ).
      IF p0027_tab IS NOT INITIAL.
        DATA(p0027) = VALUE #( p0027_tab[ 1 ] OPTIONAL ).
      ELSE.
*     Fill general fields first in case no IT0008 exists
*     Assign pernr
        p0027-pernr = sap_id.
*     Set infotype
        p0027-infty = /sew/cl_int_constants=>it0027.
      ENDIF.
*     Fill other fields
      p0027-begda = begda.
      p0027-endda = endda.
      p0027-subty = '01'.
      p0027-kstar = '01'.
*     Fill tarif info TRFAR, TRFGB, TRFGR
*      READ TABLE fields WITH KEY field_sap = 'TRFAR' ASSIGNING FIELD-SYMBOL(<trfar>).
*      IF <trfar> IS ASSIGNED.
*        p0008-trfar = COND #( WHEN <trfar>-value_converted IS NOT INITIAL THEN <trfar>-value_converted
*                              "WHEN <trfar>-value_mapped IS NOT INITIAL THEN <trfar>-value_mapped
*                              WHEN <trfar>-value_mapped IS NOT INITIAL THEN <trfar>-value_mapped
*                              ELSE <trfar>-value ).
*      ENDIF.
*      READ TABLE fields WITH KEY field_sap = 'TRFGB' ASSIGNING FIELD-SYMBOL(<trfgb>).
*      IF <trfgb> IS ASSIGNED.
*        p0008-trfgb = COND #( WHEN <trfgb>-value_converted IS NOT INITIAL THEN <trfgb>-value_converted
*                              WHEN <trfgb>-value_mapped IS NOT INITIAL THEN <trfgb>-value_mapped
*                              "WHEN <trfgb>-value_converted IS NOT INITIAL THEN <trfgb>-value_converted
*                              ELSE <trfgb>-value ).
**        p0008-trfgb = split2.
*      ENDIF.
*      READ TABLE fields WITH KEY field_sap = 'TRFGR' ASSIGNING FIELD-SYMBOL(<trfgr>).
*      IF <trfgr> IS ASSIGNED AND <trfar> IS ASSIGNED.
*        p0008-trfgr = COND #( WHEN <trfgr>-value_converted IS NOT INITIAL THEN <trfgr>-value_converted
*                              "WHEN <trfgr>-value_mapped IS NOT INITIAL THEN <trfgr>-value_mapped
*                              WHEN <trfar>-value_mapped IS NOT INITIAL THEN <trfar>-value_mapped
*                              ELSE <trfgr>-value ).
*      ENDIF.
*      READ TABLE fields WITH KEY field_sap = 'TRFST' ASSIGNING FIELD-SYMBOL(<trfst>).
*      IF <trfst> IS ASSIGNED AND <trfar> IS ASSIGNED.
*        p0008-trfst = COND #( WHEN <trfst>-value_converted IS NOT INITIAL THEN <trfst>-value_converted
*                              "WHEN <trfst>-value_mapped IS NOT INITIAL THEN <trfst>-value_mapped
*                              WHEN <trfar>-value_mapped IS NOT INITIAL THEN <trfar>-value_mapped
*                              ELSE <trfst>-value ).
*      ENDIF.
*     Collect payments in table.
      DATA(costings) = VALUE t_costing( FOR costing IN fields WHERE ( field_sap = 'KST01' AND begda LE endda AND endda GE begda ) ( company_code = costing-value cost_center = costing-value_converted seqnr = costing-seqnr ) ).
      CHECK costings IS NOT INITIAL.
      LOOP AT costings ASSIGNING FIELD-SYMBOL(<costing>).
*        DATA(offset) = strlen( <payment>-oracle_wagetype ) - 4.
*        <payment>-wagetype = <payment>-oracle_wagetype+offset(4).
        READ TABLE fields WITH KEY seqnr = <costing>-seqnr field_sap = 'KPR01' ASSIGNING FIELD-SYMBOL(<field>).
        IF <field> IS ASSIGNED.
          <costing>-proportion = <field>-value_converted.
        ENDIF.
      ENDLOOP.
      SORT costings ASCENDING BY cost_center.
      DELETE ADJACENT DUPLICATES FROM costings COMPARING company_code cost_center.
*     Start processing of infotype 0008.
      DATA(continue) = abap_true.
      DATA(kostl_done) = CONV char2( '01' ).
      DATA(kostl_nr) = 'KST' && kostl_done.
      DATA(bukrs_nr) = 'KBU' && kostl_done.
      DATA(prop_nr) = 'KPR' && kostl_done.
      WHILE continue = abap_true.
        ASSIGN COMPONENT kostl_nr OF STRUCTURE p0027 TO FIELD-SYMBOL(<kostl>).
        ASSIGN COMPONENT prop_nr OF STRUCTURE p0027 TO FIELD-SYMBOL(<prop>).
        ASSIGN COMPONENT bukrs_nr OF STRUCTURE p0027 TO FIELD-SYMBOL(<bukrs>).
**       Check if IT0008 is filled and
        IF ( <kostl> IS ASSIGNED AND <kostl> IS NOT INITIAL ) AND ( <prop> IS ASSIGNED AND <prop> IS NOT INITIAL ) AND ( <bukrs> IS ASSIGNED AND <bukrs> IS NOT INITIAL ).
          READ TABLE costings WITH KEY cost_center = <kostl> ASSIGNING FIELD-SYMBOL(<new_costing>).
          IF sy-subrc IS NOT INITIAL.
            <prop> = <new_costing>-proportion.
            <bukrs> = <new_costing>-company_code.
            UNASSIGN <new_costing>.
            DELETE costings WHERE cost_center = <kostl>.
          ENDIF.
        ELSE.
          READ TABLE costings ASSIGNING <new_costing> INDEX 1.
          IF <new_costing> IS ASSIGNED.
            <kostl> = <new_costing>-cost_center.
            <prop> = <new_costing>-proportion.
            <bukrs> = <new_costing>-company_code.
            UNASSIGN <new_costing>.
            DELETE costings INDEX 1.
          ELSE.
            continue = abap_false.
          ENDIF.
        ENDIF.
        kostl_done = kostl_done + 1.
        UNPACK kostl_done TO kostl_done.
        kostl_nr = 'KST' && kostl_done.
        prop_nr = 'KPR' && kostl_done.
        bukrs_nr = 'KBU' && kostl_done.
        IF kostl_done = 26.
          continue = abap_false.
        ENDIF.
      ENDWHILE.
    ENDIF.
**     Transfer data to IT_AEND structure
    infotype_xml_processor->pa_to_prelp( EXPORTING infotype = p0027
                               aend_id  = aend_id
                     IMPORTING prelp    = DATA(prelp) ).
    it_aend = CORRESPONDING /sew/int_it_aend( prelp ).
    it_aend-aend_id = aend_id.
**    ENDIF.
  ENDMETHOD.
ENDCLASS.
