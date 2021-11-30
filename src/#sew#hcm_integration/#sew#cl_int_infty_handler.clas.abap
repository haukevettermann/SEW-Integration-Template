class /SEW/CL_INT_INFTY_HANDLER definition
  public
  final
  create public .

public section.

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
    TYPES ignore_timeslices type boole_d.
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

        CHECK <value> NOT IN /sew/cl_int_constants=>non_relevant_action( ).

*     Set action for IT0000
        infotype_xml_processor->action = <value>.
        IF <value> IS NOT INITIAL AND <value> NE /sew/cl_int_constants=>hire_date_change.
          IF infotype_xml_processor->object_handler->action NE /sew/cl_int_constants=>termination.
            infotype_xml_processor->object_handler->action = <value>.
          ENDIF.
*      Set termination date for delimiting
          IF infotype_xml_processor->object_handler->action = /sew/cl_int_constants=>termination.
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
      "Assign pernr
      ASSIGN COMPONENT /sew/cl_int_constants=>pernr OF STRUCTURE <structure> TO <value>.
      <value> = sap_id.

      "Transfer data to prelp
      infotype_xml_processor->pa_to_prelp( EXPORTING infotype = <structure>
                                 aend_id  = aend_id
                       IMPORTING prelp    = DATA(prelp) ).
      cl_hr_pnnnn_type_cast=>pnnnn_to_prelp( EXPORTING pnnnn = <structure>
                                             IMPORTING prelp = prelp_orig ).


      it_aend = CORRESPONDING /sew/int_it_aend( prelp_orig ).
      it_aend-aend_id = aend_id.

      "Set action for infotypes other then IT0000
      it_aend-action = infotype_xml_processor->action.
      "Check for active status.
      READ TABLE fields WITH KEY field_oracle = 'AssStatusType' ASSIGNING FIELD-SYMBOL(<status>).
      IF <status> IS ASSIGNED AND <status>-value CS 'INACTIVE'.
        it_aend-active = 'I'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
