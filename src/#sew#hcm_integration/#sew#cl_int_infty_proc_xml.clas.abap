class /SEW/CL_INT_INFTY_PROC_XML definition
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

  methods PT_200X_LOGIC
    importing
      value(DATA) type ANY
      !BEGDA type DATS
      !ENDDA type DATS
      !SAP_ID type /SEW/VALUE
      !CLOUD_ID type /SEW/VALUE
      !AEND_ID type GUID_32
      !HAS_ERROR type BOOLE_D
      !FIELDS type T_FIELDS
    exporting
      !PRELP type /SEW/PRELP
    changing
      !IT_AEND type /SEW/INT_IT_AEND .
  methods PT_2006_LOGIC
    importing
      value(DATA) type ANY
      !BEGDA type DATS
      !ENDDA type DATS
      !SAP_ID type /SEW/VALUE
      !CLOUD_ID type /SEW/VALUE
      !AEND_ID type GUID_32
      !HAS_ERROR type BOOLE_D
      !FIELDS type T_FIELDS .
  methods PROCESS_CONVERSIONS .
  methods PROCESS_MAPPINGS .
  methods BUILD_INFOTYPE_OLD
    importing
      !SAP_ID type HROBJID
      !CLOUD_ID type /SEW/DD_OBJECTID
      !INT_RUN type GUID_32
      !MOLGA type MOLGA
      !OBJECT_TYPE type OTYPE
      !OBJECT_SEQNR type SEQNR
    exporting
      !MESSAGE type BAPIRET1 .
  methods BUILD_INFOTYPE
    importing
      !SAP_ID type HROBJID
      !CLOUD_ID type /SEW/DD_OBJECTID
      !INT_RUN type GUID_32
      !MOLGA type MOLGA
      !OBJECT_TYPE type OTYPE
      !OBJECT_SEQNR type SEQNR
    exporting
      !MESSAGE type BAPIRET1 .
  methods OBJECT_SPECIFIC_LOGIC
    importing
      !DATA type ANY
      !BEGDA type DATS
      !ENDDA type DATS
      !SAP_ID type /SEW/DD_VALUE
      !CLOUD_ID type /SEW/DD_VALUE
      !AEND_ID type GUID_32
      !OBJECT_TYPE type OTYPE
      value(FIELDS) type T_FIELDS
    exporting
      !HAS_ERROR type BOOLE_D
      !MESSAGE type BAPIRET1 .
  class-methods GET_SAP_IDS
    importing
      !OTYPE type OTYPE
      !BEGDA type DATS
      !ENDDA type DATS
      !CLOUD_ID type /SEW/DD_VALUE
    exporting
      !SAP_ID type /SEW/DD_VALUE
      !MESSAGE type BAPIRET1 .
  methods OM_SPECIFIC_LOGIC
    importing
      value(DATA) type ANY
      !BEGDA type DATS
      !ENDDA type DATS
      !SAP_ID type /SEW/DD_VALUE
      !CLOUD_ID type /SEW/DD_VALUE
      !AEND_ID type GUID_32
      !HAS_ERROR type BOOLE_D
      !FIELDS type T_FIELDS
    exporting
      !WPLOG type /SEW/WPLOG .
  methods PA_SPECIFIC_LOGIC
    importing
      value(DATA) type ANY
      !BEGDA type DATS
      !ENDDA type DATS
      value(SAP_ID) type /SEW/VALUE
      !CLOUD_ID type /SEW/VALUE
      !AEND_ID type GUID_32
      !HAS_ERROR type BOOLE_D
      !FIELDS type T_FIELDS .
  methods PROCESS_INFOTYPE_0001
    importing
      value(DATA) type ANY
      !BEGDA type DATS
      !ENDDA type DATS
      value(SAP_ID) type /SEW/VALUE
      !CLOUD_ID type /SEW/VALUE
      !AEND_ID type GUID_32
      !HAS_ERROR type BOOLE_D
      !FIELDS type T_FIELDS
    exporting
      !PRELP type /SEW/PRELP .
  methods PROCESS_INFOTYPE_NNNN_MOLGA
    importing
      value(DATA) type ANY
      !BEGDA type DATS
      !ENDDA type DATS
      value(SAP_ID) type /SEW/VALUE
      !CLOUD_ID type /SEW/VALUE
      !AEND_ID type GUID_32
      !HAS_ERROR type BOOLE_D
      !FIELDS type T_FIELDS
    exporting
      !PRELP type /SEW/PRELP .
  methods ACTION_HANDLING
    importing
      !SAP_ID type /SEW/DD_VALUE
      !CLOUD_ID type /SEW/DD_VALUE
      !HAS_ERROR type BOOLE_D .
  methods PT_SPECIFIC_LOGIC
    importing
      value(DATA) type ANY
      !BEGDA type DATS
      !ENDDA type DATS
      !SAP_ID type /SEW/VALUE
      !CLOUD_ID type /SEW/VALUE
      !AEND_ID type GUID_32
      !HAS_ERROR type BOOLE_D
      !FIELDS type T_FIELDS
    exporting
      !PRELP type /SEW/PRELP .
  methods OM_TO_PRELP
    importing
      !INFOTYPE type ANY
      !AEND_ID type GUID_32
    exporting
      !WPLOG type /SEW/WPLOG .
  class-methods PA_TO_PRELP
    importing
      !INFOTYPE type ANY
      !AEND_ID type GUID_32
    exporting
      !PRELP type /SEW/PRELP .
  methods PT_TO_PRELP
    importing
      !INFOTYPE type ANY
      !AEND_ID type GUID_32
    exporting
      !PRELP type /SEW/PRELP .
  methods DETERMINE_TIME_SLICES
    importing
      !FOLDERS type T_FOLDERS
    exporting
      !MESSAGE type BAPIRET1
      !TIME_SLICES type T_TIME_SLICES .
  methods DETERMINE_TIME_SLICES_OLD
    exporting
      !MESSAGE type BAPIRET1 .
  methods DETERMINE_TIME_SLICES_FOLDER
    importing
      !FOLDER type /SEW/CL_INT_INFTY_PROC_XML=>S_FOLDER
    exporting
      !MESSAGE type BAPIRET1
      !TIME_SLICES type T_TIME_SLICES
    returning
      value(NEXT_EXISTS) type BOOLE_D .
  methods CONSTRUCTOR
    importing
      !CUSTOMIZING type ref to /SEW/CL_INT_CUSTOMIZING_XML
      !XML_NODE type ref to IF_IXML_NODE
      !INFTY type INFTY
      !SEQNR type SEQNR
      !INT_RUN type GUID_32
      !MOLGA type MOLGA .
  methods GET_FIELDS
    exporting
      !MESSAGE type BAPIRET1 .
  methods GET_FOLDERS
    exporting
      !MESSAGE type BAPIRET1 .
  methods AGGREGATE_FOLDERS_FIELDS
    exporting
      !MESSAGE type BAPIRET1 .
  methods GET_NEXT_FOLDER
    importing
      !PARENT_FOLDERS type T_FOLDERS
    changing
      !CHILD_FOLDERS type T_FOLDERS .
  methods PROCESS_FOLDER_CUSTOMIZING
    importing
      !CUSTOMIZING type /SEW/CL_INT_CUSTOMIZING_XML=>S_CUSTOMIZING_FOLDERS
      !PARENT_FOLDER_SEQNR type SEQNR
      !FOLDER_XML_NODE type ref to IF_IXML_NODE
      !PARENT_FOLDER type /SEW/DD_FOLDER
    exporting
      !MESSAGE type BAPIRET1
    changing
      !SEQNR type SEQNR .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS /SEW/CL_INT_INFTY_PROC_XML IMPLEMENTATION.


  METHOD action_handling.
    DATA(status_handler) = /sew/cl_int_status_handler=>get_instance( sap_id      = CONV #( sap_id )
                                                   cloud_id    = CONV #( cloud_id )
                                                   cloud_pernr = CONV #( me->object_handler->cloud_pernr )
                                                   int_run     = me->int_run
                                                   molga       = me->molga ).
*   Loop at actions. Could be more than one...
    CASE me->action.
      WHEN /sew/cl_int_constants=>hire.
      WHEN /sew/cl_int_constants=>termination.
    ENDCASE.
*    LOOP AT status_handler->new_it_aend WHERE infty EQ /sew/cl_int_constants=>it0000.
**     Assign infotypes
*      LOOP AT status_handler->new_it_aend WHERE infty NE /sew/cl_int_constants=>it0000.
*
*      ENDLOOP.
*    ENDLOOP.
* Generelle Methode pro Maßnahme.
*   Immer Process_MN z.B.: Process_01
*   Konstellationen:
    "Hire
      "Nur eine Zeitscheibe IT0001. Alle Infotypen mit Begda = Begda Maßnahme der Maßnahme zuordnen und auf Highdate setzen falls nicht aus XML auf high date
      "Was ist wenn am gleichen Tag HIRE und Org Change
    "Termination
      "Abgrenzen Infotypen
        "IT0105 - Aktueller Satz mit Endedatum = Begda Maßnahme + 1
        "IT0050 - Aktueller Satz mit Endedatum = Begda Maßnahme + 1
    "Contingent Worker to Employee
      "Termination old Pernr
      "New entry
    "Wenn Maßnahmenmethode nicht existiert, prozessieren allgemeine Methode.
  ENDMETHOD.


  METHOD aggregate_folders_fields.
    DATA(parent_folders) = VALUE t_folders( FOR folder IN me->folders WHERE ( parent_folder_seqnr = 000 ) ( folder ) ).
    IF me->has_multiple = abap_true AND VALUE boole_d( me->customizing->customizing[ infty = me->infty seqnr = me->seqnr ]-multiple OPTIONAL ) = abap_true.
      DATA(child_folders) = VALUE t_folders( ).
      me->get_next_folder(
        EXPORTING
          parent_folders = parent_folders
        CHANGING
          child_folders  = child_folders ).
      DATA(seqnr) = CONV seqnr( 000 ).
      DATA(parent_seqnr) = CONV seqnr( 000 ).
      LOOP AT child_folders ASSIGNING FIELD-SYMBOL(<child>).
        seqnr = seqnr + 1.
        APPEND INITIAL LINE TO me->folders_aggregated ASSIGNING FIELD-SYMBOL(<folder_aggregated>).
        <folder_aggregated>-id = seqnr.
*     Append folder
        APPEND <child> TO <folder_aggregated>-folders.
*     Get corresponding fields
        <folder_aggregated>-fields = CORRESPONDING #( BASE ( <folder_aggregated>-fields ) VALUE t_fields( FOR field IN me->fields WHERE ( folder = <child>-folder AND seqnr = <child>-seqnr ) ( field ) ) ).
        parent_seqnr = <child>-parent_folder_seqnr.
        WHILE parent_seqnr NE 000.
          READ TABLE me->folders WITH KEY seqnr = parent_seqnr ASSIGNING FIELD-SYMBOL(<next_parent>).
          IF sy-subrc IS INITIAL.
            APPEND <next_parent> TO <folder_aggregated>-folders.
*         Get corresponding fields
            <folder_aggregated>-fields = CORRESPONDING #( BASE ( <folder_aggregated>-fields ) VALUE t_fields( FOR field IN me->fields WHERE ( folder = <next_parent>-folder AND seqnr = <next_parent>-seqnr ) ( field ) ) ).
            parent_seqnr = <next_parent>-parent_folder_seqnr.
            UNASSIGN <next_parent>.
          ENDIF.
        ENDWHILE.
*       Aggregate folders that are not relevant for timeslice
        LOOP AT me->folders ASSIGNING FIELD-SYMBOL(<folder>) WHERE ignore_timeslices = abap_true.
          <folder_aggregated>-fields = CORRESPONDING #( BASE ( <folder_aggregated>-fields ) VALUE t_fields( FOR field IN me->fields WHERE ( folder = <folder>-folder AND seqnr = <folder>-seqnr ) ( field ) ) ).
        ENDLOOP.
        SORT <folder_aggregated>-folders BY seqnr ASCENDING.
      ENDLOOP.
    ELSE.
      APPEND INITIAL LINE TO me->folders_aggregated ASSIGNING <folder_aggregated>.
      <folder_aggregated>-id = CONV seqnr( 000 ).
      LOOP AT me->folders ASSIGNING <folder>.
        APPEND <folder> TO <folder_aggregated>-folders.
        <folder_aggregated>-fields = CORRESPONDING #( BASE ( <folder_aggregated>-fields ) VALUE t_fields( FOR field IN me->fields WHERE ( folder = <folder>-folder AND seqnr = <folder>-seqnr ) ( field ) ) ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD build_infotype.
    DATA:
      lr_structdescr  TYPE REF TO cl_abap_structdescr,
      lr_tabledescr   TYPE REF TO cl_abap_tabledescr,
      lr_structure    TYPE REF TO data,
      lr_table        TYPE REF TO data,
      infty_values    LIKE me->fields,
      aend_id         TYPE /sew/value,
      converted_value TYPE /sew/dd_value,
      type            TYPE dd02v-tabclass.
    FIELD-SYMBOLS: <struc>    TYPE any,
                   <prelp>    TYPE prelp,
                   <ft_table> TYPE STANDARD TABLE.

    "Instantiate Status Handler
    DATA(status_handler) = /sew/cl_int_status_handler=>get_instance( sap_id      = CONV #( sap_id )
                                                                     cloud_id    = CONV #( cloud_id )
                                                                     cloud_pernr = CONV #( me->object_handler->cloud_pernr )
                                                                     int_run     = int_run
                                                                     molga       = molga ).

    LOOP AT me->folders_aggregated ASSIGNING FIELD-SYMBOL(<folder_aggregated>).

      "Determine time slices based on field table
      me->determine_time_slices( EXPORTING folders     = <folder_aggregated>-folders
                                 IMPORTING time_slices = DATA(time_slices)
                                           message     = message ).

      "Creat dynamic strutcture for given infotype
      lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV #( 'P' && me->infty ) ).

      "Create table from structure descriptor
      lr_tabledescr ?= cl_abap_tabledescr=>create( p_line_type = lr_structdescr ).

      "Create data tables and assing data refrence
      CREATE DATA lr_table TYPE HANDLE lr_tabledescr.
      ASSIGN lr_table->* TO <ft_table>.

      "Instantiate infotype change  class
      me->infotype_changes = NEW /sew/cl_int_it_aend( ).

*--------------------------------------------------------------------*
*   Start of field value processing
*--------------------------------------------------------------------*
      "Loop at time slices and generate infotype entries.
      LOOP AT time_slices ASSIGNING FIELD-SYMBOL(<time_slice>).
        "One change id per infotype entry
        aend_id = /sew/cl_int_status_handler=>generate_guid( ).
        APPEND INITIAL LINE TO <ft_table> ASSIGNING FIELD-SYMBOL(<structure>).

        "First read begda and endda as they can never be initial.
        ASSIGN COMPONENT /sew/cl_int_constants=>begda OF STRUCTURE <structure> TO FIELD-SYMBOL(<value>).
        IF <value> IS ASSIGNED.
          <value> = <time_slice>-begda.
        ENDIF.
        ASSIGN COMPONENT /sew/cl_int_constants=>endda OF STRUCTURE <structure> TO <value>.
        IF <value> IS ASSIGNED.
          <value> = <time_slice>-endda.
        ENDIF.
*     Fill field infotype
        ASSIGN COMPONENT /sew/cl_int_constants=>infty OF STRUCTURE <structure> TO <value>.
        <value> = me->infty.

*       Save field for processing of mapping
        DATA(fields_folder) = <folder_aggregated>-fields.

*     Get values for infotype and build infotype structure.
        LOOP AT <folder_aggregated>-fields ASSIGNING FIELD-SYMBOL(<infty_value>) WHERE ( ( begda <= <time_slice>-endda   AND
                                                                                           begda >= <time_slice>-begda ) OR
                                                                                         ( endda >= <time_slice>-begda   AND
                                                                                           endda <= <time_slice>-endda ) ).
*       Get customizing instance in order to process mappings and conversions
          DATA(customizing_instance) = /sew/cl_int_customizing_xml=>get_instance( object_type = object_type molga = molga ).

*       Process mappings
          IF sy-subrc IS INITIAL.
            DATA(value) = <infty_value>-value.
            check value ne 'DELETED'.
            /sew/cl_int_mapping=>process_mapping(
              EXPORTING
                pernr = sap_id
*               HaVe 03.11. auskommentiert zum test. Begda und Endda wird immer mit gegeben.
                begda = CONV #( <infty_value>-begda )
*                begda = COND #( WHEN infty = 0000 AND me->seqnr = 000 THEN <infty_value>-begda ELSE '' )
                endda = CONV #( <infty_value>-endda )
*                endda = COND #( WHEN infty = 0000 AND me->seqnr = 000 THEN <infty_value>-endda ELSE '' )
                objid = sap_id
                infty          = <infty_value>-infty
                seqnr          = me->seqnr
                field_sap      = <infty_value>-field_sap
                field_oracle   = <infty_value>-field_oracle
                mapping_fields = customizing_instance->custom_mapping_fields
                mapping_values = customizing_instance->custom_mapping_values
                import         = abap_true
                export         = abap_false
                fields         = fields_folder
              IMPORTING
                message        = message
                skip           = DATA(skip)
                CHANGING
                value       = value ).
            IF skip = abap_true.
              CLEAR: <structure>.
              CONTINUE.
            ENDIF.
*           Set status. If already error, do not overwrite.
            IF message-type = /sew/cl_int_constants=>error.
              has_error = abap_true.
            ENDIF.
            IF message IS NOT INITIAL.
*         Log message
              status_handler->add_log_message( EXPORTING aend_id = CONV #( aend_id ) bapiret1 = message ).
              CLEAR: message.
            ENDIF.
          ENDIF.
*       Process conversions
          customizing->get_customizing_conversion( IMPORTING customizing        = DATA(customizing_conversion)
                                                             customizing_exists = DATA(customizing_exists) ).
*         Convert Dates (begda and endda) to SAP Format
          IF has_error = abap_false.
            IF value IS INITIAL.
              value = <infty_value>-value.
            ELSEIF value = '-'.
              CLEAR: value.
            ENDIF.
          ENDIF.

          READ TABLE me->customizing->custom_conversion WITH KEY field_sap = <infty_value>-field_sap infty = <infty_value>-infty ASSIGNING FIELD-SYMBOL(<conversion>).
          IF sy-subrc = 0.
            /sew/cl_int_conversion=>get_instance( molga = me->molga ).    "JMB20211016 I
            CALL METHOD /sew/cl_int_conversion=>(<conversion>-conversion_method)
              EXPORTING
                value_in  = CONV /sew/dd_value( value )
              RECEIVING
                value_out = value.
          ENDIF.
*        ENDIF.
*       Transfer value to infotype structure with conversion to target type
          ASSIGN COMPONENT <infty_value>-field_sap OF STRUCTURE <structure> TO <value>.
          IF sy-subrc IS INITIAL.
            CHECK <value> IS ASSIGNED.
            MOVE value TO <value>.
            UNASSIGN <value>.
          ENDIF.
        ENDLOOP.

        IF skip = abap_true.
          CLEAR: skip.
          CONTINUE.
        ENDIF.
*--------------------------------------------------------------------*
*     End of infotype construction logic
*--------------------------------------------------------------------*
*--------------------------------------------------------------------*
*     Fill fallback values
*--------------------------------------------------------------------*
        DATA(fallback_values) = VALUE /sew/cl_int_customizing_xml=>t_customizing_values( FOR fallback_value IN customizing->custom_values WHERE ( infty = infty ) ( fallback_value ) ).
        LOOP AT fallback_values ASSIGNING FIELD-SYMBOL(<fallback_value>).
          ASSIGN COMPONENT <fallback_value>-field OF STRUCTURE <structure> TO <value>.
          IF <value> IS ASSIGNED AND <value> IS INITIAL.
            IF <fallback_value>-value+0(2) = 'sy'.
              ASSIGN (<fallback_value>-value) TO <value>.
            ELSE.
              <value> = <fallback_value>-value.
            ENDIF.
          ENDIF.
        ENDLOOP.
*--------------------------------------------------------------------*
*     End of Fill fallback values
*--------------------------------------------------------------------*

*--------------------------------------------------------------------*
*     Start of Object Specific Logic
*--------------------------------------------------------------------*
        me->object_specific_logic( EXPORTING data        = <structure>
                                             begda       = <time_slice>-begda
                                             endda       = <time_slice>-endda
                                             sap_id      = CONV #( sap_id )
                                             cloud_id    = CONV #( cloud_id )
                                             aend_id     = CONV #( aend_id )
                                             object_type = object_type
                                             fields      = <folder_aggregated>-fields
                                   IMPORTING
                                             has_error   = has_error
                                             message     = message ).

        CLEAR has_error.
*         Log message
        status_handler->add_log_message( EXPORTING aend_id = CONV #( aend_id ) bapiret1 = message ).
        CLEAR: message.
*--------------------------------------------------------------------*
*     End of Object Specific Logic
*--------------------------------------------------------------------*
        CLEAR: fields_folder.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD build_infotype_old.
*    DATA:
*      lr_structdescr  TYPE REF TO cl_abap_structdescr,
*      lr_tabledescr   TYPE REF TO cl_abap_tabledescr,
*      lr_structure    TYPE REF TO data,
*      lr_table        TYPE REF TO data,
*      infty_values    LIKE me->fields,
*      aend_id         TYPE /sew/value,
*      converted_value TYPE /sew/dd_value.
*    FIELD-SYMBOLS: <struc>    TYPE any,
*                   <prelp>    TYPE prelp,
*                   <ft_table> TYPE STANDARD TABLE.
**   Instantiate Status Handler
*    DATA(status_handler) = /sew/cl_int_status_handler=>get_instance(
*                             sap_id   = CONV #( sap_id )
*                             cloud_id = CONV #( cloud_id )
*                             int_run  = int_run
*                             molga    = molga ).
**   Determine time slices based on field table
*    me->determine_time_slices_old( IMPORTING message = message ).
**   Creat dynamic strutcture for given infotype
*    lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV #( 'P' && me->infty ) ).
**   Create table from structure descriptor
*    lr_tabledescr ?= cl_abap_tabledescr=>create( p_line_type = lr_structdescr ).
**   Create data tables and assing data refrence
*    CREATE DATA lr_table TYPE HANDLE lr_tabledescr.
*    ASSIGN lr_table->* TO <ft_table>.
**   Instantiate infotype change  class
*    me->infotype_changes = NEW /sew/cl_int_it_aend( ).
**--------------------------------------------------------------------*
**   Start of field value processing
**--------------------------------------------------------------------*
**   Loop at time slices and generate infotype entries.
*    LOOP AT me->time_slices ASSIGNING FIELD-SYMBOL(<time_slice>).
**     One change id per infotype entry
*      aend_id = /sew/cl_int_status_handler=>generate_guid( ).
*      APPEND INITIAL LINE TO <ft_table> ASSIGNING FIELD-SYMBOL(<structure>).
**     First read begda and endda as they can never be initial.
*      ASSIGN COMPONENT /sew/cl_int_constants=>begda OF STRUCTURE <structure> TO FIELD-SYMBOL(<value>).
*      IF <value> IS ASSIGNED.
*        <value> = <time_slice>-begda.
*      ENDIF.
*      ASSIGN COMPONENT /sew/cl_int_constants=>endda OF STRUCTURE <structure> TO <value>.
*      IF <value> IS ASSIGNED.
*        <value> = <time_slice>-endda.
*      ENDIF.
**--------------------------------------------------------------------*
**      TODO Error Handling if begda or endda is initial
**--------------------------------------------------------------------*
**--------------------------------------------------------------------*
**     Begin of infotype construction logic
**--------------------------------------------------------------------*
**     Fill field infotype
*      ASSIGN COMPONENT /sew/cl_int_constants=>infty OF STRUCTURE <structure> TO <value>.
*      <value> = me->infty.
**     Get values for infotype and build infotype structure.
*      IF <time_slice>-seqnr NE 000.
*        infty_values = VALUE #( FOR infty_value IN me->fields WHERE ( begda >= <time_slice>-begda AND endda <= <time_slice>-endda AND ( ( parent_folder_seqnr = <time_slice>-seqnr ) OR ( parent_folder_seqnr = 000 AND seqnr = <time_slice>-seqnr ) )  ) (
*        infty_value ) ).
*      ELSE.
*        infty_values = VALUE #( FOR infty_value IN me->fields WHERE ( begda >= <time_slice>-begda AND endda <= <time_slice>-endda ) ( infty_value ) ).
*      ENDIF.
*      LOOP AT infty_values ASSIGNING FIELD-SYMBOL(<infty_value>).
*        CLEAR: message.
**       Get customizing instance in order to process mappings and conversions
*        DATA(customizing_instance) = /sew/cl_int_customizing_xml=>get_instance( object_type = object_type molga = molga ).
**       Process mappings
*        IF sy-subrc IS INITIAL.
*          /sew/cl_int_mapping=>process_mapping(
*              EXPORTING
*                pernr = sap_id
*                begda = <infty_value>-begda
*                endda = <infty_value>-endda
*                objid = sap_id
*                infty          = <infty_value>-infty
*                seqnr          = me->seqnr
*                field_sap      = <infty_value>-field_sap
*                field_oracle   = <infty_value>-field_oracle
*                mapping_fields = customizing_instance->custom_mapping_fields
*                mapping_values = customizing_instance->custom_mapping_values
*                import         = abap_true
*                export         = abap_false
**                fields         = fields_folder
*              IMPORTING
*                message        = message
*                skip           = DATA(skip)
*                CHANGING
*                value       = value ).
*          DATA(has_error) = COND boole_d( WHEN message-type = /sew/cl_int_constants=>error THEN abap_true ELSE abap_false ).
*          IF <infty_value>-value_mapped IS NOT INITIAL.
*            <infty_value>-value = <infty_value>-value_mapped.
*          ENDIF.
*          IF message IS NOT INITIAL.
**         Specific IT0000 logic - if no mapping found for MASSN we skip the IT0000 record
**            IF <infty_value>-infty = /sew/cl_int_constants=>it0000
**         Log message
*            status_handler->add_log_message( EXPORTING aend_id = CONV #( aend_id ) bapiret1 = message ).
**            IF message-type = 'E'.
**           OBSOLETE DUE TO NEW LOGIC
**           Error in mapping means field should be editabl through cockpit!
**              status_handler->add_changed_fields(
**                EXPORTING
**                  aend_id    = CONV #( aend_id )
**                  infty      = <infty_value>-infty
**                  field      = CONV #( <infty_value>-field_sap )
**                  subty      = VALUE #( infty_values[ field_sap = /sew/cl_int_constants=>subty ]-value OPTIONAL )
**                  value_old  = <infty_value>-value
**                  value_new  = <infty_value>-value_mapped
**                  editable   = me->customizing->check_field_changeable( field = <infty_value>-field_sap )
**                  simulation = abap_false ).
**            ENDIF.
*          ENDIF.
*        ENDIF.
**       Process conversions
*        customizing->get_customizing_conversion(
*          IMPORTING
*            customizing        = DATA(customizing_conversion)
*            customizing_exists = DATA(customizing_exists) ).
*
**        IF <infty_value>-field_sap = /sew/cl_int_constants=>begda OR <infty_value>-field_sap = /sew/cl_int_constants=>endda.
**         Convert Dates (begda and endda) to SAP Format
*        READ TABLE me->customizing->custom_conversion WITH KEY field_sap = <infty_value>-field_sap ASSIGNING FIELD-SYMBOL(<conversion>).
*        IF sy-subrc = 0.
*          CALL METHOD /sew/cl_int_conversion=>(<conversion>-conversion_method)
*            EXPORTING
*              value_in  = CONV /sew/dd_value( <infty_value>-value )
*            RECEIVING
*              value_out = <infty_value>-value.
*        ENDIF.
**        ENDIF.
**       Transfer value to infotype structure with conversion to target type
*        ASSIGN COMPONENT <infty_value>-field_sap OF STRUCTURE <structure> TO <value>.
*        CHECK <value> IS ASSIGNED.
*        DATA(field_type) = 'P' && me->infty && '-' && <infty_value>-field_sap.
*        <value> = CONV field_type( <infty_value>-value ).
*        UNASSIGN <value>.
*      ENDLOOP.
**--------------------------------------------------------------------*
**     End of infotype construction logic
**--------------------------------------------------------------------*
**--------------------------------------------------------------------*
**     Fill fallback values
**--------------------------------------------------------------------*
*      DATA(fallback_values) = VALUE /sew/cl_int_customizing_xml=>t_customizing_values( FOR value IN customizing->custom_values WHERE ( infty = infty ) ( value ) ).
*      LOOP AT fallback_values ASSIGNING FIELD-SYMBOL(<fallback_value>).
*        ASSIGN COMPONENT <fallback_value>-field OF STRUCTURE <structure> TO <value>.
*        IF <value> IS ASSIGNED AND <value> IS INITIAL.
*          <value> = <fallback_value>-value.
*        ENDIF.
*      ENDLOOP.
**--------------------------------------------------------------------*
**     End of Fill fallback values
**--------------------------------------------------------------------*
**--------------------------------------------------------------------*
**     Start of Object Specific Logic
**--------------------------------------------------------------------*
**      me->object_specific_logic(
**        EXPORTING
**            data      = <structure>
**            begda     = <time_slice>-begda
**            endda     = <time_slice>-endda
**            sap_id    = CONV #( sap_id )
**            cloud_id = CONV #( cloud_id )
**            aend_id   = CONV #( aend_id )
**            object_type = object_type
**            has_error = has_error
**       IMPORTING message = message ).
**--------------------------------------------------------------------*
**     End of Object Specific Logic
**--------------------------------------------------------------------*
*    ENDLOOP.
  ENDMETHOD.


  METHOD constructor.
    me->customizing = customizing.
    me->xml_node = xml_node.
    me->infty = infty.
    me->seqnr = seqnr.
    me->int_run = int_run.
    me->molga = molga.
  ENDMETHOD.


  METHOD determine_time_slices.
    DATA: hrperiods TYPE hrperiods_tab.
    hrperiods = VALUE #( FOR time_slice IN folders WHERE ( ignore_timeslices = abap_false ) ( begda = time_slice-begda endda = time_slice-endda ) ).
    SORT hrperiods BY begda endda ASCENDING.
    CALL FUNCTION 'RHXPROVIDE_PERIODS'
      TABLES
        provide_tab = hrperiods.
    time_slices = CORRESPONDING #( hrperiods ).
  ENDMETHOD.


  METHOD determine_time_slices_folder.
    DATA(next_folders) = VALUE /sew/cl_int_infty_proc_xml=>t_folders( FOR next_folder IN me->folders WHERE ( parent_folder = folder-folder AND parent_folder_seqnr = folder-seqnr ) ( next_folder ) ).
    IF sy-subrc IS INITIAL.
      next_exists = abap_true.
    ELSE.
      next_exists = abap_false.
    ENDIF.
    time_slices = VALUE #( FOR time_slice IN next_folders ( begda = time_slice-begda endda = time_slice-endda folder = time_slice-parent_folder ) ).
    WHILE next_exists IS NOT INITIAL.
      LOOP AT next_folders ASSIGNING FIELD-SYMBOL(<next_folder>).
        next_exists = me->determine_time_slices_folder(
          EXPORTING
            folder      = <next_folder>
          IMPORTING
            message     = message
            time_slices = time_slices ).
      ENDLOOP.
    ENDWHILE.
  ENDMETHOD.


  METHOD determine_time_slices_old.
    DATA: time_slices TYPE hrperiods_tab.
    IF infty NE /sew/cl_int_constants=>it0105 AND infty NE /sew/cl_int_constants=>it0001.
*        endloop.
      me->time_slices = VALUE #( FOR time_slice IN me->folders ( begda  = time_slice-begda endda = time_slice-endda seqnr = time_slice-seqnr ) ).
      SORT me->time_slices BY begda endda ASCENDING.
      DELETE ADJACENT DUPLICATES FROM me->time_slices COMPARING begda endda.
*      CALL FUNCTION 'RHXPROVIDE_PERIODS'
*        TABLES
*          provide_tab = time_slices.
*      me->time_slices = CORRESPONDING #( time_slices ).
    ELSEIF infty EQ /sew/cl_int_constants=>it0001.
      LOOP AT me->folders ASSIGNING FIELD-SYMBOL(<folder>) WHERE parent_folder_seqnr = 000.
        DATA(new_folders) = VALUE /sew/cl_int_infty_proc_xml=>t_folders( FOR folder IN me->folders WHERE (  seqnr NE 000 AND parent_folder_seqnr = <folder>-seqnr ) ( folder ) ).
        APPEND INITIAL LINE TO new_folders ASSIGNING FIELD-SYMBOL(<new_folder>).
        <new_folder> = <folder>.
        time_slices = VALUE #( FOR time_slice_folder IN new_folders WHERE (  parent_folder_seqnr = <folder>-seqnr ) ( begda = time_slice_folder-begda endda = time_slice_folder-endda ) ).
        APPEND INITIAL LINE TO time_slices ASSIGNING FIELD-SYMBOL(<time_slice>).
        <time_slice>-begda = <folder>-begda.
        <time_slice>-endda = <folder>-endda.
        CALL FUNCTION 'RHXPROVIDE_PERIODS'
          TABLES
            provide_tab = time_slices.
        LOOP AT time_slices ASSIGNING <time_slice>.
          APPEND INITIAL LINE TO me->time_slices ASSIGNING FIELD-SYMBOL(<new_time_slice>).
          <new_time_slice>-begda = <time_slice>-begda.
          <new_time_slice>-endda = <time_slice>-endda.
          <new_time_slice>-folder = <new_folder>-folder.
          <new_time_slice>-seqnr = <folder>-seqnr.
        ENDLOOP.
      ENDLOOP.
    ELSEIF infty EQ /sew/cl_int_constants=>it0105.
      me->time_slices = VALUE #( FOR time_slice_folder IN me->folders ( begda  = time_slice_folder-begda endda = time_slice_folder-endda seqnr = time_slice_folder-seqnr ) ).
    ENDIF.
  ENDMETHOD.


  METHOD get_fields.
    LOOP AT me->customizing->custom_fields ASSIGNING FIELD-SYMBOL(<field_customizing>) WHERE infty = me->infty AND seqnr = me->seqnr.
      LOOP AT me->folders ASSIGNING FIELD-SYMBOL(<folder>) WHERE folder = <field_customizing>-folder .
        IF <folder> IS ASSIGNED.
*          IF <field_customizing>-folder NE '//SewSapId'.
          CHECK <field_customizing>-element IS NOT INITIAL.
*          ENDIF.

*          IF <field_customizing>-element IS NOT INITIAL.
          /sew/cl_int_xml=>get_xpath_element(
            EXPORTING
              element_xpath = CONV #( <field_customizing>-element )
              xml_node      = <folder>-node
            IMPORTING
              value         = DATA(value)
              message       = message ).
*          ENDIF.

          "Check if fallback needs to take place
          IF value IS INITIAL.
            SELECT * FROM /sew/int_values INTO TABLE @DATA(fallback_values) WHERE object = @me->object_handler->object_type
                                                  AND molga = @me->molga
                                                  AND infty = @me->infty
                                                  AND field = @<field_customizing>-field.
            IF sy-subrc IS NOT INITIAL.
              SELECT * FROM /sew/int_values INTO TABLE @fallback_values WHERE object = @me->object_handler->object_type
                                                                              AND molga = '*'
                                                                              AND infty = @me->infty
                                                                              AND field = @<field_customizing>-field.
              IF fallback_values IS NOT INITIAL.
                READ TABLE fallback_values ASSIGNING FIELD-SYMBOL(<fallback>) INDEX 1.
                value = <fallback>-value.
              ENDIF.
            ENDIF.
          ENDIF.
          IF value IS NOT INITIAL.
            APPEND INITIAL LINE TO me->fields ASSIGNING FIELD-SYMBOL(<new_field>).
            <new_field>-folder = <field_customizing>-folder.
            <new_field>-cust_seqnr = <folder>-cust_seqnr.
            <new_field>-seqnr = <folder>-seqnr.
            <new_field>-parent_folder = <folder>-parent_folder.
            <new_field>-parent_folder_seqnr = <folder>-parent_folder_seqnr.
            <new_field>-begda = <folder>-begda.
            <new_field>-endda = <folder>-endda.
            <new_field>-infty = me->infty.
            <new_field>-field_sap = <field_customizing>-field.
            <new_field>-field_oracle = <field_customizing>-element.
            REPLACE ALL OCCURRENCES OF '/' IN <new_field>-field_oracle WITH ''.
            <new_field>-value = value.
          ENDIF.
        ENDIF.
        CLEAR: value.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_folders.
    me->customizing_folders =  VALUE /sew/cl_int_customizing_xml=>t_customizing_folders( FOR folders IN me->customizing->custom_folders WHERE ( infty = me->infty AND seqnr = me->seqnr ) ( folders ) ).
*    me->customizing_folders =  VALUE /sew/cl_int_customizing_xml=>t_customizing_fields( FOR field IN me->customizing->custom_fields WHERE ( infty = me->infty AND seqnr = me->seqnr ) ( field ) ).
    SORT customizing_folders BY folder.
    DELETE ADJACENT DUPLICATES FROM customizing_folders COMPARING folder.
    DATA(seqnr) = CONV seqnr( 000 ).
    LOOP AT customizing_folders ASSIGNING FIELD-SYMBOL(<customizing>) WHERE prev_folder IS INITIAL.
      me->process_folder_customizing(
        EXPORTING
          customizing       = <customizing>
          folder_xml_node = me->xml_node
          parent_folder = <customizing>-folder
          parent_folder_seqnr = 000
        IMPORTING
          message           = message
        CHANGING
          seqnr = seqnr ).
    ENDLOOP.
*   Fill Dates for folders without date.
    LOOP AT me->folders ASSIGNING FIELD-SYMBOL(<folder>) WHERE begda IS INITIAL OR endda IS INITIAL.
      data(seqnr_up) = <folder>-parent_folder_seqnr.
      WHILE <folder>-begda IS INITIAL OR <folder>-endda IS INITIAL.
        READ TABLE me->folders WITH KEY seqnr = seqnr_up ASSIGNING FIELD-SYMBOL(<folder_up>).
        IF sy-subrc IS INITIAL.
          IF <folder>-begda IS INITIAL and <folder_up>-begda is NOT INITIAL.
            <folder>-begda = <folder_up>-begda.
          ENDIF.
          IF <folder>-endda IS INITIAL and <folder_up>-endda is NOT INITIAL.
            <folder>-endda = <folder_up>-endda.
          ENDIF.
          seqnr_up = <folder_up>-parent_folder_seqnr.
          UNASSIGN <folder_up>.
        ELSE.
          EXIT.
        ENDIF.
      ENDWHILE.
    ENDLOOP.
    DELETE me->folders WHERE begda EQ 0 OR endda EQ 0.
  ENDMETHOD.


  METHOD get_next_folder.
    DATA(new_parents) = VALUE t_folders( ).
    LOOP AT parent_folders ASSIGNING FIELD-SYMBOL(<parent_folder>).
      DATA(next_folders) = VALUE t_folders( FOR folder IN me->folders WHERE ( parent_folder_seqnr = <parent_folder>-seqnr ) ( folder ) ).
      IF next_folders IS INITIAL.
        APPEND <parent_folder> TO child_folders.
      ELSE.
        APPEND LINES OF next_folders TO new_parents.
      ENDIF.
    ENDLOOP.
    IF new_parents IS NOT INITIAL.
      me->get_next_folder(
        EXPORTING
          parent_folders = new_parents
          CHANGING
          child_folders  = child_folders ).
    ENDIF.
  ENDMETHOD.


  METHOD get_sap_ids.
    "Get SAP ID according to CLOUD ID from HRP9401
    IF otype = /sew/cl_int_constants=>orgunit.
      SELECT SINGLE objid FROM hrp9401 INTO sap_id WHERE oracleid = cloud_id AND begda LE endda AND endda GE begda.
      "GET SAP ID according to CLOUD ID from PA9400
    ELSEIF otype = /sew/cl_int_constants=>person.
      SELECT SINGLE pernr FROM pa9400 INTO sap_id WHERE oracleid = cloud_id AND begda LE endda AND endda GE begda.
    ENDIF.

    "No mapping oracle - sap in custom IT found
    IF sy-subrc <> 0.
      message = VALUE bapiret1( type = /sew/cl_int_constants=>warning
                          id = /sew/cl_int_constants=>msg_class_int
                          number = /sew/cl_int_constants=>msg_no-m14
                          message_v1 = cloud_id
                          message_v2 = otype ).
    ENDIF.
  ENDMETHOD.


METHOD object_specific_logic.
**JMB20211111 start insert - insert time relevant logic
*
  DATA(pt_obj) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = /sew/cl_int_constants=>absence )
                                     ( sign = 'I' option = 'EQ' low = /sew/cl_int_constants=>del_absence )
                                     ( sign = 'I' option = 'EQ' low = /sew/cl_int_constants=>attendance )
                                     ( sign = 'I' option = 'EQ' low = /sew/cl_int_constants=>del_attendance )
                                     ( sign = 'I' option = 'EQ' low = /sew/cl_int_constants=>quota )
                                     ( sign = 'I' option = 'EQ' low = /sew/cl_int_constants=>balance )
                                     ( sign = 'I' option = 'EQ' low = /sew/cl_int_constants=>time_events ) ).
*JMB20211111 insert end

  IF object_type = /sew/cl_int_constants=>person.
*     Process customer specific PA logic
    me->pa_specific_logic( data      = data
                           begda     = begda
                           endda     = endda
                           sap_id    = CONV #( sap_id )
                           cloud_id  = CONV #( cloud_id )
                           aend_id   = aend_id
                           has_error = has_error
                           fields    = fields ).
**JMB20211111 start insert - insert time relevant logic
*
  ELSEIF object_type IN pt_obj.
    pt_specific_logic( data      = data
                       begda     = begda
                       endda     = endda
                       sap_id    = CONV #( sap_id )
                       cloud_id  = CONV #( cloud_id )
                       aend_id   = aend_id
                       has_error = has_error
                       fields    = fields ).
  ELSE.
*JMB20211111 insert end
*     Process customer specific OM logic
    me->om_specific_logic(           data      = data
                                     begda     = begda
                                     endda     = endda
                                     sap_id    = sap_id
                                     cloud_id  = cloud_id
                                     aend_id   = aend_id
                                     has_error = has_error
                                     fields    = fields ).
  ENDIF.
ENDMETHOD.


  METHOD om_specific_logic.
    ASSIGN data TO FIELD-SYMBOL(<structure>).
*    DATA(sap_id_tmp) = sap_id.
*     Check action and set instance attribute.
    IF me->infty = /sew/cl_int_constants=>it0000.
      ASSIGN COMPONENT /sew/cl_int_constants=>massn OF STRUCTURE <structure> TO FIELD-SYMBOL(<value>).
      me->action = <value>.
      me->object_handler->action = <value>.
    ENDIF.
*     In case of Hire position & position relation to orgunit need to be created
    ASSIGN COMPONENT 'OTYPE' OF STRUCTURE <structure> TO FIELD-SYMBOL(<otype>).
    IF me->infty = /sew/cl_int_constants=>it1001.
      ASSIGN COMPONENT 'SCLAS' OF STRUCTURE <structure> TO FIELD-SYMBOL(<sclas>).
      ASSIGN COMPONENT 'SOBID' OF STRUCTURE <structure> TO FIELD-SYMBOL(<sobid>).
      IF <sobid> IS ASSIGNED AND ( <sobid> IS INITIAL OR <sobid> = '00000000' ).
        IF <sclas> = 'K'.
          CLEAR wplog.
          EXIT.
        ELSE.
          IF <otype> IS NOT INITIAL.
            <sobid> = sap_id.
          ELSE.
            CLEAR wplog.
            EXIT.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    "If otype is empty, entry is not relevant
    IF <otype> IS ASSIGNED AND <otype> IS INITIAL.
      CLEAR wplog.
      EXIT.
    ENDIF.
*     Assign pernr
    ASSIGN COMPONENT /sew/cl_int_constants=>objid OF STRUCTURE <structure> TO <value>.
*    Read SAP ID to according Cloud ID from mapping IT
*    IF sap_id IS INITIAL OR sap_id = '00000000'.
*      me->get_sap_ids( EXPORTING otype = /sew/cl_int_constants=>orgunit begda = begda endda = endda cloud_id = cloud_id
*                  IMPORTING sap_id = sap_id_tmp message = DATA(message) ).
    IF <value> IS INITIAL.
      <value> = sap_id.
      me->object_handler->sap_id = sap_id.
    ENDIF.
*  ENDIF.
*    <value> = sap_id_tmp.

*   Transfer data to prelp
    me->om_to_prelp(
      EXPORTING
        infotype             = <structure>
        aend_id = aend_id
      IMPORTING
        wplog                = wplog ).
*    APPEND me->infotype_changes->wplog_to_om_aend( EXPORTING wplog = wplog ) TO me->infotype_changes->om_aend_orig.
*   Instantiate Status Handler
    DATA(om_aend) = CORRESPONDING /sew/int_om_aend( wplog ).
    om_aend-aend_id = aend_id.
*   Set status of Department in case of inactivation.
    IF om_aend-infty = /sew/cl_int_constants=>it1000.
      om_aend-department_status = VALUE #( me->fields[ field_sap = 'STATUS' ]-value OPTIONAL ).
    ENDIF.

    "Set related object
    IF om_aend-infty = /sew/cl_int_constants=>it1001.
      om_aend-relat_obj = VALUE #( me->fields[ field_sap = 'RELAT_OBJ' ]-value OPTIONAL ).
    ENDIF.

    om_aend-status = COND #( WHEN has_error = abap_true THEN /sew/cl_int_constants=>booking_status-error_des ELSE /sew/cl_int_constants=>booking_status-initial ).
    /sew/cl_int_status_handler=>get_instance(
                             sap_id   = CONV #( sap_id )
                             cloud_id = CONV #( cloud_id )
                             int_run  = me->int_run
                             molga    = me->molga )->add_new_aend( CHANGING om_aend = om_aend ).
  ENDMETHOD.


  METHOD om_to_prelp.
*   Transform infotype entry to prelp
    IF infotype IS NOT INITIAL.
      /sew/cl_int_type_cast=>pnnnn_to_wplog(
       EXPORTING
          pnnnn = infotype
          aend_id = aend_id
        IMPORTING
         wplog = wplog ).
    ENDIF.
  ENDMETHOD.


  METHOD pa_specific_logic.
    DATA:
      type_descr        TYPE REF TO cl_abap_typedescr,
      classdescr        TYPE REF TO cl_abap_classdescr,
      custom_classdescr TYPE REF TO cl_abap_classdescr,
      it_aend           TYPE /sew/int_it_aend.
    classdescr ?= cl_abap_typedescr=>describe_by_name( '/SEW/CL_INT_INFTY_PROC_XML' ).
    DATA(infotype_handler) = /sew/cl_int_general_settings=>get_infotyphandler( ).
*   Get exception class description
    CALL METHOD cl_abap_classdescr=>describe_by_name
      EXPORTING
        p_name         = infotype_handler
      RECEIVING
        p_descr_ref    = type_descr
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2.
    custom_classdescr ?= type_descr.
    TRY. custom_classdescr ?= cl_abap_typedescr=>describe_by_name( infotype_handler ). CATCH cx_root. ENDTRY.
    DATA(method) = |PROCESS_| && me->infty.
    IF custom_classdescr IS BOUND.
      READ TABLE custom_classdescr->methods WITH KEY name = method TRANSPORTING NO FIELDS.
    ELSE.
      sy-subrc = 4.
    ENDIF.
    IF sy-subrc IS INITIAL.
      CALL METHOD (infotype_handler)=>(method)
        EXPORTING
          data                   = data
          begda                  = begda
          endda                  = endda
          sap_id                 = sap_id
          cloud_id               = cloud_id
          aend_id                = aend_id
          has_error              = has_error
          fields                 = fields
          infotype_xml_processor = me
        IMPORTING
          it_aend                = it_aend.
    ELSE.
      READ TABLE classdescr->methods WITH KEY name = method TRANSPORTING NO FIELDS.
      IF sy-subrc IS INITIAL."Infty spefic method availlable
        CALL METHOD me->(method)
          EXPORTING
            data                   = data
            begda                  = begda
            endda                  = endda
            sap_id                 = sap_id
            cloud_id               = cloud_id
            aend_id                = aend_id
            has_error              = has_error
            fields                 = fields
            infotype_xml_processor = me
          IMPORTING
            it_aend                = it_aend.
      ELSE." Logic if not infty specific.
        DATA: prelp_orig TYPE prelp,
              endda_term TYPE dats.
        CLEAR: endda_term.
        ASSIGN data TO FIELD-SYMBOL(<structure>).
*     Check action and set instance attribute.
*        IF me->infty = /sew/cl_int_constants=>it0000.
*          READ TABLE fields WITH KEY field_sap = /sew/cl_int_constants=>massn ASSIGNING FIELD-SYMBOL(<action>).
*
*          "SEW specific Hire and Termination handled in customizing via infotype 0000 seqnr 001. Therefore ignore for seqnr 000
*          IF me->seqnr = 001 AND <action>-value IN /sew/cl_int_constants=>non_relevant_action( ).
*            EXIT.
*          ENDIF.
*
*          ASSIGN COMPONENT /sew/cl_int_constants=>massn OF STRUCTURE <structure> TO FIELD-SYMBOL(<value>).
*          IF <value> = 'RE'.
*            EXIT.
*          ENDIF.
*
*          CHECK <value> NOT IN /sew/cl_int_constants=>non_relevant_action( ).
*
**     Set action for IT0000
*          me->action = <value>.
*          IF <value> IS NOT INITIAL AND <value> NE /sew/cl_int_constants=>hire_date_change.
*            IF me->object_handler->action NE /sew/cl_int_constants=>termination.
*              me->object_handler->action = <value>.
*            ENDIF.
**      Set termination date for delimiting
*            IF me->object_handler->action = /sew/cl_int_constants=>termination.
*              ASSIGN COMPONENT /sew/cl_int_constants=>endda OF STRUCTURE <structure> TO <value>.
*              endda_term = <value>.
**          IF <value> = /sew/cl_int_constants=>highdate.
*              ASSIGN COMPONENT /sew/cl_int_constants=>begda OF STRUCTURE <structure> TO <value>.
*              me->object_handler->delimit_date = endda_term + 1.
**          ENDIF.
*            ENDIF.
*          ENDIF.
*        ENDIF.

        IF me->infty = /sew/cl_int_constants=>it0050.
          ASSIGN COMPONENT /sew/cl_int_constants=>bdegr OF STRUCTURE <structure> TO FIELD-SYMBOL(<value>).
          IF <value> IS ASSIGNED AND <value> IS INITIAL.
            <value> = '000'.
          ENDIF.
          ASSIGN COMPONENT /sew/cl_int_constants=>grawg OF STRUCTURE <structure> TO <value>.
          IF <value> IS ASSIGNED AND <value> IS INITIAL.
            <value> = '000'.
          ENDIF.
        ENDIF.

        IF me->infty = /sew/cl_int_constants=>it0105.
          ASSIGN COMPONENT /sew/cl_int_constants=>subty OF STRUCTURE <structure> TO <value>.
          IF <value> IS ASSIGNED AND <value> IS INITIAL.
            EXIT.
          ENDIF.
        ENDIF.

*   Rehire special logic
*        IF me->infty NE /sew/cl_int_constants=>it0000 AND me->object_handler->action = '04'.
*          ASSIGN COMPONENT /sew/cl_int_constants=>endda OF STRUCTURE <structure> TO <value>.
*          IF <value> NE /sew/cl_int_constants=>highdate.
*            EXIT.
*          ENDIF.
*        ENDIF.

*    DEVETHAU, 09.11.2021, Position generated in new method /SEW/CL_INT_ACTIONS->PROCESS_01
*    "In case of Hire position & position relation to orgunit need to be created
*    IF me->infty                  = /sew/cl_int_constants=>it0001 AND
*       me->object_handler->action = /sew/cl_int_constants=>hire.
*      /sew/cl_int_utility=>get_spras_by_molga( EXPORTING molga = me->molga
*                                               IMPORTING spras = DATA(spras)
*                                                         langu = DATA(langu) ).
*
*      DATA(infty_operation) = NEW /sew/cl_int_it_operation( int_run = me->int_run
*                                                            molga   = me->molga ).
*
*      ASSIGN COMPONENT /sew/cl_int_constants=>orgeh OF STRUCTURE <structure> TO <value>.
*
*      DATA(pos_id) = infty_operation->create_dummy_pos( orgeh = <value>
*                                                        begda = begda
*                                                        endda = endda
*                                                        langu = CONV #( spras )
*                                                         simu = /sew/cl_int_statics=>test_run ).
*      ASSIGN COMPONENT /sew/cl_int_constants=>plans OF STRUCTURE <structure> TO <value>.
*      <value> = pos_id.
*    ENDIF.

*    DEVETHAU, 09.11.2021, Position generated in new method /SEW/CL_INT_ACTIONS->PROCESS_01
        "In case of Termination the termination date must be set for each IT for delimiting
*    IF me->object_handler->action = /sew/cl_int_constants=>termination AND
*       me->infty                  = /sew/cl_int_constants=>it0105.
*      ASSIGN COMPONENT /sew/cl_int_constants=>endda OF STRUCTURE <structure> TO <value>.
*      IF <value> = /sew/cl_int_constants=>highdate.
*        <value> = me->object_handler->delimit_date.
*      ENDIF.
*    ENDIF.

*    IF me->object_handler->action = /sew/cl_int_constants=>termination AND
*       ( sap_id                   = 00000000                           OR
*         sap_id IS INITIAL ).
*
*      DATA(oracleid) = CONV /sew/dd_objectid( cloud_id ).
*
*      SELECT SINGLE pernr FROM pa9400 INTO @DATA(pernr) WHERE oracleid = @oracleid.
*      IF sy-subrc IS INITIAL.
*        ASSIGN COMPONENT /sew/cl_int_constants=>pernr OF STRUCTURE <structure> TO <value>.
*        <value> = pernr.
*      ENDIF.
*    ELSE.

        "Assign pernr
        ASSIGN COMPONENT /sew/cl_int_constants=>pernr OF STRUCTURE <structure> TO <value>.
        <value> = sap_id.

        "Transfer data to prelp
*    me->pa_to_prelp( EXPORTING infotype = <structure>
*                               aend_id  = aend_id
*                     IMPORTING prelp    = prelp ).
        cl_hr_pnnnn_type_cast=>pnnnn_to_prelp( EXPORTING pnnnn = <structure>
                                               IMPORTING prelp = DATA(prelp) ).


        it_aend = CORRESPONDING /sew/int_it_aend( prelp ).
        it_aend-aend_id = aend_id.

*    "Set action for infotypes other then IT0000
*    it_aend-action = COND #( WHEN me->infty EQ /sew/cl_int_constants=>it0000
*                             THEN me->object_handler->action
*                             ELSE me->action ).
        "Set status for entries
        it_aend-status = COND #( WHEN has_error = abap_true
                                 THEN /sew/cl_int_constants=>booking_status-error_des
                                 ELSE /sew/cl_int_constants=>booking_status-initial ).

*    "Check for active status.
        IF me->infty = /sew/cl_int_constants=>it0001. "OR me->infty = /sew/cl_int_constants=>it0000.
          READ TABLE fields WITH KEY field_oracle = 'AssStatusType' ASSIGNING FIELD-SYMBOL(<status>).
          IF <status> IS ASSIGNED AND <status>-value CS 'INACTIVE'.
            it_aend-active = 'I'.
          ENDIF.
        ENDIF.

        "Set related object id
        IF me->infty = /sew/cl_int_constants=>it0001.
          READ TABLE fields WITH KEY field_sap = 'RELAT_OBJ' ASSIGNING FIELD-SYMBOL(<relat_obj>).
          IF <status> IS ASSIGNED AND <relat_obj> IS ASSIGNED.
            it_aend-relat_obj = <relat_obj>-value.
          ENDIF.
        ENDIF.

      ENDIF.
    ENDIF.

*   Before persisting infotype in staging table transfer old values from current infotype.
    /sew/cl_int_infty_delta=>transfer_infty_data_v2(
      EXPORTING
        infty   = me->infty
        pernr   = CONV pernr_d( sap_id )
        stidat  = endda
        fields  = fields
      CHANGING
        it_aend = it_aend ).

*   Create IT_AEND entries with respective operation
    IF it_aend-infty IS INITIAL.
      exit.
    ENDIF.
    /sew/cl_int_status_handler=>get_instance( sap_id      = CONV #( sap_id )
                                              cloud_id    = CONV #( cloud_id )
                                              cloud_pernr = CONV #( me->object_handler->cloud_pernr )
                                              int_run     = me->int_run
                                              molga       = me->molga )->add_new_aend( CHANGING it_aend = it_aend ).

***JMB20210707 start insert - Check for time specific sequence number (Absence/Attendance) and set operation
** Vor Auslieferung an DÜRR LÖSCHEN!
*        DATA(pt_seq) = VALUE rsdsselopt_t( ( sign = 'I' option = 'BT' low = 002 high = 003 )
*                                           ( sign = 'I' option = 'BT' low = 005 high = 007 ) ).
*
*        IF me->object_handler->object_seq NOT IN pt_seq.
*          "Create IT_AEND entries with respective operation
*          /sew/cl_int_status_handler=>get_instance( sap_id      = CONV #( sap_id )
*                                                    cloud_id    = CONV #( cloud_id )
*                                                    cloud_pernr = CONV #( me->object_handler->cloud_pernr )
*                                                    int_run     = me->int_run
*                                                    molga       = me->molga )->add_new_aend( CHANGING it_aend = it_aend ).
*        ENDIF.
*        "Add sequence number for time events
*        APPEND VALUE #( sign = 'I' option = 'EQ' low = 001 ) TO pt_seq.
**JMB20210707 end insert
*JMB20211111 delete end

*    DEVETHAU, 09.11.2021, Position generated in new method /SEW/CL_INT_ACTIONS->PROCESS_01
*    "In case of hire with enddate provide also a termination action.
*    IF ( me->infty = /sew/cl_int_constants=>it0000 AND me->seqnr = 000 )                                  AND
*       ( action = /sew/cl_int_constants=>hire      OR  action = /sew/cl_int_constants=>hire_date_change ) AND
*       it_aend-endda                               NE /sew/cl_int_constants=>highdate.
*
*      it_aend-begda = it_aend-endda + 1.
*      it_aend-endda = /sew/cl_int_constants=>highdate.
*      it_aend-action = /sew/cl_int_constants=>termination.
*      me->object_handler->action = /sew/cl_int_constants=>termination.
*
*      /sew/cl_int_status_handler=>get_instance( sap_id      = CONV #( sap_id )
*                                                cloud_id    = CONV #( cloud_id )
*                                                cloud_pernr = CONV #( me->object_handler->cloud_pernr )
*                                                int_run     = me->int_run
*                                                molga       = me->molga )->add_new_aend( CHANGING it_aend = it_aend ).
*    ENDIF.

**JMB20211111 delete start - export time logic
*
*        "Time specific logic
*        IF me->object_handler->object_seq IN pt_seq.
*
*          me->pt_specific_logic( EXPORTING data      = data
*                                           begda     = begda
*                                           endda     = endda
*                                           sap_id    = sap_id
*                                           cloud_id  = cloud_id
*                                           aend_id   = aend_id
*                                           has_error = has_error
*                                           fields    = fields
*                                 IMPORTING prelp     = prelp
*                                 CHANGING  it_aend   = it_aend ).
*        ENDIF.
*JMB2021111 delete end
  ENDMETHOD.


  METHOD pa_to_prelp.
*   Transform infotype entry to prelp
    IF infotype IS NOT INITIAL.
      /sew/cl_int_type_cast=>pnnnn_to_prelp(
       EXPORTING
          pnnnn = infotype
          aend_id = aend_id
        IMPORTING
         prelp = prelp ).
    ENDIF.
  ENDMETHOD.


  METHOD process_conversions.
    LOOP AT me->fields ASSIGNING FIELD-SYMBOL(<field>).
      READ TABLE customizing->custom_conversion WITH KEY field_sap = <field> ASSIGNING FIELD-SYMBOL(<conversion>).
      IF sy-subrc IS INITIAL AND <conversion> IS ASSIGNED.
        CALL METHOD /sew/cl_int_conversion=>(<conversion>-conversion_method)
          EXPORTING
            value_in  = CONV /sew/dd_value( <field>-value )
          RECEIVING
            value_out = <field>-value.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD process_folder_customizing.
    /sew/cl_int_xml=>get_xpath_folder_with_begda(
      EXPORTING
        folder_xpath = CONV #( customizing-folder )
        xml_document = folder_xml_node
      IMPORTING
        nodes        = DATA(nodes)
        message      = message ).
    IF lines( nodes ) > 1.
      me->has_multiple = abap_true.
    ENDIF.
    LOOP AT nodes ASSIGNING FIELD-SYMBOL(<node>).
      seqnr = seqnr + 1.
      APPEND INITIAL LINE TO me->folders ASSIGNING FIELD-SYMBOL(<add_folder>).
      <add_folder>-folder = customizing-folder.
      "TEST
      IF <add_folder>-folder = '//CancelledHireData'.
        <node>-begda = '20200101'.
      ENDIF.

      <add_folder>-begda = <node>-begda.

      IF <node>-endda NE 0.
        <add_folder>-endda = <node>-endda.
      ELSE.
*         Default enddate if not given
        SELECT * FROM /sew/int_values INTO TABLE @DATA(fallback_values) WHERE object = @me->object_handler->object_type
                                                                        AND molga = @me->molga
                                                                        AND infty = @customizing-infty.
        IF sy-subrc IS NOT INITIAL.
          SELECT * FROM /sew/int_values INTO TABLE @fallback_values WHERE object = @me->object_handler->object_type
                                                                          AND molga = '*'
                                                                          AND infty = @customizing-infty.
        ENDIF.
        IF sy-subrc IS INITIAL.
          IF <add_folder>-begda = 0 OR <add_folder>-begda IS INITIAL.
            READ TABLE fallback_values WITH KEY field = /sew/cl_int_constants=>begda ASSIGNING FIELD-SYMBOL(<fallback_value>).
            IF <fallback_value> IS ASSIGNED.
              IF <fallback_value>-value = 'sy-datum'.
                <add_folder>-begda = sy-datum.
              ELSE.
                <add_folder>-begda = <fallback_value>-value.
              ENDIF.
            ENDIF.
          ENDIF.
          IF <add_folder>-endda = 0 OR <add_folder>-endda IS INITIAL.
            READ TABLE fallback_values WITH KEY infty = customizing-infty field = /sew/cl_int_constants=>endda ASSIGNING <fallback_value>.
            IF <fallback_value> IS ASSIGNED.
              <add_folder>-endda = <fallback_value>-value.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      <add_folder>-node = <node>-node.
      <add_folder>-ignore_timeslices = customizing-time_slice.
      <add_folder>-cust_seqnr = customizing-seqnr.
      <add_folder>-seqnr = seqnr.
      <add_folder>-parent_folder_seqnr = parent_folder_seqnr.
      <add_folder>-parent_folder = parent_folder.
      LOOP AT me->customizing_folders ASSIGNING FIELD-SYMBOL(<child_node>) WHERE prev_folder = customizing-folder.
        me->process_folder_customizing(
          EXPORTING
            customizing       = <child_node>
            parent_folder = <add_folder>-folder
            parent_folder_seqnr = <add_folder>-seqnr
            folder_xml_node = <add_folder>-node
          IMPORTING
            message           = message
            CHANGING
              seqnr = seqnr ).
      ENDLOOP.
    ENDLOOP.
    CLEAR: nodes.
  ENDMETHOD.


  METHOD process_infotype_0001.
    DATA: classdescr TYPE REF TO cl_abap_classdescr.
    classdescr ?= cl_abap_typedescr=>describe_by_name( '/SEW/CL_INT_INFTY_PROC_XML' ).
    DATA(method_molga) = |PROCESS_| && me->infty && '_' && me->molga.
    READ TABLE classdescr->methods WITH KEY name = method_molga TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL."Infty spefic method availlable
      CALL METHOD me->(method_molga)
        EXPORTING
          data      = data
          begda     = begda
          endda     = endda
          sap_id    = sap_id
          cloud_id  = cloud_id
          aend_id   = aend_id
          has_error = has_error
          fields    = fields
        IMPORTING
          prelp     = prelp.
    ELSE.
    ENDIF.
  ENDMETHOD.


  METHOD PROCESS_INFOTYPE_NNNN_MOLGA.
    "Template for infotype specific processing.
  ENDMETHOD.


  METHOD process_mappings.
    LOOP AT me->fields ASSIGNING FIELD-SYMBOL(<field>).
      READ TABLE customizing->custom_mapping_fields WITH KEY field_sap = <field> ASSIGNING FIELD-SYMBOL(<conversion>).
      IF sy-subrc IS INITIAL AND <conversion> IS ASSIGNED.
*        CALL METHOD /sew/cl_int_conversion=>(<conversion>-conversion_method)
*          EXPORTING
*            value_in  = CONV /sew/dd_value( <field>-value )
*          RECEIVING
*            value_out = <field>-value.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


 METHOD pt_2006_logic.
   DATA: ora_bal   TYPE /sew/int_ora_bal.

   ASSIGN data TO FIELD-SYMBOL(<structure>).

   "get pernr and oracle entry id
   ASSIGN COMPONENT /sew/cl_int_constants=>pernr OF STRUCTURE <structure> TO FIELD-SYMBOL(<pernr>).
   ASSIGN COMPONENT 'KTART'                      OF STRUCTURE <structure> TO FIELD-SYMBOL(<ktart>).
   ASSIGN COMPONENT 'DESTA'                      OF STRUCTURE <structure> TO FIELD-SYMBOL(<desta>).
   ASSIGN COMPONENT 'DEEND'                      OF STRUCTURE <structure> TO FIELD-SYMBOL(<deend>).
   ASSIGN COMPONENT 'KVERB'                      OF STRUCTURE <structure> TO FIELD-SYMBOL(<kverb>).
   ASSIGN COMPONENT 'ANZHL'                      OF STRUCTURE <structure> TO FIELD-SYMBOL(<anzhl>).
   ASSIGN COMPONENT 'AEDTM'                      OF STRUCTURE <structure> TO FIELD-SYMBOL(<aedtm>).
   ASSIGN COMPONENT 'QUONR'                      OF STRUCTURE <structure> TO FIELD-SYMBOL(<quonr>).

   CHECK <ktart> IS NOT INITIAL.

   ora_bal-pernr          = <pernr>.
   ora_bal-oracle_pernr   = me->object_handler->cloud_pernr.
   ora_bal-ktart          = <ktart>.
   ora_bal-plan_begda     = <desta>.
   ora_bal-plan_endda     = <deend>.
   ora_bal-oracle_bal_all = <anzhl> + <kverb>.
   ora_bal-oracle_bal_act = <anzhl>.
   ora_bal-oracle_ded     = <kverb>.
   ora_bal-aedtm          = <aedtm>.
   ora_bal-quonr          = <quonr>.

   "Create IT_AEND entries with respective operation
   /sew/cl_int_status_handler=>get_instance( sap_id      = CONV #( sap_id )
                                             cloud_id    = CONV #( cloud_id )
                                             cloud_pernr = CONV #( me->object_handler->cloud_pernr )
                                             int_run     = me->int_run
                                             molga       = me->molga )->add_new_aend( EXPORTING ora_bal = ora_bal ).

 ENDMETHOD.


 METHOD pt_200x_logic.
   DATA: prelp_old   TYPE prelp,
         art01       TYPE art01,
         it          TYPE REF TO data,
         sew_prelp   TYPE /sew/prelp,
         it_aend_old TYPE /sew/int_it_aend.

   FIELD-SYMBOLS: <pnnnn_old> TYPE any,
                  <infotype>  TYPE any.

   ASSIGN data TO FIELD-SYMBOL(<structure>).

   "get pernr and oracle entry id
   ASSIGN COMPONENT /sew/cl_int_constants=>pernr OF STRUCTURE <structure> TO FIELD-SYMBOL(<pernr>).
   ASSIGN COMPONENT 'AWREF'                      OF STRUCTURE <structure> TO FIELD-SYMBOL(<awref>).
   ASSIGN COMPONENT 'AWORG'                      OF STRUCTURE <structure> TO FIELD-SYMBOL(<aworg>).
   ASSIGN COMPONENT 'SUBTY'                      OF STRUCTURE <structure> TO FIELD-SYMBOL(<subty>).
   ASSIGN COMPONENT 'FLAG1'                      OF STRUCTURE <structure> TO FIELD-SYMBOL(<flag1>). "JMB20211012 I - ORA_WITHDRAWN in normal extract

   CHECK <awref> IS NOT INITIAL AND
         <aworg> IS NOT INITIAL.

**JMB20210929 start insert - check subtype if absence or attendance
*
   cl_pt_req_customizing=>get_modificators( EXPORTING  im_pernr         = <pernr>
                                                       im_date          = endda
                                            IMPORTING  ex_moabw         = DATA(moabw)
                                            EXCEPTIONS it0001_not_found = 1
                                                       OTHERS           = 2 ).

   "in case of deletion, subty is emtpy
   IF <subty> IS NOT INITIAL.
     SELECT SINGLE art01 FROM t554s INTO art01 WHERE moabw EQ moabw AND
                                                     endda GE begda AND
                                                     begda LE endda AND
                                                     subty EQ <subty>.
     CHECK art01 IS NOT INITIAL.
   ELSE.
     art01 = SWITCH #( me->infty
                      WHEN '2001' THEN 'A'
                      WHEN '2002' THEN 'P' ).
   ENDIF.

   CONCATENATE 'P' me->infty INTO DATA(it_name).
   CREATE DATA it TYPE (it_name).
   ASSIGN it->* TO <pnnnn_old>.
   ASSIGN it->* TO <infotype>.

   CASE art01.
     WHEN 'A'.
       CHECK me->infty EQ '2001'.

       "Check if absence entry id is already stored
       SELECT SINGLE * FROM pa2001 INTO CORRESPONDING FIELDS OF <infotype> WHERE pernr EQ <pernr> AND
                                                                                 awref EQ <awref> AND
                                                                                 aworg EQ <aworg>.

     WHEN 'P'.
       CHECK me->infty EQ '2002'.

       "Check if attendance entry id is already stored
       SELECT SINGLE * FROM pa2002 INTO CORRESPONDING FIELDS OF <infotype> WHERE pernr EQ <pernr> AND
                                                                                 awref EQ <awref> AND
                                                                                 aworg EQ <aworg>.
   ENDCASE.
*JMB20210929 insert end

   "in case infotype isn´t posted yet, check entry in int_it_aend
   IF <infotype> IS INITIAL.

     DATA(pernr)  = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = <pernr> ) ).
     DATA(it_aend_entries) = NEW /sew/cl_int_it_aend( )->get( pernr = pernr ).

     "delete not needed infotypes
     DELETE it_aend_entries WHERE infty NE me->infty.

     IF <subty> IS NOT INITIAL.
       DELETE it_aend_entries WHERE subty NE <subty>.
     ENDIF.

     LOOP AT it_aend_entries ASSIGNING FIELD-SYMBOL(<it_aend_del>).
       "in case of deletion keep entry in table
       CHECK <it_aend_del>-operation EQ /sew/cl_int_constants=>infty_ins.

       MOVE-CORRESPONDING <it_aend_del> TO prelp_old.

       /sew/cl_int_type_cast=>prelp_to_pnnnn( EXPORTING prelp = CONV #( prelp_old )
                                              IMPORTING pnnnn = <pnnnn_old> ).

       ASSIGN COMPONENT 'AWREF' OF STRUCTURE <pnnnn_old> TO FIELD-SYMBOL(<awref_old>).
       ASSIGN COMPONENT 'AWORG' OF STRUCTURE <pnnnn_old> TO FIELD-SYMBOL(<aworg_old>).

       "in case id was transferred before
       CHECK <awref_old> EQ <awref> AND
             <aworg_old> EQ <aworg>.

       "delete entry from table, due to potential change of sequence number during booking
       DELETE /sew/int_it_aend FROM <it_aend_del>.
       EXIT.
     ENDLOOP.

     "infotype entry exists
   ELSE.

     it_aend_old-operation = /sew/cl_int_constants=>infty_del.

     MOVE-CORRESPONDING <infotype> TO <pnnnn_old>.

     cl_hr_pnnnn_type_cast=>pnnnn_to_prelp( EXPORTING pnnnn = <pnnnn_old>
                                            IMPORTING prelp = prelp_old ).

     MOVE-CORRESPONDING prelp_old TO it_aend_old.
     it_aend_old-infty   = me->infty.
     it_aend_old-aend_id = aend_id.

     "Create IT_AEND entries with respective operation
     /sew/cl_int_status_handler=>get_instance( sap_id      = CONV #( sap_id )
                                               cloud_id    = CONV #( cloud_id )
                                               cloud_pernr = CONV #( me->object_handler->cloud_pernr )
                                               int_run     = me->int_run
                                               molga       = me->molga )->add_new_aend( CHANGING it_aend = it_aend_old ).

     "set new AEND_ID for insert entry
     it_aend-aend_id = /sew/cl_int_status_handler=>generate_guid( ).
   ENDIF.

   "in case of deletion, subty isn't passed or flag1 is set
   CHECK <subty> IS NOT INITIAL AND
         <flag1> IS INITIAL.

   it_aend-operation = /sew/cl_int_constants=>infty_ins.

**JMB20211011 start insert - in case alldf is set or several days, clear beguz and enduz
*
   ASSIGN COMPONENT 'ALLDF' OF STRUCTURE <structure> TO FIELD-SYMBOL(<alldf>).
   ASSIGN COMPONENT 'BEGUZ' OF STRUCTURE <structure> TO FIELD-SYMBOL(<beguz>).
   ASSIGN COMPONENT 'ENDUZ' OF STRUCTURE <structure> TO FIELD-SYMBOL(<enduz>).

   IF <alldf> EQ abap_true OR
      begda   NE endda.
     <alldf> = abap_true.
     CLEAR: <beguz>, <enduz>.

     /sew/cl_int_type_cast=>pnnnn_to_prelp( EXPORTING pnnnn   = <structure>
                                                      aend_id = it_aend-aend_id
                                            IMPORTING prelp = DATA(prelp_new) ).

     MOVE-CORRESPONDING prelp_new TO it_aend.
   ENDIF.
*JMB20211011 insert end

   "Create IT_AEND entries with respective operation
   /sew/cl_int_status_handler=>get_instance( sap_id      = CONV #( sap_id )
                                             cloud_id    = CONV #( cloud_id )
                                             cloud_pernr = CONV #( me->object_handler->cloud_pernr )
                                             int_run     = me->int_run
                                             molga       = me->molga )->add_new_aend( CHANGING it_aend = it_aend ).

 ENDMETHOD.


METHOD pt_specific_logic.
  DATA: it_aend_old TYPE /sew/int_it_aend,
        prelp_orig  type prelp,
        p2011_old   TYPE p2011.

  ASSIGN data TO FIELD-SYMBOL(<structure>).
  cl_hr_pnnnn_type_cast=>pnnnn_to_prelp( EXPORTING pnnnn = <structure>
                                         IMPORTING prelp = prelp_orig ).

  DATA(it_aend) = CORRESPONDING /sew/int_it_aend( prelp_orig ).
  it_aend-aend_id = aend_id.

  "Set action for infotypes other then IT0000
  it_aend-action = me->action.

  "Set status for entries
  it_aend-status = COND #( WHEN has_error = abap_true
                           THEN /sew/cl_int_constants=>booking_status-error_des
                           ELSE /sew/cl_int_constants=>booking_status-initial ).

  CASE me->infty.
      "Handle Updates -> seperate storno entry is needed
    WHEN /sew/cl_int_constants=>it2011.
      ASSIGN COMPONENT /sew/cl_int_constants=>user2 OF STRUCTURE <structure> TO FIELD-SYMBOL(<user2>).
      ASSIGN COMPONENT 'PERNR'                      OF STRUCTURE <structure> TO FIELD-SYMBOL(<pernr>).

      IF <user2> IS ASSIGNED    AND
         <user2> IS NOT INITIAL.

        SELECT * FROM teven INTO TABLE @DATA(teven) WHERE user2 EQ @<user2> AND
                                                          stokz EQ ' '      AND
                                                          pernr EQ @<pernr>.

        LOOP AT teven ASSIGNING FIELD-SYMBOL(<teven>).
          it_aend_old-operation = /sew/cl_int_constants=>infty_del.

          MOVE-CORRESPONDING <teven> TO p2011_old.
          p2011_old-begda = <teven>-ldate.
          p2011_old-endda = <teven>-ldate.

          cl_hr_pnnnn_type_cast=>pnnnn_to_prelp( EXPORTING pnnnn = p2011_old
                                                 IMPORTING prelp = DATA(prelp_old) ).

          MOVE-CORRESPONDING prelp_old TO it_aend_old.
          it_aend_old-infty   = me->infty.
          it_aend_old-aend_id = /sew/cl_int_status_handler=>generate_guid( ).

          "Create IT_AEND entries with respective operation
          /sew/cl_int_status_handler=>get_instance( sap_id      = CONV #( sap_id )
                                                    cloud_id    = CONV #( cloud_id )
                                                    cloud_pernr = CONV #( me->object_handler->cloud_pernr )
                                                    int_run     = me->int_run
                                                    molga       = me->molga )->add_new_aend( CHANGING it_aend = it_aend_old ).
        ENDLOOP.

      ENDIF.

      "In case of an absence or attendance entry
    WHEN /sew/cl_int_constants=>it2001 OR
         /sew/cl_int_constants=>it2002.

      pt_200x_logic( EXPORTING data      = data
                               begda     = begda
                               endda     = endda
                               sap_id    = sap_id
                               cloud_id  = cloud_id
                               aend_id   = aend_id
                               has_error = has_error
                               fields    = fields
                     IMPORTING prelp     = prelp
                     CHANGING  it_aend   = it_aend ).

    WHEN '2006'.
      pt_2006_logic( data      = data
                     begda     = begda
                     endda     = endda
                     sap_id    = sap_id
                     cloud_id  = cloud_id
                     aend_id   = aend_id
                     has_error = has_error
                     fields    = fields ).
  ENDCASE.
ENDMETHOD.


METHOD pt_to_prelp.
*   Transform infotype entry to prelp
  IF infotype IS NOT INITIAL.
    /sew/cl_int_type_cast=>pnnnn_to_prelp(
     EXPORTING
        pnnnn = infotype
        aend_id = aend_id
      IMPORTING
       prelp = prelp ).
  ENDIF.
ENDMETHOD.
ENDCLASS.
