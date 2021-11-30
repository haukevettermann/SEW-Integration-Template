class /SEW/CL_INT_CUSTOMIZING_XML definition
  public
  final
  create private .

public section.

  types:
    t_customizing_infotypes TYPE TABLE OF /sew/int_infotyp WITH DEFAULT KEY .
  types:
    t_customizing_fields TYPE TABLE OF /sew/int_fields WITH DEFAULT KEY .
  types:
    t_customizing_folders TYPE TABLE OF /sew/int_folders WITH DEFAULT KEY .
  types S_CUSTOMIZING_FIELDS type /SEW/INT_FIELDS .
  types S_CUSTOMIZING_FOLDERS type /SEW/INT_FOLDERS .
  types:
    t_customizing_values TYPE TABLE OF /sew/int_values WITH DEFAULT KEY .
  types:
    t_customizing_conversion TYPE TABLE OF /sew/int_convers WITH DEFAULT KEY .
  types:
    t_customizing_mapping_fields TYPE TABLE OF /sew/int_mapp_fi WITH DEFAULT KEY .
  types:
    t_customizing_mapping TYPE TABLE OF /sew/int_mapping WITH DEFAULT KEY .

  data OBJECT_TYPE type OTYPE .
  data OBJECT_SEQNR type SEQNR .
  data MOLGA type MOLGA .
  data CUSTOMIZING type T_CUSTOMIZING_INFOTYPES .
  data CUSTOM_FIELDS type T_CUSTOMIZING_FIELDS .
  data CUSTOM_CONVERSION type T_CUSTOMIZING_CONVERSION .
  data CUSTOM_MAPPING_VALUES type T_CUSTOMIZING_MAPPING .
  data CUSTOM_MAPPING_FIELDS type T_CUSTOMIZING_MAPPING_FIELDS .
  data CUSTOM_VALUES type T_CUSTOMIZING_VALUES .
  data CUSTOM_FOLDERS type T_CUSTOMIZING_FOLDERS .

  class-methods GET_INSTANCE
    importing
      !MOLGA type MOLGA
      !OBJECT_TYPE type OTYPE
      !OBJECT_SEQNR type SEQNR optional
    returning
      value(RO_INSTANCE) type ref to /SEW/CL_INT_CUSTOMIZING_XML .
  methods GET_CUSTOMIZING_CONVERSION
    exporting
      !CUSTOMIZING type T_CUSTOMIZING_CONVERSION
      !CUSTOMIZING_EXISTS type BOOLE_D .
  methods GET_CUSTOMIZING_MAPPING
    exporting
      !CUSTOMIZING type T_CUSTOMIZING_MAPPING
      !CUSTOMIZING_EXISTS type BOOLE_D .
  methods GET_CUSTOMIZING_FIELDS
    importing
      !ADD_DEFAULT type BOOLEAN optional
    exporting
      !CUSTOMIZING type T_CUSTOMIZING_FIELDS
      !CUSTOMIZING_EXISTS type BOOLE_D .
  methods GET_CUSTOMIZING_FOLDERS
    exporting
      !CUSTOMIZING type T_CUSTOMIZING_FOLDERS
      !CUSTOMIZING_EXISTS type BOOLE_D .
  methods GET_CUSTOMIZING_VALUES
    exporting
      !CUSTOMIZING type T_CUSTOMIZING_VALUES
      !CUSTOMIZING_EXISTS type BOOLE_D .
  methods GET_CUSTOMIZING_INFTY
    exporting
      !CUSTOMIZING type T_CUSTOMIZING_INFOTYPES
      !CUSTOMIZING_EXISTS type BOOLE_D .
  methods CHECK_FIELD_CHANGEABLE
    importing
      !FIELD type /SEW/DD_FIELD
    returning
      value(IS_CHANGEABLE) type BOOLE_D .
  PROTECTED SECTION.
private section.

  class-data GO_INSTANCE type ref to /SEW/CL_INT_CUSTOMIZING_XML .

  methods CONSTRUCTOR
    importing
      !OBJECT_TYPE type OTYPE
      !MOLGA type MOLGA
      !OBJECT_SEQNR type SEQNR .
ENDCLASS.



CLASS /SEW/CL_INT_CUSTOMIZING_XML IMPLEMENTATION.


  METHOD check_field_changeable.
*   First check molga specific customizing
    READ TABLE me->custom_fields WITH KEY molga = me->molga field = field INTO DATA(field_customizing).
    IF sy-subrc IS NOT INITIAL.
*     Noting found? Check molga independent customizing
      READ TABLE me->custom_fields WITH KEY molga = /sew/cl_int_constants=>star field = field INTO field_customizing.
    ENDIF.
    is_changeable = field_customizing-changeable.
  ENDMETHOD.


METHOD constructor.
  me->object_type = object_type.
  me->molga = molga.
  me->object_seqnr = object_seqnr.

**JMB20210831 start insert - fallback Seqnr
*
  CHECK me->object_seqnr IS INITIAL.
  me->object_seqnr = '*'.
*JMB20210831 insert end
ENDMETHOD.


  METHOD get_customizing_conversion.
    DATA: lr_infty TYPE RANGE OF infty.
    IF me->custom_fields IS NOT INITIAL AND me->custom_conversion IS INITIAL.
      lr_infty = VALUE #( FOR infty IN me->customizing ( sign = 'I' option = 'EQ' low = infty-infty ) ).
      SELECT * FROM /sew/int_convers INTO TABLE me->custom_conversion WHERE molga = molga AND infty IN lr_infty.
      IF sy-subrc IS INITIAL.
        customizing = me->custom_conversion.
        customizing_exists = abap_true.
      ENDIF.
    ELSE.
      customizing = me->custom_conversion.
      customizing_exists = abap_true.
    ENDIF.
  ENDMETHOD.


METHOD get_customizing_fields.
  DATA: lr_infty TYPE RANGE OF infty.

  "First get folders
  me->get_customizing_folders( IMPORTING customizing_exists = customizing_exists ).

  IF me->custom_folders IS NOT INITIAL AND me->custom_fields IS INITIAL.

    LOOP AT me->custom_folders ASSIGNING FIELD-SYMBOL(<folder>).
      SELECT * FROM /sew/int_fields INTO TABLE @DATA(fields) WHERE object       = @<folder>-object       AND
                                                                   object_seqnr = @<folder>-object_seqnr AND
                                                                   molga        = @<folder>-molga        AND
                                                                   infty        = @<folder>-infty        AND
                                                                   seqnr        = @<folder>-seqnr        AND
                                                                   folder       = @<folder>-folder.
      IF sy-subrc IS INITIAL.
        me->custom_fields = CORRESPONDING #( BASE ( me->custom_fields ) fields ).
        CLEAR fields.
      ELSE.
        SELECT * FROM /sew/int_fields INTO TABLE fields WHERE object       = <folder>-object       AND
                                                              object_seqnr = <folder>-object_seqnr AND
                                                              molga        = '*'                   AND "No molga specific entry. Maybe general entry exists
                                                              infty        = <folder>-infty        AND
                                                              seqnr        = <folder>-seqnr        AND
                                                              folder       = <folder>-folder.
        IF sy-subrc IS INITIAL.
          me->custom_fields = CORRESPONDING #( BASE ( me->custom_fields ) fields ).
          CLEAR fields.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF me->custom_fields IS NOT INITIAL.
      customizing = me->custom_fields.
      customizing_exists = abap_true.
    ENDIF.
  ENDIF.
*      lr_infty = VALUE #( FOR infty IN me->customizing ( sign = 'I' option = 'EQ' low = infty-infty ) ).
*      SELECT * FROM /sew/int_fields INTO TABLE me->custom_fields WHERE object = me->object_type AND object_seqnr = me->object_seqnr AND molga = molga AND infty IN lr_infty.
*      IF sy-subrc IS INITIAL.
*        customizing = me->custom_fields.
*        customizing_exists = abap_true.
*      ELSE.
*        SELECT * FROM /sew/int_fields INTO TABLE me->custom_fields WHERE object = me->object_type AND object_seqnr = me->object_seqnr AND molga = '*' AND infty IN lr_infty.
*        IF sy-subrc IS INITIAL.
*          customizing = me->custom_fields.
*          customizing_exists = abap_true.
*        ELSE.
*          customizing = me->custom_fields.
*          customizing_exists = abap_true.
*        ENDIF.
*      ENDIF.
*    ENDIF.

  me->get_customizing_values( IMPORTING customizing_exists = customizing_exists ).

*   Get conversion customizing
  IF me->custom_conversion IS INITIAL.
    SELECT * FROM /sew/int_convers INTO TABLE me->custom_conversion WHERE molga = molga.

    IF sy-subrc    IS NOT INITIAL OR
       add_default EQ abap_true. "JMB20210709 I
      SELECT * FROM /sew/int_convers INTO TABLE @DATA(int_conv_tmp) WHERE molga = '*'.
      APPEND LINES OF int_conv_tmp TO me->custom_conversion.
    ENDIF.
  ENDIF.

*   Get mapping customizing
  IF me->custom_mapping_fields IS INITIAL.
    SELECT * FROM /sew/int_mapp_fi INTO TABLE me->custom_mapping_fields WHERE molga = molga.

    IF sy-subrc    IS NOT INITIAL OR
       add_default EQ abap_true. "JMB20210709 I
      SELECT * FROM /sew/int_mapp_fi INTO TABLE @DATA(int_map_fi_tmp) WHERE molga = '*'.
      APPEND LINES OF int_map_fi_tmp TO me->custom_mapping_fields.
    ENDIF.
  ENDIF.

  "Get mapping values
  IF me->custom_mapping_values IS INITIAL.
    SELECT * FROM /sew/int_mapping INTO TABLE me->custom_mapping_values WHERE molga = molga.

    IF sy-subrc    IS NOT INITIAL OR
       add_default EQ abap_true. "JMB20210709 I
      SELECT * FROM /sew/int_mapping INTO TABLE @DATA(int_map_tmp) WHERE molga = '*'.
      APPEND LINES OF int_map_tmp TO me->custom_mapping_values.
    ENDIF.
  ENDIF.
ENDMETHOD.


  METHOD GET_CUSTOMIZING_FOLDERS.
    DATA: lr_infty TYPE RANGE OF infty.
    IF me->customizing IS NOT INITIAL AND me->custom_folders IS INITIAL.
      lr_infty = VALUE #( FOR infty IN me->customizing ( sign = 'I' option = 'EQ' low = infty-infty ) ).
      SELECT * FROM /sew/int_folders INTO TABLE me->custom_folders WHERE object = me->object_type AND object_seqnr = me->object_seqnr AND molga = molga AND infty IN lr_infty.
      IF sy-subrc IS INITIAL.
        customizing = me->custom_folders.
        customizing_exists = abap_true.
      ELSE.
        SELECT * FROM /sew/int_folders INTO TABLE me->custom_folders WHERE object = me->object_type AND object_seqnr = me->object_seqnr AND molga = '*' AND infty IN lr_infty.
        IF sy-subrc IS INITIAL.
          customizing = me->custom_folders.
          customizing_exists = abap_true.
        ELSE.
          customizing = me->custom_folders.
          customizing_exists = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_customizing_infty.
    IF me->customizing IS INITIAL.
      customizing_exists = abap_true.
      SELECT * FROM /sew/int_infotyp INTO TABLE @me->customizing WHERE object       = @me->object_type  AND
                                                                       object_seqnr = @me->object_seqnr AND
                                                                       molga        = @molga.
      IF sy-subrc IS INITIAL.
        customizing = me->customizing.
      ELSE.
        SELECT * FROM /sew/int_infotyp INTO TABLE @me->customizing WHERE object       = @me->object_type  AND
                                                                         object_seqnr = @me->object_seqnr AND
                                                                         molga        = '*'.
        IF sy-subrc IS INITIAL.
          customizing = me->customizing.
        ELSE.
          customizing_exists = abap_false.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_customizing_mapping.
    DATA: lr_infty TYPE RANGE OF infty.
    IF me->custom_fields IS NOT INITIAL AND me->custom_mapping_values IS INITIAL.
*      lr_infty = VALUE #( FOR infty IN me->customizing ( sign = 'I' option = 'EQ' low = infty-infty ) ).
      LOOP AT me->customizing ASSIGNING FIELD-SYMBOL(<customizing>).
        SELECT * FROM /sew/int_mapping INTO TABLE me->custom_mapping_values WHERE molga = molga AND infty EQ <customizing>-infty AND seqnr = <customizing>-seqnr.
        IF sy-subrc IS INITIAL.
          customizing = me->custom_mapping_values.
          customizing_exists = abap_true.
        ENDIF.
      ENDLOOP.
    ELSE.
      customizing = me->custom_mapping_values.
      customizing_exists = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD get_customizing_values.
    DATA: lr_infty TYPE RANGE OF infty.
    IF me->customizing IS NOT INITIAL AND me->custom_values IS INITIAL.
      lr_infty = VALUE #( FOR infty IN me->customizing ( sign = 'I' option = 'EQ' low = infty-infty ) ).
      SELECT * FROM /sew/int_values INTO TABLE me->custom_values WHERE object = me->object_type AND molga = molga AND infty IN lr_infty.
      IF sy-subrc IS INITIAL.
        customizing = me->custom_values.
        customizing_exists = abap_true.
      ELSE.
        SELECT * FROM /sew/int_values INTO TABLE me->custom_values WHERE object = me->object_type AND molga = '*' AND infty IN lr_infty.
        IF sy-subrc IS INITIAL.
          customizing = me->custom_values.
          customizing_exists = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_instance.
    IF /sew/cl_int_customizing_xml=>go_instance IS BOUND                   AND
       /sew/cl_int_customizing_xml=>go_instance->molga = molga             AND
       /sew/cl_int_customizing_xml=>go_instance->object_type = object_type.
      ro_instance = /sew/cl_int_customizing_xml=>go_instance.
    ELSE.
      CREATE OBJECT /sew/cl_int_customizing_xml=>go_instance
        EXPORTING
          object_type  = object_type
          molga        = molga
          object_seqnr = object_seqnr.
      ro_instance = /sew/cl_int_customizing_xml=>go_instance.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
