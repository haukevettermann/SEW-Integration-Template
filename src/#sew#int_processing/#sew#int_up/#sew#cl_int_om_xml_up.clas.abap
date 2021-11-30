class /SEW/CL_INT_OM_XML_UP definition
  public
  final
  create public .

public section.

  types:
    t_omaendup TYPE TABLE OF /sew/int_om_aeup WITH KEY
    mandt sap_id plvar otype infty subty istat endda begda seqnr operation timestamp .

  data XML_NODE type ref to IF_IXML_NODE .
  data CHANGED_DATA type T_OMAENDUP .
  data OBJID type HROBJID .
  data MOLGA type MOLGA .
  data ORACLE_ID type CHAR10 .
  data GO_CUSTOMIZING type ref to /SEW/CL_INT_CUSTOMIZING_XML .
  data GO_INFOTYPE type ref to /SEW/CL_INT_INFTY_PROC_XML_UP .
  data XML_DOCUMENT type ref to IF_IXML_DOCUMENT .

  methods BUILD_XML .
  methods CONSTRUCTOR
    importing
      !XML_DOCUMENT type ref to IF_IXML_DOCUMENT
      !XML_NODE type ref to IF_IXML_NODE
      !OBJID type HROBJID
      !CHANGED_DATA type T_OMAENDUP .
  class-methods READ_ORACLE_ID
    importing
      !OBJID type HROBJID
      value(DATE) type DATS optional
    exporting
      !MESSAGE type BAPIRET1
    returning
      value(ORACLE_ID) type /SEW/DD_OBJECTID .
  methods READ_MOLGA
    exporting
      !MESSAGE type BAPIRET1 .
  methods READ_CUSTOMIZING
    exporting
      !MESSAGE type BAPIRET1 .
  methods PROCESS_CHANGED_DATA
    exporting
      !MESSAGE type BAPIRET1 .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF s_xml_node .
    TYPES folder TYPE string.
    TYPES node TYPE REF TO if_ixml_node.
    TYPES END OF s_xml_node .
  types:
    t_xml_node TYPE TABLE OF s_xml_node .
ENDCLASS.



CLASS /SEW/CL_INT_OM_XML_UP IMPLEMENTATION.


  METHOD BUILD_XML.
*   First process first level
    SORT me->go_infotype->folders BY hierarchy.
    LOOP AT me->go_infotype->folders ASSIGNING FIELD-SYMBOL(<folder>).
      IF <folder>-hierarchy = 1.
        /sew/cl_int_xml_up=>get_direct_child(
          EXPORTING
            name     = CONV #( <folder>-folder )
            node_in  = me->xml_node
          IMPORTING
            begda = DATA(begda)
            endda = DATA(endda)
            node_out = DATA(node_out) ).
        IF node_out IS INITIAL OR ( begda NE <folder>-begda AND begda IS NOT INITIAL ) OR ( endda NE <folder>-endda AND endda IS NOT INITIAL ).
          /sew/cl_int_xml_up=>add_node(
            EXPORTING
              name         = CONV #( <folder>-folder )
              xml_in       = me->xml_document
              begda = COND #( WHEN <folder>-has_elements = abap_true THEN <folder>-begda )
              endda = COND #( WHEN <folder>-has_elements = abap_true THEN <folder>-endda )
            IMPORTING
              node_out     = <folder>-node
            CHANGING
              node_in      = me->xml_node ).
        ENDIF.
      ELSE.
        READ TABLE me->go_infotype->folders WITH KEY folder = <folder>-prev_folder ASSIGNING FIELD-SYMBOL(<prev_folder>).
        /sew/cl_int_xml_up=>get_direct_child(
          EXPORTING
            name     = CONV #( <folder>-folder )
            node_in  = <prev_folder>-node
          IMPORTING
            begda = begda
            endda = endda
            node_out = node_out ).
        IF node_out IS INITIAL OR begda NE <folder>-begda OR endda NE <folder>-endda.
          /sew/cl_int_xml_up=>add_node(
            EXPORTING
              name         = CONV #( <folder>-folder )
              xml_in       = me->xml_document
              begda = COND #( WHEN <folder>-has_elements = abap_true THEN <folder>-begda )
              endda = COND #( WHEN <folder>-has_elements = abap_true THEN <folder>-endda )
            IMPORTING
              node_out     = <folder>-node
            CHANGING
              node_in      = <prev_folder>-node ).
        ENDIF.
      ENDIF.

      "add child elements
      IF <folder>-has_elements = abap_true.
        DATA(fields) = VALUE /sew/cl_int_infty_proc_xml_up=>t_fields( FOR field IN me->go_infotype->fields WHERE ( folder CS <folder>-folder  AND
                                                                                                                   begda  >= <folder>-begda   AND
                                                                                                                   endda  <= <folder>-endda ) ( field ) ).
        LOOP AT fields ASSIGNING FIELD-SYMBOL(<field>).
          DATA(element) = <field>-element.
          REPLACE ALL OCCURRENCES OF '/' IN element WITH''.
          /sew/cl_int_xml_up=>add_element(
            EXPORTING
              name    = CONV #( element )
              value   = CONV #( <field>-value )
              xml_in  = me->xml_document
            CHANGING
              node_in = <folder>-node ).
          READ TABLE me->go_infotype->fields WITH KEY infty = <field>-infty field = <field>-field begda = <field>-begda endda = <field>-endda folder = <field>-folder value = <field>-value ASSIGNING FIELD-SYMBOL(<processed_field>).
          IF <processed_field> IS ASSIGNED.
            <processed_field>-processed = abap_true.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD CONSTRUCTOR.
    me->objid = objid.
    me->changed_data = changed_data.
    me->xml_node = xml_node.
    me->xml_document = xml_document.
  ENDMETHOD.


  METHOD PROCESS_CHANGED_DATA.
    me->go_infotype = NEW /sew/cl_int_infty_proc_xml_up( changed_data_om = me->changed_data
                                                         customizing = me->go_customizing ).
    go_infotype->get_fields( IMPORTING message = message ).
    go_infotype->get_folders( IMPORTING message = message ).
  ENDMETHOD.


  METHOD READ_CUSTOMIZING.
    DATA(ok) = abap_true.
    me->go_customizing = /sew/cl_int_customizing_xml=>get_instance(
      EXPORTING
        object_type = /sew/cl_int_constants=>orgunit
        molga       = me->molga ).
    me->go_customizing->get_customizing_infty(
        IMPORTING
          customizing_exists = ok ).
    IF ok IS INITIAL.
      message-id = /sew/cl_int_constants=>msg_class_int.
      message-type = /sew/cl_int_constants=>error.
      message-number = 000."Kein Infotyp customizing
      message-message_v1 = me->molga.
      message-message_v2 = /sew/cl_int_constants=>orgunit.
    ELSE.
      me->go_customizing->get_customizing_fields(
        IMPORTING
          customizing_exists = ok ).
      IF ok IS INITIAL.
        message-id = /sew/cl_int_constants=>msg_class_int.
        message-type = /sew/cl_int_constants=>error.
        message-number = 001."Kein Infotypfeld customizing
        message-message_v1 = me->molga.
        message-message_v2 = /sew/cl_int_constants=>orgunit.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD READ_MOLGA.
    me->molga = '01'.
  ENDMETHOD.


  METHOD read_oracle_id.
    IF date IS INITIAL.
      date = sy-datum.
    ENDIF.
    SELECT SINGLE oracleid FROM hrp9401 INTO oracle_id WHERE objid = objid AND begda <= date AND endda >= date.
    IF sy-subrc IS NOT INITIAL.
      SELECT SINGLE oracleid FROM hrp9401 INTO oracle_id WHERE objid = objid AND begda <= sy-datum AND endda >= sy-datum.
      "TODO
    ENDIF.
  ENDMETHOD.
ENDCLASS.
