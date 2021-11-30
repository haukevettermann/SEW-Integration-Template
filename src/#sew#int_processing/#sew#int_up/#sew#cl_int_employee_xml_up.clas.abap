class /SEW/CL_INT_EMPLOYEE_XML_UP definition
  public
  final
  create public .

public section.

  types:
    t_itaendup TYPE TABLE OF /sew/int_it_aeup WITH KEY
    mandt pernr infty subty objps sprps endda begda seqnr operation timestamp .

  data XML_NODE type ref to IF_IXML_NODE .
  data CHANGED_DATA type T_ITAENDUP .
  data PERNR type PERNR_D .
  data MOLGA type MOLGA .
  data ORACLE_ID type CHAR10 .
  data GO_CUSTOMIZING type ref to /SEW/CL_INT_CUSTOMIZING_XML .
  data GO_INFOTYPE type ref to /SEW/CL_INT_INFTY_PROC_XML_UP .
  data XML_DOCUMENT type ref to IF_IXML_DOCUMENT .
  data SEQNR type SEQNR .

  methods BUILD_XML .
  methods CONSTRUCTOR
    importing
      !XML_DOCUMENT type ref to IF_IXML_DOCUMENT
      !XML_NODE type ref to IF_IXML_NODE
      !PERNR type PERNR_D
      !CHANGED_DATA type T_ITAENDUP
      !SEQNR type SEQNR optional .
  class-methods READ_ORACLE_ID
    importing
      !PERNR type PERNR_D
      value(DATE) type DATS optional
    exporting
      !MESSAGE type BAPIRET1
    returning
      value(ORACLE_ID) type /SEW/DD_OBJECTID .
  methods READ_MOLGA
    exporting
      !MESSAGE type BAPIRET1 .
  methods READ_CUSTOMIZING
    importing
      !EXPORT type FLAG optional
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



CLASS /SEW/CL_INT_EMPLOYEE_XML_UP IMPLEMENTATION.


  METHOD build_xml.
*   First process first level
    SORT me->go_infotype->folders BY hierarchy.
    LOOP AT me->go_infotype->folders ASSIGNING FIELD-SYMBOL(<folder>).
      IF <folder>-hierarchy = 1.
**JMB20211026 start insert - generate a new object in case of followinf infotypes
*
        DATA(new_objec) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '2001' )
                                              ( sign = 'I' option = 'EQ' low = '2006' ) ).
        IF me->go_infotype->infty NOT IN new_objec.
*JMB20211026 insert end
          /sew/cl_int_xml_up=>get_direct_child(
            EXPORTING
              name     = CONV #( <folder>-folder )
              node_in  = me->xml_node
            IMPORTING
              begda = DATA(begda)
              endda = DATA(endda)
              node_out = DATA(node_out) ).
        ENDIF.

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
      IF <folder>-has_elements = abap_true.
        DATA(folder) = '//' && <folder>-folder.
        DATA(fields) = VALUE /sew/cl_int_infty_proc_xml_up=>t_fields( FOR field IN me->go_infotype->fields WHERE ( folder EQ folder          AND "JMB20210913 D -folder CS <folder>-folder AND
                                                                                                                   begda  >= <folder>-begda  AND
                                                                                                                   endda  <= <folder>-endda ) ( field ) ).
        LOOP AT fields ASSIGNING FIELD-SYMBOL(<field>).
          DATA(element) = <field>-element.
          REPLACE ALL OCCURRENCES OF '/' IN element WITH''.

          READ TABLE me->go_customizing->custom_conversion WITH KEY field_sap = <field>-field infty = <field>-infty ASSIGNING FIELD-SYMBOL(<conversion>).
          IF sy-subrc = 0.
            CALL METHOD /sew/cl_int_conversion=>(<conversion>-conversion_method)
              EXPORTING
                value_in  = <field>-value
              RECEIVING
                value_out = <field>-value.
          ENDIF.
          /sew/cl_int_xml_up=>add_element(
            EXPORTING
              name    = CONV #( element )
              value   = CONV #( <field>-value )
              xml_in  = me->xml_document
            CHANGING
              node_in = <folder>-node ).
          READ TABLE me->go_infotype->fields WITH KEY infty  = <field>-infty
                                                      field  = <field>-field
                                                      begda  = <field>-begda
                                                      endda  = <field>-endda
                                                      folder = <field>-folder
                                                      value  = <field>-value ASSIGNING FIELD-SYMBOL(<processed_field>).
          IF <processed_field> IS ASSIGNED.
            <processed_field>-processed = abap_true.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


METHOD constructor.
  me->pernr = pernr.
  me->changed_data = changed_data.
  me->xml_node = xml_node.
  me->xml_document = xml_document.
  me->seqnr = seqnr.
ENDMETHOD.


METHOD process_changed_data.
  me->go_infotype = NEW /sew/cl_int_infty_proc_xml_up( changed_data_it = me->changed_data
                                                       customizing     = me->go_customizing ).

  go_infotype->get_fields(  IMPORTING message = message ).
  go_infotype->get_folders( IMPORTING message = message ).
ENDMETHOD.


METHOD read_customizing.
  DATA(ok) = abap_true.
  DATA: tmp_fields TYPE /sew/cl_int_customizing_xml=>t_customizing_fields.
  me->go_customizing = /sew/cl_int_customizing_xml=>get_instance( EXPORTING object_type  = /sew/cl_int_constants=>person
                                                                            molga        = me->molga
                                                                            object_seqnr = me->seqnr ).

  me->go_customizing->get_customizing_infty( IMPORTING customizing_exists = ok ).

  IF ok IS INITIAL.
    message-id = /sew/cl_int_constants=>msg_class_int.
    message-type = /sew/cl_int_constants=>error.
    message-number = 000."Kein Infotyp customizing
    message-message_v1 = me->molga.
    message-message_v2 = /sew/cl_int_constants=>person.
  ELSE.
    me->go_customizing->get_customizing_fields( IMPORTING customizing_exists = ok ). " filter for only export values in customizing
    IF me->go_customizing->custom_fields IS NOT INITIAL AND export EQ abap_true.
      LOOP AT me->go_customizing->custom_fields ASSIGNING FIELD-SYMBOL(<field>).
        IF <field>-export EQ abap_true.
          APPEND <field> TO tmp_fields.
        ENDIF.
      ENDLOOP.
      CLEAR me->go_customizing->custom_fields.
      MOVE-CORRESPONDING tmp_fields TO me->go_customizing->custom_fields.
    ENDIF.
    IF ok IS INITIAL.
      message-id = /sew/cl_int_constants=>msg_class_int.
      message-type = /sew/cl_int_constants=>error.
      message-number = 001."Kein Infotypfeld customizing
      message-message_v1 = me->molga.
      message-message_v2 = /sew/cl_int_constants=>person.
    ENDIF.
  ENDIF.
ENDMETHOD.


METHOD read_molga.
  me->molga = '01'.
**JMB20210831 start insert - get molga from employee
*
  me->molga = /sew/cl_int_utility=>get_molga( pernr = me->pernr
                                              begda = sy-datum
                                              endda = sy-datum ).

  CHECK me->molga IS INITIAL.
  me->molga = '*'.
*JMB20210831 end insert
ENDMETHOD.


  METHOD read_oracle_id.
    IF date IS INITIAL.
      date = sy-datum.
    ENDIF.
    SELECT SINGLE oracleid FROM pa9400 INTO oracle_id WHERE pernr = pernr AND begda <= date AND endda >= date.
    IF sy-subrc IS NOT INITIAL.
      SELECT SINGLE oracleid FROM pa9400 INTO oracle_id WHERE pernr = pernr AND begda <= sy-datum AND endda >= sy-datum.
      "TODO
    ENDIF.
  ENDMETHOD.
ENDCLASS.
