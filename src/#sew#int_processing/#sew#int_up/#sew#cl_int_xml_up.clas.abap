class /SEW/CL_INT_XML_UP definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ts_xpath_data.
        INCLUDE TYPE /sew/int_fields.
        TYPES id TYPE string.
    TYPES begda TYPE begda.
    TYPES endda TYPE endda.
    TYPES value TYPE text100.
    TYPES END OF ts_xpath_data .
  types:
    tt_xpath_data TYPE TABLE OF ts_xpath_data .
  types BEGDA type BEGDA .
  types:
    BEGIN OF s_folder.
    TYPES node TYPE REF TO if_ixml_node.
    TYPES END OF s_folder .
  types:
    BEGIN OF s_folder_with_begda.
    TYPES begda TYPE begda.
    TYPES endda TYPE endda.
    TYPES node TYPE REF TO if_ixml_node.
    TYPES END OF s_folder_with_begda .
  types:
    t_folder_with_begda TYPE TABLE OF s_folder_with_begda .
  types:
    t_folder TYPE TABLE OF s_folder .
  types:
    tt_data TYPE TABLE OF docs .
  types:
    tt_customizing TYPE TABLE OF /sew/int_fields .
  types:
    tt_string TYPE TABLE OF string WITH DEFAULT KEY .

  data GO_XML type ref to IF_IXML_DOCUMENT .
  data GT_CUSTOMIZING type TT_CUSTOMIZING .
  data GO_PROCESSOR type ref to CL_XSLT_PROCESSOR .
  data GO_COLLECTION type ref to IF_IXML_NODE_COLLECTION .

  class-methods CREATE_STRING_FROM_XSTRING
    importing
      !XSTRING type XSTRING
    returning
      value(STRING) type STRING .
  class-methods CREATE_XSTRING_FROM_XML
    importing
      !XML type ref to IF_IXML_DOCUMENT
    returning
      value(XSTRING) type XSTRING .
  class-methods CREATE_INITIAL_XML
    exporting
      value(XML_DOCUMENT) type ref to IF_IXML_DOCUMENT
      !XML_DATA_NODE type ref to IF_IXML_NODE .
  class-methods ADD_NODE
    importing
      !NAME type STRING
      !XML_IN type ref to IF_IXML_DOCUMENT
      !BEGDA type BEGDA optional
      !ENDDA type BEGDA optional
    exporting
      !NODE_OUT type ref to IF_IXML_NODE
    changing
      !NODE_IN type ref to IF_IXML_NODE .
  class-methods ADD_ELEMENT
    importing
      !NAME type STRING
      !VALUE type TEXT100
      !XML_IN type ref to IF_IXML_DOCUMENT
    changing
      !NODE_IN type ref to IF_IXML_NODE .
  class-methods ADD_NODE_OBJECT_EMPLOYEE
    importing
      !OBJECT_ID_CLOUD type STRING
      !OBJECT_ID_SAP type STRING
      !XPATH_ORACLE type STRING
      !XPATH_SAP type STRING
      !XPATH_OBJECT type STRING
      !XML_DOCUMENT type ref to IF_IXML_DOCUMENT
      !AEND_UP type /SEW/INT_IT_AEUP optional
    exporting
      !XML_NODE type ref to IF_IXML_NODE
    changing
      !XML_DATA_NODE type ref to IF_IXML_NODE .
  class-methods ADD_NODE_OBJECT_OM
    importing
      !OBJECT_ID_CLOUD type STRING
      !OBJECT_ID_SAP type STRING
      !XPATH_ORACLE type STRING
      !XPATH_SAP type STRING
      !XPATH_OBJECT type STRING
      !XML_DOCUMENT type ref to IF_IXML_DOCUMENT
      !AEND_UP type /SEW/INT_OM_AEUP optional
    exporting
      !XML_NODE type ref to IF_IXML_NODE
    changing
      !XML_DATA_NODE type ref to IF_IXML_NODE .
  class-methods SPLIT_XPATH
    importing
      value(XPATH) type STRING
    returning
      value(NODES) type TT_STRING .
  class-methods GET_DIRECT_CHILD
    importing
      !NAME type STRING
      !NODE_IN type ref to IF_IXML_NODE
    exporting
      value(NODE_OUT) type ref to IF_IXML_NODE
      !BEGDA type DATS
      !ENDDA type DATS .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /SEW/CL_INT_XML_UP IMPLEMENTATION.


  METHOD add_element.
**-- CREATE the root Node
    data(element) = xml_in->create_element( name = name ).
    element->set_value( value = conv #( value ) ).
    node_in->append_child( new_child = element ).
  ENDMETHOD.


  METHOD add_node.
    node_out = xml_in->create_element( name = name ).
    node_in->append_child( new_child = node_out ).
*   Add begda
    IF begda IS NOT INITIAL.
      DATA(node_begda) = xml_in->create_element( name = /sew/cl_int_constants=>startdate ).
      node_begda->set_value( value = CONV #( /sew/cl_int_conversion=>convert_date_oracle_om( value_in = CONV #( begda ) ) ) ).
      node_out->append_child( new_child = node_begda ).
    ENDIF.
*   Add endda
    IF endda IS NOT INITIAL.
      DATA(node_endda) = xml_in->create_element( name = /sew/cl_int_constants=>enddate ).
      node_endda->set_value( value = CONV #( /sew/cl_int_conversion=>convert_date_oracle_om( value_in = CONV #( endda ) ) ) ).
      node_out->append_child( new_child = node_endda ).
    ENDIF.
  ENDMETHOD.


  METHOD add_node_object_employee.
    DATA:
          xml_element TYPE REF TO if_ixml_element.
    DATA(xpath_object_new) = xpath_object.
    REPLACE ALL OCCURRENCES OF '/' IN xpath_object_new WITH ''.
    DATA(xpath_oracle_new) = xpath_oracle.
    REPLACE ALL OCCURRENCES OF '/' IN xpath_oracle_new WITH ''.
    DATA(xpath_sap_new) = xpath_sap.
    REPLACE ALL OCCURRENCES OF '/' IN xpath_sap_new WITH ''.
*   Create and append Object
    DATA(xml_element_object) = xml_document->create_element(
    name      = xpath_object_new ).
    xml_data_node->append_child( new_child = xml_element_object ).
    xml_node = xml_element_object.

**JMB20211007 start insert - add oracle person number
*
    xml_element = xml_document->create_element( name = 'OraclePernr' ).
    DATA(oracle_pernr) = /sew/cl_forms_utils=>get_oraclepernr( CONV #( object_id_sap ) ).
    xml_element->set_value( value = CONV #( oracle_pernr ) ).
    xml_node->append_child( new_child = xml_element ).
*JMB20211007 insert end

*   Create and append element oracle id
    xml_element = xml_document->create_element( name = xpath_oracle_new ).
    xml_element->set_value( value = object_id_cloud ).
    xml_node->append_child( new_child = xml_element ).

*   Create and append element oracle id
    xml_element = xml_document->create_element( name = xpath_sap_new ).
    xml_element->set_value( value = object_id_sap ).
    xml_node->append_child( new_child = xml_element ).
*   Create and append element company code
    xml_element = xml_document->create_element( name = 'CompanyCode' ).
    xml_element->set_value( value = CONV #( aend_up-legal_entity ) ).
    xml_node->append_child( new_child = xml_element ).

*   Create and append element source system owner
    CONCATENATE 'SAP' '_' sy-mandt INTO DATA(source).
    xml_element = xml_document->create_element( name = 'SourceSystemOwner' ).
    xml_element->set_value( value = source ).
    xml_node->append_child( new_child = xml_element ).

*   Set Start Date in case of pernr update.
    IF aend_up IS NOT INITIAL.
      xml_element = xml_document->create_element( name = /sew/cl_int_constants=>startdate ).
      xml_element->set_value( value = CONV #( /sew/cl_int_conversion=>convert_date_oracle( value_in = CONV #( aend_up-begda ) ) ) ).
      xml_node->append_child( new_child = xml_element ).
    ENDIF.
  ENDMETHOD.


  METHOD add_node_object_om.
    DATA:
          xml_element TYPE REF TO if_ixml_element.
    DATA(xpath_object_new) = xpath_object.
    REPLACE ALL OCCURRENCES OF '/' IN xpath_object_new WITH ''.
    DATA(xpath_oracle_new) = xpath_oracle.
    REPLACE ALL OCCURRENCES OF '/' IN xpath_oracle_new WITH ''.
    DATA(xpath_sap_new) = xpath_sap.
    REPLACE ALL OCCURRENCES OF '/' IN xpath_sap_new WITH ''.
*   Create and append Object
    DATA(xml_element_object) = xml_document->create_element(
    name      = xpath_object_new ).
    xml_data_node->append_child( new_child = xml_element_object ).
    xml_node = xml_element_object.
*   Create and append element oracle id
    xml_element = xml_document->create_element( name = xpath_oracle_new ).
    xml_element->set_value( value = object_id_cloud ).
    xml_node->append_child( new_child = xml_element ).
*   Create and append element oracle id
    xml_element = xml_document->create_element( name = xpath_sap_new ).
    xml_element->set_value( value = object_id_sap ).
    xml_node->append_child( new_child = xml_element ).
*   Create and append org name
    xml_element = xml_document->create_element( name = 'DepartmentName' ).
    SELECT SINGLE stext FROM hrp1000 INTO @DATA(stext) WHERE langu = 'E' AND objid = @object_id_sap.
    xml_element->set_value( value = CONV #( stext ) ).
    xml_node->append_child( new_child = xml_element ).
*   Create and append element company code
*    xml_element = xml_document->create_element( name = 'CompanyCode' ).
*    xml_element->set_value( value = CONV #( aend_up-legal_entity ) ).
*    xml_node->append_child( new_child = xml_element ).
*   Create and append element source system owner
    CONCATENATE 'SAP' '_' sy-mandt INTO DATA(source).
    xml_element = xml_document->create_element( name = 'SourceSystemOwner' ).
    xml_element->set_value( value = CONV #( source ) ).
    xml_node->append_child( new_child = xml_element ).
*   Set Start Date in case of pernr update.
    IF aend_up IS NOT INITIAL.
      xml_element = xml_document->create_element( name = /sew/cl_int_constants=>startdate ).
      xml_element->set_value( value = CONV #( /sew/cl_int_conversion=>convert_date_oracle_om( value_in = CONV #( aend_up-begda ) ) ) ).
      xml_node->append_child( new_child = xml_element ).
    ENDIF.
  ENDMETHOD.


  METHOD create_initial_xml.
    DATA:
      ref_ixml TYPE REF TO if_ixml.
**-- Create the Main Factory
    ref_ixml = cl_ixml=>create( ).
**-- Create the Initial Document
    xml_document = ref_ixml->create_document( ).
*   create and append object
    data(xml_element_object) = xml_document->create_element(
    name      = 'Abstraction' ).
    xml_document->append_child( new_child = xml_element_object ).
    xml_data_node = xml_element_object.
  ENDMETHOD.


  METHOD create_string_from_xstring.
**--convert xstring to string
    DATA(conversion) = cl_abap_conv_in_ce=>create(
      EXPORTING
        encoding    = 'UTF-8'
        endian      = 'L'
        ignore_cerr = 'X'
        replacement = '#'
        input       = xstring ).
    CALL METHOD conversion->read
      IMPORTING
        data = string.                  " String
  ENDMETHOD.


  METHOD create_xstring_from_xml.
    CALL FUNCTION 'SDIXML_DOM_TO_XML'
      EXPORTING
        document      = xml
      IMPORTING
        xml_as_string = xstring
      EXCEPTIONS
        no_document   = 1
        OTHERS        = 2.
  ENDMETHOD.


METHOD get_direct_child.
  DATA(children) = node_in->get_children( ).
  DATA(iterator) = children->create_iterator( ).
  DATA(child) = iterator->get_next( ).
  WHILE child IS BOUND.
    IF child->get_name( ) = name.
      node_out = child.
      children = child->get_children( ).
      iterator = children->create_iterator( ).
      child = iterator->get_next( ).
      WHILE child IS BOUND.
        IF child->get_name( ) = /sew/cl_int_constants=>startdate.
          begda = child->get_value( ).
        ENDIF.
        IF child->get_name( ) = /sew/cl_int_constants=>enddate.
          endda = child->get_value( ).
        ENDIF.
        IF begda IS NOT INITIAL AND endda IS NOT INITIAL.
          EXIT.
        ENDIF.
      ENDWHILE.
      EXIT.
    ELSE.
      child = iterator->get_next( ).
    ENDIF.
  ENDWHILE.
ENDMETHOD.


  METHOD split_xpath.
    DATA(xpath_new) = xpath.
    REPLACE ALL OCCURRENCES OF '//' IN xpath_new WITH ''.
    SPLIT xpath_new AT '/' INTO TABLE nodes.
  ENDMETHOD.
ENDCLASS.
