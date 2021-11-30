class /SEW/CL_INT_OBJECT_HANDLER definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF s_xml_node .
    TYPES folder TYPE string.
    TYPES node TYPE REF TO if_ixml_node.
    TYPES END OF s_xml_node .
  types:
    t_xml_node TYPE TABLE OF s_xml_node .
  types S_INFTY_INSTANCES type ref to /SEW/CL_INT_INFTY_PROC_XML .
  types:
    t_infty_instances TYPE TABLE OF s_infty_instances .

  data XML_NODE type ref to IF_IXML_NODE .
  data SAP_ID type HROBJID .
  data CLOUD_ID type /SEW/DD_OBJECTID .
  data CLOUD_PERNR type /SEW/DD_OBJECTNUMBER .
  data MOLGA type MOLGA .
  data INT_RUN type GUID_32 .
  data GO_CUSTOMIZING type ref to /SEW/CL_INT_CUSTOMIZING_XML .
  data INFOTYPES type T_INFTY_INSTANCES .
  data ACTION type MASSN .
  data PRELP_TAB type /SEW/TT_PRELP_TAB .
  data PRELP_TAB_ORIG type /SEW/TT_PRELP_TAB .
  data PRELP_TAB_NOCHNG type /SEW/TT_PRELP_TAB .
  data WPLOG_TAB type WPLOG_TAB .
  data WPLOG_TAB_ORIG type WPLOG_TAB .
  data WPLOG_TAB_NOCHNG type WPLOG_TAB .
  data OBJECT_TYPE type OTYPE .
  data DELIMIT_DATE type DATS .
  data BUKRS type BUKRS .
  data OBJECT_SEQ type SEQNR .

  methods CONSTRUCTOR
    importing
      !XML_NODE type ref to IF_IXML_NODE
      !SAP_ID type TEXT100
      !CLOUD_ID type TEXT100
      !CLOUD_PERNR type CHAR15
      !MOLGA type MOLGA
      !INT_RUN type GUID_32 optional
      !OBJECT_TYPE type OTYPE
      !BUKRS type BUKRS
      !SEQNR type SEQNR .
  methods READ_CUSTOMIZING
    importing
      !OBJECT_TYPE type OTYPE
      !OBJECT_SEQNR type SEQNR
    exporting
      !MESSAGE type BAPIRET1 .
  methods PROCESS_CUSTOMZING
    exporting
      !MESSAGE type BAPIRET1 .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /SEW/CL_INT_OBJECT_HANDLER IMPLEMENTATION.


  METHOD constructor.
    me->molga = molga.
    me->object_type = object_type.
    me->sap_id = sap_id.
    me->cloud_id = cloud_id.
    me->cloud_pernr = cloud_pernr.
    me->xml_node = xml_node.
    me->int_run = int_run.
    me->bukrs = bukrs.
    me->object_seq = seqnr.
  ENDMETHOD.


  METHOD process_customzing.
    LOOP AT me->go_customizing->customizing ASSIGNING FIELD-SYMBOL(<infotype>).
      DATA(status_handler) = /sew/cl_int_status_handler=>get_instance( sap_id      = CONV #( me->sap_id )
                                                                       cloud_id    = CONV #( me->cloud_id )
                                                                       cloud_pernr = CONV #( me->cloud_pernr )
                                                                       int_run     = me->int_run
                                                                       molga       = me->molga ).
      status_handler->bukrs      = me->bukrs.
      status_handler->object_seq = me->object_seq.
*      DATA(lo_infotype) = NEW /sew/cl_int_infty_proc_xml( field_customizing = infty_customizing_folder xml_node = me->xml_node infty = <infotype>-infty ).

      DATA(go_infotype) = NEW /sew/cl_int_infty_proc_xml( customizing = me->go_customizing
                                                          xml_node    = me->xml_node
                                                          infty       = <infotype>-infty
                                                          seqnr       = <infotype>-seqnr
                                                          int_run     = me->int_run
                                                          molga       = me->molga ).
      APPEND go_infotype TO me->infotypes.
      go_infotype->object_handler = me.

*     Get folders from field customizing
      go_infotype->get_folders( IMPORTING message = message ).

*     Log message
      status_handler->add_log_message( EXPORTING aend_id = CONV #( me->cloud_id ) bapiret1 = message ).

*     Get fields
      go_infotype->get_fields( IMPORTING message = message ).

*     Log message.
      status_handler->add_log_message( EXPORTING aend_id = CONV #( me->cloud_id ) bapiret1 = message ).

*     Aggregate folders and fields
      go_infotype->aggregate_folders_fields( IMPORTING message = message ).

*     Log message.
      status_handler->add_log_message( EXPORTING aend_id = CONV #( me->cloud_id ) bapiret1 = message ).

*     Build infotype based on field information
      go_infotype->build_infotype( EXPORTING object_type  = <infotype>-object
                                             object_seqnr = <infotype>-object_seqnr
                                             sap_id       = me->sap_id
                                             cloud_id     = me->cloud_id
                                             int_run      = me->int_run
                                             molga        = me->molga
                                  IMPORTING  message      = message ).

      IF me->sap_id IS INITIAL OR me->sap_id = '00000000'.
        me->sap_id = go_infotype->object_handler->sap_id.
      ENDIF.


    ENDLOOP.
*     Process customer specific action handling
*     Make sure actions are assigned to correct infotypes
    /sew/cl_int_action_handler=>process_action(
      IMPORTING
        has_error   = DATA(has_error)
      CHANGING
        new_it_aend = status_handler->new_it_aend ).
    /sew/cl_int_statics=>ora_bal  = CORRESPONDING #( BASE ( /sew/cl_int_statics=>ora_bal ) status_handler->ora_bal ). "JMB20211017 I
    /sew/cl_int_statics=>it_aend  = CORRESPONDING #( BASE ( /sew/cl_int_statics=>it_aend ) status_handler->new_it_aend ).
    /sew/cl_int_statics=>om_aend  = CORRESPONDING #( BASE ( /sew/cl_int_statics=>om_aend ) status_handler->new_om_aend ).
    /sew/cl_int_statics=>messages = CORRESPONDING #( BASE ( /sew/cl_int_statics=>messages ) status_handler->new_log_messages ).
*     persist data
    IF /sew/cl_int_statics=>test_run = abap_false.
      status_handler->persist_data( source = /sew/cl_int_constants=>serializer ).
    ENDIF.
    CLEAR: status_handler->new_it_aend, status_handler->new_om_aend, status_handler->ora_bal. "JMB20211011 I
    FREE status_handler.
  ENDMETHOD.


  METHOD read_customizing.
    DATA(ok) = abap_true.
    me->go_customizing = /sew/cl_int_customizing_xml=>get_instance(
      EXPORTING
        object_type = CONV #( object_type )
        molga       = me->molga
        object_seqnr = object_seqnr ).
    me->go_customizing->get_customizing_infty(
        IMPORTING
          customizing_exists = ok ).
    IF ok IS INITIAL.
      message-id = /sew/cl_int_constants=>msg_class_int.
      message-type = /sew/cl_int_constants=>error.
      message-number = 000."Kein Infotyp customizing
      message-message_v1 = me->molga.
      message-message_v2 = /sew/cl_int_constants=>person.
    ELSE.
      me->go_customizing->get_customizing_fields(
        IMPORTING
          customizing_exists = ok ).
      IF ok IS INITIAL.
        message-id = /sew/cl_int_constants=>msg_class_int.
        message-type = /sew/cl_int_constants=>error.
        message-number = 001."Kein Infotypfeld customizing
        message-message_v1 = me->molga.
        message-message_v2 = /sew/cl_int_constants=>person.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
