class /SEW/CL_INT_OM_AEND definition
  public
  create public .

public section.

  constants IT_AEND_MC type CHAR20 value '/SEW/MC_INT_IT_AEND' ##NO_TEXT.
  data MSG_CONT type ref to /IWBEP/IF_MESSAGE_CONTAINER .
  constants STATUS_INITIAL type /SEW/DD_STATUS value '01' ##NO_TEXT.
  constants STATUS_ERROR type /SEW/DD_STATUS value '03' ##NO_TEXT.
  constants STATUS_SUCCESS type /SEW/DD_STATUS value '02' ##NO_TEXT.
  data AEND_NEW type /SEW/TT_OM_AEND .
  data AEND_ORIG type /SEW/TT_OM_AEND .
  data AEND_NOCHNG type /SEW/TT_OM_AEND .
  data ACTION type MASSN .
  data AEND_ID type GUID_32 .
  data SAP_ID type HROBJID .
  data CLOUD_ID type HROBJID .
  data INT_RUN type GUID_32 .
  data MOLGA type MOLGA .

  methods GET
    importing
      !DATES type RSDSSELOPT_T optional
      !STATUS type RSDSSELOPT_T optional
      !OBJID type RSDSSELOPT_T optional
      !INTRUN type RSDSSELOPT_T optional
    returning
      value(OM_AEND) type /SEW/TT_OM_AEND .
  methods SAVE_ENTRIES
    importing
      !OM_AEND type /SEW/TT_OM_AEND
    returning
      value(IS_OK) type BOOLEAN .
  methods CONSTRUCTOR .
  methods PNNNN_TO_OM_AEND
    importing
      !PNNNN type ANY
    returning
      value(OM_AEND) type /SEW/INT_OM_AEND .
  methods PRELP_TO_OM_AEND
    importing
      !WPLOG_NEW type WPLOG_TAB
      !WPLOG_ORIG type WPLOG_TAB
      !WPLOG_NOCHNG type WPLOG_TAB
    returning
      value(OM_AEND) type /SEW/TT_OM_AEND .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS infty_ins TYPE actio VALUE 'INS' ##NO_TEXT.
    CONSTANTS infty_del TYPE actio VALUE 'DEL' ##NO_TEXT.
    CONSTANTS infty_mod TYPE actio VALUE 'MOD' ##NO_TEXT.
ENDCLASS.



CLASS /SEW/CL_INT_OM_AEND IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    msg_cont = /iwbep/cl_mgw_msg_container=>get_mgw_msg_container( ).

  ENDMETHOD.


  METHOD get.

    SELECT * FROM /sew/int_om_aend INTO TABLE om_aend WHERE sap_id   IN objid  AND
                                                            status  IN status AND
                                                            int_run IN intrun AND
                                                            ( begda IN dates OR
                                                              endda IN dates ) .

  ENDMETHOD.


  METHOD PNNNN_TO_OM_AEND.
*!!!OBSOLETE: Do not use -> moved to /SEW/CL_INT_TYPE_CAST!!!
*
*    CHECK pnnnn IS NOT INITIAL.
*
*    cl_hr_pnnnn_type_cast=>pnnnn_to_wplog( EXPORTING pnnnn = pnnnn
*                                           IMPORTING wplog = DATA(wplog) ).
*
*    MOVE-CORRESPONDING wplog TO om_aend.

  ENDMETHOD.


  METHOD prelp_to_om_aend.
*!!!OBSOLETE: Do not use -> moved to /SEW/CL_INT_TYPE_CAST!!!
*
**    DATA(infotype) = /sew/cl_int_infty_proc_xml=>get_instance( ).
**    DATA(data_transform) = /sew/cl_int_data_transform=>get_instance( ).
*
**    DATA(prelp_new) = infotype->prelp_tab.
**    DATA(prelp_orig) = infotype->prelp_tab_orig.
**    DATA(prelp_nochng) = infotype->prelp_tab_nochng.
*
*    me->aend_new = CORRESPONDING #( wplog_new ).
*    me->aend_orig = CORRESPONDING #( wplog_orig ).
*    me->aend_nochng = CORRESPONDING #( wplog_nochng ).
*
**    me->enhance_entries( EXPORTING aend_new = aend_new aend_orig = aend_orig aend_nochng = aend_nochng
**                         IMPORTING om_aend = om_aend ).
  ENDMETHOD.


  METHOD save_entries.
* !!!OBSOLETE: Do not use -> moved to /SEW/CL_INT_STATUS_HANDLER -> PERSTIST_DATA!!!
*
*    MODIFY /sew/int_om_aend FROM TABLE om_aend.
*
*    is_ok = abap_true.
*    IF sy-subrc NE 0.
*      is_ok = abap_false.
*
*      msg_cont->add_message( iv_msg_type               = /iwbep/cl_cos_logger=>error
*                             iv_msg_id                 = it_aend_mc
*                             iv_msg_number             = '001'
*                             iv_add_to_response_header = abap_true ).
*    ENDIF.
  ENDMETHOD.
ENDCLASS.
