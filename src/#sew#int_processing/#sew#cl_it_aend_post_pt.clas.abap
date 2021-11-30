class /SEW/CL_IT_AEND_POST_PT definition
  public
  create public .

public section.

  types:
    BEGIN OF ty_field,
        infty type infty,
        subty type subty,
        field  TYPE char50,
        field_old TYPE char50,
        field_new TYPE char50,
        field4 TYPE char50,
      END OF ty_field .
  types:
    ty_fields TYPE TABLE OF ty_field .

  data DATES type RSDSSELOPT_T .
  data STATUS type RSDSSELOPT_T .
  data IT_AEND type /SEW/TT_IT_AEND .
  data IT_AEND_ERROR type /SEW/TT_IT_AEND .
  data IT_AEND_POST type /SEW/TT_IT_AEND .
  data IT_AEND_NOCHANGE type /SEW/TT_IT_AEND .
  data MESSAGE_HANDLER type ref to CL_HRPAY00_MESSAGE_HANDLER .
  data SKIPPED_PERNRS type HRAHQ_PERNR_TABLE .
  data SIMU type BOOLEAN .
  data STATUS_HANDLER type ref to /SEW/CL_INT_STATUS_HANDLER .
  data IT_AEND_LOCKED type /SEW/TT_IT_AEND .

  methods SET_DATE_RANGE
    importing
      !DATES type RSDSSELOPT_T .
  methods IT_SPECIFIC_LOGIC
    importing
      !SIMU type BOOLE_D
      !EXEC_OPTION type STRING
      !MOLGA type MOLGA
      !INFTY type INFTY
      !SUBTY type SUBTY
      !MASSN type MASSN
      !CHANGED_FIELDS type TY_FIELDS
    exporting
      !RETURN type BAPIRET1
      !RETURN_TAB type HRPAD_RETURN_TAB
    changing
      !RECORD type ANY .
  methods UPDATE_STATUS
    importing
      !SIMU type BOOLE_D .
  methods CHECK_STATUS_RANGE
    importing
      !INITIAL type BOOLEAN
      !ERROR type BOOLEAN .
  methods GET_IT_AEND_DATA
    importing
      !PERNR type RSDSSELOPT_T
      !INTRUN type RSDSSELOPT_T
      !DATES type RSDSSELOPT_T
      !STATUS type RSDSSELOPT_T
    returning
      value(IT_AEND) type /SEW/TT_IT_AEND .
  methods PREPARE_PROTOCOL .
  methods START_POST .
  methods CONSTRUCTOR
    importing
      !ALV type BOOLEAN
      !BATCH type BOOLEAN
      !SIMU type BOOLEAN
      !DATES type RSDSSELOPT_T .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS /SEW/CL_IT_AEND_POST_PT IMPLEMENTATION.


  METHOD check_status_range.

    "Default both status
    status = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = /sew/cl_int_it_aend=>status_error )
                                 ( sign = 'I' option = 'EQ' low = /sew/cl_int_it_aend=>status_initial ) ).

    "In case only initial requests should be posted
    IF initial EQ abap_true.
      status = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = /sew/cl_int_it_aend=>status_initial ) ).
    ENDIF.

    "In case only failed requests should be posted
    IF error EQ abap_true.
      status = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = /sew/cl_int_it_aend=>status_error ) ).
    ENDIF.

  ENDMETHOD.


  METHOD CONSTRUCTOR.

    me->simu  = simu.
    me->dates = dates.

    "prepare protocol in case dialog is active and ALV is requested
    IF alv   EQ abap_true AND
       batch EQ abap_false.
      message_handler = cl_hrpay00_message_handler=>get_message_handler( ).
    ENDIF.

  ENDMETHOD.


  METHOD GET_IT_AEND_DATA.

    "get infotype changes
    it_aend = NEW /sew/cl_int_it_aend( )->get( pernr  = pernr
                                               intrun = intrun
                                               dates  = dates
                                               status = status ).

    CHECK it_aend IS NOT INITIAL.

    SORT it_aend BY pernr timestamp infty ASCENDING.

  ENDMETHOD.


  METHOD IT_SPECIFIC_LOGIC.

    ASSIGN COMPONENT /sew/cl_int_constants=>pernr OF STRUCTURE record TO FIELD-SYMBOL(<pernr>).
    ASSIGN COMPONENT /sew/cl_int_constants=>begda OF STRUCTURE record TO FIELD-SYMBOL(<begda>).
    ASSIGN COMPONENT /sew/cl_int_constants=>endda OF STRUCTURE record TO FIELD-SYMBOL(<endda>).

    IF exec_option = /sew/cl_int_constants=>before.
      /sew/cl_int_infotypes=>process_it_specifics_before_op(
      EXPORTING
         simu           = simu
         massn          = massn
         molga          = molga
         pernr          = <pernr>
         begda          = <begda>
         endda          = <endda>
         infty          = infty
         subty          = subty
         changed_fields = changed_fields
      IMPORTING
         return         = return
         return_tab     = return_tab
      CHANGING
         record         = record ).
    ELSE.
      /sew/cl_int_infotypes=>process_it_specifics_after_op(
      EXPORTING
         simu           = simu
         massn          = massn
         molga          = molga
         pernr          = <pernr>
         begda          = <begda>
         endda          = <endda>
         infty          = infty
         subty          = subty
         changed_fields = changed_fields
      IMPORTING
         return         = return
         return_tab     = return_tab
      CHANGING
         record         = record ).
    ENDIF.
  ENDMETHOD.


  METHOD PREPARE_PROTOCOL.

    "Check message handler
    IF message_handler IS BOUND.

      "prepare layout options
      DATA(layout) = VALUE slis_layout_alv( zebra = 'X'
                                               colwidth_optimize = 'X' ).

      DATA: root_node TYPE hrpad_pal_node_key VALUE 'ROOT'.

      "Define structure of successful and failed IT_AEND entries ALV table
      message_handler->if_hrpay00_pal_services~create_fcat( EXPORTING i_structure_name = '/SEW/INT_IT_AEND'
                                                            IMPORTING et_fcat          = DATA(fcat_it_aend) ).

      "add post table
*      IF it_aend_post IS NOT INITIAL.
*        message_handler->if_hrpay00_pal_services~add_table(
*          EXPORTING
*            i_parent_node_key = root_node
*            it_fcat           = fcat_it_aend
*            it_append_table   = it_aend_post
*            is_layout         = layout
*            i_node_txt        = COND string( WHEN simu EQ abap_true
*                                             THEN TEXT-005
*                                             ELSE TEXT-001 ) ).
*      ENDIF.
*
*      "add error table
*      IF it_aend_error IS NOT INITIAL.
*        message_handler->if_hrpay00_pal_services~add_table(
*          EXPORTING
*            i_parent_node_key = root_node
*            it_fcat           = fcat_it_aend
*            it_append_table   = it_aend_error
*            is_layout         = layout
*            i_node_txt        = COND string( WHEN simu EQ abap_true
*                                             THEN TEXT-006
*                                             ELSE TEXT-002 ) ).
*      ENDIF.
        message_handler->if_hrpay00_pal_services~add_table(
          EXPORTING
            i_parent_node_key = root_node
            it_fcat           = fcat_it_aend
            it_append_table   = me->status_handler->new_it_aend
            is_layout         = layout
            i_node_txt        = COND string( WHEN simu EQ abap_true
                                             THEN TEXT-009
                                             ELSE TEXT-010 ) ).
      "add skipped pernr's
      IF skipped_pernrs IS NOT INITIAL.
        message_handler->if_hrpay00_pal_services~add_table(
        EXPORTING
          i_parent_node_key = root_node
          it_append_table   = skipped_pernrs
          is_layout         = layout
          i_node_txt        = COND string( WHEN simu EQ abap_true
                                             THEN TEXT-008
                                             ELSE TEXT-004 ) ).
      ENDIF.

      "add message table
      message_handler->if_hrpay00_pal_services~add_messages(
        EXPORTING
          i_parent_node_key = root_node
      ).

      "show pals
      CALL METHOD message_handler->display_pal.

    ENDIF.

  ENDMETHOD.


  METHOD set_date_range.

    me->dates = dates.

  ENDMETHOD.


  METHOD start_post.

    "call post for all infotypes, except 2011 due to special handling
    DATA(cl_int_it_aend) = NEW /sew/cl_int_it_aend( ).

    "passing the instance
    cl_int_it_aend->status_handler = me->status_handler.

    cl_int_it_aend->post_it_aend(
      EXPORTING
        simu            = simu
      IMPORTING
        it_aend_post    = it_aend_post
        it_aend_error   = it_aend_error
        it_aend_locked  = it_aend_locked
      CHANGING
        message_handler = message_handler
        it_aend         = me->status_handler->it_aend
    ).

    "puffer status update
    me->update_status( EXPORTING simu = simu ).

    "persist status update
    DATA(is_ok) = me->status_handler->persist_data( source = /sew/cl_int_constants=>poster simu = simu ).

    "Check simulation status and rollback if needed
    IF simu  EQ abap_true OR
       is_ok EQ abap_false.
      ROLLBACK WORK.
    ENDIF.

  ENDMETHOD.


  METHOD UPDATE_STATUS.

    "Update Success
    LOOP AT me->it_aend_post ASSIGNING FIELD-SYMBOL(<it_aend_post>).
      me->status_handler->set_status( aend_id = <it_aend_post>-aend_id status = /sew/cl_int_constants=>booking_status-success ).
    ENDLOOP.
    "Update Error
    LOOP AT me->it_aend_error ASSIGNING FIELD-SYMBOL(<it_aend_error>).
      me->status_handler->set_status( aend_id = <it_aend_error>-aend_id status = /sew/cl_int_constants=>booking_status-error ).
    ENDLOOP.
    "Update No Change
    LOOP AT me->it_aend_nochange ASSIGNING FIELD-SYMBOL(<it_aend_nochange>).
      me->status_handler->set_status( aend_id = <it_aend_nochange>-aend_id status = /sew/cl_int_constants=>booking_status-nochange ).
    ENDLOOP.
    "Update Locked
    LOOP AT me->it_aend_locked ASSIGNING FIELD-SYMBOL(<it_aend_locked>).
      me->status_handler->set_status( aend_id = <it_aend_locked>-aend_id status = /sew/cl_int_constants=>booking_status-pernr_locked ).
    ENDLOOP.

    "Set simulated status if simulation run
    IF simu = abap_true.
      me->status_handler->simu_it_aend = me->status_handler->new_it_aend.
      LOOP AT me->status_handler->simu_it_aend ASSIGNING FIELD-SYMBOL(<simu_it_aend>).
        IF <simu_it_aend>-action = /sew/cl_int_constants=>hire.
          CLEAR <simu_it_aend>-pernr.
        ENDIF.
        <simu_it_aend>-status = /sew/cl_int_constants=>booking_status-simulated.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
