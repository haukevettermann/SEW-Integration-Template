class /SEW/CL_INT_IT_AEND definition
  public
  create public .

public section.

  constants IT_AEND_MC type CHAR20 value '/SEW/MC_INT_IT_AEND' ##NO_TEXT.
  data MSG_CONT type ref to /IWBEP/IF_MESSAGE_CONTAINER .
  constants STATUS_INITIAL type /SEW/DD_STATUS value '01' ##NO_TEXT.
  constants STATUS_ERROR type /SEW/DD_STATUS value '03' ##NO_TEXT.
  constants STATUS_SUCCESS type /SEW/DD_STATUS value '02' ##NO_TEXT.
  data AEND_ID type GUID_32 .
  data SAP_ID type HROBJID .
  data CLOUD_ID type HROBJID .
  data INT_RUN type GUID_32 .
  data MOLGA type MOLGA .
  data STATUS_HANDLER type ref to /SEW/CL_INT_STATUS_HANDLER .

  methods GET
    importing
      !DATES type RSDSSELOPT_T optional
      !STATUS type RSDSSELOPT_T optional
      !PERNR type RSDSSELOPT_T optional
      !INTRUN type RSDSSELOPT_T optional
    returning
      value(IT_AEND) type /SEW/TT_IT_AEND .
  methods GET_PERAS
    importing
      !BEGDA type BEGDA
      !ENDDA type ENDDA
    returning
      value(PERNR) type RSDSSELOPT_T .
  methods SAVE_ENTRIES
    importing
      !IT_AEND type /SEW/TT_IT_AEND optional
      !OM_AEND type /SEW/TT_OM_AEND optional
    returning
      value(IS_OK) type BOOLEAN .
  methods CONSTRUCTOR .
  methods PNNNN_TO_IT_AEND
    importing
      !PNNNN type ANY
    returning
      value(IT_AEND) type /SEW/INT_IT_AEND .
  methods PRELP_TO_IT_AEND
    importing
      !PRELP type /SEW/PRELP
    returning
      value(IT_AEND) type /SEW/INT_IT_AEND .
  methods POST_IT_AEND
    importing
      !SIMU type BOOLEAN
    exporting
      !IT_AEND_POST type /SEW/TT_IT_AEND
      !IT_AEND_ERROR type /SEW/TT_IT_AEND
      !IT_AEND_LOCKED type /SEW/TT_IT_AEND
    changing
      !IT_AEND type /SEW/TT_IT_AEND
      !MESSAGE_HANDLER type ref to CL_HRPAY00_MESSAGE_HANDLER .
  PROTECTED SECTION.
private section.

  methods CHECK_TIME_EVENT
    exporting
      !IS_OK type BOOLE_D
      !RETURN type BAPIRETURN1
    changing
      !IT_AEND type /SEW/INT_IT_AEND
      !EVENT type ANY .
  methods POST_OPER
    importing
      !SIMU_TMP type BOOLEAN
    changing
      !RETURN type BAPIRETURN1 optional
      !IT_AEND type /SEW/INT_IT_AEND
      !KEY type BAPIPAKEY optional .
  methods POST_TEVE
    importing
      !SIMU_TMP type BOOLEAN
    changing
      !RETURN type BAPIRETURN1 optional
      !IT_AEND type /SEW/INT_IT_AEND
      !KEY type BAPIPAKEY optional .
ENDCLASS.



CLASS /SEW/CL_INT_IT_AEND IMPLEMENTATION.


METHOD check_time_event.

  DATA: begda_ext   TYPE /sew/dd_date_ext,
*        ltime_ext   TYPE /sew/dd_time_ext,
        error_v     TYPE char19,
        error_hrpad TYPE hrpad_return.

*
  IF event IS NOT INITIAL.

    is_ok = abap_true.


    begda_ext = /sew/cl_int_utility=>get_external_date( date = it_aend-begda ).

    ASSIGN COMPONENT 'LTIME' OF STRUCTURE event TO FIELD-SYMBOL(<time>).
    IF <time> IS ASSIGNED.
*      ltime_ext = /sew/cl_int_utility=>get_external_time( time = <time> ).

*      CONCATENATE begda_ext ltime_ext INTO error_v SEPARATED BY space.
      error_v = begda_ext.

    ELSE.

      error_v = begda_ext.
    ENDIF.

    "check satza
    ASSIGN COMPONENT 'SATZA' OF STRUCTURE event TO FIELD-SYMBOL(<satza>).
    IF <satza> IS INITIAL.
      "in error table and skip
      it_aend-status = /sew/cl_int_constants=>booking_status-error.

      "create error message
      return = VALUE bapiret1( type = /sew/cl_int_constants=>error
                               id = /sew/cl_int_constants=>msg_class_int
                               number = /sew/cl_int_constants=>msg_no-m36
                               message_v1 = error_v
                               message_v2 = it_aend-pernr
                               message_v3 = it_aend-cloud_id
                               message_v4 = it_aend-int_run
                              ).


      is_ok = abap_false.
      RETURN.
    ENDIF.

    "check user2 -> do not book without user2
    ASSIGN COMPONENT 'USER2' OF STRUCTURE event TO FIELD-SYMBOL(<user2>).
    IF <user2> IS INITIAL.
      "in error table and skip
      it_aend-status = /sew/cl_int_constants=>booking_status-error.

      "create error message
      return = VALUE bapiret1( type = /sew/cl_int_constants=>error
                               id = /sew/cl_int_constants=>msg_class_int
                               number = /sew/cl_int_constants=>msg_no-m37
                               message_v1 = error_v
                               message_v2 = it_aend-pernr
                               message_v3 = it_aend-cloud_id
                               message_v4 = it_aend-int_run
                              ).

      is_ok = abap_false.
      RETURN.
    ENDIF.

    "set default values für erdat and ertim if one of both is initial.
    ASSIGN COMPONENT 'ERDAT' OF STRUCTURE event TO FIELD-SYMBOL(<erdat>).
    ASSIGN COMPONENT 'ERTIM' OF STRUCTURE event TO FIELD-SYMBOL(<ertim>).

    IF <erdat> IS INITIAL OR <ertim> IS INITIAL.

      ASSIGN COMPONENT 'LDATE' OF STRUCTURE event TO FIELD-SYMBOL(<ldate>).
      ASSIGN COMPONENT 'LTIME' OF STRUCTURE event TO FIELD-SYMBOL(<ltime>).

      <erdat> = <ldate>.
      <ertim> = <ltime>.

    ENDIF.

    "clear not used fields
    ASSIGN COMPONENT 'HRAZL' OF STRUCTURE event TO FIELD-SYMBOL(<hrazl>).
    IF <hrazl> IS NOT INITIAL.
      CLEAR <hrazl>.
    ENDIF.

    ASSIGN COMPONENT 'HRBET' OF STRUCTURE event TO FIELD-SYMBOL(<hrbet>).
    IF <hrbet> IS NOT INITIAL.
      CLEAR <hrbet>.
    ENDIF.

    ASSIGN COMPONENT 'BWGRL' OF STRUCTURE event TO FIELD-SYMBOL(<bwgrl>).
    IF <bwgrl> IS NOT INITIAL.
      CLEAR <bwgrl>.
    ENDIF.

    "check terminal is valid?
*        if <teven_i_tab>-terid
*        endif.

  ENDIF.

ENDMETHOD.


  METHOD constructor.

    msg_cont = /iwbep/cl_mgw_msg_container=>get_mgw_msg_container( ).

  ENDMETHOD.


  METHOD get.

    SELECT * FROM /sew/int_it_aend INTO TABLE it_aend WHERE pernr   IN pernr  AND
                                                            status  IN status AND
                                                            int_run IN intrun AND
                                                            ( begda IN dates OR
                                                              endda IN dates ) .

  ENDMETHOD.


  METHOD get_peras.

    SELECT DISTINCT pernr FROM /sew/int_it_aend INTO TABLE @DATA(lt_pernr) WHERE begda LE @endda AND
                                                                                 endda GE @begda.

    LOOP AT lt_pernr ASSIGNING FIELD-SYMBOL(<pernr>).
      APPEND VALUE rsdsselopt( sign = 'I' option = 'EQ' low = <pernr> ) TO pernr.
    ENDLOOP.

  ENDMETHOD.


  METHOD pnnnn_to_it_aend.
*!!!OBSOLETE: Do not use -> moved to /SEW/CL_INT_TYPE_CAST!!!
*
*    CHECK pnnnn IS NOT INITIAL.
*
*    cl_hr_pnnnn_type_cast=>pnnnn_to_prelp( EXPORTING pnnnn = pnnnn
*                                           IMPORTING prelp = DATA(prelp) ).
*
*    MOVE-CORRESPONDING prelp TO it_aend.

  ENDMETHOD.


  METHOD post_it_aend.

    DATA: pernr        TYPE pernr_d,
          it           TYPE REF TO data,
          return       TYPE bapireturn1,
          pernr_locked TYPE rsdsselopt_t,
          error_tab    TYPE hrpad_return_tab,
          error_hrpad  TYPE hrpad_return.

    FIELD-SYMBOLS: <struc>    TYPE any,
                   <infotype> TYPE any.

    LOOP AT it_aend ASSIGNING FIELD-SYMBOL(<it_aend>).

      "store current line for comparison
      me->status_handler->aend_id = <it_aend>-aend_id.
      me->status_handler->begda = <it_aend>-begda.
      me->status_handler->endda = <it_aend>-endda.
      me->status_handler->molga = <it_aend>-molga.
      me->status_handler->cloud_id = <it_aend>-cloud_id.
      me->status_handler->sap_id = <it_aend>-pernr.
      me->status_handler->int_run = <it_aend>-int_run.

      "if already an error for guid occurred go to next entry
      IF <it_aend>-pernr   IN pernr_locked AND
         pernr_locked       IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      "enqueue pernr
      IF pernr NE <it_aend>-pernr OR
         pernr IS INITIAL.

        "dequeue old pernr
        IF pernr IS NOT INITIAL.
          CALL FUNCTION 'HR_EMPLOYEE_DEQUEUE'
            EXPORTING
              number = pernr
            IMPORTING
              return = return.

          "check for enqueue pernr
          IF return-type EQ /sew/cl_int_constants=>error.
            me->status_handler->add_log_message( aend_id = <it_aend>-aend_id bapiret1 = return ).
            CLEAR: pernr, return.
          ENDIF.
        ENDIF.

        CALL FUNCTION 'HR_EMPLOYEE_ENQUEUE'
          EXPORTING
            number = <it_aend>-pernr
          IMPORTING
            return = return.

        "check for enqueue pernr
        IF return-type EQ /sew/cl_int_constants=>error.
          me->status_handler->add_log_message( aend_id = <it_aend>-aend_id bapiret1 = return ).
          CLEAR: return.

          APPEND VALUE #( sign = 'I' option = 'EQ' low = <it_aend>-pernr ) TO pernr_locked.
*            APPEND VALUE #( sign = 'I' option = 'EQ' low = <it_aend>-cloud_id ) TO pernr_locked.
*          count = max.
          CONTINUE.
        ENDIF.

        pernr = <it_aend>-pernr.
      ENDIF.

**--------------------------------------------------------------------*
**   Start infotype processing
**   Split in sub routines because of different infotype handling
**--------------------------------------------------------------------*
      DATA(method) = SWITCH char9( <it_aend>-infty
        WHEN '2011' THEN 'POST_TEVE'
        WHEN '2001' THEN 'POST_OPER'
        WHEN '2002' THEN 'POST_OPER'
        WHEN '2006' THEN 'POST_OPER'
        WHEN '2006' THEN 'POST_OPER' ).

      "post
      CALL METHOD (method)
        EXPORTING
          simu_tmp = simu
        CHANGING
          return   = return
          it_aend  = <it_aend>.

      "check for operation errors
      IF return-type EQ 'E'.
        error_hrpad = CORRESPONDING #( return ).
        APPEND error_hrpad TO error_tab.
        me->status_handler->add_log_message( aend_id = <it_aend>-aend_id
                                             bapiret1 = return
                                             hrpad_return = error_tab ).

        CLEAR: return, error_hrpad, error_tab.

        "set error status
        <it_aend>-status = status_error.
        APPEND <it_aend> TO it_aend_error.

        ROLLBACK WORK.
      ELSE.

        "mark entry as successful
        <it_aend>-status = /sew/cl_int_constants=>booking_status-success.

        APPEND <it_aend> TO it_aend_post.

        "do commit only in case simulation is inactive
        CHECK simu EQ abap_false.
        COMMIT WORK.
      ENDIF.

    ENDLOOP.

    "dequeue last pernr
    IF pernr IS NOT INITIAL.
      CALL FUNCTION 'HR_EMPLOYEE_DEQUEUE'
        EXPORTING
          number = pernr
        IMPORTING
          return = return.

      "check for enqueue pernr
      IF return-type EQ /sew/cl_int_constants=>error.
        me->status_handler->add_log_message( aend_id = <it_aend>-aend_id bapiret1 = return ).
        CLEAR: pernr, return.
      ENDIF.
    ENDIF.

  ENDMETHOD.


METHOD post_oper.

  DATA: it          TYPE REF TO data,
        prelp       TYPE prelp.

  FIELD-SYMBOLS: <struk>    TYPE any,
                 <infotype> TYPE any.


  "create infotype structure
  CONCATENATE 'P' it_aend-infty INTO DATA(it_name).
  CREATE DATA it TYPE (it_name).
  ASSIGN it->* TO <struk>.

  IF <struk> IS ASSIGNED.

    "get infotype data
    MOVE-CORRESPONDING it_aend TO prelp.
    cl_hr_pnnnn_type_cast=>prelp_to_pnnnn(
      EXPORTING
        prelp = prelp
      IMPORTING
        pnnnn = <struk> ).

    "perform infotype operation
    CALL FUNCTION 'HR_INFOTYPE_OPERATION'
      EXPORTING
        infty         = it_aend-infty
        recordnumber  = it_aend-seqnr
        number        = it_aend-pernr
        subtype       = it_aend-subty
        validityend   = it_aend-endda
        validitybegin = it_aend-begda
        record        = <struk>
        operation     = it_aend-operation
        nocommit      = simu_tmp
      IMPORTING
        return        = return
        key           = key.
  ENDIF.

ENDMETHOD.


METHOD post_teve.

  DATA: it               TYPE REF TO data,
        prelp            TYPE prelp,
        teven_i_tab      TYPE tteven,
        teven_u_tab      TYPE tteven,
        teven_more_i_tab TYPE tteven_more,
        teven_more_u_tab TYPE tteven_more,
        begda_ext        TYPE /sew/dd_date_ext,
*        ltime_ext        TYPE /sew/dd_time_ext,
        error_v          TYPE char19.

  FIELD-SYMBOLS: <struc>            TYPE any,
                 <infotype>         TYPE any,
                 <teven_tmp>        TYPE teven,
                 <teven_more_u_tab> TYPE teven_more.


  "create infotype structure
  CONCATENATE 'P' it_aend-infty INTO DATA(it_name).
  CREATE DATA it TYPE (it_name).
  ASSIGN it->* TO <struc>.

  IF <struc> IS ASSIGNED.

    "get infotype data
    MOVE-CORRESPONDING it_aend TO prelp.
    cl_hr_pnnnn_type_cast=>prelp_to_pnnnn(
      EXPORTING
        prelp = prelp
      IMPORTING
        pnnnn = <struc> ).


    "do time event related checks regarding
    me->check_time_event( IMPORTING is_ok = DATA(is_ok)
                                    return = return
                          CHANGING  event = <struc>
                                    it_aend = it_aend ).

    IF is_ok EQ abap_false.
      "set error status for current entry
      it_aend-status = /sew/cl_int_constants=>booking_status-error.

      RETURN.
    ENDIF.


    "check of there entries with same event_id from oracle, stored in user2
    ASSIGN COMPONENT /sew/cl_int_constants=>user2 OF STRUCTURE <struc> TO FIELD-SYMBOL(<user2>).
    IF <user2> IS ASSIGNED.
      DATA(user2_range) = VALUE rsdsselopt_t( sign = 'I' option = 'EQ' ( low = <user2> ) ).

      SELECT * FROM teven INTO TABLE @DATA(teven_tmp) WHERE user2 IN @user2_range
                                                        AND stokz NE @abap_true.
    ENDIF.

    "new entry or modify
    IF it_aend-operation = /sew/cl_int_constants=>infty_operation-pa_insert OR
       it_aend-operation = /sew/cl_int_constants=>infty_operation-pa_modify.

      "append current entry to insert table
      APPEND INITIAL LINE TO teven_i_tab ASSIGNING FIELD-SYMBOL(<teven_i_tab>).
      MOVE-CORRESPONDING <struc> TO <teven_i_tab>.

      "fill pdsnr number
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          object                  = 'PD_SEQ_NR'
          quantity                = '00000000000000000001'
        IMPORTING
          number                  = <teven_i_tab>-pdsnr
*         quantity                = qty
*         returncode              = returncode
        EXCEPTIONS "#EC
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.

      IF sy-subrc <> 0.
        begda_ext = /sew/cl_int_utility=>get_external_date( date = it_aend-begda ).

        return = VALUE bapiret1( type = /sew/cl_int_constants=>error
                                 id = /sew/cl_int_constants=>msg_class_int
                                 number = /sew/cl_int_constants=>msg_no-m38
                                 message_v1 = begda_ext
                                 message_v2 = it_aend-pernr
                                 message_v3 = it_aend-cloud_id
                                 message_v4 = it_aend-int_run
                               ).

        "set error status for current entry
        it_aend-status = /sew/cl_int_constants=>booking_status-error.

        "remove the entry from insert table.
        DELETE teven_i_tab FROM <teven_i_tab>.

        RETURN.
      ENDIF.

      "Fill more tab
      APPEND INITIAL LINE TO teven_more_i_tab ASSIGNING FIELD-SYMBOL(<teven_more_i_tab>).
      MOVE-CORRESPONDING <teven_i_tab> TO <teven_more_i_tab>.

      "Cancel existing entries -> Get entries with same USER2 and set STOKZ = 'X'
      LOOP AT teven_tmp ASSIGNING <teven_tmp>.
        <teven_tmp>-stokz = abap_true.
        APPEND <teven_tmp> TO teven_u_tab.

        "also fill more tab
        APPEND INITIAL LINE TO teven_more_u_tab ASSIGNING <teven_more_u_tab>.
        MOVE-CORRESPONDING <teven_tmp> TO <teven_more_u_tab>.
      ENDLOOP.

      "Delte only
    ELSEIF it_aend-operation = /sew/cl_int_constants=>infty_operation-pa_delete.

      IF teven_tmp IS INITIAL.
        "cannot find the corresponding entry
        begda_ext = /sew/cl_int_utility=>get_external_date( date = it_aend-begda ).


        ASSIGN COMPONENT 'LTIME' OF STRUCTURE <struc> TO FIELD-SYMBOL(<ltime>).
        IF <ltime> IS ASSIGNED.
*          ltime_ext = /sew/cl_int_utility=>get_external_time( time = <ltime> ).

*          CONCATENATE begda_ext ltime_ext INTO error_v SEPARATED BY space.
          error_v = begda_ext.
        ELSE.

          error_v = begda_ext.
        ENDIF.


        return = VALUE bapiret1( type = /sew/cl_int_constants=>error
                                id = /sew/cl_int_constants=>msg_class_int
                                number = /sew/cl_int_constants=>msg_no-m39
                                message_v1 = error_v
                                message_v2 = it_aend-pernr
                                message_v3 = it_aend-cloud_id
                                message_v4 = it_aend-int_run
                              ).

        "set error status for current entry
        it_aend-status = /sew/cl_int_constants=>booking_status-error.
        RETURN.
      ELSE.

        LOOP AT teven_tmp ASSIGNING <teven_tmp>.
          <teven_tmp>-stokz = abap_true.
          APPEND <teven_tmp> TO teven_u_tab.

          "also fill more tab
          APPEND INITIAL LINE TO teven_more_u_tab ASSIGNING <teven_more_u_tab>.
          MOVE-CORRESPONDING <teven_tmp> TO <teven_more_u_tab>.
        ENDLOOP.
      ENDIF.

    ENDIF.

* OBSOELTE
*    "Cancel existing entries -> Get entries with same USER2 and set STOKZ = 'X'
*    ASSIGN COMPONENT /sew/cl_int_constants=>user2 OF STRUCTURE <struc> TO FIELD-SYMBOL(<user2>).
*    IF <user2> IS ASSIGNED.
*      DATA(user2_range) = VALUE rsdsselopt_t( sign = 'I' option = 'EQ' ( low = <user2> ) ).
*
*      SELECT * FROM teven INTO TABLE @DATA(teven_tmp) WHERE user2 IN @user2_range
*                                                        AND stokz NE @abap_true.
*      LOOP AT teven_tmp ASSIGNING FIELD-SYMBOL(<teven_tmp>).
*        <teven_tmp>-stokz = abap_true.
*        APPEND <teven_tmp> TO teven_u_tab.
*
*        "also fill more tab
*        APPEND INITIAL LINE TO teven_more_u_tab ASSIGNING FIELD-SYMBOL(<teven_more_u_tab>).
*        MOVE-CORRESPONDING <teven_tmp> TO <teven_more_u_tab>.
*      ENDLOOP.
*    ENDIF.

    CALL FUNCTION 'HR_TMW_DB_UPDATE_TEVENT'
      TABLES
        del_teven      = teven_u_tab
        ins_teven      = teven_i_tab
        del_teven_more = teven_more_u_tab
        ins_teven_more = teven_more_i_tab
      EXCEPTIONS
        insert_failed  = 1
        update_failed  = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.

      return = VALUE bapiret1( type = /sew/cl_int_constants=>error
                                id = /sew/cl_int_constants=>msg_class_int
                                number = /sew/cl_int_constants=>msg_no-m32
                                message_v1 = it_aend-infty
                                message_v2 = it_aend-pernr
                                message_v3 = it_aend-int_run
                              ).

      "set error status for current entry
      it_aend-status = /sew/cl_int_constants=>booking_status-error.

    ELSE.

      "mark entry as successful
      it_aend-status = /sew/cl_int_constants=>booking_status-success.
    ENDIF.


  ENDIF.

ENDMETHOD.


  METHOD prelp_to_it_aend.
*!!!OBSOLETE: Do not use -> moved to /SEW/CL_INT_TYPE_CAST!!!
*
*    it_aend = CORRESPONDING #( prelp ).
*    me->enhance_entries( CHANGING it_aend = it_aend ).
  ENDMETHOD.


  METHOD save_entries.
* !!!OBSOLETE: Do not use -> moved to /SEW/CL_INT_STATUS_HANDLER -> PERSTIST_DATA!!!
*
*    IF it_aend IS SUPPLIED AND it_aend IS NOT INITIAL.
*      MODIFY /sew/int_it_aend FROM TABLE it_aend.
*
*      is_ok = abap_true.
*      IF sy-subrc NE 0.
*        is_ok = abap_false.
*
*        msg_cont->add_message( iv_msg_type               = /iwbep/cl_cos_logger=>error
*                               iv_msg_id                 = it_aend_mc
*                               iv_msg_number             = '001'
*                               iv_add_to_response_header = abap_true ).
*      ENDIF.
*    ENDIF.
*    IF om_aend IS SUPPLIED AND om_aend IS NOT INITIAL.
*      MODIFY /sew/int_it_aend FROM TABLE it_aend.
*
*      is_ok = abap_true.
*      IF sy-subrc NE 0.
*        is_ok = abap_false.
*
*        msg_cont->add_message( iv_msg_type               = /iwbep/cl_cos_logger=>error
*                               iv_msg_id                 = it_aend_mc
*                               iv_msg_number             = '001'
*                               iv_add_to_response_header = abap_true ).
*      ENDIF.
*    ENDIF.
  ENDMETHOD.
ENDCLASS.
