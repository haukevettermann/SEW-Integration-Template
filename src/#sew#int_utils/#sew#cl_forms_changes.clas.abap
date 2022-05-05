class /SEW/CL_FORMS_CHANGES definition
  public
  create public .

public section.

  data MESSAGE_HANDLER type ref to CL_HRPAY00_MESSAGE_HANDLER .
  data SKIPPED_PERNRS type HRAHQ_PERNR_TABLE .
  data DATES type RSDSSELOPT_T .
  data STATUS type RSDSSELOPT_T .
  data SIMU type BOOLEAN .
  data INT_FO_AEUP type /SEW/TT_FO_AEUP .
  data STATUS_HANDLER type ref to /SEW/CL_INT_STATUS_HANDLER .
  data MSG_CONT type ref to /IWBEP/IF_MESSAGE_CONTAINER .

  methods DETECT_TIME_CHANGES
    importing
      !IR_PERNR type RSDSSELOPT_T
      !PERIODS type /SEW/CL_FORMS_UTILS=>PERIODS_T
    exporting
      !STATUS type /SEW/DD_STATUS .
  methods CREATE_FO_AEUP_ENTRIES
    importing
      !RESULT type /SEW/TIME_PAY_RESULTS
      !PERNR type PERNR_D
      !ORACLEPERNR type /SEW/DD_OBJECTNUMBER
      !AEDTM type AEDTM
      !OTYPE type CHAR1
    exporting
      !FO_AEUP type /SEW/INT_FO_AEUP .
  methods CONSTRUCTOR
    importing
      !ALV type BOOLEAN
      !BATCH type BOOLEAN
      !SIMU type BOOLEAN
      !DATES type RSDSSELOPT_T .
  methods PREPARE_PROTOCOL .
  methods GET
    importing
      !DATES type RSDSSELOPT_T
      !STATUS type RSDSSELOPT_T
      !PERNR type RSDSSELOPT_T
      !INTRUN type RSDSSELOPT_T
    returning
      value(FO_AEUP) type /SEW/TT_FO_AEUP .
  methods DETECT_CHANGES
    importing
      !IR_PERNR type RSDSSELOPT_T
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !PA_FTIM type BOOLEAN
      !PA_FPAY type BOOLEAN
      !PA_FBOTH type BOOLEAN
      !PA_FORCE type BOOLEAN
    exporting
      !FO_AEUP type /SEW/TT_FO_AEUP .
protected section.
private section.

  methods GET_REM_STATEMENTS
    importing
      !IR_PERNR type RSDSSELOPT_T
      !PERIODS type /SEW/CL_FORMS_UTILS=>PERIODS_T
    exporting
      !STATUS type /SEW/DD_STATUS .
  methods GET_TIME_STATEMENTS
    importing
      !IR_PERNR type RSDSSELOPT_T
      !PERIODS type /SEW/CL_FORMS_UTILS=>PERIODS_T
    exporting
      value(STATUS) type /SEW/DD_STATUS .
  methods SAVE_ENTRIES
    importing
      !FO_AEUP type /SEW/TT_FO_AEUP
    returning
      value(IS_OK) type BOOLEAN .
  methods DETECT_REM_CHANGES
    importing
      !IR_PERNR type RSDSSELOPT_T
      !PERIODS type /SEW/CL_FORMS_UTILS=>PERIODS_T
    exporting
      !STATUS type /SEW/DD_STATUS .
ENDCLASS.



CLASS /SEW/CL_FORMS_CHANGES IMPLEMENTATION.


  METHOD constructor.

    me->simu     = simu.
    me->dates    = dates.
    msg_cont     = /iwbep/cl_mgw_msg_container=>get_mgw_msg_container( ).
    "prepare protocol in case dialog is active and ALV is requested
    IF alv   EQ abap_true AND
       batch EQ abap_false.
      message_handler = cl_hrpay00_message_handler=>get_message_handler( ).
    ENDIF.

  ENDMETHOD.


  METHOD create_fo_aeup_entries.
    DATA: fo_aeup_line TYPE /sew/int_fo_aeup.


    fo_aeup_line-mandt        = sy-mandt.

    fo_aeup_line-pernr        = pernr.
    fo_aeup_line-oraclepernr  = oraclepernr.

    fo_aeup_line-otype        = otype.
    fo_aeup_line-endda        = /sew/cl_forms_utils=>convert_date_oracle_to_sap( result-date_to   ).
    fo_aeup_line-begda        = /sew/cl_forms_utils=>convert_date_oracle_to_sap( result-date_from ).
    fo_aeup_line-status       = result-status.
    fo_aeup_line-xstring      = result-xstring.
    fo_aeup_line-aedtm        = aedtm.
    GET TIME STAMP FIELD fo_aeup_line-timestamp.

    fo_aeup = fo_aeup_line.
    CLEAR  fo_aeup_line.


  ENDMETHOD.


METHOD detect_changes.

  DATA: pernr        TYPE pernr_d,
        cluster_b2   TYPE hrf_tim_b2,
        periods      TYPE /sew/cl_forms_utils=>periods_t,
        aedtm        TYPE aedtm,
        period       TYPE char6,
        tmp_results  TYPE /sew/cl_forms_utils=>time_pay_results_t,
        results      TYPE /sew/cl_forms_utils=>time_pay_results_t,
        oracle_id    TYPE /sew/dd_element,
        fo_aeup_line TYPE /sew/int_fo_aeup,
        fo_aeup_mod  TYPE TABLE OF /sew/int_fo_aeup,
        fo_aeup_tmp  TYPE TABLE OF /sew/int_fo_aeup,
        length       TYPE i,
        status       TYPE /sew/dd_status,
        l_endda      TYPE endda,
        l_begda      TYPE begda.

  IF endda >= sy-datum.
    l_endda = sy-datum.
  ELSE.
    l_endda = endda.
  ENDIF.

  IF begda >= sy-datum.

  ELSE.
    l_begda = begda.
  ENDIF.


  CALL METHOD /sew/cl_forms_utils=>get_periods
    EXPORTING
      begda   = l_begda
      endda   = l_endda
    IMPORTING
      periods = periods.

  IF pa_force EQ 'X'.

    CASE 'X'.
      WHEN pa_ftim.

        CALL METHOD get_time_statements
          EXPORTING
            ir_pernr = ir_pernr
            periods  = periods
          IMPORTING
            status   = status.

      WHEN pa_fpay.

        CALL METHOD get_rem_statements
          EXPORTING
            ir_pernr = ir_pernr
            periods  = periods
          IMPORTING
            status   = status.

      WHEN pa_fboth.

        CALL METHOD get_time_statements
          EXPORTING
            ir_pernr = ir_pernr
            periods  = periods
          IMPORTING
            status   = status.

        CALL METHOD get_rem_statements
          EXPORTING
            ir_pernr = ir_pernr
            periods  = periods
          IMPORTING
            status   = status.

    ENDCASE.

  ELSE.

    CASE 'X'.

      WHEN pa_ftim.

        CALL METHOD detect_time_changes
          EXPORTING
            ir_pernr = ir_pernr
            periods  = periods
          IMPORTING
            status   = status.

      WHEN pa_fpay.

        CALL METHOD detect_rem_changes
          EXPORTING
            ir_pernr = ir_pernr
            periods  = periods
          IMPORTING
            status   = status.

      WHEN pa_fboth.

        CALL METHOD detect_rem_changes
          EXPORTING
            ir_pernr = ir_pernr
            periods  = periods
          IMPORTING
            status   = status.

        CALL METHOD detect_time_changes
          EXPORTING
            ir_pernr = ir_pernr
            periods  = periods
          IMPORTING
            status   = status.

    ENDCASE.

  ENDIF.

ENDMETHOD.


  METHOD detect_rem_changes.
    DATA: pernr        TYPE pernr_d,
          oraclepernr  TYPE /sew/dd_objectnumber,
          period       TYPE char6,
          aedtm        TYPE aedtm,
          tmp_result   TYPE /sew/time_pay_results,
          fo_aeup      TYPE TABLE OF /sew/int_fo_aeup,
          fo_aeup_line TYPE /sew/int_fo_aeup,
          is_ok        TYPE flag.

    LOOP AT ir_pernr ASSIGNING FIELD-SYMBOL(<pernr>).

      pernr     = CONV #( <pernr>-low ).
      oraclepernr = /sew/cl_forms_utils=>get_oraclepernr( pernr ).

      LOOP AT periods ASSIGNING FIELD-SYMBOL(<period>).

        DATA(period_range) = VALUE rsdsselopt_t( ( sign    = 'I'
                                                   option  = 'BT'
                                                   low     = <period>-datuv
                                                   high    = <period>-datub ) ).

        SELECT * FROM /sew/int_fo_aeup WHERE pernr =  @pernr
                                       AND   begda IN @period_range
                                       AND   endda IN @period_range
                                       AND   otype =  @/sew/cl_forms_utils=>rem_statement
                                       INTO  TABLE    @DATA(int_fo_aeup).

        period = CONV #( <period>-datuv+0(6) ).
        aedtm  = /sew/cl_rem_changes=>get_change_date(  pernr  = pernr
                                                        begda  = <period>-datuv
                                                        endda  = <period>-datub ).

        IF int_fo_aeup IS INITIAL.
          CALL METHOD /sew/cl_forms_utils=>get_rem_pdf_data
            EXPORTING
              begda       = <period>-datuv
              endda       = <period>-datub
              pernr       = pernr
              oraclepernr = oraclepernr
            IMPORTING
              payresult   = tmp_result.

          CHECK tmp_result IS NOT INITIAL.
          CHECK aedtm      IS NOT INITIAL.

          CALL METHOD create_fo_aeup_entries
            EXPORTING
              result      = tmp_result
              pernr       = pernr
              oraclepernr = oraclepernr
              aedtm       = aedtm
              otype       = /sew/cl_forms_utils=>rem_statement
            IMPORTING
              fo_aeup     = fo_aeup_line. " create initial records

          APPEND fo_aeup_line TO fo_aeup.

        ELSE.

          READ TABLE int_fo_aeup WITH KEY pernr = pernr
                                          begda = <period>-datuv
                                          endda = <period>-datub
                                          INTO DATA(fo_aeup_old).

          CHECK sy-subrc EQ 0. " sy-subrc NE 0 should never appear.


          period = CONV #( <period>-datuv+0(6) ).
          aedtm  = /sew/cl_rem_changes=>get_change_date(  pernr  = pernr
                                                          begda  = <period>-datuv
                                                          endda  = <period>-datub ).

          oraclepernr = /sew/cl_forms_utils=>get_oraclepernr( pernr ).

          IF fo_aeup_old-aedtm NE aedtm.

            CALL METHOD /sew/cl_forms_utils=>get_rem_pdf_data
              EXPORTING
                begda       = fo_aeup_old-begda
                endda       = fo_aeup_old-endda
                pernr       = pernr
                oraclepernr = oraclepernr
              IMPORTING
                payresult   = tmp_result.

            CHECK tmp_result IS NOT INITIAL.
            CHECK aedtm      IS NOT INITIAL.

            CALL METHOD create_fo_aeup_entries
              EXPORTING
                result      = tmp_result
                pernr       = pernr
                oraclepernr = oraclepernr
                aedtm       = aedtm
                otype       = /sew/cl_forms_utils=>rem_statement
              IMPORTING
                fo_aeup     = fo_aeup_line.

            APPEND fo_aeup_line TO fo_aeup.

          ENDIF.


        ENDIF.
        CLEAR int_fo_aeup.
      ENDLOOP.


      APPEND LINES OF fo_aeup TO me->int_fo_aeup.

      CALL METHOD save_entries
        EXPORTING
          fo_aeup = fo_aeup
        RECEIVING
          is_ok   = is_ok.

      IF is_ok EQ abap_false.
        status = '03'.
      ELSE.
        status = '02'.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD detect_time_changes.
    DATA: pernr        TYPE pernr_d,
          oraclepernr  TYPE /sew/dd_objectnumber,
          period       TYPE char6,
          aedtm        TYPE aedtm,
          tmp_result   TYPE  /sew/time_pay_results,
          fo_aeup      TYPE TABLE OF /sew/int_fo_aeup,
          fo_aeup_line TYPE /sew/int_fo_aeup,
          is_ok        TYPE flag.


    LOOP AT ir_pernr ASSIGNING FIELD-SYMBOL(<pernr>).
      pernr = CONV #( <pernr>-low ).
      oraclepernr = /sew/cl_forms_utils=>get_oraclepernr( pernr ).

      LOOP AT periods ASSIGNING FIELD-SYMBOL(<period>).
        DATA(period_range) = VALUE rsdsselopt_t( ( sign    = 'I'
                                                   option  = 'BT'
                                                   low     = <period>-datuv
                                                   high    = <period>-datub ) ).
        SELECT * FROM /sew/int_fo_aeup WHERE pernr =  @pernr
                                       AND   begda IN @period_range
                                       AND   endda IN @period_range
                                       AND   otype =  @/sew/cl_forms_utils=>time_statement
                                       INTO  TABLE    @DATA(int_fo_aeup).

        period = CONV #( <period>-datuv+0(6) ).
        aedtm  = /sew/cl_time_changes=>get_change_date( pernr  = pernr
                                                        period = period ).

        IF int_fo_aeup IS INITIAL.

          CALL METHOD /sew/cl_forms_utils=>get_time_pdf_data
            EXPORTING
              begda       = <period>-datuv
              endda       = <period>-datub
              pernr       = pernr
              oraclepernr = oraclepernr
            IMPORTING
              timeresult  = tmp_result.

          CHECK tmp_result IS NOT INITIAL.
          CHECK aedtm      IS NOT INITIAL.

          CALL METHOD create_fo_aeup_entries
            EXPORTING
              result      = tmp_result
              pernr       = pernr
              oraclepernr = oraclepernr
              aedtm       = aedtm
              otype       = /sew/cl_forms_utils=>time_statement
            IMPORTING
              fo_aeup     = fo_aeup_line.

          APPEND fo_aeup_line TO fo_aeup.

        ELSE.
          READ TABLE int_fo_aeup WITH KEY pernr = pernr
                                          begda = <period>-datuv
                                          endda = <period>-datub
                                          INTO DATA(fo_aeup_old).

          CHECK sy-subrc EQ 0.  " sy-subrc NE 0 should never appear.

          period = CONV #( fo_aeup_old-begda+0(6) ).
          aedtm  = /sew/cl_time_changes=>get_change_date(  pernr  = pernr
                                                          period = period ).

          oraclepernr = /sew/cl_forms_utils=>get_oraclepernr( pernr ).
          IF fo_aeup_old-aedtm NE aedtm.

            CALL METHOD /sew/cl_forms_utils=>get_time_pdf_data
              EXPORTING
                begda       = fo_aeup_old-begda
                endda       = fo_aeup_old-endda
                pernr       = pernr
                oraclepernr = oraclepernr
              IMPORTING
                timeresult  = tmp_result.

            CHECK tmp_result IS NOT INITIAL.
            CHECK aedtm      IS NOT INITIAL.

            CALL METHOD create_fo_aeup_entries
              EXPORTING
                result      = tmp_result
                pernr       = pernr
                oraclepernr = oraclepernr
                aedtm       = aedtm
                otype       = /sew/cl_forms_utils=>time_statement
              IMPORTING
                fo_aeup     = fo_aeup_line. " create initial records

            APPEND fo_aeup_line TO fo_aeup.

          ENDIF.
        ENDIF.
        CLEAR int_fo_aeup.
      ENDLOOP.


      APPEND LINES OF fo_aeup TO me->int_fo_aeup.

      CALL METHOD save_entries
        EXPORTING
          fo_aeup = fo_aeup
        RECEIVING
          is_ok   = is_ok.

      IF is_ok EQ abap_false.
        status = '03'.
      ELSE.
        status = '02'.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD get.

    SELECT * FROM /sew/int_fo_aeup INTO TABLE fo_aeup WHERE pernr  IN pernr  AND
                                                           status  IN status AND
                                                           int_run IN intrun AND
                                                           ( begda IN dates OR
                                                             endda IN dates ).

  ENDMETHOD.


  METHOD get_rem_statements.

    DATA: tmp_result   TYPE /sew/time_pay_results,
          fo_aeup      TYPE TABLE OF /sew/int_fo_aeup,
          fo_aeup_line TYPE /sew/int_fo_aeup,
          is_ok        TYPE flag.

    LOOP AT ir_pernr ASSIGNING FIELD-SYMBOL(<pernr>).
      DATA(pernr) = CONV pernr_d( <pernr>-low ).
      DATA(oraclepernr) = /sew/cl_forms_utils=>get_oraclepernr( pernr ).

      LOOP AT periods ASSIGNING FIELD-SYMBOL(<period>).
        DATA(period) =  CONV char6( <period>-datuv+0(6) ).
        DATA(aedtm)  = /sew/cl_rem_changes=>get_change_date( pernr = pernr
                                                             begda = <period>-datuv
                                                             endda = <period>-datub ).
        CALL METHOD /sew/cl_forms_utils=>get_rem_pdf_data
          EXPORTING
            begda       = <period>-datuv
            endda       = <period>-datub
            pernr       = pernr
            oraclepernr = oraclepernr
          IMPORTING
            payresult   = tmp_result.

        CHECK tmp_result IS NOT INITIAL.
        CHECK aedtm      IS NOT INITIAL.

        CALL METHOD create_fo_aeup_entries
          EXPORTING
            result      = tmp_result
            pernr       = pernr
            oraclepernr = oraclepernr
            aedtm       = aedtm
            otype       = /sew/cl_forms_utils=>rem_statement
          IMPORTING
            fo_aeup     = fo_aeup_line.

        APPEND fo_aeup_line TO fo_aeup.

      ENDLOOP.

      APPEND LINES OF fo_aeup TO me->int_fo_aeup.

      CALL METHOD save_entries
        EXPORTING
          fo_aeup = fo_aeup
        RECEIVING
          is_ok   = is_ok.

      IF is_ok EQ abap_true.
        status = '03'.
      ELSE.
        status = '03'.
      ENDIF.

    ENDLOOP.


  ENDMETHOD.


  METHOD get_time_statements.

    DATA: tmp_result   TYPE /sew/time_pay_results,
          fo_aeup_line TYPE /sew/int_fo_aeup,
          fo_aeup      TYPE TABLE OF /sew/int_fo_aeup,
          is_ok        TYPE flag.



    LOOP AT ir_pernr ASSIGNING FIELD-SYMBOL(<pernr>).
      DATA(pernr) = CONV pernr_d( <pernr>-low ).
      DATA(oraclepernr) = /sew/cl_forms_utils=>get_oraclepernr( pernr ).

      LOOP AT periods ASSIGNING FIELD-SYMBOL(<period>).
        DATA(period) = CONV char6( <period>-datuv+0(6) ).

        DATA(aedtm) = /sew/cl_time_changes=>get_change_date( pernr  = pernr
                                                             period = period ).
        CALL METHOD /sew/cl_forms_utils=>get_time_pdf_data
          EXPORTING
            begda       = <period>-datuv
            endda       = <period>-datub
            pernr       = pernr
            oraclepernr = oraclepernr
          IMPORTING
            timeresult  = tmp_result.

        CHECK tmp_result IS NOT INITIAL.
        CHECK aedtm      IS NOT INITIAL.

        CALL METHOD create_fo_aeup_entries
          EXPORTING
            result      = tmp_result
            pernr       = pernr
            oraclepernr = oraclepernr
            aedtm       = aedtm
            otype       = /sew/cl_forms_utils=>time_statement
          IMPORTING
            fo_aeup     = fo_aeup_line. " create initial records

        APPEND fo_aeup_line TO fo_aeup.

      ENDLOOP.

      APPEND LINES OF fo_aeup TO me->int_fo_aeup.

      CALL METHOD save_entries
        EXPORTING
          fo_aeup = fo_aeup
        RECEIVING
          is_ok   = is_ok.

      IF is_ok EQ abap_false.
        status = '03'.
      ELSE.
        status = '02'.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD prepare_protocol.

    "Check message handler
    IF message_handler IS BOUND.

      "prepare layout options
      DATA(layout) = VALUE slis_layout_alv( zebra = 'X'
                                               colwidth_optimize = 'X' ).

      DATA: root_node TYPE hrpad_pal_node_key VALUE 'ROOT'.

      "Define structure of successful and failed IT_AEND entries ALV table
      message_handler->if_hrpay00_pal_services~create_fcat( EXPORTING i_structure_name = '/SEW/INT_FO_AEUP'
                                                            IMPORTING et_fcat          = DATA(fcat_it_aend) ).

      "add post table
      IF me->int_fo_aeup IS NOT INITIAL.
        message_handler->if_hrpay00_pal_services~add_table(
          EXPORTING
            i_parent_node_key = root_node
            it_fcat           = fcat_it_aend
            it_append_table   = me->int_fo_aeup
            is_layout         = layout
            i_node_txt        = COND string( WHEN simu EQ abap_true
                                             THEN TEXT-005
                                             ELSE TEXT-001 ) ).
      ENDIF.

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


  METHOD save_entries.

    IF fo_aeup IS NOT INITIAL AND me->simu NE 'X'.

      MODIFY /sew/int_fo_aeup FROM TABLE fo_aeup.
      COMMIT WORK.

      IF sy-subrc NE 0.
        is_ok = abap_false.
        msg_cont->add_message( iv_msg_type         = /iwbep/cl_cos_logger=>error
                         iv_msg_id                 = 'it_aend_mc'
                         iv_msg_number             = '001'
                         iv_add_to_response_header = abap_true ).
      ENDIF.
    ENDIF.


  ENDMETHOD.
ENDCLASS.
