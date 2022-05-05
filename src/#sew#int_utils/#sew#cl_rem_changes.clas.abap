class /SEW/CL_REM_CHANGES definition
  public
  create public .

public section.

  types:
    REM_MAP_TT type table of /sew/int_rem_map .
  types:
    REM_UPD_TT type table of /sew/int_rem_upd .
  types:
    PERIODS_T type table of PERIOD .
  types:
    SEQNR_T type table of CDSEQ .
  types:
    TIME_PAY_RESULTS_T type table of /SEW/TIME_PAY_RESULTS .

  data MESSAGE_HANDLER type ref to CL_HRPAY00_MESSAGE_HANDLER .
  data SKIPPED_PERNRS type HRAHQ_PERNR_TABLE .
  data DATES type RSDSSELOPT_T .
  data STATUS type RSDSSELOPT_T .
  data SIMU type BOOLEAN .
  data INT_REM_UPD type /SEW/TT_REM_UPD .
  data STATUS_HANDLER type ref to /SEW/CL_INT_STATUS_HANDLER .
  data MSG_CONT type ref to /IWBEP/IF_MESSAGE_CONTAINER .

  class-methods DETERMINE_STRUKNAME
    importing
      !MOLGA type MOLGA
    returning
      value(STRUKNAME) type STRUKNAME .
  class-methods GET_CHANGE_DATE
    importing
      !PERNR type PERNR_D
      !BEGDA type BEGDA
      !ENDDA type ENDDA
    returning
      value(CHANGE_DATE) type DATS .
  class-methods GET_WAGE_TYPE_TEXT
    importing
      !LGART type LGART
    returning
      value(LGTXT) type LGTXT .
  methods CONSTRUCTOR
    importing
      !ALV type BOOLEAN
      !BATCH type BOOLEAN
      !SIMU type BOOLEAN
      !DATES type RSDSSELOPT_T .
  methods CREATE_ENTRIES
    importing
      !PAYXX_RESULT type ANY TABLE
      !MOLGA type MOLGA
      !PERNR type PERNR_D
      !INT_REM_MAP type /SEW/CL_REM_CHANGES=>REM_MAP_TT
      !PERIOD type RSDSSELOPT_T
    exporting
      !INT_REM_UPD type /SEW/CL_REM_CHANGES=>REM_UPD_TT .
  methods DETECT_CHANGES
    importing
      !PERNR type PERNR_D
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !MOLGA type MOLGA
      !PAYXX_RESULT type ANY TABLE
    exporting
      !REM_UPD_CHANGES type /SEW/CL_REM_CHANGES=>REM_UPD_TT .
  methods PREPARE_PROTOCOL .
  methods READ_CLUSTER
    importing
      !IR_PERNR type RSDSSELOPT_T
      !BEGDA type BEGDA
      !ENDDA type ENDDA .
  methods READ_CLUSTER_FORCE
    importing
      !IR_PERNR type RSDSSELOPT_T
      !BEGDA type BEGDA
      !ENDDA type ENDDA .
  methods SAVE_ENTRIES
    importing
      !REM_UPD type /SEW/CL_REM_CHANGES=>REM_UPD_TT
    returning
      value(IS_OK) type BOOLEAN .
protected section.
private section.

  class-methods GET_QUARTER_PERIOD
    importing
      !BEGDA type BEGDA
      !ENDDA type ENDDA
    exporting
      !FPBEG type DATS
      !FPEND type DATS .
  methods COMPARE_TABLES
    importing
      !REM_UPD_OLD type /SEW/CL_REM_CHANGES=>REM_UPD_TT
      !REM_UPD_NEW type /SEW/CL_REM_CHANGES=>REM_UPD_TT
    exporting
      !REM_UPD_DEL type /SEW/CL_REM_CHANGES=>REM_UPD_TT
      !REM_UPD_ADD type /SEW/CL_REM_CHANGES=>REM_UPD_TT
      !REM_UPD_MOD type /SEW/CL_REM_CHANGES=>REM_UPD_TT
      !REM_UPD_NO_CHANGES type /SEW/CL_REM_CHANGES=>REM_UPD_TT
      !NO_CHANGES type FLAG .
ENDCLASS.



CLASS /SEW/CL_REM_CHANGES IMPLEMENTATION.


  METHOD compare_tables.

    DATA: rem_upd_line TYPE /sew/int_rem_upd.

    LOOP AT rem_upd_new ASSIGNING FIELD-SYMBOL(<rem_upd_new>).
      READ TABLE rem_upd_old WITH KEY mandt     = <rem_upd_new>-mandt
                                      molga     = <rem_upd_new>-molga
                                      pernr     = <rem_upd_new>-pernr
                                      oracle_id = <rem_upd_new>-oracle_id
                                      tabname   = <rem_upd_new>-tabname
                                      lgart     = <rem_upd_new>-lgart
                                      endda     = <rem_upd_new>-endda
                                      begda     = <rem_upd_new>-begda
                                      INTO DATA(rem_upd_old_line).

      IF sy-subrc NE 0.
        APPEND <rem_upd_new> TO rem_upd_add.
      ELSE.
        IF <rem_upd_new>-waers NE rem_upd_old_line-waers OR
           <rem_upd_new>-betrg NE rem_upd_old_line-betrg OR
           <rem_upd_new>-cumty NE rem_upd_old_line-cumty.

          APPEND <rem_upd_new> TO rem_upd_mod.

        ELSE.
          MOVE-CORRESPONDING <rem_upd_new> TO rem_upd_line.
          rem_upd_line-status = '02'.
          APPEND rem_upd_line TO rem_upd_no_changes.
        ENDIF.
      ENDIF.

      CLEAR: rem_upd_old_line.

    ENDLOOP.

    IF rem_upd_del IS INITIAL AND rem_upd_mod IS INITIAL AND rem_upd_add IS INITIAL.
      no_changes = 'X'.
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    me->simu  = simu.
    me->dates = dates.
    msg_cont = /iwbep/cl_mgw_msg_container=>get_mgw_msg_container( ).
    "prepare protocol in case dialog is active and ALV is requested
    IF alv   EQ abap_true AND
       batch EQ abap_false.
      message_handler = cl_hrpay00_message_handler=>get_message_handler( ).
    ENDIF.

  ENDMETHOD.


  METHOD create_entries.
    DATA: rem_upd_t TYPE STANDARD TABLE OF /sew/int_rem_upd,
          rem_upd_l TYPE /sew/int_rem_upd.

    FIELD-SYMBOLS: <res_tab> TYPE ANY TABLE.

    LOOP AT payxx_result ASSIGNING FIELD-SYMBOL(<payxx_result_line>).
      LOOP AT int_rem_map ASSIGNING FIELD-SYMBOL(<line_map>).
        ASSIGN COMPONENT 'EVP-FPEND'           OF STRUCTURE <payxx_result_line> TO FIELD-SYMBOL(<fpend>).
        ASSIGN COMPONENT 'EVP-FPBEG'           OF STRUCTURE <payxx_result_line> TO FIELD-SYMBOL(<fpbeg>).
        ASSIGN COMPONENT 'EVP-SRTZA'           OF STRUCTURE <payxx_result_line> TO FIELD-SYMBOL(<srtza>).

        CHECK <srtza> EQ 'A'.
        CHECK <fpend> IN period.
        CHECK <fpbeg> IN period.

        ASSIGN COMPONENT 'INTER'               OF STRUCTURE <payxx_result_line> TO FIELD-SYMBOL(<inter>).
        ASSIGN COMPONENT 'INTER-VERSION-DATUM' OF STRUCTURE <payxx_result_line> TO FIELD-SYMBOL(<aedtm>).
        ASSIGN COMPONENT 'INTER-VERSC-WAERS'   OF STRUCTURE <payxx_result_line> TO FIELD-SYMBOL(<waers>).
        ASSIGN COMPONENT <line_map>-tabname    OF STRUCTURE <inter>             TO <res_tab>.


        CHECK <res_tab> IS ASSIGNED.

        LOOP AT <res_tab> ASSIGNING FIELD-SYMBOL(<tab_line>).
          ASSIGN COMPONENT 'LGART' OF STRUCTURE <tab_line> TO FIELD-SYMBOL(<lgart>).
          CHECK <lgart> EQ <line_map>-lgart.

          ASSIGN COMPONENT 'BETRG' OF STRUCTURE <tab_line> TO FIELD-SYMBOL(<betrg>).
          ASSIGN COMPONENT 'CUMTY' OF STRUCTURE <tab_line> TO FIELD-SYMBOL(<cumty>).


          IF <lgart> IS ASSIGNED.
            rem_upd_l-mandt     = sy-mandt.
            rem_upd_l-molga     = molga.
            rem_upd_l-pernr     = pernr.
            rem_upd_l-oracle_id = <line_map>-oracle_id.
            rem_upd_l-tabname   = <line_map>-tabname.
            rem_upd_l-lgart     = <lgart>.
            rem_upd_l-status    = '01'.

            IF <fpend> IS ASSIGNED.
              rem_upd_l-endda = <fpend>.
            ENDIF.

            IF <fpbeg> IS ASSIGNED.
              rem_upd_l-begda = <fpbeg>.
            ENDIF.

            IF <waers> IS ASSIGNED.
              rem_upd_l-waers = <waers>.
            ENDIF.

            IF <betrg> IS ASSIGNED.
              rem_upd_l-betrg = <betrg>.
            ENDIF.

            IF <aedtm> IS ASSIGNED.
              rem_upd_l-aedtm = <aedtm>.
            ENDIF.

            IF <cumty> IS ASSIGNED.
              rem_upd_l-cumty = <cumty>.
              UNASSIGN: <cumty>.
            ENDIF.

            APPEND rem_upd_l TO rem_upd_t.
            UNASSIGN: <cumty>, <lgart>, <betrg>.

            CLEAR rem_upd_l.

          ENDIF.

        ENDLOOP.
      ENDLOOP.
    ENDLOOP.


    int_rem_upd = rem_upd_t.

  ENDMETHOD.


  METHOD detect_changes.

    DATA: change_date        TYPE dats,
          rem_upd_t          TYPE STANDARD TABLE OF /sew/int_rem_upd,
          rem_upd_del        TYPE STANDARD TABLE OF /sew/int_rem_upd,
          rem_upd_add        TYPE STANDARD TABLE OF /sew/int_rem_upd,
          rem_upd_mod        TYPE STANDARD TABLE OF /sew/int_rem_upd,
          no_changes         TYPE flag,
          rem_upd_tmp        TYPE STANDARD TABLE OF /sew/int_rem_upd,
          rem_upd_no_changes TYPE STANDARD TABLE OF /sew/int_rem_upd.


    SELECT * FROM /sew/int_rem_map WHERE begda <= @sy-datum
                                   AND   endda >= @sy-datum
                                   INTO  TABLE @DATA(int_rem_map).

    DATA(oracle_con) = VALUE rsdsselopt_t( FOR <rem_map> IN int_rem_map ( sign   = 'I'
                                                                          option = 'EQ'
                                                                          low    = <rem_map>-oracle_id ) ).
    DATA(period_range) = VALUE rsdsselopt_t( ( sign   = 'I'
                                               option = 'BT'
                                               low    = begda
                                               high   = endda ) ).

    SELECT * FROM /sew/int_rem_upd WHERE molga     = @molga
                                   AND   pernr     = @pernr
                                   AND   oracle_id IN @oracle_con
                                   AND   status    = '02'
                                   AND   begda     IN @period_range
                                   AND   endda     IN @period_range
                                   INTO  TABLE     @DATA(rem_upd_old).


    IF rem_upd_old IS INITIAL.

      me->create_entries(
        EXPORTING
          payxx_result = payxx_result
          pernr        = pernr
          int_rem_map  = int_rem_map
          molga        = molga
          period       = period_range
        IMPORTING
          int_rem_upd  = rem_upd_t ).

      rem_upd_changes  = rem_upd_t.
      RETURN.
    ENDIF.

    me->create_entries(
      EXPORTING
        payxx_result = payxx_result
        pernr        = pernr
        int_rem_map  = int_rem_map
        molga        = molga
        period       = period_range
      IMPORTING
        int_rem_upd  = rem_upd_t ).

    CHECK rem_upd_t IS NOT INITIAL.

    me->compare_tables(
      EXPORTING
        rem_upd_old        = rem_upd_old
        rem_upd_new        = rem_upd_t
      IMPORTING
        rem_upd_add        = rem_upd_add
        rem_upd_mod        = rem_upd_mod
        rem_upd_del        = rem_upd_del
        rem_upd_no_changes = rem_upd_no_changes
        no_changes         = no_changes ).

    IF no_changes EQ 'X'.
      RETURN.
    ENDIF.

    APPEND LINES OF rem_upd_mod        TO rem_upd_changes.
    APPEND LINES OF rem_upd_add        TO rem_upd_changes.
    APPEND LINES OF rem_upd_no_changes TO rem_upd_changes.

  ENDMETHOD.


  METHOD determine_strukname.

    DATA: exists        TYPE dd02l-actflag,
          strukname_tmp TYPE e071-obj_name.

    DATA(country_iso) = /sew/cl_forms_utils=>get_country_by_molga( molga ).

    CONCATENATE 'PAY' country_iso '_RESULT' INTO strukname_tmp.

    CALL FUNCTION 'DD_OBJECT_EXISTS'
      EXPORTING
        name          = strukname_tmp
      IMPORTING
        exists        = exists
      EXCEPTIONS
        illegal_input = 1.

    IF sy-subrc EQ 0 AND exists = 'A'.
      strukname = strukname_tmp.
    ELSE.
      strukname = 'PAY99_RESULT'.
    ENDIF.

  ENDMETHOD.


  METHOD get_change_date.

    FIELD-SYMBOLS: <payxx_result> TYPE STANDARD TABLE.

    DATA: payxx_result TYPE REF TO data,
          strukname    TYPE string,
          molga        TYPE molga.

    molga = /sew/cl_forms_utils=>get_molga( pernr  ).

    strukname = determine_strukname( molga ).


    CREATE DATA payxx_result TYPE STANDARD TABLE OF (strukname).

    ASSIGN payxx_result->* TO <payxx_result>.


    CALL FUNCTION 'HR_GET_PAYROLL_RESULTS'
      EXPORTING
        pernr                         = pernr
        pabrj                         = begda+0(4)
        pabrp                         = begda+4(2)
        pabrj_end                     = endda+0(4)
        pabrp_end                     = endda+4(2)
      TABLES
        result_tab                    = <payxx_result>
      EXCEPTIONS
        no_results                    = 1
        error_in_currency_conversion  = 2
        t500l_entry_not_found         = 3
        period_mismatch_error         = 4
        t549q_entry_not_found         = 5
        internal_error                = 6
        wrong_structure_of_result_tab = 7
        OTHERS                        = 8.

    CHECK sy-subrc EQ 0.

    LOOP AT <payxx_result> ASSIGNING FIELD-SYMBOL(<payxx_result_line>).
      ASSIGN COMPONENT 'EVP-FPEND'           OF STRUCTURE <payxx_result_line> TO FIELD-SYMBOL(<fpend>).
      ASSIGN COMPONENT 'EVP-FPBEG'           OF STRUCTURE <payxx_result_line> TO FIELD-SYMBOL(<fpbeg>).

      CHECK <fpend> LE endda.
      CHECK <fpbeg> GE begda.

      ASSIGN COMPONENT 'INTER-VERSION-DATUM' OF STRUCTURE <payxx_result_line> TO FIELD-SYMBOL(<aedtm>).
      change_date = <aedtm>.
      EXIT.
    ENDLOOP.




  ENDMETHOD.


  METHOD get_quarter_period.

    fpbeg = SWITCH #( begda+4(2)
                      WHEN '01' OR '02' OR '03' THEN begda+0(4) && '01' && '01'
                      WHEN '04' OR '05' OR '06' THEN begda+0(4) && '04' && '01'
                      WHEN '07' OR '08' OR '09' THEN begda+0(4) && '07' && '01'
                      WHEN '10' OR '11' OR '12' THEN begda+0(4) && '10' && '01' ).
    fpend = SWITCH #( endda+4(2)
                      WHEN '01' OR '02' OR '03' THEN begda+0(4) && '03' && '31'
                      WHEN '04' OR '05' OR '06' THEN begda+0(4) && '06' && '30'
                      WHEN '07' OR '08' OR '09' THEN begda+0(4) && '09' && '30'
                      WHEN '10' OR '11' OR '12' THEN begda+0(4) && '12' && '31' ).


  ENDMETHOD.


  METHOD get_wage_type_text.

    SELECT SINGLE lgtxt FROM t512t WHERE sprsl = @sy-langu
                               AND   lgart = @lgart
                               INTO  @DATA(tmp_lgart_txt).

    lgtxt = tmp_lgart_txt.

  ENDMETHOD.


  METHOD prepare_protocol.
    "Check message handler
    IF message_handler IS BOUND.

      "prepare layout options
      DATA(layout) = VALUE slis_layout_alv( zebra = 'X'
                                               colwidth_optimize = 'X' ).

      DATA: root_node TYPE hrpad_pal_node_key VALUE 'ROOT'.

      "Define structure of successful and failed IT_AEND entries ALV table
      message_handler->if_hrpay00_pal_services~create_fcat( EXPORTING i_structure_name = '/SEW/INT_REM_UPD'
                                                            IMPORTING et_fcat          = DATA(fcat_rem_upd) ).

      "add post table
      IF me->int_rem_upd IS NOT INITIAL.
        message_handler->if_hrpay00_pal_services~add_table(
          EXPORTING
            i_parent_node_key = root_node
            it_fcat           = fcat_rem_upd
            it_append_table   = me->int_rem_upd
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


  METHOD read_cluster.

    DATA: periods         TYPE /sew/cl_rem_changes=>periods_t,
          pernr           TYPE pernr_d,
          molga           TYPE molga,
          rem_upd         TYPE TABLE OF /sew/int_rem_upd,
          rem_upd_changes TYPE TABLE OF /sew/int_rem_upd.


    LOOP AT ir_pernr ASSIGNING FIELD-SYMBOL(<pernr>).

      FIELD-SYMBOLS: <payxx_result> TYPE STANDARD TABLE.

      DATA: payxx_result TYPE REF TO data,
            strukname    TYPE string.

      molga = /sew/cl_forms_utils=>get_molga( CONV #( <pernr>-low ) ).

      strukname = determine_strukname( molga ).


      CREATE DATA payxx_result TYPE STANDARD TABLE OF (strukname).

      ASSIGN payxx_result->* TO <payxx_result>.
      pernr = CONV #( <pernr>-low ).

      CALL FUNCTION 'HR_GET_PAYROLL_RESULTS'
        EXPORTING
          pernr                         = pernr
          pabrj                         = begda+0(4)
          pabrp                         = begda+4(2)
          pabrj_end                     = endda+0(4)
          pabrp_end                     = endda+4(2)
        TABLES
          result_tab                    = <payxx_result>
        EXCEPTIONS
          no_results                    = 1
          error_in_currency_conversion  = 2
          t500l_entry_not_found         = 3
          period_mismatch_error         = 4
          t549q_entry_not_found         = 5
          internal_error                = 6
          wrong_structure_of_result_tab = 7
          OTHERS                        = 8.

      CHECK sy-subrc EQ 0.

      me->detect_changes(
        EXPORTING
          pernr        = pernr
          begda        = begda
          endda        = endda
          payxx_result = <payxx_result>
          molga        = molga
        IMPORTING
          rem_upd_changes  = rem_upd_changes ).

      IF rem_upd_changes IS NOT INITIAL.
        APPEND LINES OF rem_upd_changes TO rem_upd.
        CLEAR rem_upd_changes.
      ENDIF.

    ENDLOOP.

    APPEND LINES OF rem_upd TO me->int_rem_upd.

    IF rem_upd IS NOT INITIAL.
      me->save_entries(
        EXPORTING
          rem_upd = rem_upd ).
    ENDIF.

  ENDMETHOD.


  METHOD read_cluster_force.

    DATA: periods     TYPE /sew/cl_forms_utils=>periods_t,
          pernr       TYPE pernr_d,
          int_rem_upd TYPE TABLE OF /sew/int_rem_upd,
          is_ok       TYPE flag.

    SELECT * FROM /sew/int_rem_map WHERE begda <= @sy-datum
                                   AND   endda >= @sy-datum
                                   INTO  TABLE @DATA(int_rem_map).

    DATA(period_range) = VALUE rsdsselopt_t( ( sign   = 'I'
                                               option = 'BT'
                                               low    = begda
                                               high   = endda ) ).


    LOOP AT ir_pernr ASSIGNING FIELD-SYMBOL(<pernr>).
      FIELD-SYMBOLS: <payxx_result> TYPE STANDARD TABLE.

      DATA: payxx_result TYPE REF TO data.

      DATA(molga) = /sew/cl_forms_utils=>get_molga( CONV #( <pernr>-low ) ).
      DATA(strukname) = me->determine_strukname( molga ).

      CREATE DATA payxx_result TYPE STANDARD TABLE OF (strukname).

      ASSIGN payxx_result->* TO <payxx_result>.
      pernr = CONV #( <pernr>-low ).

      CALL FUNCTION 'HR_GET_PAYROLL_RESULTS'
        EXPORTING
          pernr                         = pernr
          pabrj                         = begda+0(4)
          pabrp                         = begda+4(2)
          pabrj_end                     = endda+0(4)
          pabrp_end                     = endda+4(2)
        TABLES
          result_tab                    = <payxx_result>
        EXCEPTIONS
          no_results                    = 1
          error_in_currency_conversion  = 2
          t500l_entry_not_found         = 3
          period_mismatch_error         = 4
          t549q_entry_not_found         = 5
          internal_error                = 6
          wrong_structure_of_result_tab = 7
          OTHERS                        = 8.

      CHECK sy-subrc EQ 0.

      me->create_entries(
        EXPORTING
          payxx_result = <payxx_result>
          pernr = pernr
          int_rem_map = int_rem_map
          molga = molga
          period = period_range
        IMPORTING
          int_rem_upd = int_rem_upd ).

      CHECK int_rem_upd IS NOT INITIAL.

      APPEND LINES OF int_rem_upd TO me->int_rem_upd.

      me->save_entries(
        EXPORTING
          rem_upd = int_rem_upd
        RECEIVING
          is_ok = is_ok ).


    ENDLOOP.

  ENDMETHOD.


  METHOD save_entries.

    IF rem_upd IS NOT INITIAL AND me->simu NE 'X'.
      MODIFY /sew/int_rem_upd FROM TABLE rem_upd.
      COMMIT WORK.
      IF sy-subrc NE 0.
        is_ok = abap_false.
        msg_cont->add_message( iv_msg_type               = /iwbep/cl_cos_logger=>error
                               iv_msg_id                 = 'it_aend_mc' " it_aend_mc nicht bekannt
                               iv_msg_number             = '001'
                               iv_add_to_response_header = abap_true ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
