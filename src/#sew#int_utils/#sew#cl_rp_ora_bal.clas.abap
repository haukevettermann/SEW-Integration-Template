class /SEW/CL_RP_ORA_BAL definition
  public
  create public .

public section.

  types:
    BEGIN OF ora_bal_comp,
        ora_bal TYPE /sew/int_ora_bal,
        endda   TYPE endda,
        begda   TYPE begda,
        desta   TYPE ptm_dedstart,
        deend   TYPE ptm_dedend,
        anzhl   TYPE ptm_quonum,
        kverb   TYPE ptm_quoded,
        corrv   TYPE ptm_quoded,
        planb   TYPE boolean,
        color   TYPE lvc_t_scol,
      END OF ora_bal_comp .
  types:
    t_ora_bal_comp TYPE TABLE OF ora_bal_comp .

  constants ADJ_EVT type STRING value 'CORR' ##NO_TEXT.
  data PERNR type RSDSSELOPT_T .
  data PERNR_PLANB type RSDSSELOPT_T .
  data BAL_COMP type /SEW/CL_RP_ORA_BAL=>T_ORA_BAL_COMP .

  methods COMBINE_ENTRIES
    importing
      !P2006 type PTT_P2006
      !ORA_BAL type /SEW/TT_ORA_BAL
    exporting
      !ORA_BAL_COMP type /SEW/CL_RP_ORA_BAL=>T_ORA_BAL_COMP .
  methods CREATE_CORRV_ENTRIES .
  methods GET_CORRV_ENTRIES
    importing
      !SEL_ROWS type SALV_T_ROW
      !BAL_COMP type /SEW/CL_RP_ORA_BAL=>T_ORA_BAL_COMP .
  methods GET_OPEN_BOOKINGS .
  methods SET_COLOR
    importing
      !PLANB type BOOLEAN
      !CORRV type PTM_QUONUM
    returning
      value(COLOR) type LVC_T_SCOL .
  methods CHECK_OPEN_BOOKINGS
    importing
      !PERNR type PERNR_D
    returning
      value(BOOKING) type BOOLEAN .
  methods GET_P2006
    changing
      !ORA_BAL type /SEW/TT_ORA_BAL
    returning
      value(P2006) type PTT_P2006 .
  methods PREPARE_ALV
    importing
      !SALV_TABLE type ref to CL_SALV_TABLE .
  methods CONSTRUCTOR
    importing
      !PERNR type RSDSSELOPT_T .
protected section.
private section.
ENDCLASS.



CLASS /SEW/CL_RP_ORA_BAL IMPLEMENTATION.


METHOD check_open_bookings.

  booking = abap_false.
  CHECK pernr       IN pernr_planb AND
        pernr_planb IS NOT INITIAL.
  booking = abap_true.

ENDMETHOD.


METHOD combine_entries.
  DATA: ora_bal_c TYPE /sew/cl_rp_ora_bal=>ora_bal_comp.

  LOOP AT ora_bal ASSIGNING FIELD-SYMBOL(<bal>).
    MOVE-CORRESPONDING <bal> TO ora_bal_c-ora_bal.

    LOOP AT p2006 ASSIGNING FIELD-SYMBOL(<p2006>) WHERE pernr EQ <bal>-pernr      AND
                                                        ktart EQ <bal>-ktart      AND
                                                        quonr EQ <bal>-quonr      AND
                                                        desta LE <bal>-plan_endda AND
                                                        deend GE <bal>-plan_begda.
      ora_bal_c-endda = <p2006>-endda.
      ora_bal_c-begda = <p2006>-begda.
      ora_bal_c-desta = <p2006>-desta.
      ora_bal_c-deend = <p2006>-deend.
      ora_bal_c-anzhl = <p2006>-anzhl.
      ora_bal_c-kverb = <p2006>-kverb.
      ora_bal_c-corrv = <p2006>-anzhl - <bal>-oracle_bal_all.
      ora_bal_c-planb = check_open_bookings( pernr = <bal>-pernr ).
      ora_bal_c-color = set_color( planb = ora_bal_c-planb
                                   corrv = ora_bal_c-corrv ).
    ENDLOOP.

    APPEND ora_bal_c TO ora_bal_comp.
    CLEAR: ora_bal_c.
  ENDLOOP.

ENDMETHOD.


METHOD constructor.
  me->pernr = pernr.
ENDMETHOD.


METHOD create_corrv_entries.
  DATA: p2006   TYPE p2006,
        it_aeup TYPE TABLE OF /sew/int_it_aeup.

  DATA(lo_it_aendup) = NEW /sew/cl_int_it_aendup( ).
  LOOP AT bal_comp ASSIGNING FIELD-SYMBOL(<bal>).
    p2006-infty = '2006'.
    p2006-pernr = <bal>-ora_bal-pernr.
    p2006-begda = <bal>-begda.
    p2006-endda = <bal>-endda.
    p2006-desta = <bal>-desta.
    p2006-deend = <bal>-deend.
    p2006-subty = <bal>-ora_bal-ktart.
    p2006-ktart = <bal>-ora_bal-ktart.
    p2006-anzhl = <bal>-corrv.

    cl_hr_pnnnn_type_cast=>pnnnn_to_prelp( EXPORTING pnnnn = p2006
                                           IMPORTING prelp = DATA(prelp) ).

    lo_it_aendup->build_it_aend_up( EXPORTING psave     = prelp
                                    IMPORTING it_aendup = DATA(it_aendup)
                                              message   = DATA(message) ).
    it_aendup-operation = 'UPD'.

    APPEND it_aendup TO it_aeup.

    CLEAR: p2006, it_aendup.
  ENDLOOP.

  MODIFY /sew/int_it_aeup FROM TABLE it_aeup.

ENDMETHOD.


METHOD get_corrv_entries.

  LOOP AT sel_rows ASSIGNING FIELD-SYMBOL(<sel_rows>).
    READ TABLE bal_comp ASSIGNING FIELD-SYMBOL(<bal_comp>) INDEX <sel_rows>.

    CHECK <bal_comp> IS ASSIGNED.
    CHECK <bal_comp> IS NOT INITIAL.
    CHECK <bal_comp>-corrv NE 0. "generate only entries with value corrections

    APPEND <bal_comp> TO me->bal_comp.
  ENDLOOP.
ENDMETHOD.


METHOD get_open_bookings.

  DATA(infty) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '2001' )
                                    ( sign = 'I' option = 'EQ' low = '2006' ) ).

  DATA(status) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '01' )
                                     ( sign = 'I' option = 'EQ' low = '03' )
                                     ( sign = 'I' option = 'EQ' low = '10' )
                                     ( sign = 'I' option = 'EQ' low = '11' ) ).

  SELECT pernr FROM /sew/int_it_aend INTO TABLE @DATA(pernr_aend) WHERE pernr  IN @pernr AND
                                                                        infty  IN @infty AND
                                                                        status IN @status.

  SELECT pernr FROM /sew/int_it_aeup INTO TABLE @DATA(pernr_aeup) WHERE pernr  IN @pernr AND
                                                                        infty  IN @infty AND
                                                                        status IN @status.

  pernr_planb = VALUE #( FOR <aend> IN pernr_aend ( sign   = 'I'
                                                    option = 'EQ'
                                                    low    = <aend> ) ).

  APPEND LINES OF VALUE rsdsselopt_t( FOR <aeup> IN pernr_aeup ( sign   = 'I'
                                                                 option = 'EQ'
                                                                 low    = <aeup> ) ) TO pernr_planb.

  SORT pernr_planb BY low.
  DELETE ADJACENT DUPLICATES FROM pernr_planb COMPARING low.

ENDMETHOD.


METHOD get_p2006.

  CHECK ora_bal IS NOT INITIAL AND
        pernr   IS NOT INITIAL.

  "get highest enddate
  SORT ora_bal BY plan_endda DESCENDING.
  READ TABLE ora_bal INTO DATA(high_end) INDEX 1.
  DATA(endda) = high_end-plan_endda.

  "get lowest start date
  SORT ora_bal BY plan_begda ASCENDING.
  READ TABLE ora_bal INTO DATA(low_start) INDEX 1.
  DATA(begda) = low_start-plan_begda.

  SORT ora_bal BY pernr ktart plan_endda ASCENDING.

  "get all relevant balances
  DATA(ktart) = VALUE rsdsselopt_t( FOR <ktart> IN ora_bal ( sign   = 'I'
                                                             option = 'EQ'
                                                             low    = <ktart>-ktart ) ).
  DELETE ADJACENT DUPLICATES FROM ktart COMPARING low.

  "get all relevant balances
  DATA(quonr) = VALUE rsdsselopt_t( FOR <quonr> IN ora_bal ( sign   = 'I'
                                                             option = 'EQ'
                                                             low    = <quonr>-quonr ) ).
  DELETE ADJACENT DUPLICATES FROM ktart COMPARING low.

  SELECT pernr,
         subty,
         endda,
         begda,
         ktart,
         anzhl,
         kverb,
         desta,
         deend FROM pa2006 INTO CORRESPONDING FIELDS OF TABLE @p2006 WHERE pernr IN @pernr AND
                                                                           subty IN @ktart AND
                                                                           quonr IN @quonr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda AND
                                                                           sprps EQ @abap_false.
ENDMETHOD.


METHOD prepare_alv.

  TRY.
      "set list title
      DATA(disp_setting) = salv_table->get_display_settings( ).
      disp_setting->set_list_header( TEXT-000 ).

      "display all available buttons of standard toolbar
      DATA(functions) = salv_table->get_functions( ).
      functions->set_all( value = abap_true ).

      "activate multiline selection
      DATA(selections) = salv_table->get_selections( ).
      selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

      "add buttons for post or delete of request
      functions->add_function( name     = CONV #( adj_evt )
                               icon     = CONV #( icon_complete )
                               text     = CONV #( TEXT-001 )
                               tooltip  = CONV #( TEXT-001 )
                               position = if_salv_c_function_position=>right_of_salv_functions ).

      "set color column
      DATA(columns) = salv_table->get_columns( ).
      columns->set_color_column( 'COLOR' ).

    CATCH cx_salv_data_error.
      MESSAGE e041(/sew/hcm_integration).
    CATCH cx_salv_existing.
      MESSAGE e041(/sew/hcm_integration).

    CATCH cx_salv_wrong_call.
      MESSAGE e041(/sew/hcm_integration).

  ENDTRY.

ENDMETHOD.


METHOD set_color.

  IF planb EQ abap_true AND
     corrv NE 0.
    APPEND VALUE #( color-col = 3
                    color-int = 1
                    color-inv = 0 ) TO color.
  ENDIF.

  IF planb EQ abap_false AND
     corrv NE 0.
    APPEND VALUE #( color-col = 6
                    color-int = 1
                    color-inv = 0 ) TO color.
  ENDIF.

ENDMETHOD.
ENDCLASS.
