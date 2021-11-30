class /SEW/CL_INT_OM_AENDUP definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR .
  methods ENHANCE_ENTRIES
    importing
      !AEND_NEW type /SEW/INT_OM_AEUP
    exporting
      !OM_AEND type /SEW/INT_OM_AEUP
      value(IS_OK) type BOOLEAN .
  methods BUILD_OM_AEND_UP
    importing
      !PSAVE type PRELP
    exporting
      !MESSAGE type BAPIRET1
      !OM_AENDUP type /SEW/INT_OM_AEUP .
  class-methods SAVE_ENTRIES
    importing
      !OM_AENDUP type /SEW/INT_OM_AEUP
    returning
      value(IS_OK) type BOOLEAN .
protected section.
private section.
ENDCLASS.



CLASS /SEW/CL_INT_OM_AENDUP IMPLEMENTATION.


  METHOD BUILD_OM_AEND_UP.
    DATA om_aendup_orig TYPE /sew/int_om_aeup.
    om_aendup_orig = CORRESPONDING #( psave ).
    me->enhance_entries( EXPORTING aend_new = om_aendup_orig IMPORTING om_aend = om_aendup is_ok = DATA(is_ok) ).
  ENDMETHOD.


  method CONSTRUCTOR.
  endmethod.


  METHOD ENHANCE_ENTRIES.

    IF aend_new IS NOT INITIAL.
      om_aend = CORRESPONDING #( aend_new ).
      om_aend-int_run = /sew/cl_int_utility=>create_guid( ).
      om_aend-mandt = sy-mandt.
      om_aend-timestamp = sy-timlo.
      om_aend-aedtm = sy-datum.
      om_aend-uname = sy-uname.
      om_aend-status = /sew/cl_int_constants=>booking_status-initial.
    ENDIF.

  ENDMETHOD.


  METHOD SAVE_ENTRIES.
    MODIFY /sew/int_om_aeup FROM om_aendup.

    is_ok = abap_true.
    IF sy-subrc NE 0.
      is_ok = abap_false.

*      msg_cont->add_message( iv_msg_type               = /iwbep/cl_cos_logger=>error
*                             iv_msg_id                 = it_aend_mc
*                             iv_msg_number             = '001'
*                             iv_add_to_response_header = abap_true ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
