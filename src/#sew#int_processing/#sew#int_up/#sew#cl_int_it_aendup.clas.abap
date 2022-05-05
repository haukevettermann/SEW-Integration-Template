class /SEW/CL_INT_IT_AENDUP definition
  public
  final
  create public .

public section.

  class-methods DELETE_ENTRIES
    importing
      value(PERNR) type PERNR_D optional
      value(CLOUD_ID) type /SEW/DD_VALUE optional
    returning
      value(RETURN_IS_OK) type BOOLEAN .
  methods CONSTRUCTOR .
  methods ENHANCE_ENTRIES
    importing
      !AEND_NEW type /SEW/INT_IT_AEUP
    exporting
      !IT_AEND type /SEW/INT_IT_AEUP
      value(IS_OK) type BOOLEAN .
  methods BUILD_IT_AEND_UP
    importing
      !PSAVE type PRELP
    exporting
      !MESSAGE type BAPIRET1
      !IT_AENDUP type /SEW/INT_IT_AEUP .
  class-methods SAVE_ENTRIES
    importing
      !IT_AENDUP type /SEW/INT_IT_AEUP
    returning
      value(IS_OK) type BOOLEAN .
protected section.
private section.
ENDCLASS.



CLASS /SEW/CL_INT_IT_AENDUP IMPLEMENTATION.


  METHOD build_it_aend_up.
    DATA it_aendup_orig TYPE /sew/int_it_aeup.
    it_aendup_orig = CORRESPONDING #( psave ).
    me->enhance_entries( EXPORTING aend_new = it_aendup_orig IMPORTING it_aend = it_aendup is_ok = DATA(is_ok) ).
  ENDMETHOD.


  method CONSTRUCTOR.
  endmethod.


METHOD delete_entries.


*Wenn pernr und cloud übergeben wird
  IF pernr IS NOT INITIAL AND cloud_id IS NOT INITIAL.
    Select * FROM /sew/int_it_aeup INTO Table @data(tab) WHERE pernr = @pernr AND cloud_id = @cloud_id.
    DELETE FROM /sew/int_it_aeup WHERE pernr = pernr AND cloud_id = cloud_id.

*Wenn nur pernr übergeben wird
  ELSEIF pernr IS NOT INITIAL AND cloud_id IS INITIAL.

    DELETE FROM /sew/int_it_aeup WHERE pernr = pernr.

  ENDIF.

  IF sy-subrc EQ 0.
    return_is_ok = abap_true.
  ENDIF.

ENDMETHOD.


  METHOD enhance_entries.

    IF aend_new IS NOT INITIAL.
      it_aend = CORRESPONDING #( aend_new ).
      it_aend-int_run = /sew/cl_int_utility=>create_guid( ).
      it_aend-mandt = sy-mandt.
      it_aend-timestamp = sy-timlo.
      it_aend-aedtm = sy-datum.
      it_aend-uname = sy-uname.
      it_aend-status = /sew/cl_int_constants=>booking_status-initial.
    ENDIF.

  ENDMETHOD.


  METHOD save_entries.
    MODIFY /sew/int_it_aeup FROM it_aendup.

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
