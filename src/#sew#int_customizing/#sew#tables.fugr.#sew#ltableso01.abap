*----------------------------------------------------------------------*
***INCLUDE /SEW/LTABLESO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  /SEW/SET_FIELD_LIST_METHODS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE /sew/set_field_list_methods OUTPUT.
  TYPE-POOLS : vrm.
  DATA: ld_field   TYPE vrm_id,
        lcl_obj    TYPE REF TO cl_abap_objectdescr,
        it_listbox TYPE vrm_values,
        wa_listbox LIKE LINE OF it_listbox,
        it_methods TYPE abap_methdescr_tab.

*  select * from /mhp/smart_hndl into table @data(lt_hander).
  lcl_obj ?= cl_abap_objectdescr=>describe_by_name( '/SEW/CL_INT_MAPPING' ).

  CLEAR: it_listbox, it_methods.
  it_methods = lcl_obj->methods.
  LOOP AT it_methods ASSIGNING FIELD-SYMBOL(<methods>).
    wa_listbox-key = <methods>-name.
    wa_listbox-text = <methods>-name.
    APPEND wa_listbox TO it_listbox.
  ENDLOOP.

  ld_field = '/SEW/INT_M_V-MAPPING_METHOD'.
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = ld_field
      values = it_listbox.
ENDMODULE.
