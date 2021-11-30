*----------------------------------------------------------------------*
***INCLUDE /SEW/LTABLESO02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  /SEW/SET_FIELD_LIST_CONV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE /sew/set_field_list_conv OUTPUT.
  TYPE-POOLS : vrm.
  DATA:
    ld_field2   TYPE vrm_id,
    lcl_obj2    TYPE REF TO cl_abap_objectdescr,
    it_listbox2 TYPE vrm_values,
    wa_listbox2 LIKE LINE OF it_listbox2,
    it_methods2 TYPE abap_methdescr_tab.

*  select * from /mhp/smart_hndl into table @data(lt_hander).
  lcl_obj2 ?= cl_abap_objectdescr=>describe_by_name( '/SEW/CL_INT_CONVERSION' ).

  CLEAR: it_listbox2, it_methods2.
  it_methods2 = lcl_obj2->methods.
  LOOP AT it_methods2 ASSIGNING FIELD-SYMBOL(<methods2>).
    wa_listbox2-key = <methods2>-name.
    wa_listbox2-text = <methods2>-name.
    APPEND wa_listbox2 TO it_listbox2.
  ENDLOOP.

  ld_field2 = '/SEW/INT_C_V-CONVERSION_METHOD'.
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = ld_field2
      values = it_listbox2.
ENDMODULE.
