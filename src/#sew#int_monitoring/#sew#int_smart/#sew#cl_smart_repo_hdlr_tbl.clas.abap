class /SEW/CL_SMART_REPO_HDLR_TBL definition
  public
  inheriting from /MHP/SMART_CL_REPO_HDLR_TBL
  final
  create public .

public section.
protected section.

  methods GET_WHERECLAUSE_WITH_PARAMS
    redefinition .
private section.
ENDCLASS.



CLASS /SEW/CL_SMART_REPO_HDLR_TBL IMPLEMENTATION.


  METHOD GET_WHERECLAUSE_WITH_PARAMS.
** add date selection fields
    IF is_reportcustomizing-report-fieldpnpdatesel IS NOT INITIAL.
      ev_whereclause = |( { is_reportcustomizing-report-fieldpnpdatesel } BETWEEN | &&
        |'{ is_dataselection-objselect_begda }' AND '{ is_dataselection-objselect_endda }' )|.
    ELSE.
      ev_whereclause = |( { is_reportcustomizing-report-fieldpnpendda } GE | &&
        |'{ is_dataselection-objselect_begda }' AND { is_reportcustomizing-report-fieldpnpbegda } | &&
        |LE '{ is_dataselection-objselect_endda }' )|.
    ENDIF.

** add object selection fields
    IF is_reportcustomizing-report-fieldpnpobjid IS NOT INITIAL.
      DATA:
        lo_ui_badi         TYPE REF TO /mhp/smart_ui_extension_badi.

      GET BADI lo_ui_badi.

      " read objects and column data
      CALL BADI lo_ui_badi->get_orgview_assignments
        EXPORTING
          iv_orgsel            = is_dataselection-objselect_orgview
          iv_user              = is_dataselection-objselect_user
          is_reportcustomizing = is_reportcustomizing
          iv_objselect_objid   = is_dataselection-objselect_objid
        IMPORTING
          "et_keyobjects        = et_objects
          et_keystruc          = data(lt_objectstructure).

      et_range_objid = VALUE #( FOR ls_objects IN lt_objectstructure ( sign = 'I' option = 'EQ' low = ls_objects-objid ) ).
      ev_whereclause = |{ ev_whereclause } AND ( { is_reportcustomizing-report-fieldpnpobjid } IN @{ iv_local_objid_varname } )|.
    ENDIF.

** add rest of the where clause
    IF iv_whereclause IS NOT INITIAL.
      ev_whereclause = |{ ev_whereclause } AND ( { iv_whereclause } )|.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
