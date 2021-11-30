class /SEW/CL_INT_CUSTOMIZING definition
  public
  final
  create public .

public section.

  types:
    t_fields TYPE TABLE OF /sew/int_fields .
  types:
    BEGIN OF s_bapiret2_with_icon,
        icon    TYPE icon_d,
        field   TYPE bapi_fld,
        message TYPE bapi_msg,
        type    TYPE bapi_mtype,
      END OF s_bapiret2_with_icon .
  types:
    t_bapiret2_with_icon TYPE TABLE OF s_bapiret2_with_icon .

  data GT_COMPONENTS type ABAP_COMPDESCR_TAB .
  data GT_BAPIRET_WI type T_BAPIRET2_WITH_ICON .
  data GT_CUSTOMIZING type T_FIELDS .
  data INFTY type INFTY .
  data MOLGA type MOLGA .

  methods READ_REL_FIELDS
    exporting
      !REL_FIELDS type /SEW/TT_REL_FLD
      !IS_OK type BOOLE_D .
  methods GET_COMPONENTS
    importing
      !IV_INFTY type INFTY
      !IV_MOLGA type MOLGA
      !IV_OBJECT type OTYPE .
  methods CONSTRUCTOR
    importing
      !INFTY type INFTY optional
      !MOLGA type MOLGA optional .
  methods BUILD_TABLE
    importing
      !IV_INFTY type INFTY
      !IV_MOLGA type MOLGA
      !IV_OBJECT type OTYPE .
  methods READ_CUSTOMIZING
    importing
      !IV_INFTY type INFTY
      !IV_MOLGA type MOLGA
      !IV_OBJECT type OTYPE
      !IV_OBJECT_SEQNR type SEQNR
      !IV_INFTY_SEQNR type SEQNR
      !IV_FOLDER type /SEW/DD_FOLDER .
protected section.
private section.
ENDCLASS.



CLASS /SEW/CL_INT_CUSTOMIZING IMPLEMENTATION.


  METHOD build_table.
    DATA: ls_bapiret2    TYPE bapiret2,
          ls_bapiret2_wi LIKE LINE OF me->gt_bapiret_wi.
    LOOP AT me->gt_components ASSIGNING FIELD-SYMBOL(<components>).
      READ TABLE me->gt_customizing WITH KEY field = <components>-name TRANSPORTING NO FIELDS.
      IF sy-subrc IS INITIAL.
        CALL FUNCTION 'FS_BAPI_BAPIRET2_FILL'
          EXPORTING
            type   = 'W'
            cl     = '/SEW/CUSTOMIZING'
            number = 001
            field  = CONV bapi_fld( 'P' && iv_infty && '-' && <components>-name )
          IMPORTING
            return = ls_bapiret2.
      ELSE.
        CALL FUNCTION 'FS_BAPI_BAPIRET2_FILL'
          EXPORTING
            type   = 'S'
            cl     = '/SEW/CUSTOMIZING'
            number = 000
            field  = CONV bapi_fld( 'P' && iv_infty && '-' && <components>-name )
          IMPORTING
            return = ls_bapiret2.
      ENDIF.
      ls_bapiret2_wi = CORRESPONDING #( ls_bapiret2 ).
      CASE ls_bapiret2_wi-type.
        WHEN 'S'.
          ls_bapiret2_wi-icon = icon_led_green.
        WHEN 'W'.
          ls_bapiret2_wi-icon = icon_led_yellow.
      ENDCASE.
      APPEND ls_bapiret2_wi TO me->gt_bapiret_wi.
      CLEAR: ls_bapiret2_wi, ls_bapiret2.
    ENDLOOP.
  ENDMETHOD.


  METHOD constructor.
    me->infty = infty.
    me->molga = molga.
  ENDMETHOD.


  METHOD get_components.
    IF iv_infty NE 9999.
      DATA: lo_strucdescr TYPE REF TO cl_abap_structdescr.
      DATA(lv_structure) = 'P' && iv_infty.
    ELSE.
      lv_structure = '/sew/dummy_fields'.
    ENDIF.
    lo_strucdescr ?= cl_abap_typedescr=>describe_by_name( lv_structure ).
    me->gt_components = lo_strucdescr->components.
    DELETE me->gt_components
      WHERE name = 'PERNR'
      OR name = 'INFTY'
      OR name = 'OBJID'
      OR name = 'OTYPE'
      OR name = 'BEGDA'
      OR name = 'ENDDA'.
  ENDMETHOD.


  METHOD read_customizing.
    DATA(infty) = iv_infty.
    SELECT * FROM /sew/int_fields INTO TABLE @me->gt_customizing WHERE molga = @iv_molga
      AND object = @iv_object
      AND infty = @iv_infty
      AND folder = @iv_folder
      AND object_seqnr = @iv_object_seqnr
      and seqnr = @iv_infty_seqnr.
  ENDMETHOD.


  METHOD read_rel_fields.
    SELECT * FROM /sew/int_rel_fld INTO TABLE @rel_fields WHERE infty = @me->infty
                                                                AND molga = @me->molga.
    IF sy-subrc = 0.
      is_ok = abap_true.
    ELSE.
      SELECT * FROM /sew/int_rel_fld INTO TABLE @rel_fields WHERE infty = @me->infty
                                                          AND molga = '*'.
      IF sy-subrc = 0.
        is_ok = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
