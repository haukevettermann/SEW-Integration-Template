*&---------------------------------------------------------------------*
*&  Include           /SEW/VCL_EVENTS
*&---------------------------------------------------------------------*
FORM vcl_refresh.
  TYPES: ty_line TYPE c LENGTH 512.
  DATA:
    lv_error_flag TYPE boole_d,
    lt_fields_all TYPE TABLE OF /sew/int_f_v,
    lt_folders_all TYPE TABLE OF /sew/int_fo_v,
    lv_vcl_new    TYPE vcl_line_l,
    string        TYPE string.
  DATA: BEGIN OF mapping.
          INCLUDE STRUCTURE /sew/int_mf_v.
          INCLUDE STRUCTURE vimtbflags.
        DATA: END OF mapping.
  FIELD-SYMBOLS: <mapping>     LIKE mapping.
  DATA: BEGIN OF lv_vcl_new1,
          line(228) TYPE c,
        END OF lv_vcl_new1.
  DATA: BEGIN OF ls_folder.
          INCLUDE STRUCTURE /sew/int_fo_v.
          INCLUDE STRUCTURE vimtbflags.
        DATA: END OF ls_folder.
  DATA: BEGIN OF ls_infotype.
          INCLUDE STRUCTURE /sew/int_i_v.
          INCLUDE STRUCTURE vimtbflags.
        DATA: END OF ls_infotype.
  FIELD-SYMBOLS: <infotyp>     LIKE ls_infotype,
                 <folder>     LIKE ls_folder,
                 <lv_char_tmp> TYPE c.
*                 <vcl_extract> TYPE table.
  IF vcl_function = 'EV_NAVI' AND vcl_next_view = '/SEW/INT_F_V'.
    PERFORM vcl_set_table_access_for_obj USING '/SEW/INT_O_V'
                                            CHANGING lv_error_flag.
    CHECK <vcl_extract> IS ASSIGNED.
    LOOP AT <vcl_extract> ASSIGNING FIELD-SYMBOL(<vcl>).
      ASSIGN <vcl> TO <folder> CASTING.
      IF <folder>-vim_mark = 'M'.
        SELECT * FROM /sew/int_fields INTO TABLE @DATA(lt_fields) WHERE molga = @<folder>-molga AND object = @<folder>-object AND object_seqnr = @<folder>-object_seqnr AND infty = @<folder>-infty AND seqnr = @<folder>-seqnr AND folder = @<folder>-folder.
        lt_fields_all = CORRESPONDING #( BASE ( lt_fields_all ) lt_fields ).
        CLEAR: lt_fields.
      ENDIF.
    ENDLOOP.
    PERFORM vcl_set_table_access_for_obj USING '/SEW/INT_F_V'
                                            CHANGING lv_error_flag.
* positionstabelle überarbeiten
    IF ( lines( lt_fields_all ) NE lines( <vcl_total> ) OR lines( lt_fields_all ) NE lines( <vcl_extract> ) ) AND lines( lt_fields_all ) > 0.
      CLEAR: <vcl_total>, <vcl_extract>.
      LOOP AT lt_fields_all ASSIGNING FIELD-SYMBOL(<field>).
        "lv_vcl_new = <field>.
        APPEND INITIAL LINE TO <vcl_total> ASSIGNING FIELD-SYMBOL(<vcl_total_line>).
        APPEND INITIAL LINE TO <vcl_extract> ASSIGNING FIELD-SYMBOL(<vcl_extract_line>).
        DO.
          ASSIGN COMPONENT sy-index OF STRUCTURE <field> TO <lv_char_tmp> CASTING.
          IF sy-subrc NE 0.
            EXIT.
          ENDIF.
          CONCATENATE string <lv_char_tmp> INTO string RESPECTING BLANKS.
        ENDDO.
        <vcl_total_line> = string .
        <vcl_extract_line> = string .
        CLEAR: string.
      ENDLOOP.
    ENDIF.
  ELSEIF vcl_function = 'EV_NAVI' AND vcl_next_view = '/SEW/INT_FO_V'.
    PERFORM vcl_set_table_access_for_obj USING '/SEW/INT_I_V'
                                            CHANGING lv_error_flag.
    CHECK <vcl_extract> IS ASSIGNED.
    LOOP AT <vcl_extract> ASSIGNING <vcl>.
      ASSIGN <vcl> TO <infotyp> CASTING.
      IF <infotyp>-vim_mark = 'M'.
        SELECT * FROM /sew/int_folders INTO TABLE @DATA(lt_folders) WHERE molga = @<infotyp>-molga AND object = @<infotyp>-object AND object_seqnr = @<infotyp>-object_seqnr AND infty = @<infotyp>-infty AND seqnr = @<infotyp>-seqnr.
        lt_folders_all = CORRESPONDING #( BASE ( lt_folders_all ) lt_folders ).
        CLEAR: lt_fields.
      ENDIF.
    ENDLOOP.
    PERFORM vcl_set_table_access_for_obj USING '/SEW/INT_FO_V'
                                            CHANGING lv_error_flag.
* positionstabelle überarbeiten
    IF ( lines( lt_fields_all ) NE lines( <vcl_total> ) OR lines( lt_folders_all ) NE lines( <vcl_extract> ) ) AND lines( lt_folders_all ) > 0.
      CLEAR: <vcl_total>, <vcl_extract>.
      LOOP AT lt_folders_all ASSIGNING FIELD-SYMBOL(<folders_all>).
        "lv_vcl_new = <field>.
        APPEND INITIAL LINE TO <vcl_total> ASSIGNING <vcl_total_line>.
        APPEND INITIAL LINE TO <vcl_extract> ASSIGNING <vcl_extract_line>.
        DO.
          ASSIGN COMPONENT sy-index OF STRUCTURE <folders_all> TO <lv_char_tmp> CASTING.
          IF sy-subrc NE 0.
            EXIT.
          ENDIF.
          CONCATENATE string <lv_char_tmp> INTO string RESPECTING BLANKS.
        ENDDO.
        <vcl_total_line> = string .
        <vcl_extract_line> = string .
        CLEAR: string.
      ENDLOOP.
    ENDIF.
  ELSEIF vcl_function = 'EV_NAVI' AND vcl_next_view = '/SEW/INT_M_V' AND vcl_last_view = '/SEW/INT_MF_V'.
    PERFORM vcl_set_table_access_for_obj USING '/SEW/INT_MF_V'
                                             CHANGING lv_error_flag.
    CHECK <vcl_extract> IS ASSIGNED.
    LOOP AT <vcl_extract> ASSIGNING <vcl>.
      ASSIGN <vcl> TO <mapping> CASTING.
      IF ( <mapping>-vim_mark = 'M' AND <mapping>-is_complex = abap_true ) OR ( lines( <vcl_extract> ) = 1 AND <mapping>-is_complex = abap_true ).
        MESSAGE 'Keine Feldzuweisung für komplexes Mapping!' TYPE 'I' DISPLAY LIKE 'I'.
*        vcl_akt_view = vcl_next_view  = '/SEW/INT_MF_V'.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_CHANGE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_change_data .
  DATA:
    lv_error_flag TYPE boole_d.
  DATA: BEGIN OF ls_objects.
          INCLUDE STRUCTURE /sew/int_o_v.
          INCLUDE STRUCTURE vimtbflags.
        DATA: END OF ls_objects.
  DATA: BEGIN OF ls_infotyp.
          INCLUDE STRUCTURE /sew/int_i_v.
          INCLUDE STRUCTURE vimtbflags.
        DATA: END OF ls_infotyp.
  DATA: BEGIN OF ls_field.
          INCLUDE STRUCTURE /sew/int_f_v.
          INCLUDE STRUCTURE vimtbflags.
        DATA: END OF ls_field.
  DATA: BEGIN OF ls_mapping.
          INCLUDE STRUCTURE /sew/int_m_v.
          INCLUDE STRUCTURE vimtbflags.
        DATA: END OF ls_mapping.
  DATA: BEGIN OF ls_mappingf.
          INCLUDE STRUCTURE /sew/int_mf_v.
          INCLUDE STRUCTURE vimtbflags.
        DATA: END OF ls_mappingf.
  FIELD-SYMBOLS:
    <fs_o>  LIKE ls_objects,
    <fs_i>  LIKE ls_infotyp,
    <fs_f>  LIKE ls_field,
    <fs_m>  LIKE ls_mapping,
    <fs_mf> LIKE ls_mappingf.
*    <vcl_extract> TYPE table,
*    <vcl_total>   TYPE table.
  IF     vcl_function = 'SAVE'.
    PERFORM vcl_set_table_access_for_obj USING vcl_akt_view
                                            CHANGING lv_error_flag.
    CASE vcl_akt_view.
      WHEN '/SEW/INT_O_V'.
        LOOP AT <vcl_total> ASSIGNING FIELD-SYMBOL(<vcl>).
          ASSIGN <vcl> TO <fs_o> CASTING.
          <fs_o>-last_changed_by = sy-uname.
          <fs_o>-last_changed_at = sy-timlo.
        ENDLOOP.
        LOOP AT <vcl_extract> ASSIGNING FIELD-SYMBOL(<extract>).
          ASSIGN <extract> TO <fs_o> CASTING.
          <fs_o>-last_changed_by = sy-uname.
          <fs_o>-last_changed_at = sy-timlo.
        ENDLOOP.
      WHEN '/SEW/INT_I_V' .
        IF <vcl_total> IS ASSIGNED.
          LOOP AT <vcl_total> ASSIGNING <vcl>.
            ASSIGN <vcl> TO <fs_i> CASTING.
            <fs_i>-last_changed_by = sy-uname.
            <fs_i>-last_changed_at = sy-timlo.
          ENDLOOP.
        ENDIF.
        IF <vcl_extract> IS ASSIGNED.
          LOOP AT <vcl_extract> ASSIGNING <extract>.
            ASSIGN <extract> TO <fs_i> CASTING.
            <fs_i>-last_changed_by = sy-uname.
            <fs_i>-last_changed_at = sy-timlo.
*            IF <fs_i>-vim_action = 'N'.
*              SELECT MAX( seqnr ) FROM /sew/int_infotyp WHERE molga = @<fs_i>-molga AND infty = @<fs_i>-infty INTO @DATA(seqnr).
*              IF sy-subrc IS INITIAL.
*                <fs_i>-seqnr = seqnr + 1.
*          ENDIF.
*        ENDIF.
          ENDLOOP.
        ENDIF.
      WHEN '/SEW/INT_F_V' .
        LOOP AT <vcl_total> ASSIGNING <vcl>.
          ASSIGN <vcl> TO <fs_f> CASTING.
          <fs_f>-last_changed_by = sy-uname.
          <fs_f>-last_changed_at = sy-timlo.
        ENDLOOP.
        LOOP AT <vcl_extract> ASSIGNING <extract>.
          ASSIGN <extract> TO <fs_f> CASTING.
          <fs_f>-last_changed_by = sy-uname.
          <fs_f>-last_changed_at = sy-timlo.
        ENDLOOP.
      WHEN '/SEW/INT_M_V' .
        LOOP AT <vcl_total> ASSIGNING <vcl>.
          ASSIGN <vcl> TO <fs_m> CASTING.
          <fs_m>-last_changed_by =  sy-uname.
          <fs_m>-last_changed_at = sy-timlo.
        ENDLOOP.
        LOOP AT <vcl_extract> ASSIGNING <extract>.
          ASSIGN <extract> TO <fs_m> CASTING.
          <fs_m>-last_changed_by =  sy-uname.
          <fs_m>-last_changed_at = sy-timlo.
        ENDLOOP.
      WHEN '/SEW/INT_MF_V' .
        LOOP AT <vcl_total> ASSIGNING <vcl>.
          ASSIGN <vcl> TO <fs_mf> CASTING.
          <fs_mf>-last_changed_by =  sy-uname.
          <fs_mf>-last_changed_at = sy-timlo.
        ENDLOOP.
        LOOP AT <vcl_extract> ASSIGNING <extract>.
          ASSIGN <extract> TO <fs_mf> CASTING.
          <fs_mf>-last_changed_by =  sy-uname.
          <fs_mf>-last_changed_at = sy-timlo.
        ENDLOOP.
    ENDCASE.
  ENDIF.
*  IF vcl_action = 'U'.
*    PERFORM vcl_set_table_access_for_obj USING vcl_akt_view
*                                            CHANGING lv_error_flag.
*    IF <vcl_total> IS ASSIGNED.
*      LOOP AT <vcl_total> ASSIGNING <vcl>.
*        ASSIGN <vcl> TO <fs_i> CASTING.
*        <fs_i>-last_changed_by = sy-uname.
*        <fs_i>-last_changed_at = sy-timlo.
*      ENDLOOP.
*    ENDIF.
*    IF <vcl_extract> IS ASSIGNED.
*      LOOP AT <vcl_extract> ASSIGNING <extract>.
*        ASSIGN <extract> TO <fs_i> CASTING.
*        <fs_i>-last_changed_by = sy-uname.
*        <fs_i>-last_changed_at = sy-timlo.
**        IF <fs_i>-vim_action = 'N'.
**          SELECT MAX( seqnr ) FROM /sew/int_infotyp WHERE molga = @<fs_i>-molga AND infty = @<fs_i>-infty INTO @seqnr.
**          IF sy-subrc IS INITIAL.
**            <fs_i>-seqnr = seqnr + 1.
**          ENDIF.
**        ENDIF.
*      ENDLOOP.
*    ENDIF.
*  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  VCL_ON_NAV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
