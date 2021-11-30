class /SEW/CL_SMART_BI_IMPL_EXT_RULE definition
  public
  final
  create public .

public section.

  interfaces /MHP/SMART_IF_RULE_EXT .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.
ENDCLASS.



CLASS /SEW/CL_SMART_BI_IMPL_EXT_RULE IMPLEMENTATION.


  method /MHP/SMART_IF_RULE_EXT~APPLY_CUSTOMIZINGRULE_ON_LINE.
    field-symbols:
      <lv_source_field> type any.

    loop at io_rule_function->ms_rule-parameter into data(ls_sourcefield) where sourcefield is not initial.
      assign component ls_sourcefield-sourcefield of structure cs_table_line to <lv_source_field>.
      check sy-subrc = 0.

      case io_rule_function->ms_rule-rule.
        when 'DATE_WEEK'.
          cl_reca_date=>get_week_info_by_date( exporting id_date = <lv_source_field> importing ed_week = cv_value ).
      endcase.
    endloop.
  endmethod.


  METHOD /mhp/smart_if_rule_ext~apply_customizingrule_on_table.
    DATA:
      lt_range_pernr    TYPE RANGE OF pernr_d,
      lt_range_objid    TYPE RANGE OF hrobjid,
      lt_idd07v_msgstat TYPE dd07v_tab,
      lt_idd07v_status  TYPE dd07v_tab,
      lt_idd07v_type    TYPE dd07v_tab.
    FIELD-SYMBOLS:
      <ls_table> TYPE any.

    IF is_rule-rule_function CP '/SEW/STATUS_TEXT*'.
      CALL FUNCTION 'DD_DOMVALUES_GET'
        EXPORTING
          domname        = '/SEW/DD_STATUS_DOM'
          text           = 'X'
          langu          = sy-langu
        TABLES
          dd07v_tab      = lt_idd07v_status
        EXCEPTIONS
          wrong_textflag = 1
          OTHERS         = 2.

* status texte vervollständigen
      LOOP AT ct_table ASSIGNING <ls_table>.
        ASSIGN COMPONENT 'STATUS' OF STRUCTURE <ls_table> TO FIELD-SYMBOL(<ls_status>).
        ASSIGN COMPONENT 'TEXT_4_STATUS' OF STRUCTURE <ls_table> TO FIELD-SYMBOL(<ls_status_text>).
        IF sy-subrc = 0.
          <ls_status_text> = VALUE #( lt_idd07v_status[ valpos = <ls_status> ]-ddtext DEFAULT '- No Status Text -'(nst) ).
        ENDIF.
      ENDLOOP.
    ELSEIF is_rule-rule_function CP '/SEW/MSGSTATUS_TEXT*'.
      CALL FUNCTION 'DD_DOMVALUES_GET'
        EXPORTING
          domname        = '/SEW/DD_MSG_STATUS_DOM'
          text           = 'X'
          langu          = sy-langu
        TABLES
          dd07v_tab      = lt_idd07v_msgstat
        EXCEPTIONS
          wrong_textflag = 1
          OTHERS         = 2.

* status texte vervollständigen
      LOOP AT ct_table ASSIGNING <ls_table>.
        ASSIGN COMPONENT 'MSG_STATUS' OF STRUCTURE <ls_table> TO FIELD-SYMBOL(<ls_msg_status>).
        ASSIGN COMPONENT 'TEXT_4_MSG_STATUS' OF STRUCTURE <ls_table> TO FIELD-SYMBOL(<ls_msg_status_text>).
        IF sy-subrc = 0.
          <ls_msg_status_text> = VALUE #( lt_idd07v_msgstat[ valpos = <ls_msg_status> ]-ddtext DEFAULT '- No Status Text -'(nst) ).
        ENDIF.
      ENDLOOP.
    ELSEIF is_rule-rule_function CP '/SEW/MSGTYPE_TEXT*'.
      CALL FUNCTION 'DD_DOMVALUES_GET'
        EXPORTING
          domname        = 'CACSMSGTYPE'
          text           = 'X'
          langu          = sy-langu
        TABLES
          dd07v_tab      = lt_idd07v_type
        EXCEPTIONS
          wrong_textflag = 1
          OTHERS         = 2.

* status texte vervollständigen
      LOOP AT ct_table ASSIGNING <ls_table> WHERE ('msg_type is not initial').
        ASSIGN COMPONENT 'MSG_TYPE' OF STRUCTURE <ls_table> TO FIELD-SYMBOL(<ls_type>).
        ASSIGN COMPONENT 'TEXT_4_MSG_TYPE' OF STRUCTURE <ls_table> TO FIELD-SYMBOL(<ls_type_text>).
        IF sy-subrc = 0.
          <ls_type_text> = VALUE #( lt_idd07v_type[ domvalue_l = <ls_type> ]-ddtext DEFAULT '- No Type Text -'(ntt) ).
        ENDIF.
      ENDLOOP.
    ELSEIF is_rule-rule_function CP '/SEW/OBJECTTYPE_TEXT*'.
* status texte vervollständigen
      LOOP AT ct_table ASSIGNING <ls_table> WHERE ('otype is not initial').
        ASSIGN COMPONENT 'OTYPE' OF STRUCTURE <ls_table> TO FIELD-SYMBOL(<ls_otype>).
        ASSIGN COMPONENT 'TEXT_4_OTYPE' OF STRUCTURE <ls_table> TO FIELD-SYMBOL(<ls_otype_text>).
        IF sy-subrc = 0.
          CASE <ls_otype>.
            WHEN 'P'.
              <ls_otype_text> = 'Person'(typ).
            WHEN 'O'.
              <ls_otype_text> = 'Organizational Unit'(tyo).
          ENDCASE.
        ENDIF.
      ENDLOOP.
    ELSEIF is_rule-rule_function CP '/SEW/OBJECT_DETAILS*'.
* hrobjekte aus Tabelle extrahieren
      DATA(lt_hrobjtab) = CORRESPONDING hrrootob_tab( ct_table ).
* Duplikate und leere einräge löschen
      SORT lt_hrobjtab BY otype objid.
      DELETE ADJACENT DUPLICATES FROM lt_hrobjtab COMPARING otype objid.
      DELETE lt_hrobjtab WHERE otype = '' OR objid = ''.
* in pernr und objid ids aufteilen
      lt_range_pernr = VALUE #( FOR <ls_hrobjtab_p> IN lt_hrobjtab WHERE ( otype = 'P' )
        ( sign = 'I' option ='EQ' low = <ls_hrobjtab_p>-objid ) ).
      lt_range_objid = VALUE #( FOR <ls_hrobjtab_o> IN lt_hrobjtab WHERE ( otype = 'O' )
        ( sign = 'I' option ='EQ' low = <ls_hrobjtab_o>-objid ) ).
* mitarbeiterdetails lesen (wenn benötigt)
      IF lt_range_pernr IS NOT INITIAL.
        SELECT 'P' AS otype, pernr AS objid, sname AS text_4_objid FROM pa0001 INTO TABLE @DATA(lt_pernr_details)
          WHERE pernr IN @lt_range_pernr AND begda <= @sy-datlo AND endda >= @sy-datlo.
      ENDIF.
* orgdetails lesen (wenn benötigt)
      IF lt_range_objid IS NOT INITIAL.
        SELECT otype, objid AS objid, short AS text_4_objid FROM hrp1000 INTO TABLE @DATA(lt_objid_details)
          WHERE otype = 'O' AND objid IN @lt_range_objid AND begda <= @sy-datlo AND endda >= @sy-datlo.
      ENDIF.
* details zusammenführen
      lt_objid_details = VALUE #( BASE lt_objid_details FOR ls_pernr_details IN lt_pernr_details
        ( CORRESPONDING #( ls_pernr_details ) ) ).
* wenn daten vorhanden, diese in die Strukturen kopieren
      IF lt_objid_details IS NOT INITIAL.
        LOOP AT ct_table ASSIGNING <ls_table> WHERE ('otype is not initial and objid is not initial').
          DATA(ls_hrobjtab) = CORRESPONDING hrrootob( <ls_table> ).
          DATA(ls_objid_details) = VALUE #( lt_objid_details[ otype = ls_hrobjtab-otype objid = ls_hrobjtab-objid ] ).
          MOVE-CORRESPONDING ls_objid_details TO <ls_table>.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  method /MHP/SMART_IF_RULE_EXT~CALCULATE_DATE_SPAN.
  endmethod.


  METHOD /mhp/smart_if_rule_ext~initialize.
**********************************************************************
* Perform initialization logic here
**********************************************************************
  ENDMETHOD.
ENDCLASS.
