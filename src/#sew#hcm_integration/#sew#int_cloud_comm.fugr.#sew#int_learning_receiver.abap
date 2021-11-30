FUNCTION /sew/int_learning_receiver.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(XML_XSTRING) TYPE  XSTRING
*"  EXPORTING
*"     VALUE(RESPONSE) TYPE  BAPIRET1
*"----------------------------------------------------------------------
*IFT20211005 Start Insert
*
  DATA: xml_table        TYPE TABLE OF smum_xmltb,
        gt_return        TYPE TABLE OF bapiret2,
        int_lear_dl      TYPE TABLE OF /sew/int_lear_dl,
        int_lear_dl_conf TYPE TABLE OF /sew/int_lear_dl.

  TRY.
      DATA(xml_parser) = NEW cl_xml_document( ).
      DATA(xml) = NEW /sew/cl_int_xml_up( ).

      xml_parser->parse_xstring( stream = xml_xstring ).

      IF sy-subrc IS NOT INITIAL. " xstring is already valid xml
        DATA(xml_string) = xml->create_string_from_xstring( xstring = xml_xstring ).
        xml_parser->parse_string( stream = xml_string ).
      ENDIF.

      IF sy-subrc IS NOT INITIAL.
        response-type = 'S'.
        response-message = 'D002/' && sy-mandt && ` XML fehlerhaft!`.
        RETURN.
      ENDIF.
  ENDTRY.


  CALL FUNCTION 'SMUM_XML_PARSE'
    EXPORTING
      xml_input = xml_xstring       " production case
*     xml_input = xml_xstring_test " test case
    TABLES
      xml_table = xml_table
      return    = gt_return.


  LOOP AT xml_table ASSIGNING FIELD-SYMBOL(<xml_table>).
    IF <xml_table>-hier EQ '4' AND <xml_table>-cname EQ 'LAST_UPDATE_DATE'.
      DATA(last_update_date) = /sew/cl_forms_utils=>convert_date_oracle_to_sap( CONV #( <xml_table>-cvalue ) ).
    ENDIF.
*    IF <xml_table>-hier EQ '4' AND <xml_table>-cname EQ 'SECURITY_RELEVANT'. " IFT20211026 D
    IF <xml_table>-hier EQ '2' AND <xml_table>-cname EQ 'SECURITY_RELEVANT'. " IFT20211026 I
      DATA(security_relevant) = <xml_table>-cvalue.
    ENDIF.
*    IF <xml_table>-hier EQ '4' AND <xml_table>-cname EQ 'G_1'. "  new G_1 node -> new Driving License " IFT 20211026 D
    IF <xml_table>-hier EQ '2' AND <xml_table>-cname EQ 'G_1'. "  new G_1 node -> new Driving License " IFT 20211026 I
      APPEND INITIAL LINE TO int_lear_dl ASSIGNING FIELD-SYMBOL(<line>).
*      <line>-last_update_date = last_update_date.   " IFT 20211102 D
*      <line>-security_relevant = security_relevant. " IFT 20211102 D
*      <line>-mandt = sy-mandt.                      " IFT 20211102 D
    ENDIF.
*    IF <xml_table>-hier EQ '5' AND <xml_table>-type EQ 'V'. " IFT20211026 D
    IF <xml_table>-hier EQ '3' AND <xml_table>-type EQ 'V'. " IFT20211026 I

      CASE <xml_table>-cname.
        WHEN 'PERSON_NUMBER'.
          CALL METHOD /sew/cl_int_utility=>get_pernr_by_cloudid
            EXPORTING
              cloud_id = CONV #( <xml_table>-cvalue )
              begda    = sy-datum
              endda    = sy-datum
            IMPORTING
              pernr    = DATA(sap_pernr).

          MOVE <xml_table>-cvalue TO <line>-person_number.
          MOVE sap_pernr TO <line>-pernr.
        WHEN 'USERNAME'.
          MOVE <xml_table>-cvalue TO <line>-username.
        WHEN 'COURSE_ID'.
          MOVE <xml_table>-cvalue TO <line>-course_id.
        WHEN 'COURSE_TITLE'.
          MOVE <xml_table>-cvalue TO <line>-course_title.
        WHEN 'COURSE_RELEVANT_FOR_SECURITY'.
          MOVE <xml_table>-cvalue TO <line>-course_relevant_for_security.
        WHEN 'TRAINING_START_DATE'.
          DATA(sap_begda) = /sew/cl_forms_utils=>convert_date_oracle_to_sap( CONV #( <xml_table>-cvalue ) ).
          MOVE sap_begda TO <line>-training_start_date.
        WHEN 'TRAINING_END_DATE'.
          DATA(sap_endda) = /sew/cl_forms_utils=>convert_date_oracle_to_sap( CONV #( <xml_table>-cvalue ) ).
          MOVE sap_endda TO <line>-training_end_date.
        WHEN 'PERSON_NAME'.
          MOVE <xml_table>-cvalue TO <line>-person_name.
        WHEN 'STATUS'.
          MOVE <xml_table>-cvalue TO <line>-status.
        WHEN 'CERTIFICATION'.
          MOVE <xml_table>-cvalue TO <line>-certification.
        WHEN 'CERTIFICATE_EXPIRY_DATE'.
          DATA(sap_certificate_expiry_date) = /sew/cl_forms_utils=>convert_date_oracle_to_sap( CONV #( <xml_table>-cvalue ) ).
          MOVE sap_certificate_expiry_date TO <line>-certificate_expiry_date.
        WHEN 'TRANSFER_RELEVANT'.
          MOVE <xml_table>-cvalue TO <line>-transfer_relevant.
      ENDCASE.
    ENDIF.
  ENDLOOP.

  IF int_lear_dl IS NOT INITIAL.

    LOOP AT int_lear_dl ASSIGNING FIELD-SYMBOL(<int_lear_dl>). " filter for only driver conform records
      CALL METHOD /sew/cl_hcm_lms_general=>is_dc_conform
        EXPORTING
          iv_begda   = <int_lear_dl>-training_start_date
          iv_endda   = <int_lear_dl>-training_end_date
          iv_pernr   = <int_lear_dl>-pernr
        IMPORTING
          ev_conform = DATA(is_conform).

      IF is_conform EQ 'X'.
        <int_lear_dl>-is_dl_conform = 'X'.
*        APPEND <int_lear_dl> TO int_lear_dl_conf. " IFT 20211026 D
      ENDIF.
    ENDLOOP.

*    MODIFY /sew/int_lear_dl FROM TABLE int_lear_dl_conf. " IFT20211026 D
    MODIFY /sew/int_lear_dl FROM TABLE int_lear_dl. " IFT20211026 I
    COMMIT WORK.

    IF sy-subrc IS INITIAL.
      response-type = 'S'.
      response-message = 'D002/' && sy-mandt && ` XML erfolgreich gelesen.`.
    ELSE.
      response-type = 'E'.
      response-message = 'D002/' && sy-mandt && ` XML Daten nicht erfolgreich abgelegt!`.
    ENDIF.
  ELSE.
    response-type = 'E'.
    response-message = 'D002/' && sy-mandt && ` XML Daten fehlerhaft.`.
  ENDIF.
*IFT20211005 End Insert
ENDFUNCTION.
