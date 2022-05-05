class /SEW/CL_FORMS_UTILS definition
  public
  create public .

public section.

  types:
    PERIODS_T type table of PERIOD .
  types:
    TIME_PAY_RESULTS_T type table of /SEW/TIME_PAY_RESULTS .
  types:
    SEQNR_T type table of CDSEQ .

  constants DELETE type CHAR20 value 'DELETE' ##NO_TEXT.
  constants MERGE type CHAR20 value 'MERGE' ##NO_TEXT.
  constants GLB_TIMESTATEMENT type CHAR40 value 'GLB_TIMESTATEMENT' ##NO_TEXT.
  constants GLB_PAYSLIP type CHAR40 value 'GLB_PAYSLIP' ##NO_TEXT.
  constants REM_STATEMENT type CHAR1 value 'R' ##NO_TEXT.
  constants TIME_STATEMENT type CHAR1 value 'T' ##NO_TEXT.

  class-methods BUILD_XML
    importing
      !RESULTS type /SEW/CL_FORMS_UTILS=>TIME_PAY_RESULTS_T
    returning
      value(XSTRING) type XSTRING .
  class-methods CLASS_CONSTRUCTOR .
  class-methods CONVERT_DATE_ORACLE
    importing
      !DATE type DATUM
    returning
      value(TRANSFORMED_DATE) type CHAR10 .
  class-methods CONVERT_DATE_ORACLE_TO_SAP
    importing
      !ORACLE_DATE type CHAR10
    returning
      value(DATE) type DATUM .
  class-methods DETERMINE_FORM_NAME
    importing
      !PERNR type PERNR_D
      !BEGDA type DATS
      !ENDDA type DATS
      !RCLAS type HRF_REP_CLASS
      !OTYPE type /SEW/INT_FO_AEUP-OTYPE optional
    exporting
      !FORM_NAME type HRF_INFONET .
  class-methods DOWNLOAD_FILES
    importing
      !AL11 type BOOLEAN default ABAP_TRUE
      !OBJECT_TYPE type CHAR1
      !FILES type /IWBEP/T_MGW_NAME_VALUE_PAIR optional
      !MOLGA type RSDSSELOPT_T optional .
  class-methods GENERATE_DOCUMENT_NAME
    importing
      !PERNR type /SEW/INT_FO_AEUP-PERNR
      !BEGDA type /SEW/INT_FO_AEUP-BEGDA
      !ENDDA type /SEW/INT_FO_AEUP-ENDDA
      !OTYPE type CHAR1
    returning
      value(DOCUMENT_NAME) type STRING .
  class-methods GENERATE_FILE_NAME
    importing
      !PERNR type /SEW/INT_FO_AEUP-PERNR
      !BEGDA type /SEW/INT_FO_AEUP-BEGDA
      !OTYPE type CHAR1
    returning
      value(DOCUMENT_NAME) type STRING .
  class-methods GENERATE_REM_DOCUMENT_CODE
    importing
      !PERIOD type DATS
      !PERNR type PERNR_D
    returning
      value(DOCUMENT_CODE) type CHAR70 .
  class-methods GENERATE_REM_DOCUMENT_NAME
    importing
      !PERIOD type CHAR6
    returning
      value(DOCUMENT_NAME) type CHAR40 .
  class-methods GENERATE_TIME_DOCUMENT_CODE
    importing
      !PERIOD type DATS
      !PERNR type PERNR_D
    returning
      value(DOCUMENT_CODE) type CHAR70 .
  class-methods GENERATE_TIME_DOCUMENT_NAME
    importing
      !PERIOD type FAPER
    returning
      value(DOCUMENT_NAME) type CHAR40 .
  class-methods GET_COUNTRY_BY_MOLGA
    importing
      !MOLGA type MOLGA
    returning
      value(COUNTRY) type CHAR2 .
  class-methods GET_MOLGA
    importing
      !PERNR type PERNR_D
    returning
      value(MOLGA) type MOLGA .
  class-methods GET_ORACLEPERNR
    importing
      !PERNR type PERNR_D
    returning
      value(ORACLEPERNR) type /SEW/DD_OBJECTNUMBER .
  class-methods GET_PERIODS
    importing
      !BEGDA type DATS
      !ENDDA type DATS
    exporting
      !PERIODS type /SEW/CL_FORMS_UTILS=>PERIODS_T .
  class-methods GET_RAW_PAYROLLRESULTS_LIST
    importing
      !PERNR type PERNR_D
    returning
      value(RGDIR) type HRPY_TT_RGDIR .
  class-methods GET_REM_DATA
    importing
      !INT_FO_AEUP type /SEW/INT_FO_AEUP
    returning
      value(DATA) type STRING .
  class-methods GET_REM_METADATA
    returning
      value(METADATA) type STRING .
  class-methods GET_REM_PDF_DATA
    importing
      !BEGDA type DATS
      !ENDDA type DATS
      !ORACLEPERNR type /SEW/DD_OBJECTNUMBER
      !PERNR type PERNR_D
    exporting
      !STATUS type SUBRC
      !PAYRESULT type /SEW/TIME_PAY_RESULTS .
  class-methods GET_SEQNR_BY_PERNR
    importing
      !PERNR type PERNR_D
      !BEGDA type DATS
      !ENDDA type DATS
    exporting
      !SEQNRS type /SEW/CL_FORMS_UTILS=>SEQNR_T .
  class-methods GET_TIMESTMT_PERIODS
    importing
      !PERNR type PERNR_D
      !BEGDA type DATS
      !ENDDA type DATS
    exporting
      !PERIODS type /SEW/CL_FORMS_UTILS=>PERIODS_T .
  class-methods GET_TIME_DATA
    importing
      !INT_FO_AEUP type /SEW/INT_FO_AEUP
      !DEL type BOOLEAN optional
    returning
      value(DATA) type STRING .
  class-methods GET_TIME_METADATA
    returning
      value(METADATA) type STRING .
  class-methods GET_TIME_PDF_DATA
    importing
      !BEGDA type DATS
      !ENDDA type DATS
      !ORACLEPERNR type /SEW/DD_OBJECTNUMBER
      !PERNR type PERNR_D
    exporting
      !TIMERESULT type /SEW/TIME_PAY_RESULTS
      !STATUS type SUBRC .
  class-methods GET_ZTART_TEXT
    importing
      !ZTART type PT_ZTART
      !SPRSL type SPRSL optional
    returning
      value(ZTEXT) type TXTLZ .
  class-methods READ_INFOTYPE_RECORD
    importing
      !PERNR type PERNR_D
      !INFTY type INFTY
      !OBJPS type OBJPS default SPACE
      !READ_MODE type HRPAD_READ_MODE default IF_HRPA_READ_INFOTYPE=>FIRST_INTERSECTING_RECORD
      !SPRPS type SPRPS default IF_HRPA_READ_INFOTYPE=>UNLOCKED
      !SUBTY type SUBTY default SPACE
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !NO_AUTH_CHECK type BOOLE_D default SPACE
    exporting
      !PNNNN type ANY
      !DATA_EXISTS type BOOLE_D .
  class-methods TEST_DATA_AND_XML
    importing
      !DOCUMENT_TYPE type CHAR1
    exporting
      !ZIP_XSTRING type XSTRING
      !STATUS type /SEW/DD_STATUS .
protected section.
private section.

  class-data GO_READ_INFOTYPE type ref to IF_HRPA_READ_INFOTYPE .
ENDCLASS.



CLASS /SEW/CL_FORMS_UTILS IMPLEMENTATION.


  METHOD build_xml.
    DATA: l_results         TYPE /sew/cl_forms_utils=>time_pay_results_t,
          xml_document_node TYPE REF TO if_ixml_node,
          single_result     TYPE /sew/time_pay_results.
    DATA(xml) = NEW /sew/cl_int_xml_up( ).
    xml->create_initial_xml( IMPORTING xml_document = DATA(xml_document) xml_data_node = DATA(xml_node) ). "erstellt Abstraction node u. basic xml doc.


    LOOP AT results ASSIGNING FIELD-SYMBOL(<line>).
      DATA(xml_single_node) = xml_document->create_element( name = 'Document' ).
      DATA(xml_abstraction_node) = xml_node.
      xml_node->append_child( new_child = xml_single_node ).
      " create the 'Document' node
      xml_node = xml_single_node. " xml_node -> Document node becomes xml_node


      xml_single_node = xml_document->create_element( name = 'Pernr' ).
      xml_single_node->set_value( value = CONV #( <line>-pernr ) ).
      xml_node->append_child( new_child = xml_single_node ). " append pernr node to document

      xml_single_node = xml_document->create_element( name = 'WorkerID' ).
      xml_single_node->set_value( value = CONV #( <line>-oraclepernr ) ).
      xml_node->append_child( new_child = xml_single_node ).

      xml_single_node = xml_document->create_element( name = 'Action' ).
      xml_single_node->set_value( value = CONV #( <line>-action ) ).
      xml_node->append_child( new_child = xml_single_node ).

      xml_single_node = xml_document->create_element( name = 'Type' ).
      xml_single_node->set_value( value = CONV #( <line>-type ) ).
      xml_node->append_child( new_child = xml_single_node ).

      xml_single_node = xml_document->create_element( name = 'Country' ).
      xml_single_node->set_value( value = CONV #( <line>-country ) ).
      xml_node->append_child( new_child = xml_single_node ).

      xml_single_node = xml_document->create_element( name = 'DocumentName' ).
      xml_single_node->set_value( value = CONV #( <line>-document_name ) ).
      xml_node->append_child( new_child = xml_single_node ).

      xml_single_node = xml_document->create_element( name = 'DocumentNumber' ).
      xml_single_node->set_value( value = CONV #( <line>-document_no ) ).
      xml_node->append_child( new_child = xml_single_node ).

      xml_single_node = xml_document->create_element( name = 'DocumentCode' ).
      xml_single_node->set_value( value = CONV #( <line>-document_code ) ).
      xml_node->append_child( new_child = xml_single_node ).

      xml_single_node = xml_document->create_element( name = 'DateFrom' ).
      xml_single_node->set_value( value = CONV #( <line>-date_from ) ).
      xml_node->append_child( new_child = xml_single_node ).

      xml_single_node = xml_document->create_element( name = 'DateTo' ).
      xml_single_node->set_value( value = CONV #( <line>-date_to ) ).
      xml_node->append_child( new_child = xml_single_node ).

      xml_single_node = xml_document->create_element( name = 'XString' ).
      xml_single_node->set_value( value = CONV #( <line>-xstring ) ).
      xml_node->append_child( new_child = xml_single_node ).


      CLEAR xml_node.
      xml_node = xml_abstraction_node. " set abstraction node as xml_node -> next iteration, append document node to abstraction node



    ENDLOOP.

    xstring = xml->create_xstring_from_xml( xml = xml_document ).

  ENDMETHOD.


  METHOD class_constructor.
    "instantiate infotype reader class
    CALL METHOD cl_hrpa_read_infotype=>get_instance
      IMPORTING
        infotype_reader = go_read_infotype.

  ENDMETHOD.


  METHOD convert_date_oracle.
    transformed_date = date+0(4) && '/' && date+4(2) && '/' && date+6(2).
  ENDMETHOD.


  method CONVERT_DATE_ORACLE_TO_SAP.

    CONCATENATE oracle_date+0(4) oracle_date+5(2) oracle_date+8(2) INTO DATA(tmp_date).
    date = CONV #( tmp_date ).

  endmethod.


  METHOD determine_form_name.
    DATA: ls_pmehf TYPE pmehf,
          ls_p0001 TYPE p0001.


    "IFT20211119 C

    CALL METHOD /sew/cl_forms_utils=>read_infotype_record
      EXPORTING
        pernr         = pernr
        infty         = '0001'
        begda         = begda
        endda         = endda
        no_auth_check = abap_true
      IMPORTING
        pnnnn         = ls_p0001.

    ls_pmehf-bukrs = ls_p0001-bukrs.
    ls_pmehf-werks = ls_p0001-werks.
    ls_pmehf-btrtl = ls_p0001-btrtl.
    ls_pmehf-persg = ls_p0001-persg.
    ls_pmehf-persk = ls_p0001-persk.
    ls_pmehf-abkrs = ls_p0001-abkrs.
    ls_pmehf-kostl = ls_p0001-kostl.
    ls_pmehf-juper = ls_p0001-juper.
    ls_pmehf-vdsk1 = ls_p0001-vdsk1.
    ls_pmehf-gsber = ls_p0001-gsber.
    ls_pmehf-rclas = rclas.
    ls_pmehf-molga = /sew/cl_forms_utils=>get_molga( pernr ).

    DATA(tmp_feature) = 'HRFOR'. "IFT20211207 C


    CALL METHOD cl_hrpa_feature=>get_value
      EXPORTING
        feature       = tmp_feature "IFT20211119 C
        struc_content = ls_pmehf
      IMPORTING
        return_value  = form_name.

  ENDMETHOD.


  METHOD download_files.

    DATA: zip_tab     TYPE swxmlcont,
          zip         TYPE  REF TO cl_abap_zip,
          zip_file    TYPE string VALUE 'Worker_JP.zip',
          filename    TYPE string,
          pdf_tab     TYPE TABLE OF tline,
          pdf_file    TYPE string,
          content_s   TYPE string,
          pdf_xstring TYPE xstring.

  ENDMETHOD.


METHOD generate_document_name.
**IFT20211119 start insert
*
  CASE otype.
    WHEN 'T'.
      CONCATENATE 'TimeStatement '
                  pernr
                  begda+0(4)
                  begda+4(2)
                  INTO document_name SEPARATED BY '_'.
    WHEN 'R'.

      DATA(begda_tmp) = begda+6(2) &&
                        '.'        &&
                        begda+4(2) &&
                        '.'        &&
                        begda+0(4).
      DATA(endda_tmp) =  endda+6(2) &&
                         '.'        &&
                         endda+4(2) &&
                         '.'        &&
                         endda+0(4).

      DATA(period_date) = '('       &&
                          begda_tmp &&
                          space     &&
                          '-'       &&
                          endda_tmp &&
                          space     &&
                          ')'.

      DATA(period) = begda+0(4) &&
                     '-'        &&
                     begda+4(2).
      CONCATENATE period
                  'Payslip'
                  period_date
                  INTO document_name
                  SEPARATED BY space.


  ENDCASE.
*IFT20211119 end insert
**IFT20211119 start delete
*
*  document_name = SWITCH #( otype
*                            WHEN 'T' THEN 'TimeStatement '
*                            WHEN 'R' THEN 'ThirdPartyPayslip ' ).
*
*  document_name = document_name &&
*                  pernr         &&
*                  '_'           &&
*                  begda+0(4)    &&
*                  '_'           &&
*                  begda+4(2).
*IFT20211119 end delete
ENDMETHOD.


METHOD GENERATE_FILE_NAME.

  document_name = SWITCH #( otype
                            WHEN 'T' THEN 'TimeStatement'
                            WHEN 'R' THEN 'Payslip' ).

  document_name = document_name &&
                  '_'           &&
                  pernr         &&
                  '_'           &&
                  begda+0(4)    &&
                  '_'           &&
                  begda+4(2)    &&
                  '.pdf'.
ENDMETHOD.


  METHOD generate_rem_document_code.

    DATA: doc_name    TYPE char40,
          month       TYPE char10,
          year        TYPE char4,
          l_month     TYPE char10,
          l_statement TYPE char40.


    SELECT ltx FROM t247 INTO @l_month WHERE spras = 'E' AND mnr = @period+4(2).
      month = l_month. " translates 01 to 'January' etc.
    ENDSELECT.


    IF sy-subrc = 0.
      year = period+0(4).
      document_code = 'PayStatement' && year && month && pernr.
    ENDIF.
  ENDMETHOD.


  METHOD generate_rem_document_name.

    DATA: doc_name TYPE char40,
          month    TYPE char10,
          year     TYPE char4,
          l_month  TYPE char10.


    SELECT ltx FROM t247 INTO @l_month WHERE spras = 'E' AND mnr = @period+4(2).
      month = l_month. " translates 01 to 'January' etc.
    ENDSELECT.

    IF sy-subrc EQ 0.
      year = period+0(4).
      document_name = 'Pay Statement' && ` ` && month && ` ` && year.
    ENDIF.
  ENDMETHOD.


  method GENERATE_TIME_DOCUMENT_CODE.

        DATA: doc_name TYPE char40,
          month    TYPE char10,
          year     TYPE char4,
          l_month  TYPE char10,
          l_statement TYPE char40.


    SELECT ltx FROM t247 INTO @l_month WHERE spras = 'E' AND mnr = @period+4(2).
      month = l_month. " translates 01 to 'January' etc.
    ENDSELECT.


    IF sy-subrc = 0.
      year = period+0(4).
      document_code = 'TimeStatement' && year && month && pernr.
    ENDIF.
  endmethod.


  METHOD generate_time_document_name.

    DATA: doc_name TYPE char40,
          month    TYPE char10,
          year     TYPE char4,
          l_month  TYPE char10.


    SELECT ltx FROM t247 INTO @l_month WHERE spras = 'E' AND mnr = @period+4(2).
      month = l_month. " translates 01 to 'January' etc.
    ENDSELECT.


    IF sy-subrc = 0.
      year = period+0(4).
      document_name = 'Time Statement' && ` ` && month && ` ` && year.
    ENDIF.

  ENDMETHOD.


  METHOD get_country_by_molga.

    SELECT SINGLE intca FROM t500l WHERE molga EQ @molga INTO @country.

    IF country IS INITIAL.
      " TODO
    ENDIF.

  ENDMETHOD.


  METHOD get_molga.

    DATA: lo_hrpa_molga    TYPE REF TO cl_hrpa_molga,
          lo_read_infotype TYPE REF TO if_hrpa_read_infotype.

    CLEAR molga.

    TRY.
        CALL METHOD cl_hrpa_read_infotype=>get_instance
          IMPORTING
            infotype_reader = lo_read_infotype.

        CREATE OBJECT lo_hrpa_molga                          "MELN2872496
          EXPORTING
            read_infotype = lo_read_infotype.

        molga = lo_hrpa_molga->read_molga_by_pernr( tclas = cl_hrpa_tclas=>tclas_employee
                                                       pernr = pernr ).

      CATCH cx_hrpa_violated_assertion.

    ENDTRY.
  ENDMETHOD.


  METHOD get_oraclepernr.

    SELECT SINGLE oraclepernr FROM pa9400 WHERE pernr = @pernr INTO @oraclepernr.

    IF sy-subrc NE 0.
      " error occured -> no oraclepernr for sap pernr found
      " TODO
    ENDIF.

  ENDMETHOD.


  METHOD get_periods.

    DATA: nperiods   TYPE i,
          years      TYPE i,
          l_begda    TYPE dats,
          l_endda    TYPE dats,
          lt_periods TYPE /sew/cl_forms_utils=>periods_t,
          l_periods  TYPE period.


    IF endda IS NOT INITIAL.
      l_begda = begda.
      l_endda = endda.
    ELSE.
      l_begda = begda.
      l_endda = sy-datum.
    ENDIF.

                                                            " 20210101
    IF l_begda+4(2) NE l_endda+4(2). " month is NE
      IF l_begda(4) = l_endda(4).
        nperiods    =  l_endda+4(2) - l_begda+4(2)  + 1.
      ELSE.
        nperiods    =  l_endda+4(2) - l_begda+4(2)  + 1.
        years       =  l_endda(4) - l_begda(4).
        nperiods    = nperiods + ( years * 12 ).
      ENDIF.
    ELSE. " month is EQ
      IF l_begda(4) = l_endda(4).
        nperiods    = 1.
      ELSE.
        years       = l_endda+0(4) - l_begda+0(4).
        nperiods    = ( years * 12 ) + 1.
      ENDIF.
    ENDIF.
    " calculate the number of periods



    " if year is NE
    IF l_begda+0(4) NE l_endda+0(4).
      IF l_begda+4(2) EQ '12'. " if it is december
        CALL FUNCTION 'OIL_MONTH_GET_FIRST_LAST'
          EXPORTING
            i_date      = l_begda
          IMPORTING
            e_first_day = l_periods-datuv
            e_last_day  = l_periods-datub.
        APPEND l_periods TO lt_periods.
        ADD 1 TO l_begda+0(4).
        l_begda+4(2) = 01.
        l_begda+6(2) = 01.
        " change date to 01.01.nextyear
        nperiods     = nperiods - 1.

      ENDIF.
      DO nperiods TIMES.
        IF l_begda+4(2) EQ '12'.

          CALL FUNCTION 'OIL_MONTH_GET_FIRST_LAST'
            EXPORTING
              i_date      = l_begda
            IMPORTING
              e_first_day = l_periods-datuv
              e_last_day  = l_periods-datub.
          APPEND l_periods TO lt_periods.
          ADD 1 TO l_begda+0(4).
          l_begda+4(2)    = 01.
          l_begda+6(2)    = 01.
          " change date to 01.01.nextyear
          nperiods        = nperiods - 1.
        ELSE.
          CALL FUNCTION 'OIL_MONTH_GET_FIRST_LAST'
            EXPORTING
              i_date      = l_begda
            IMPORTING
              e_first_day = l_periods-datuv
              e_last_day  = l_periods-datub.
          APPEND l_periods TO lt_periods.
          ADD 1 TO l_begda+4(2).
        ENDIF.
      ENDDO.
    ELSE.
      DO nperiods TIMES.
        CALL FUNCTION 'OIL_MONTH_GET_FIRST_LAST'
          EXPORTING
            i_date      = l_begda
          IMPORTING
            e_first_day = l_periods-datuv
            e_last_day  = l_periods-datub.
        APPEND l_periods TO lt_periods.
        ADD 1 TO l_begda+4(2).
      ENDDO.
    ENDIF.

    periods = lt_periods.
  ENDMETHOD.


  METHOD get_raw_payrollresults_list.

    DATA: lo_payslip_helper TYPE REF TO cl_hrxss_rem_helper,
          lt_rgdir          TYPE hrpy_tt_rgdir.

    CLEAR rgdir.

    CREATE OBJECT lo_payslip_helper
      EXPORTING
        iv_pernr = pernr.

    lt_rgdir = lo_payslip_helper->get_filtered_rgdir( ).

    rgdir = lt_rgdir.

  ENDMETHOD.


  METHOD get_rem_data.

    DATA: dor           TYPE string,
          da            TYPE string,
          document_code TYPE char6,
          document_name TYPE char40,
          file_name     TYPE char40.

    DATA(oraclepernr) = /sew/cl_forms_utils=>get_oraclepernr( int_fo_aeup-pernr ).
    DATA(molga)       = /sew/cl_forms_utils=>get_molga( int_fo_aeup-pernr ).
    DATA(date_from)   = /sew/cl_forms_utils=>convert_date_oracle( int_fo_aeup-begda ).
    DATA(date_to)     = /sew/cl_forms_utils=>convert_date_oracle( int_fo_aeup-endda ).

    document_code = int_fo_aeup-begda+0(6).
    document_name = /sew/cl_forms_utils=>generate_document_name( pernr = int_fo_aeup-pernr
                                                                 begda = int_fo_aeup-begda
                                                                 endda = int_fo_aeup-endda "IFT20211119 I
                                                                 otype = int_fo_aeup-otype ).

    file_name     = /sew/cl_forms_utils=>generate_file_name( pernr = int_fo_aeup-pernr
                                                             begda = int_fo_aeup-begda
                                                             otype  = int_fo_aeup-otype ).
    DATA(documenttype) = SWITCH string( int_fo_aeup-otype
                                        WHEN 'T' THEN 'Time Statement'
                                        WHEN 'R' THEN 'Payslip' ).

    CONCATENATE /sew/cl_forms_utils=>merge
                'DocumentsOfRecord'
                oraclepernr
                documenttype
                ''
                document_code
                document_name
                ''
                date_from
                date_to
                ''
                ''
                ''
                ''
                ''
                ''
                ''
                ''
                ''
                ''
                INTO dor SEPARATED BY '|'.

    CONCATENATE /sew/cl_forms_utils=>merge
                'DocumentAttachment'
                oraclepernr
                documenttype
                ''
                document_code
                'FILE'
                document_name
                file_name
                file_name
                INTO da SEPARATED BY '|'.

    CONCATENATE dor
                da
                INTO data SEPARATED BY cl_abap_char_utilities=>newline.
  ENDMETHOD.


  METHOD get_rem_metadata.

    CONCATENATE 'METADATA|DocumentsOfRecord|PersonNumber|DocumentType|Country|DocumentCode|DocumentName|DocumentNumber|DateFrom|DateTo|'
            'IssuingAuthority|IssuedDate|IssuingCountry|IssuingLocation|Comments|Publish|PublishDate|Status|VerifiedBy|VerifiedDate'
            cl_abap_char_utilities=>newline
            'METADATA|DocumentAttachment|PersonNumber|DocumentType|Country|DocumentCode|DataTypeCode|Title|URLorTextorFileName|File'
            INTO metadata.
  ENDMETHOD.


METHOD get_rem_pdf_data.

  DATA: molga          TYPE molga,
        form_name      TYPE hrf_name,
        payslip_helper TYPE REF TO cl_hrxss_rem_helper,
        l_payresult    TYPE /sew/time_pay_results,
        xstring        TYPE xstring,
        lt_rgdir       TYPE STANDARD TABLE OF pc261,
        lt_seqnr       TYPE /sew/cl_forms_utils=>seqnr_t,
        country_iso    TYPE char2,
        l_period       TYPE char6.

  CLEAR: lt_rgdir,  payresult.


  TRY.
      molga = /sew/cl_forms_utils=>get_molga( pernr ).
      country_iso = /sew/cl_forms_utils=>get_country_by_molga( molga ).

      IF lt_rgdir IS INITIAL.
        lt_rgdir  = /sew/cl_forms_utils=>get_raw_payrollresults_list( pernr ).
      ENDIF.

      IF lt_rgdir IS NOT INITIAL.

        CALL METHOD get_seqnr_by_pernr
          EXPORTING
            pernr  = pernr
            begda  = begda
            endda  = endda
          IMPORTING
            seqnrs = lt_seqnr.

        CREATE OBJECT payslip_helper
          EXPORTING
            iv_pernr = pernr.

        IF lt_seqnr IS NOT INITIAL.
          LOOP AT lt_seqnr ASSIGNING FIELD-SYMBOL(<single_seqnr>).
            READ TABLE lt_rgdir WITH KEY seqnr = <single_seqnr>
                                         fpend = endda
                                         fpbeg = begda
                                         ASSIGNING FIELD-SYMBOL(<ls_rgdir>).
            CHECK sy-subrc EQ 0. " sequenznummer nicht in lt_rgdir enthalten oder fehler passiert


              CALL METHOD determine_form_name
                EXPORTING
                  pernr     = pernr
                  begda     = <ls_rgdir>-ipend
                  endda     = <ls_rgdir>-ipend
                  rclas     = 'CESS'
*                  otype     = 'R' " IFT20211207 D
                IMPORTING
                  form_name = form_name.

            CALL METHOD payslip_helper->get_payslip
              EXPORTING
                is_rgdir                   = <ls_rgdir>
                iv_form_name               = form_name
              IMPORTING
                ev_document                = xstring
              EXCEPTIONS
                ex_payslip_creation_failed = 1.

            IF xstring IS NOT INITIAL AND sy-subrc IS INITIAL.
              l_payresult-xstring = xstring.
              l_payresult-status = '01'.
            ELSE.
              l_payresult-status = '03'.
            ENDIF.
            l_payresult-pernr         = pernr.
            l_payresult-oraclepernr   = oraclepernr.
            l_payresult-action        = /sew/cl_forms_utils=>delete.
            l_payresult-type          = /sew/cl_forms_utils=>glb_payslip.
            l_payresult-country       = country_iso.

            l_period = begda+0(6).

            l_payresult-document_name = generate_rem_document_name( l_period ).
            l_payresult-document_no   = convert_date_oracle( date = begda ).
            l_payresult-date_from     = convert_date_oracle( date = begda ).
            l_payresult-date_to       = convert_date_oracle( date = endda ).

            l_payresult-document_code = generate_rem_document_code( pernr  = pernr
                                                                    period = begda ).

            l_payresult-otype         = /sew/cl_forms_utils=>rem_statement.

            payresult                 = l_payresult.

            CLEAR: l_payresult, l_period.

          ENDLOOP.
        ENDIF.
      ENDIF.
  ENDTRY.

ENDMETHOD.


  METHOD get_seqnr_by_pernr.


    DATA: lt_rgdir TYPE STANDARD TABLE OF pc261.


    IF lt_rgdir IS INITIAL.
      lt_rgdir = /sew/cl_forms_utils=>get_raw_payrollresults_list( pernr ).
    ENDIF.

    DATA(period_range) = VALUE rsdsselopt_t( ( sign   = 'I'
                                               option = 'BT'
                                               low    = begda
                                               high   = endda ) ).

    LOOP AT lt_rgdir ASSIGNING FIELD-SYMBOL(<rgdir_line>).
      IF <rgdir_line>-fpbeg IN period_range AND
         <rgdir_line>-fpend IN period_range.

        APPEND <rgdir_line>-seqnr TO seqnrs.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_timestmt_periods.
    DATA: lt_periods                 TYPE hrxss_tim_per_periods_t,
          lo_badi_inst_timestatement TYPE REF TO hress_wda_time_statemt_config,
          l_period                   TYPE period,
          start_date                 TYPE dats,
          end_date                   TYPE dats.

    FIELD-SYMBOLS: <l_period> TYPE hrxss_tim_per_periods.

    start_date = begda.
    end_date = endda.

    TRY.
        GET BADI lo_badi_inst_timestatement.
      CATCH cx_badi.
        CLEAR lo_badi_inst_timestatement.
    ENDTRY.
    TRY.
        CALL BADI lo_badi_inst_timestatement->provide_period_table
          EXPORTING
            iv_pernr   = pernr
          IMPORTING
            ex_periods = lt_periods.
      CATCH cx_badi_initial_reference
        cx_sy_dyn_call_illegal_method.
    ENDTRY.

    LOOP AT lt_periods ASSIGNING <l_period> WHERE begda >= start_date AND
                                                  endda <= end_date.
      IF <l_period>-begda LE endda AND <l_period>-endda GT begda.
        l_period-datuv = <l_period>-begda.
        l_period-datub = <l_period>-endda.
        APPEND l_period TO periods.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


METHOD get_time_data.

  DATA: dor           TYPE string,
        da            TYPE string,
        document_code TYPE char6,
        document_name TYPE char40,
        file_name     TYPE char40.

  DATA(oraclepernr) = /sew/cl_forms_utils=>get_oraclepernr( int_fo_aeup-pernr ).
  DATA(molga)       = /sew/cl_forms_utils=>get_molga( int_fo_aeup-pernr ).
  DATA(date_from)   = /sew/cl_forms_utils=>convert_date_oracle( int_fo_aeup-begda ).
  DATA(date_to)     = /sew/cl_forms_utils=>convert_date_oracle( int_fo_aeup-endda ).

  document_code = int_fo_aeup-begda+0(6).
  document_name = /sew/cl_forms_utils=>generate_document_name( pernr = int_fo_aeup-pernr
                                                               begda = int_fo_aeup-begda
                                                               endda = int_fo_aeup-endda "IFT20211119 I
                                                               otype = int_fo_aeup-otype ).

  file_name     = /sew/cl_forms_utils=>generate_file_name( pernr = int_fo_aeup-pernr
                                                           begda = int_fo_aeup-begda
                                                           otype = int_fo_aeup-otype ).
  DATA(documenttype) = SWITCH string( int_fo_aeup-otype
                                      WHEN 'T' THEN 'Time Statement'
                                      WHEN 'R' THEN 'Payslip' ).

**JMB20211025 start insert - check for deletion flag
*
  DATA(operation) = COND string( WHEN del EQ abap_true
                                 THEN /sew/cl_forms_utils=>delete
                                 ELSE /sew/cl_forms_utils=>merge ).
*JMB20211025 insert end

  CONCATENATE operation
              'DocumentsOfRecord'
              oraclepernr
              documenttype
              ''
              document_code
              document_name
              ''
              date_from
              date_to
              ''
              ''
              ''
              ''
              ''
              ''
              ''
              ''
              ''
              ''
              INTO dor SEPARATED BY '|'.

  IF del IS INITIAL. "JMB20211025 I
    CLEAR: da.
    CONCATENATE /sew/cl_forms_utils=>merge
                'DocumentAttachment'
                oraclepernr
                documenttype
                ''
                document_code
                'FILE'
                document_name
                file_name
                file_name
                INTO da SEPARATED BY '|'.
  ENDIF.

  CONCATENATE dor
              da
              INTO data SEPARATED BY cl_abap_char_utilities=>newline.
ENDMETHOD.


  METHOD GET_TIME_METADATA.

    CONCATENATE 'METADATA|DocumentsOfRecord|PersonNumber|DocumentType|Country|DocumentCode|DocumentName|DocumentNumber|DateFrom|DateTo|'
                'IssuingAuthority|IssuedDate|IssuingCountry|IssuingLocation|Comments|Publish|PublishDate|Status|VerifiedBy|VerifiedDate'
                cl_abap_char_utilities=>newline
                'METADATA|DocumentAttachment|PersonNumber|DocumentType|Country|DocumentCode|DataTypeCode|Title|URLorTextorFileName|File'
                INTO metadata.

  ENDMETHOD.


  METHOD get_time_pdf_data.
    DATA: form_name    TYPE hrf_infonet,
          molga        TYPE molga,
          l_begda      TYPE dats,
          l_endda      TYPE dats,
          periods      TYPE /sew/cl_forms_utils=>periods_t,
          l_timeresult TYPE /sew/time_pay_results,
          l_period     TYPE char6,
          country_iso  TYPE char2,
          ld_return    TYPE bapireturn,
          xstring      TYPE xstring,
          message      TYPE bapiret2.


    molga       = /sew/cl_forms_utils=>get_molga( pernr ).
    country_iso = /sew/cl_forms_utils=>get_country_by_molga( molga ).



    CALL METHOD determine_form_name
      EXPORTING
        pernr     = pernr
        begda     = begda
        endda     = endda
        rclas     = 'TESP'
      IMPORTING
        form_name = form_name.

    CALL FUNCTION 'HRXSS_TIM_GET_TIMESTATEMENT'
      EXPORTING
        pernr       = pernr
        begin_date  = begda
        end_date    = endda
        hrform_name = form_name
      IMPORTING
        pdf_doc     = xstring
        message     = message.

    IF message IS NOT INITIAL.

    ENDIF.

    IF xstring IS NOT INITIAL.
      l_timeresult-xstring     = xstring.
      l_timeresult-status      = '01'.
    ELSE.
      l_timeresult-status      = '03'.
    ENDIF.

    l_timeresult-pernr         = pernr.
    l_timeresult-oraclepernr   = oraclepernr.
    l_timeresult-action        = /sew/cl_forms_utils=>delete.
    l_timeresult-type          = /sew/cl_forms_utils=>glb_timestatement.
    l_timeresult-country       = country_iso.

    l_period = begda+0(6).

    l_timeresult-document_name = generate_time_document_name( l_period ).
    l_timeresult-document_no   = convert_date_oracle( date = begda ).
    l_timeresult-date_from     = convert_date_oracle( date = begda ).
    l_timeresult-date_to       = convert_date_oracle( date = endda ).

    l_timeresult-document_code = generate_time_document_code( pernr  = pernr
                                                              period = begda ).
    l_timeresult-otype         = /sew/cl_forms_utils=>time_statement.

    timeresult = l_timeresult.

    CLEAR: l_timeresult, l_period.

  ENDMETHOD.


  METHOD get_ztart_text.

    DATA: sprsl_l TYPE sprsl.

    IF sprsl IS INITIAL.
      sprsl_l = 'DE'.
    ELSE.
      sprsl_l = sprsl.
    ENDIF.

    SELECT SINGLE ztext FROM t555b WHERE ztart = @ztart
                                   AND   sprsl = @sprsl_l
                                   INTO  @ztext.

  ENDMETHOD.


  METHOD read_infotype_record.
* instantiate infotype reader class


    DATA: lr_pnnnn TYPE REF TO data,
          lr_descr TYPE REF TO cl_abap_typedescr.

    FIELD-SYMBOLS: <ls_pnnnn> TYPE any.


    CLEAR: pnnnn, data_exists.

    lr_descr = cl_abap_typedescr=>describe_by_data( pnnnn ).
    CREATE DATA lr_pnnnn TYPE (lr_descr->absolute_name).
    ASSIGN lr_pnnnn->* TO <ls_pnnnn>.

    CALL METHOD go_read_infotype->read_single
      EXPORTING
        tclas         = cl_hrpa_tclas=>tclas_employee
        pernr         = pernr
        infty         = infty
        subty         = subty
        objps         = objps
        sprps         = sprps
        begda         = begda
        endda         = endda
        mode          = read_mode
        no_auth_check = no_auth_check
      IMPORTING
        pnnnn         = <ls_pnnnn>
        data_exists   = data_exists.

    pnnnn = <ls_pnnnn>.
  ENDMETHOD.


  METHOD test_data_and_xml.


    DATA: is_aendup  TYPE /sew/int_it_aeup,
          up_success TYPE TABLE OF /sew/int_it_aeup,
          s0000      TYPE p0000,
          s0002      TYPE p0002,
          s0001      TYPE p0001,
          filename   TYPE string,
          message    TYPE bapiret1.


    SELECT SINGLE * FROM /sew/int_objects INTO @DATA(ls_object) WHERE object = 'P'.
    SELECT * FROM /sew/int_it_aeup INTO TABLE @DATA(changed_data) UP TO 5 ROWS
                                                                  WHERE status = '02'
                                                                  AND infty NE '2001'
                                                                  AND infty IS NOT NULL
                                                                  AND infty NE @space.

    DATA(changed_data_tmp) = changed_data.
    SORT changed_data_tmp ASCENDING BY pernr legal_entity infty timestamp.
    DELETE ADJACENT DUPLICATES FROM changed_data_tmp COMPARING pernr legal_entity.   "JMB20210914 D

    DATA(xml) = NEW /sew/cl_int_xml_up( ).
    xml->create_initial_xml( IMPORTING xml_document = DATA(new_xml)
                                       xml_data_node = DATA(xml_data_node) ).

    LOOP AT changed_data_tmp ASSIGNING FIELD-SYMBOL(<change>).

      DATA(changed_data_pernr) = VALUE /sew/cl_int_employee_xml_up=>t_itaendup( FOR     change IN changed_data
                                                                                WHERE ( pernr = <change>-pernr ) ( change ) ).

      xml->add_node_object_employee( EXPORTING object_id_cloud = CONV #( /sew/cl_int_employee_xml_up=>read_oracle_id( EXPORTING pernr   = <change>-pernr
                                                                                                                                date    = <change>-begda
                                                                                                                      IMPORTING message =  message ) )
                                               object_id_sap   = CONV #( <change>-pernr )
                                               xpath_oracle    = CONV #( ls_object-id_oracle )
                                               xpath_sap       = CONV #( ls_object-id_sap )
                                               xpath_object    = CONV #( ls_object-folder )
                                               xml_document    = new_xml
                                               aend_up         = VALUE #( changed_data_pernr[ infty = '' ] OPTIONAL )
                                     IMPORTING xml_node        = DATA(xml_node)
                                      CHANGING xml_data_node   = xml_data_node ).

      <change>-status = '02'.

      DELETE changed_data_pernr WHERE infty IS INITIAL.

      LOOP AT changed_data_pernr ASSIGNING FIELD-SYMBOL(<data_pernr>).

        DATA(data_pernr) = VALUE /sew/cl_int_employee_xml_up=>t_itaendup( ( <data_pernr> ) ).

**JMB20210907 start insert - set SEQNR according to INFTY
*
        DATA(seqnr) = SWITCH seqnr( <data_pernr>-infty
                                    WHEN '2001' THEN '002'
                                    WHEN '2006' THEN '004'
                                    ELSE '000' ).
*JMB20210907 end insert

        DATA(employee) = NEW /sew/cl_int_employee_xml_up( changed_data = data_pernr
                                                          pernr        = <data_pernr>-pernr
                                                          xml_node     = xml_node
                                                          xml_document = new_xml
                                                          seqnr        = seqnr ). "JMB20210907 I

        employee->read_molga( IMPORTING message = message ).
        IF message-type = /sew/cl_int_constants=>error.
          CONTINUE.
        ENDIF.

        employee->read_customizing( IMPORTING message = message ).
        IF message-type = /sew/cl_int_constants=>error.
          CONTINUE.
        ENDIF.

        employee->process_changed_data( IMPORTING message = message ).
        IF message-type = /sew/cl_int_constants=>error.
          CONTINUE.
        ENDIF.

        <data_pernr>-status = '02'. "JMB20211005 I

        employee->build_xml( ).
      ENDLOOP.
      APPEND LINES OF changed_data_pernr TO up_success.   "JMB20211005 I
    ENDLOOP.

    DATA(xstring) = xml->create_xstring_from_xml( xml = new_xml ).
    DATA(string) = xml->create_string_from_xstring( xstring = xstring ).
    zip_xstring = xstring.

    IF zip_xstring IS NOT INITIAL.
*      MODIFY /sew/int_it_aeup FROM TABLE changed_data_tmp. "JMB20211005 D
*      MODIFY /sew/int_it_aeup FROM TABLE up_success. "JMB20211005 I
*      COMMIT WORK.
    ENDIF.

    IF changed_data_tmp IS INITIAL.
      status = 'E'.
    ENDIF.

    cl_abap_browser=>show_xml(
      EXPORTING
      xml_string   =  string ).  " XML in String


  ENDMETHOD.
ENDCLASS.
