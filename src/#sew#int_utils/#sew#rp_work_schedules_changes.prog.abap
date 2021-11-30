*&---------------------------------------------------------------------*
*& Report /SEW/RP_WORK_SCHEDULES_CHANGES
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /sew/rp_work_schedules_changes.

TABLES: pernr.

NODES: peras.

DATA: gr_pernr         TYPE rsdsselopt_t.

INITIALIZATION.
  pnptimed = 'A'.

**---------------------------------------------------------------------*
* Selectionscreen
**---------------------------------------------------------------------*
  "selection of status
  SELECTION-SCREEN BEGIN OF BLOCK sel_stat WITH FRAME TITLE TEXT-001.
  SELECTION-SCREEN COMMENT 1(79) TEXT-003.
  PARAMETERS: pa_schkz TYPE schkn.
  SELECTION-SCREEN END OF BLOCK sel_stat.

  "selection of simulation and protocol display (only in dialog mode supplied)
  SELECTION-SCREEN BEGIN OF BLOCK sel_opt WITH FRAME TITLE TEXT-002.

  PARAMETERS: pa_simu TYPE flag DEFAULT abap_true.
  SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS pa_alv  TYPE flag RADIOBUTTON GROUP r01 DEFAULT 'X'.
  SELECTION-SCREEN COMMENT 4(26) TEXT-004.
  PARAMETERS pa_xml TYPE flag RADIOBUTTON GROUP r01.
  SELECTION-SCREEN COMMENT 34(26) TEXT-005.

  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END OF BLOCK sel_opt.

START-OF-SELECTION.

  "clear range, so that get peras can fill the structure according auth check.
  REFRESH gr_pernr[].

GET peras.
  "collect all pernr
  APPEND VALUE rsdsselopt( sign = 'I' option = 'EQ' low = pernr-pernr ) TO gr_pernr.

END-OF-SELECTION.

  "instantiate class for report
  DATA(cl_work_schedule_changes) = NEW /sew/cl_work_schedule_changes( alv   = pa_alv
                                                      batch = sy-batch
                                                      simu = pa_simu
                                                      dates = VALUE rsdsselopt_t( ( sign = 'I'
                                                                                    option = 'BT'
                                                                                    low = pn-begda
                                                                                    high = pn-endda ) ) ).

  "basend on schkz
  IF pa_schkz IS NOT INITIAL.
    SELECT pernr FROM pa0007 INTO TABLE @DATA(pernr_tab) WHERE schkz = @pa_schkz
                                                           AND begda LE @pn-endda
                                                           AND endda GE @pn-begda.

    SELECT pernr FROM pa2003 INTO TABLE @DATA(pernr_tmp) WHERE schkz = @pa_schkz
                                                           AND begda LE @pn-endda
                                                           AND endda GE @pn-begda.

    APPEND LINES OF pernr_tmp TO pernr_tab.
    SORT pernr_tab.
    DELETE ADJACENT DUPLICATES FROM pernr_tab COMPARING pernr.

    REFRESH gr_pernr[]. " delete all entries in gr_pernr

    LOOP AT pernr_tab ASSIGNING FIELD-SYMBOL(<pernr_tab>).
      APPEND VALUE rsdsselopt( sign = 'I' option = 'EQ' low = <pernr_tab>-pernr ) TO gr_pernr. " fill gr_pernr
    ENDLOOP.
  ENDIF.

  "get work schedule times
  cl_work_schedule_changes->get_work_schedules_times( pernr_range = gr_pernr begda = pn-begda endda = pn-endda ).

  "create and store xml
  DATA(xml_string) = cl_work_schedule_changes->create_and_store_xml_file( ).

  "protocol
  IF pa_alv EQ abap_true.
    cl_work_schedule_changes->prepare_protocol( ).
  ELSEIF pa_xml EQ abap_true.
**--Displaying xml
    cl_abap_browser=>show_xml( EXPORTING xml_string   = CONV #( xml_string ) ).
  ENDIF.
