*&---------------------------------------------------------------------*
*& Report /SEW/RP_IT_AEND_POST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /sew/rp_it_aend_post.

TABLES: pernr,
        /sew/int_it_aend.

NODES: peras.

DATA: gr_pernr         TYPE rsdsselopt_t,
      gr_pernr_it_aend TYPE rsdsselopt_t.

INITIALIZATION.
  pnptimed = 'A'.

**---------------------------------------------------------------------*
* Selectionscreen
**---------------------------------------------------------------------*
  "selection of status
*SELECTION-SCREEN BEGIN OF BLOCK sel_stat WITH FRAME TITLE TEXT-001.
*SELECTION-SCREEN BEGIN OF LINE.
*PARAMETERS pa_both RADIOBUTTON GROUP r01 DEFAULT 'X'.
*SELECTION-SCREEN COMMENT 4(26) TEXT-003.
*PARAMETERS pa_init RADIOBUTTON GROUP r01.
*SELECTION-SCREEN COMMENT 34(15) TEXT-004.
*PARAMETERS pa_err  RADIOBUTTON GROUP r01.
*SELECTION-SCREEN COMMENT 53(15) TEXT-005.
*SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN END OF BLOCK sel_stat.

  "selection of simulation and protocol display (only in dialog mode supplied)
  SELECTION-SCREEN BEGIN OF BLOCK sel_opt WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS so_intr FOR /sew/int_it_aend-int_run.
  SELECT-OPTIONS so_cldid FOR /sew/int_it_aend-cloud_id.
  PARAMETERS: pa_simu TYPE flag DEFAULT abap_true.
  PARAMETERS: pa_alv  TYPE flag DEFAULT abap_true.
  SELECTION-SCREEN END OF BLOCK sel_opt.

START-OF-SELECTION.

  DATA(lo_it_aend) = NEW /sew/cl_int_it_aend( ).

  "Restric pernr selection in case of no selection
  IF pnppernr[] IS INITIAL.
*    pnppernr[] = lo_it_aend->get_peras( begda = pn-begda endda = pn-endda ).
  ENDIF.

  "no pernr was found in /SEW/INT_IT_AEND
  IF pnppernr[] IS INITIAL.
    STOP.
  ENDIF.

  "clear range, so that get peras can fill the structure according auth check.
  REFRESH gr_pernr[].

GET peras.
  "collect all pernr
  APPEND VALUE rsdsselopt( sign = 'I' option = 'EQ' low = pernr-pernr ) TO gr_pernr.

END-OF-SELECTION.

  "instantiate class for report
  DATA(it_aend_post) = NEW /sew/cl_it_aend_post( alv   = pa_alv
                                                 batch = sy-batch
                                                 simu  = pa_simu
                                                 dates = VALUE rsdsselopt_t( ( sign = 'I'
                                                                             option = 'BT'
                                                                                low = pn-begda
                                                                               high = pn-endda ) ) ).

  "Prepare range-tables
  "Exclude time infotypes
  DATA(inftys) = VALUE rsdsselopt_t( ( sign = 'I' option = 'NE' low = '2001' high = '' )
                                     ( sign = 'I' option = 'NE' low = '2002' high = '' )
                                     ( sign = 'I' option = 'NE' low = '2006' high = '' )
                                     ( sign = 'I' option = 'NE' low = '2007' high = '' )
                                     ( sign = 'I' option = 'NE' low = '2011' high = '' )
                                  ).


  DATA(intruns) = VALUE rsdsselopt_t( FOR intrun IN so_intr[] ( sign   = intrun-sign
                                                                option = intrun-option
                                                                low    = intrun-low
                                                                high   = intrun-high ) ).

  DATA(cloud_ids) = VALUE rsdsselopt_t( FOR cloud_id IN so_cldid[] ( sign   = cloud_id-sign
                                                                     option = cloud_id-option
                                                                     low    = cloud_id-low
                                                                     high   = cloud_id-high ) ).

  "get corresponding request data
  it_aend_post->status_handler = NEW /sew/cl_int_status_handler( sap_id_rng    = gr_pernr
                                                                  infty_rng    = inftys
                                                                  cloud_id_rng = cloud_ids
                                                                  int_run_rng  = intruns ).

  "Sort IT_AEND for processing
  SORT it_aend_post->status_handler->it_aend
    BY int_run
       cloud_pernr
       infty
       begda
       timestamp
       endda ASCENDING.

*  SORT it_aend_post->status_handler->it_aend
*    BY timestamp ASCENDING.

  "start post
*  IF /sew/cl_int_utility=>check_for_om_error( ) = abap_false.
*  DATA(global_hr) = 'GLOB'.
*  SET PARAMETER ID 'GLOB' FIELD global_hr.
  it_aend_post->start_post( ).

  "get skipped pernrs according to auth check
  pnp_get_auth_skipped_pernrs it_aend_post->skipped_pernrs.

  "prepare protocol
  it_aend_post->prepare_protocol( ).
*  ELSE.
*    "Worker is not being executed because of initial or error entries for department/department tree bookings
*    MESSAGE TEXT-006 TYPE 'I' DISPLAY LIKE 'E'.
*  ENDIF.
