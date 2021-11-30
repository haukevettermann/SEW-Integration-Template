*&---------------------------------------------------------------------*
*& Report /SEW/RP_IT_AEND_POST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /sew/rp_it_aend_post_pt.

TABLES: pernr,
        /sew/int_it_aend.

NODES: peras.

DATA: gr_pernr         TYPE rsdsselopt_t,
      gr_pernr_it_aend TYPE rsdsselopt_t,
      gr_inftys        TYPE rsdsselopt_t.

INITIALIZATION.
  pnptimed = 'A'.

**---------------------------------------------------------------------*
* Selectionscreen
**---------------------------------------------------------------------*
  SELECTION-SCREEN BEGIN OF BLOCK sel_infty WITH FRAME TITLE TEXT-006.
  SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS pa_timte RADIOBUTTON GROUP r02.
  SELECTION-SCREEN COMMENT 4(26) TEXT-007.
  PARAMETERS pa_ptoth RADIOBUTTON GROUP r02.
  SELECTION-SCREEN COMMENT 34(26) TEXT-008.
  PARAMETERS pa_ptbot RADIOBUTTON GROUP r02 DEFAULT 'X'.
  SELECTION-SCREEN COMMENT 64(26) TEXT-009.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END OF BLOCK sel_infty.

  "selection of simulation and protocol display (only in dialog mode supplied)
  SELECTION-SCREEN BEGIN OF BLOCK sel_opt WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS so_stat FOR /sew/int_it_aend-status.
  SELECT-OPTIONS so_intr FOR /sew/int_it_aend-int_run.
  SELECT-OPTIONS so_cldid FOR /sew/int_it_aend-cloud_id.
  PARAMETERS: pa_simu TYPE flag DEFAULT abap_true.
  PARAMETERS: pa_alv  TYPE flag DEFAULT abap_true.
  SELECTION-SCREEN END OF BLOCK sel_opt.

START-OF-SELECTION.

  "Restric pernr selection in case of no selection
  IF pnppernr[] IS INITIAL.
    pnppernr[] = NEW /sew/cl_int_it_aend( )->get_peras( begda = pn-begda endda = pn-endda ).
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
  DATA(cl_it_aend_post_pt) = NEW /sew/cl_it_aend_post_pt( alv   = pa_alv
                                                          batch = sy-batch
                                                          simu  = pa_simu
                                                          dates = VALUE rsdsselopt_t( ( sign = 'I'
                                                                                      option = 'BT'
                                                                                         low = pn-begda
                                                                                        high = pn-endda ) ) ).

  "set date range
  cl_it_aend_post_pt->set_date_range( VALUE rsdsselopt_t( ( sign = 'I' option = 'BT' low = pn-begda high = pn-endda ) ) ).

  DATA(gr_intruns) = VALUE rsdsselopt_t( FOR intrun IN so_intr[] ( sign   = intrun-sign
                                                                   option = intrun-option
                                                                   low    = intrun-low
                                                                   high   = intrun-high ) ).

  DATA(gr_cloud_ids) = VALUE rsdsselopt_t( FOR cloud_id IN so_cldid[] ( sign   = cloud_id-sign
                                                                        option = cloud_id-option
                                                                        low    = cloud_id-low
                                                                        high   = cloud_id-high ) ).

  DATA(gr_status) = VALUE rsdsselopt_t( FOR stat IN so_stat[] ( sign   = so_stat-sign
                                                                option = so_stat-option
                                                                low    = so_stat-low
                                                                high   = so_stat-high ) ).

**JMB20211004 start insert - default status: Initial, error
*
  IF gr_status IS INITIAL.
    gr_status = value rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '01')
                                    ( sign = 'I' option = 'EQ' low = '03')
                                    ( sign = 'I' option = 'EQ' low = '10') ).
  ENDIF.
*JMB20211004 insert end

  "split infotypes for this report
  IF pa_timte EQ abap_true.
    gr_inftys = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '2011' high = '' ) ).
  ELSEIF pa_ptoth EQ abap_true.
    gr_inftys = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '2001' high = '' )
                                    ( sign = 'I' option = 'EQ' low = '2002' high = '' )
                                    ( sign = 'I' option = 'EQ' low = '2006' high = '' )
                                    ( sign = 'I' option = 'EQ' low = '2007' high = '' ) ).
  ELSE.
    gr_inftys = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '2011' high = '' )
                                    ( sign = 'I' option = 'EQ' low = '2001' high = '' )
                                    ( sign = 'I' option = 'EQ' low = '2002' high = '' )
                                    ( sign = 'I' option = 'EQ' low = '2006' high = '' )
                                    ( sign = 'I' option = 'EQ' low = '2007' high = '' ) ).
  ENDIF.



  "get corresponding request data
  cl_it_aend_post_pt->status_handler = NEW /sew/cl_int_status_handler( sap_id_rng   = gr_pernr
                                                                       infty_rng    = gr_inftys
                                                                       cloud_id_rng = gr_cloud_ids
                                                                       int_run_rng  = gr_intruns
                                                                       status_rng   = gr_status
                                                                       dates_rng    = VALUE rsdsselopt_t( ( sign = 'I'
                                                                                                          option = 'BT'
                                                                                                             low = pn-begda
                                                                                                            high = pn-endda ) )
                                                                        it_aend_only = abap_true
                                                                      ).

  "Sort IT_AEND for processing
  SORT cl_it_aend_post_pt->status_handler->it_aend
    BY int_run
       cloud_pernr
       infty
       begda
       endda
       timestamp ASCENDING.

  "start post
  cl_it_aend_post_pt->start_post( ).

  "get skipped pernrs according to auth check
  pnp_get_auth_skipped_pernrs cl_it_aend_post_pt->skipped_pernrs.

  "prepare protocol
  cl_it_aend_post_pt->prepare_protocol( ).
