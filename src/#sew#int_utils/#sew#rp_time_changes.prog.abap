*&---------------------------------------------------------------------*
*& Report /SEW/RP_TIME_CHANGES
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /sew/rp_time_changes.

""" zwei sterne zusammen ein/auskommentieren """

TABLES: pernr
**        /sew/int_fo_aeup.
        .

NODES: peras.

DATA: gr_pernr         TYPE rsdsselopt_t,
      gr_pernr_it_aend TYPE rsdsselopt_t,
      gr_inftys        TYPE rsdsselopt_t.

INITIALIZATION.
  pnptimed = 'A'.

**---------------------------------------------------------------------*
* Selectionscreen
**---------------------------------------------------------------------*
  "selection of simulation and protocol display (only in dialog mode supplied)
  SELECTION-SCREEN BEGIN OF BLOCK sel_opt WITH FRAME TITLE TEXT-002.
  PARAMETERS: pa_simu  TYPE flag DEFAULT abap_true.
  PARAMETERS: pa_alv   TYPE flag DEFAULT abap_true.
  PARAMETERS: pa_force TYPE flag DEFAULT abap_false.
  SELECTION-SCREEN END OF BLOCK sel_opt.

START-OF-SELECTION.

  "clear range, so that get peras can fill the structure according auth check.
  REFRESH gr_pernr[].

GET peras.
  "collect all pernr
  APPEND VALUE rsdsselopt( sign = 'I' option = 'EQ' low = pernr-pernr ) TO gr_pernr.

END-OF-SELECTION.

  "instantiate class for report
  DATA(cl_time_changes) = NEW /sew/cl_time_changes( alv   = pa_alv
                                                      batch = sy-batch
                                                      simu  = pa_simu
                                                      dates = VALUE rsdsselopt_t( ( sign = 'I'
                                                                                    option = 'BT'
                                                                                    low = pn-begda
                                                                                    high = pn-endda ) ) ).

  IF pa_force NE 'X'.
    cl_time_changes->read_cluster_b2( EXPORTING
                                         begda    = pn-begda
                                         endda    = pn-endda
                                         ir_pernr = gr_pernr ).
  ELSE.
    cl_time_changes->read_cluster_b2_force( EXPORTING
                                              begda    = pn-begda
                                              endda    = pn-endda
                                              ir_pernr = gr_pernr ).
  ENDIF.

  "prepare protocol
  cl_time_changes->prepare_protocol( ).
