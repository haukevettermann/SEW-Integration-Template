*&---------------------------------------------------------------------*
*& Report /SEW/RP_FORMS_CHANGES
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /sew/rp_forms_changes.

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
  SELECTION-SCREEN BEGIN OF BLOCK sel_infty WITH FRAME TITLE TEXT-006.
  SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS pa_ftim RADIOBUTTON GROUP r02.
  SELECTION-SCREEN COMMENT 4(26) TEXT-007.
  PARAMETERS pa_fpay RADIOBUTTON GROUP r02.
  SELECTION-SCREEN COMMENT 34(26) TEXT-008.
  PARAMETERS pa_fboth RADIOBUTTON GROUP r02 DEFAULT 'X'. " pa_ftim, pa_fpay, pa_fboth
  SELECTION-SCREEN COMMENT 64(26) TEXT-009.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END OF BLOCK sel_infty.

  "selection of simulation and protocol display (only in dialog mode supplied)
  SELECTION-SCREEN BEGIN OF BLOCK sel_opt WITH FRAME TITLE TEXT-002.
  PARAMETERS: pa_simu TYPE flag DEFAULT abap_true.
  PARAMETERS: pa_alv  TYPE flag DEFAULT abap_true.
  PARAMETERS: pa_force TYPE flag DEFAULT abap_false.
  SELECTION-SCREEN END OF BLOCK sel_opt.

START-OF-SELECTION.

  "clear range, so that get peras can fill the structure according auth check.
  REFRESH gr_pernr[].

GET peras.
  "collect all pernr
  APPEND VALUE rsdsselopt( sign = 'I' option = 'EQ' low = pernr-pernr ) TO gr_pernr.

END-OF-SELECTION.

  DATA(cl_forms_changes) = NEW /sew/cl_forms_changes( alv      = pa_alv
                                                      batch    = sy-batch
                                                      simu     = pa_simu
                                                      dates    = VALUE rsdsselopt_t( ( sign   = 'I'
                                                                                       option = 'BT'
                                                                                       low    = pn-begda
                                                                                       high   = pn-endda ) ) ).


  cl_forms_changes->detect_changes( EXPORTING
                                      ir_pernr = gr_pernr
                                      begda    = pn-begda
                                      endda    = pn-endda
                                      pa_ftim  = pa_ftim
                                      pa_fpay  = pa_fpay
                                      pa_fboth = pa_fboth
                                      pa_force = pa_force ). " pa_ftim, pa_fpay, pa_fboth

  "prepare protocol
  cl_forms_changes->prepare_protocol( ).
