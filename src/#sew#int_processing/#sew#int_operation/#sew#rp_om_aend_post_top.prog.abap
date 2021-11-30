*&---------------------------------------------------------------------*
*& Include /SEW/RP_OM_AEND_POST_TOP                          Report /SEW/RP_OM_AEND_POST
*&
*&---------------------------------------------------------------------*
*REPORT /SEW/RP_OM_AEND_POST.
DATA: int_run  TYPE guid_32,
      objid    TYPE hrobjid,
      cloud_id TYPE /sew/dd_value.
*-- Selection Screen Parameters
PARAMETERS:
  p_test   TYPE c AS CHECKBOX DEFAULT abap_true,
  pa_alv   TYPE flag DEFAULT abap_true,
  p_manag TYPE flag DEFAULT abap_false.
*  p_back TYPE c AS CHECKBOX DEFAULT abap_true.
*  if p_back is selected, the sl_index should not be mandatory
*  p_back  TYPE c AS CHECKBOX,
*  p_email TYPE ad_smtpadr
*SELECT-OPTIONS: sl_index FOR s_index. "OBLIGATORY.
SELECT-OPTIONS so_intr FOR int_run.
SELECT-OPTIONS so_sapid FOR objid.
SELECT-OPTIONS so_cldid FOR cloud_id.
"for booking of single index
PARAMETERS:
*  p_inact TYPE c AS CHECKBOX DEFAULT abap_true, "for booking of inactivations
  p_otype TYPE otype OBLIGATORY,
  p_etype TYPE /sew/dd_ext_type OBLIGATORY. "for booking of...
