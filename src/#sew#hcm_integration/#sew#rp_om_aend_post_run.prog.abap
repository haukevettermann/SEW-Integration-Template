*&---------------------------------------------------------------------*
*&  Include           /SEW/RP_OM_AEND_POST_RUN
*&---------------------------------------------------------------------*

  DATA(om_aend_post) = NEW /sew/cl_om_aend_post( alv   = pa_alv
                                                    batch = sy-batch
                                                    simu  = p_test ).
*                                                    dates = VALUE rsdsselopt_t( ( sign = 'I' option = 'BT' low = pn-begda high = pn-endda ) ) ).

  DATA(intrun_range) = VALUE rsdsselopt_t( FOR intrun IN so_intr[] ( sign   = intrun-sign
                                                                     option = intrun-option
                                                                     low    = intrun-low
                                                                     high   = intrun-high ) ).

  DATA(sap_id_range) = VALUE rsdsselopt_t( FOR sap_id IN so_sapid[] ( sign   = sap_id-sign
                                                                   option = sap_id-option
                                                                   low    = sap_id-low
                                                                   high   = sap_id-high ) ).

  DATA(cloud_id_range) = VALUE rsdsselopt_t( FOR cld_id IN so_cldid[] ( sign   = cld_id-sign
                                                                 option = cld_id-option
                                                                 low    = cld_id-low
                                                                 high   = cld_id-high ) ).

  "get corresponding request data
  om_aend_post->status_handler = NEW /sew/cl_int_status_handler( sap_id_rng = sap_id_range cloud_id_rng = cloud_id_range
                                                         int_run_rng = intrun_range ).

  DELETE om_aend_post->status_handler->om_aend WHERE ext_type NE p_etype.
  SORT om_aend_post->status_handler->om_aend BY sap_id timestamp infty ASCENDING.
  om_aend_post->book_manager_relation = p_manag.
  "Delete unrelevant data
*  DELETE om_aend_post->status_handler->om_aend WHERE status = '02' AND status = '05'.

*  om_aend_post->get_om_aend_data( objid  = sap_id_range
*                                     intrun = intrun_range ).

  "start post
  "Change
*  IF /sew/cl_int_utility=>check_for_om_error( type = p_etype ) = abap_false.
  om_aend_post->start_post( ).
*  ELSE.
*    "Department Tree is not being executed because of initial or error entries for department bookings
*    MESSAGE TEXT-001 TYPE 'I' DISPLAY LIKE 'E'.
*  ENDIF.

  "prepare protocol
  om_aend_post->prepare_protocol( ).
