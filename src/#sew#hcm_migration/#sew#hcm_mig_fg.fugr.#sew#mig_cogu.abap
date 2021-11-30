FUNCTION /sew/mig_cogu.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(COUNTRY) TYPE  STRING
*"     VALUE(OM) TYPE  BOOLEAN
*"  EXPORTING
*"     VALUE(ZIP) TYPE  XSTRING
*"----------------------------------------------------------------------

  DATA: filename TYPE string.

  "get country file
  DATA(zip_file) = SWITCH string( country
                                  WHEN 'DE' THEN /sew/cl_mig_pa_file=>de_file
                                  WHEN 'FR' THEN /sew/cl_mig_pa_file=>fr_file
                                  WHEN 'AT' THEN /sew/cl_mig_pa_file=>at_file
                                  WHEN 'AU' THEN /sew/cl_mig_pa_file=>au_file
                                  WHEN 'NZ' THEN /sew/cl_mig_pa_file=>nz_file
                                  WHEN 'NL' THEN /sew/cl_mig_pa_file=>nl_file
                                  WHEN 'IT' THEN /sew/cl_mig_pa_file=>it_file ).

  IF om eq abap_true.
      zip_file = SWITCH string( country
                                WHEN 'DE' THEN /sew/cl_mig_om_data=>de_file
                                WHEN 'FR' THEN /sew/cl_mig_om_data=>fr_file
                                WHEN 'AT' THEN /sew/cl_mig_om_data=>at_file
                                WHEN 'AU' THEN /sew/cl_mig_om_data=>au_file
                                WHEN 'NZ' THEN /sew/cl_mig_om_data=>nz_file
                                WHEN 'NL' THEN /sew/cl_mig_om_data=>nl_file
                                WHEN 'IT' THEN /sew/cl_mig_om_data=>it_file ).
  ENDIF.

  DATA(logical_filename) = CONV filename-fileintern( '/SEW/HCM_ORACLE_EXTRACTS_COGU').
  CALL FUNCTION 'FILE_GET_NAME'
    EXPORTING
      logical_filename = logical_filename
      parameter_1      = zip_file
    IMPORTING
      file_name        = filename
    EXCEPTIONS
      file_not_found   = 1
      OTHERS           = 2.

  CHECK filename IS NOT INITIAL.

  OPEN DATASET filename FOR INPUT IN BINARY MODE.
  CHECK sy-subrc IS INITIAL.

  READ DATASET filename INTO zip.

ENDFUNCTION.
