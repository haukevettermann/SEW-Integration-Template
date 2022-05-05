*&---------------------------------------------------------------------*
*& Report /SEW/RP_MIG_POS_FILE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /sew/rp_mig_pos_file.

TABLES: objec,
        sscrfields.

DATA: objec_all TYPE objec_t,
      plvar     TYPE plvar,
      ucomm     TYPE sscrfields-ucomm.

SELECTION-SCREEN BEGIN OF BLOCK sel_stat WITH FRAME TITLE TEXT-002.
PARAMETERS: zipfile TYPE boolean AS CHECKBOX USER-COMMAND us.

SELECTION-SCREEN END OF BLOCK sel_stat.

SELECTION-SCREEN BEGIN OF BLOCK file WITH FRAME TITLE TEXT-001.

PARAMETERS: file LIKE rlgrap-filename     OBLIGATORY DEFAULT 'C:\Position.dat' MODIF ID std.  "standard output
PARAMETERS: zip_file LIKE rlgrap-filename OBLIGATORY DEFAULT 'C:\Position.zip' MODIF ID zpf.  "zip output

SELECTION-SCREEN END OF BLOCK file.

AT SELECTION-SCREEN.
  ucomm = sscrfields-ucomm.

AT SELECTION-SCREEN OUTPUT.
  IF zipfile EQ abap_true.
    LOOP AT SCREEN.
      screen-active = 1.
      CASE screen-group1.
        WHEN 'ZPF'.
          screen-active = 1.
        WHEN 'STD'.
          screen-active = 0.
        WHEN OTHERS.
          screen-active = 1.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.
  ELSE.
    CASE ucomm.
      WHEN 'STAT'.
        LOOP AT SCREEN.
          screen-active = 0.
          CASE screen-group1.
            WHEN 'ZPF'.
              IF zipfile NE abap_true.
                screen-active = 0.
              ELSE.
                screen-active = 1.
              ENDIF.
            WHEN OTHERS.
              CONTINUE.
          ENDCASE.
          MODIFY SCREEN.
        ENDLOOP.
      WHEN OTHERS.
        LOOP AT SCREEN.
          screen-active = 1.
          CASE screen-group1.
            WHEN 'ZPF'.
              IF zipfile NE abap_true.
                screen-active = 0.
              ELSE.
                screen-active = 1.
              ENDIF.
            WHEN OTHERS.
              CONTINUE.
          ENDCASE.
          MODIFY SCREEN.
        ENDLOOP.
    ENDCASE.
  ENDIF.

INITIALIZATION.

  "Aktive Planvariante ermitteln
  CALL FUNCTION 'RH_GET_ACTIVE_WF_PLVAR'
    IMPORTING
      act_plvar       = plvar
    EXCEPTIONS
      no_active_plvar = 1
      OTHERS          = 2.

  pchplvar = plvar.   "Aktive Planvariante
  pchotype = 'S'.     "Planstelle
  pchostat = 1.       "Status


AT SELECTION-SCREEN ON VALUE-REQUEST FOR file.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = ' '
    IMPORTING
      file_name     = file.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR zip_file.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = ' '
    IMPORTING
      file_name     = zip_file.

START-OF-SELECTION.

GET objec.

  APPEND objec TO objec_all.

END-OF-SELECTION.

  DATA(pos_data) = NEW /sew/cl_mig_pos_data( begda = pchbegda
                                             endda = pchendda
                                             objec = objec_all
                                             plvar = plvar ).
  DATA(metadata) = pos_data->create_metadata( ).
  DATA(data) = pos_data->proceed_pos_data( ).
  DATA(content) = pos_data->proceed_file_construction( metadata = metadata
                                                       data     = data ).

  DATA(files) = VALUE /iwbep/t_mgw_name_value_pair( ( name = file value = content ) ).

  IF zipfile EQ abap_true.
    "add filepath to zip_files
    DATA(zip_files) = VALUE /iwbep/t_mgw_name_value_pair( ( name = 1 value = zip_file ) ).
  ENDIF.
  pos_data->download_files( files = files zip_files = zip_files ).
