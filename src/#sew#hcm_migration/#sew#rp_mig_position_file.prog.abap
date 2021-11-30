*&---------------------------------------------------------------------*
*& Report /SEW/RP_MIG_OM_FILE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /sew/rp_mig_position_file.

TABLES: objec, gdstr.

DATA: objec_all    TYPE objec_t,
      position_data_mig TYPE string.

SELECTION-SCREEN BEGIN OF BLOCK blo WITH FRAME TITLE TEXT-001.

PARAMETERS: file    LIKE rlgrap-filename OBLIGATORY DEFAULT 'C:\Position.dat'.
PARAMETERS: file_h LIKE rlgrap-filename OBLIGATORY DEFAULT 'C:\Position_Hierarchy.dat'.
PARAMETERS: al11    TYPE boolean         AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK blo.

INITIALIZATION.

  PERFORM fill_selfields.
  PERFORM deactivate_otype.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR file.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = ' '
    IMPORTING
      file_name     = file.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR file_h.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = ' '
    IMPORTING
      file_name     = file_h.

AT SELECTION-SCREEN OUTPUT.
  PERFORM deactivate_otype.

START-OF-SELECTION.

GET objec.

  APPEND objec TO objec_all.

END-OF-SELECTION.

  DATA(position_data) = NEW /sew/cl_mig_position_data( begda = pchbegda
                                           endda = pchendda
                                           objec = objec_all
                                           al11  = al11 ).

  position_data->proceed_position_data( IMPORTING position_data_mig = position_data_mig ).

  DATA(files) = VALUE /iwbep/t_mgw_name_value_pair( ( name = file    value = position_data_mig ) ).
  position_data->download_files( files ).

INCLUDE /SEW/RP_MIG_OM_FILE_SELFIELFP.
*INCLUDE /SEW/RP_MIG_OM_FILE_SELFIELF02.
*  INCLUDE /sew/rp_mig_om_file_selfielf01.
INCLUDE /SEW/RP_MIG_OM_FILE_SELOUTPUP.
*INCLUDE /SEW/RP_MIG_OM_FILE_SELOUTPU02.
*  INCLUDE /sew/rp_mig_om_file_seloutpu01.
