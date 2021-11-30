"Name: \PR:SAPFP50M\FO:IS_INFOTYPE_DECOUPLED\SE:END\EI
ENHANCEMENT 0 /SEW/HCM_ORACLE_INT.
*
    DATA: molga TYPE molga,
        dcif TYPE TABLE OF /sew/int_dcif_co.

*      CALL FUNCTION 'RH_PM_GET_MOLGA_FROM_PERNR'
*      EXPORTING
**       PLVAR           =
*        pernr           = pspar-pernr
*        begda           = pspar-begda
*        endda           = pspar-endda
*      IMPORTING
*        molga           = molga
**       TRFKZ           =
**      EXCEPTIONS
**        nothing_found   = 1
**        no_active_plvar = 2
**        OTHERS          = 3.
*.

  SELECT * FROM /sew/int_dcif_co INTO TABLE dcif WHERE molga = t001p-molga AND infty = infty.
    IF dcif IS INITIAL.
      SELECT * FROM /sew/int_dcif_co INTO TABLE dcif WHERE molga = '*' AND infty = infty.
        IF dcif IS INITIAL.
          SELECT * FROM /sew/int_dcif_co INTO TABLE dcif WHERE molga = t001p-molga AND infty = '*'.
            IF dcif IS INITIAL.
              SELECT * FROM /sew/int_dcif_co INTO TABLE dcif WHERE molga = '*' AND infty = '*'.
              ENDIF.
          ENDIF.
      ENDIF.
  IF dcif IS NOT INITIAL.
    is_decoupled = space.
    ENDIF.
ENDENHANCEMENT.
