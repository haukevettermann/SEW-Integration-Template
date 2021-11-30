"Name: \TY:CL_HRPA_INFTY_NNNN\ME:READ_METADATA\SE:END\EI
ENHANCEMENT 0 /SEW/HCM_ORACLE_INT_INFTY.
*
  DATA: dcif TYPE TABLE OF /sew/int_dcif_co.

  IF <pshdr>-infty = '0002'.
*  ASSIGN COMPONENT 'SPRSL' OF STRUCTURE pnnnn TO FIELD-SYMBOL(<spras>).
*  IF <spras> IS ASSIGNED AND <spras> IS NOT INITIAL.
*      /sew/cl_int_utility=>get_molga_by_spras( EXPORTING spras = <spras>
*                                             IMPORTING molga = DATA(molga) ).
*
*   SELECT * FROM /sew/int_dcif_co INTO TABLE dcif WHERE molga = molga AND infty = <pshdr>-infty.
*    IF dcif IS INITIAL.
*      SELECT * FROM /sew/int_dcif_co INTO TABLE dcif WHERE molga = '*' AND infty = <pshdr>-infty.
*        IF dcif IS INITIAL.
*          SELECT * FROM /sew/int_dcif_co INTO TABLE dcif WHERE molga = molga AND infty = '*'.
*            IF dcif IS INITIAL.
*              SELECT * FROM /sew/int_dcif_co INTO TABLE dcif WHERE molga = '*' AND infty = '*'.
*              ENDIF.
*          ENDIF.
*      ENDIF.
*     IF dcif IS NOT INITIAL.
*       LOOP AT field_metadata_tab ASSIGNING FIELD-SYMBOL(<metadata>).
*         IF <metadata>-fname = 'NATIO'.
*         clear <metadata>-mandatory.
*         ENDIF.
*       ENDLOOP.
*     ENDIF.
*     ELSE.
       IF sy-cprog = '/SEW/RP_IT_AEND_POST'.
       LOOP AT field_metadata_tab ASSIGNING FIELD-SYMBOL(<metadata>).
         IF <metadata>-fname = 'NATIO'.
         clear <metadata>-mandatory.
         ENDIF.
       ENDLOOP.
       ENDIF.
*       ENDIF.
     ENDIF.
ENDENHANCEMENT.
