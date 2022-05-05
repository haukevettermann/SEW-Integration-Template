class /SEW/CL_INT_UTILITY definition
  public
  final
  create public .

public section.

  class-data IT0105_PROCESSER type BOOLE_D value ABAP_FALSE ##NO_TEXT.

  class-methods CHECK_PROJECT
    importing
      !MOLGA type MOLGA
    returning
      value(PROJECT) type CHAR4 .
  class-methods BUILD_OBJECT
    importing
      !BEGDA type DATS
      !ENDDA type DATS
      !OTYPE type OTYPE
      !SOBID type SOBID optional
      !OBJID type HROBJID optional
      !BUKRS type BUKRS optional
    returning
      value(OBJECT) type OBJEC .
  class-methods CHECK_FOR_OM_ERROR
    importing
      !TYPE type /SEW/DD_EXT_TYPE optional
    returning
      value(HAS_ERROR) type BOOLE_D .
  class-methods CREATE_GUID
    returning
      value(GUID) type GUID_32 .
  class-methods GET_EXTERNAL_DATE
    importing
      !DATE type DATS
    returning
      value(DATE_EXT) type /SEW/DD_DATE_EXT .
  class-methods GET_MESSAGE
    importing
      !MSGID type SY-MSGID
      !MSGNO type SY-MSGNO
      !MSGV1 type SY-MSGV1 optional
      !MSGV2 type SY-MSGV2 optional
      !MSGV3 type SY-MSGV3 optional
      !MSGV4 type SY-MSGV4 optional
    exporting
      !MESSAGE type BAPI_MSG .
  class-methods DEFAULT_NATIO
    importing
      value(MOLGA) type MOLGA
    returning
      value(NATIO) type NATIO .
  class-methods GET_ACTION
    importing
      !REHIRE type BOOLE_D optional
      !ORGCHANGE type BOOLE_D optional
      !TERMINATION type BOOLE_D optional
      !HIRE type BOOLE_D optional
      !MOLGA type MOLGA
    returning
      value(ACTION) type MASSN .
  class-methods GET_MOLGA
    importing
      !PERNR type PERNR_D
      !BEGDA type DATS
      !ENDDA type DATS
    returning
      value(MOLGA) type MOLGA .
  class-methods GET_OBJID_BY_CLOUDID
    importing
      !CLOUD_ID type /SEW/DD_ELEMENT
      !BEGDA type BEGDA
      !ENDDA type ENDDA
    exporting
      !OBJID type HROBJID .
  class-methods GET_PERNR_BY_CLOUDID
    importing
      !CLOUD_ID type /SEW/DD_ELEMENT
      !BEGDA type BEGDA
      !ENDDA type ENDDA
    exporting
      !PERNR type PERNR_D .
  class-methods GET_PLANS_IT0001
    importing
      !PERNR type PERNR_D
      !BEGDA type BEGDA
      !ENDDA type ENDDA
    exporting
      !PLANS type PLANS .
  class-methods GET_MOLGA_BY_SPRAS
    importing
      !SPRAS type SPRAS
    exporting
      !MOLGA type MOLGA .
  class-methods GET_SPRAS_BY_MOLGA
    importing
      !MOLGA type MOLGA
    exporting
      value(LANGU) type LAISO
      !SPRAS type SPRAS .
  class-methods MAINTAIN_RELATION
    importing
      !ACTION type OKCODE
      !BEGDA type DATS
      !ENDDA type DATS
      !PARENT type OBJEC
      !CHILD type OBJEC
      !RSIGN type RSIGN
      !RELAT type RELAT
      !SIMU type BOOLE_D optional
      !SPRAS type SPRAS
    exporting
      !RETURN type BAPIRET1 .
  class-methods MAP_MSG_TAB
    importing
      !MESSAGES type HRPAD_MESSAGE_TAB
    returning
      value(RETURN_TAB) type HRPAD_RETURN_TAB .
  class-methods MAP_SY_MSG
    importing
      !MSGID type SY-MSGID
      !MSGTY type SY-MSGTY
      !MSGNO type SY-MSGNO
      !MSGV1 type SY-MSGV1
      !MSGV2 type SY-MSGV2
      !MSGV3 type SY-MSGV3
      !MSGV4 type SY-MSGV4
    returning
      value(RETURN) type BAPIRET1 .
  class-methods READ_MASSN_TXT
    importing
      !SPRSL type SPRAS
      !MASSN type MASSN
      !MASSG type MASSG optional
    returning
      value(MASSN_TXT) type MNTXT .
  class-methods START_INT_RUN
    importing
      !INT_RUN type GUID_32
      !SAP_ID type /SEW/DD_VALUE
      !CLOUD_ID type /SEW/DD_VALUE
      !TYPE type OTYPE
      !EXT_TYPE type /SEW/DD_EXT_TYPE optional .
  class-methods GET_EXTERNAL_TIME
    importing
      !TIME type TIMS
    returning
      value(TIME_EXT) type /SEW/DD_DATE_EXT .
  class-methods GET_TIME_CONSTRAINT
    importing
      value(MV_INFTY) type CHAR4
      value(MV_SUBTY) type CHAR4
    returning
      value(MV_TIMECONST) type CHAR1 .
protected section.
private section.
ENDCLASS.



CLASS /SEW/CL_INT_UTILITY IMPLEMENTATION.


  METHOD build_object.
    DATA: kostl TYPE c LENGTH 10.
*          kokrs TYPE kokrs.
    "Build child
    object-plvar = /sew/cl_int_constants=>plvar.
    object-endda = endda.
    object-begda = begda.
    object-otype = otype.
    object-objid = objid.
    object-istat = '1'.
    IF otype = /sew/cl_int_constants=>costcenter.
      object-objid = sobid.
      "Get kostenrechnungskreis
*      sobid = objid.
*      CALL FUNCTION 'RH_GET_CONTROLLING_AREA'
*        EXPORTING
*          plvar         = '01'
*          otype         = 'K'
*          objid         = objid
*          find_kokrs_up = 'X'
*        IMPORTING
*          kokrs         = kokrs
*        EXCEPTIONS
*          not_found     = 1
*          OTHERS        = 2.
*
*      IF sy-subrc <> 0.
** Implement suitable error handling here
*      ENDIF.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = sobid+4(10)
        IMPORTING
          output = kostl.

*      SELECT SINGLE kokrs FROM csks INTO @DATA(kokrs) WHERE kostl = @k AND bukrs = @b AND datab LE @endda AND datbi GE @begda.
      SELECT SINGLE kokrs FROM csks INTO @DATA(kokrs) WHERE kostl = @kostl AND bukrs = @bukrs AND datab LE @endda AND datbi GE @begda.
*      IF kokrs IS NOT INITIAL.
*        CONCATENATE objid kokrs INTO object-realo.
*      ELSE.
      DATA(leng) = strlen( kostl ).
      IF leng LT 10.
        DATA separator TYPE string.
        WHILE leng LT 10.
          CONCATENATE separator ' ' INTO separator RESPECTING BLANKS.
          leng = leng + 1.
        ENDWHILE.
      ENDIF.
      CONCATENATE kostl separator kokrs INTO object-realo.
*      ENDIF.
    ELSEIF otype = /sew/cl_int_constants=>position OR otype = /sew/cl_int_constants=>orgunit.
      object-realo = objid.
    ENDIF.


  ENDMETHOD.


  METHOD check_for_om_error.
    DATA om_aend TYPE /sew/tt_om_aend.
    IF type IS INITIAL.
      SELECT * FROM /sew/int_om_aend INTO TABLE @om_aend WHERE status = '01' OR status = '03' OR status = '07' OR status = '08'.
      IF om_aend IS NOT INITIAL.
        has_error = abap_true.
      ENDIF.
    ELSEIF type = 'DT'.
      SELECT * FROM /sew/int_om_aend INTO TABLE @om_aend WHERE ( status = '01' OR status = '03' OR status = '07' OR status = '08' ) AND ext_type = 'D'.
      IF om_aend IS NOT INITIAL.
        has_error = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD check_project.
    CASE molga.
      WHEN '01'.
        project = 'COFU'.
      WHEN '03'.
        project = 'COFU'.
      WHEN '05'.
        project = 'COFU'.
      WHEN '06'.
        project = 'COFU'.
      WHEN '15'.
        project = 'COFU'.
      WHEN OTHERS.
        project = 'COGL'.
    ENDCASE.
  ENDMETHOD.


  METHOD create_guid.
    TRY.
        guid = cl_system_uuid=>create_uuid_c32_static( ).
      CATCH cx_uuid_error INTO DATA(error).
    ENDTRY.
  ENDMETHOD.


  METHOD default_natio.
    SELECT SINGLE * FROM t500l INTO @DATA(w500l) WHERE molga EQ @molga.
    IF sy-subrc EQ 0.
      SELECT SINGLE * FROM t005 INTO @DATA(w005) WHERE intca EQ @w500l-intca.
      IF sy-subrc EQ 0.
        natio = w005-land1.
      ENDIF.
      CLEAR: w005,w500l.
    ENDIF.

  ENDMETHOD.


METHOD get_action.
  IF hire = abap_true.
    SELECT SINGLE action FROM /sew/int_map_act INTO @action WHERE molga = @molga AND is_hire = @hire.
  ELSEIF termination = abap_true.
    SELECT SINGLE action FROM /sew/int_map_act INTO @action WHERE molga = @molga AND is_hire = @termination.
  ELSEIF orgchange = abap_true.
    SELECT SINGLE action FROM /sew/int_map_act INTO @action WHERE molga = @molga AND is_hire = @orgchange.
  ELSEIF rehire = abap_true.
    SELECT SINGLE action FROM /sew/int_map_act INTO @action WHERE molga = @molga AND is_hire = @rehire.
  ENDIF.
ENDMETHOD.


  METHOD get_external_date.
    CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
      EXPORTING
        date_internal            = date
      IMPORTING
        date_external            = date_ext
      EXCEPTIONS
        date_internal_is_invalid = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.


  METHOD get_external_time.

*    DATA(user_timefm) = cl_abap_timefm=>get_user_timefm( ).

    CALL METHOD cl_abap_timefm=>conv_time_int_to_ext
      EXPORTING
        time_int = time
*       without_seconds     = abap_false       " Flag für Sekunden
*       format_according_to =                  " Format gemäß ENVIRONMENT oder ISO
      IMPORTING
        time_ext = DATA(time_string).

    time_ext = time_string.

  ENDMETHOD.


  METHOD get_message.

    CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
      EXPORTING
        id         = msgid
        number     = msgno
*       LANGUAGE   = SY-LANGU
        textformat = 'RTF'
*       LINKPATTERN        =
        message_v1 = msgv1
        message_v2 = msgv2
        message_v3 = msgv3
        message_v4 = msgv4
*       LANGUAGE_ISO       =
*       LINE_SIZE  =
      IMPORTING
        message    = message
*       RETURN     =
*           TABLES
*       TEXT       =
      .

  ENDMETHOD.


METHOD get_molga.

  CALL FUNCTION 'RH_PM_GET_MOLGA_FROM_PERNR'
    EXPORTING
*     PLVAR           =
      pernr           = pernr
      begda           = begda
      endda           = endda
    IMPORTING
      molga           = molga
*     TRFKZ           =
    EXCEPTIONS
      nothing_found   = 1
      no_active_plvar = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDMETHOD.


  METHOD get_molga_by_spras.
*    SELECT SINGLE intca FROM t500l WHERE molga = @molga INTO @DATA(country).
*    SELECT SINGLE spras FROM t001 WHERE land1 = @country INTO @spras.
*    CALL FUNCTION 'CONVERSION_EXIT_ISOLA_OUTPUT'
*      EXPORTING
*        input  = spras
*      IMPORTING
*        output = langu.
*
*    spras = 'E'.
*    langu = 'EN'.

    SELECT SINGLE land1 FROM t001 WHERE spras = @spras INTO @DATA(country).
    SELECT SINGLE molga FROM t500l WHERE intca = @country INTO @molga.

  ENDMETHOD.


  METHOD get_objid_by_cloudid.
    SELECT SINGLE objid FROM hrp9401 INTO objid WHERE oracleid = cloud_id AND begda LE endda AND endda GE begda.
  ENDMETHOD.


  METHOD get_pernr_by_cloudid.
    SELECT SINGLE pernr FROM pa9400 INTO pernr WHERE oracleid = cloud_id AND begda LE endda AND endda GE begda.
    IF pernr IS INITIAL.
      SELECT SINGLE pernr FROM pa9400 INTO pernr WHERE oraclepernr = cloud_id AND begda LE endda AND endda GE begda.
      IF pernr IS INITIAL.
        pernr = cloud_id.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_plans_it0001.

    SELECT SINGLE plans FROM pa0001 INTO plans WHERE pernr = pernr AND begda LE endda AND endda GE begda.

    SELECT * FROM pa0001 INTO TABLE @DATA(p0001_tab) WHERE pernr = @pernr AND begda LE @endda AND endda GE @begda.
    SORT p0001_tab DESCENDING BY endda.
    READ TABLE p0001_tab INTO DATA(p0001) INDEX 1.
    plans = p0001-plans.
  ENDMETHOD.


  METHOD get_spras_by_molga.
    SELECT SINGLE intca FROM t500l WHERE molga = @molga INTO @DATA(country).
    SELECT SINGLE spras FROM t001 WHERE land1 = @country INTO @spras.
    CALL FUNCTION 'CONVERSION_EXIT_ISOLA_OUTPUT'
      EXPORTING
        input  = spras
      IMPORTING
        output = langu.

    DATA(project) = /sew/cl_int_utility=>check_project( molga ).
    IF project = /sew/cl_int_constants=>cogl AND sy-cprog = '/SEW/RP_OM_AEND_POST'.
      spras = 'E'.
      langu = 'EN'.
    ENDIF.
  ENDMETHOD.


  METHOD get_time_constraint.

* Import mv_infty, mv_subty
* Export mv_timeconst
* Infty time Constraint Table T582A
* SUBTY time COnstraint Table T591A

*Check time constraint infty
    SELECT SINGLE * FROM t582a INTO @DATA(infty) WHERE infty = @mv_infty.
*      LOOP AT lt_infty ASSIGNING FIELD-SYMBOL(<fs_infty>).
*        mv_timeconst = <fs_infty>-zeitb.
*      ENDLOOP.

*Check time constraint infty and subty
    IF infty-zeitb = 'T'.
      SELECT SINGLE * FROM t591a INTO @DATA(subty) WHERE infty = @mv_infty AND subty = @mv_subty.
*      LOOP AT lt_subty ASSIGNING FIELD-SYMBOL(<fs_subty>).
*        mv_timeconst = <fs_subty>-zeitb.
*      ENDLOOP.
      mv_timeconst = subty-zeitb.
    ELSE.
      mv_timeconst = infty-zeitb.
    ENDIF.












**Check time constraint infty
*    IF mv_infty <> '' AND mv_subty = ''.
*      SELECT * FROM t582a INTO TABLE @DATA(lt_infty).
*      LOOP AT lt_infty ASSIGNING FIELD-SYMBOL(<fs_infty>) WHERE infty = mv_infty.
*        mv_timeconst = <fs_infty>-zeitb.
*      ENDLOOP.
*
**Check time constraint infty and subty
*    ELSE.
*      SELECT * FROM t591a INTO TABLE @DATA(lt_subty).
*      LOOP AT lt_subty ASSIGNING FIELD-SYMBOL(<fs_subty>) WHERE infty = mv_infty AND subty = mv_subty.
*        mv_timeconst = <fs_subty>-zeitb.
*      ENDLOOP.
*    ENDIF.
*
*  ENDMETHOD.
*

*    DATA: wa1 TYPE t582a,
*            wa2 TYPE t591a.
*
**Check time constraint infty
*    IF mv_infty <> '' AND mv_subty = ''.
*      SELECT SINGLE * FROM t582a INTO wa1 WHERE infty = mv_infty.
*      mv_timeconst = wa1-zeitb.
*
**Check time constraint infty and subty
*    ELSE.
*      SELECT SINGLE * FROM t591a INTO wa2 WHERE infty = mv_infty AND subty = mv_subty.
*      mv_timeconst = wa2-zeitb.
*    ENDIF.
*
*  ENDMETHOD.
  ENDMETHOD.


  METHOD maintain_relation.
    DATA plvar TYPE plvar.
*    CALL FUNCTION 'RHOM_ALL_BUFFER_INIT'.

    IF simu = abap_true.
      CONCATENATE rsign relat INTO DATA(subty).
      DATA(begda_ext) = /sew/cl_int_utility=>get_external_date( date = begda ).
      DATA(endda_ext) = /sew/cl_int_utility=>get_external_date( date = endda ).
      CONCATENATE begda_ext '-' endda_ext INTO DATA(date) SEPARATED BY space.
      return = VALUE bapiret1( type = /sew/cl_int_constants=>warning
                                 id = /sew/cl_int_constants=>msg_class_int
                                 number = /sew/cl_int_constants=>msg_no-m24
                                 message_v1 = parent
                                 message_v2 = child
                                 message_v3 = subty
                                 message_v4 = date
).

    ELSE.

      "Relation to child can be earliest with begda of child object
*      me->read_hrpxxxx(
*      EXPORTING
*        begda = begda
*        endda = endda
*        infty = /sew/cl_int_constants=>it1000
*        langu = CONV #( spras )
*        otype = <om_aend>-otype
*        sap_id = <om_aend>-sap_id
*        subty = <om_aend>-subty
*        plvar = CONV #( /sew/cl_int_constants=>plvar )
*        IMPORTING
*          hrp_old = <hrp_old>
*          return = return ).

      plvar = /sew/cl_int_constants=>plvar.
      CALL FUNCTION 'RHOM_MAINTAIN_RELATION_BUFF'
        EXPORTING
          act_fcode               = action
          act_plvar               = plvar
          act_istat               = '1'
          vbegda                  = begda
          vendda                  = endda
*         DEFIBEGDA               =
*         DEFIENDDA               =
          vprozt                  = '100'
*         VPRIOX                  =
*         VSEQNR                  =
          parent_object           = parent
*         ACT_INFTY               = /sew/cl_int_constants=>it1001
          act_rsign               = rsign
          act_relat               = relat
          child_object            = child
*         MOVE_FLAG               =
*         OLD_PARENT_OBJECT       =
*         CUT_DATE                =
*         HR_ACTION_INFO          =
          change_manager_relation = ' '
*     IMPORTING
*         VBEGDA                  =
*         VENDDA                  =
        EXCEPTIONS
          no_active_plvar         = 1
          no_authority            = 2
          write_error             = 3
          OTHERS                  = 4.
      IF sy-subrc <> 0.
* Implement suitable error handling here
        "Create Message for Error during maintain relation between S - O
        return = /sew/cl_int_utility=>map_sy_msg( msgid = sy-msgid msgty = sy-msgty msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 msgv4 = sy-msgv4 ).
*      /sew/cl_int_utility=>get_message( EXPORTING msgid = return-id msgno = return-number
*                                        IMPORTING message = return-message ).
        IF return-number = 075.
          return-type = /sew/cl_int_constants=>error.
        ENDIF.
      ELSE.
        "Success create relation
*      CALL FUNCTION 'RHOM_WRITE_BUFFER_TO_DB'.
*      COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD map_msg_tab.
    DATA: return LIKE LINE OF return_tab.

    LOOP AT messages ASSIGNING FIELD-SYMBOL(<message>).
      return = CORRESPONDING #( <message> ).
      return-type = <message>-msgty.
      return-id = <message>-msgid.
      return-number = <message>-msgno.
*      return-message = <message>-
      return-message_v1 = <message>-msgv1.
      return-message_v2 = <message>-msgv2.
      return-message_v3 = <message>-msgv3.
      return-message_v4 = <message>-msgv4.
      /sew/cl_int_utility=>get_message( EXPORTING msgid = return-id msgno = return-number msgv1 = return-message_v1 msgv2 = return-message_v2
                                      msgv3 = return-message_v3 msgv4 = return-message_v4
                        IMPORTING message = return-message ).
      APPEND return TO return_tab.
      CLEAR: return.
    ENDLOOP.
  ENDMETHOD.


  METHOD map_sy_msg.
    return-id = msgid.
    return-type = msgty.
    return-number = msgno.
    return-message_v1 = msgv1.
    return-message_v2 = msgv2.
    return-message_v3 = msgv3.
    return-message_v4 = msgv4.

    /sew/cl_int_utility=>get_message( EXPORTING msgid = return-id msgno = return-number msgv1 = return-message_v1 msgv2 = return-message_v2
                                                msgv3 = return-message_v3 msgv4 = return-message_v4
                                  IMPORTING message = return-message ).
  ENDMETHOD.


  METHOD read_massn_txt.

    SELECT SINGLE mntxt FROM t529t INTO @massn_txt WHERE sprsl = @sprsl AND massn = @massn.

  ENDMETHOD.


  METHOD start_int_run.
    IF type = /sew/cl_int_constants=>person.
      SUBMIT /sew/rp_it_aend_post AND RETURN
        WITH pnppernr INCL sap_id
        WITH so_cldid INCL cloud_id
        WITH so_intr INCL int_run
        WITH pa_simu EQ abap_false
        WITH pa_alv EQ abap_false.
    ELSEIF type = /sew/cl_int_constants=>position OR type = /sew/cl_int_constants=>orgunit.
      IF ext_type = 'D'.
        SUBMIT /sew/rp_om_aend_post AND RETURN
          WITH so_sapid INCL sap_id
          WITH so_cldid INCL cloud_id
          WITH so_intr INCL int_run
          WITH p_otype INCL type
          WITH p_etype INCL ext_type
          WITH p_test EQ abap_false
          WITH pa_alv EQ abap_false
          WITH p_manag EQ abap_false.
        SUBMIT /sew/rp_om_aend_post AND RETURN
          WITH so_sapid INCL sap_id
          WITH so_cldid INCL cloud_id
          WITH so_intr INCL int_run
          WITH p_otype INCL type
          WITH p_etype INCL ext_type
          WITH p_test EQ abap_false
          WITH pa_alv EQ abap_false
          WITH p_manag EQ abap_true.
      ELSEIF ext_type = 'DT'.
        SUBMIT /sew/rp_om_aend_post AND RETURN
          WITH so_sapid INCL sap_id
          WITH so_cldid INCL cloud_id
          WITH so_intr INCL int_run
          WITH p_otype INCL type
          WITH p_etype INCL ext_type
          WITH p_test EQ abap_false
          WITH pa_alv EQ abap_false
          WITH p_manag EQ abap_false.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
