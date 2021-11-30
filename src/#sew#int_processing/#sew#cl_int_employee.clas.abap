class /SEW/CL_INT_EMPLOYEE definition
  public
  final
  create public .

public section.

  data PERNR type PERNR .
  data MOLGA type MOLGA .
  data INT_RUN type GUID_32 .

  methods MAINTAIN_RELATION
    importing
      !ACTION type OKCODE
      !BEGDA type DATS
      !ENDDA type DATS
      !PARENT type OBJEC
      !CHILD type OBJEC
      !RSIGN type RSIGN
      !RELAT type RELAT .
  methods CREATE_DUMMY_POS
    importing
      !BEGDA type DATS
      !ENDDA type DATS
      !ORGEH type HROBJID
    returning
      value(ID_POS) type HROBJID .
  methods CONSTRUCTOR
    importing
      !PERNR type PERNR_D optional
      !MOLGA type MOLGA optional
      !INT_RUN type GUID_32 optional .
  methods PERFORM_HIRE
    importing
      !IT_AEND type /SEW/INT_IT_AEND
      !IT_AEND_TAB type /SEW/TT_IT_AEND
      !SIMU type BOOLE_D
    exporting
      !IS_OK type BOOLE_D
      !RETURN_TAB type HRPAD_RETURN_TAB
      !PERNR type PERNR_D .
protected section.
private section.
ENDCLASS.



CLASS /SEW/CL_INT_EMPLOYEE IMPLEMENTATION.


  METHOD constructor.
    me->pernr = pernr.
    me->molga = molga.
    me->int_run = int_run.
  ENDMETHOD.


  METHOD create_dummy_pos.
    DATA: pos_idx   TYPE TABLE OF hrobject,
          pos_struc TYPE p1000,
          pos       TYPE TABLE OF p1000,
          child     TYPE objec,
          parent    TYPE objec.
    IF orgeh IS NOT INITIAL.
      "Testing purpose
      pos_struc-aedtm = sy-datum.
      pos_struc-langu = 'DE'.
      pos_struc-stext = 'Dummy'.
      pos_struc-short = 'Dummy Position'.

      CONCATENATE pos_struc-stext me->pernr INTO pos_struc-stext SEPARATED BY space.
      APPEND pos_struc TO pos.
      CALL FUNCTION 'RHOM_ALL_BUFFER_INIT'.
      CALL FUNCTION 'RHOM_CREATE_OBJECTS_FROM_TAB'
        EXPORTING
          plvar               = '01'
          otype               = /sew/cl_int_constants=>position
          ostat               = '1'
          begda               = begda
          endda               = endda
*         BUFFER_MODE         =
*         SUPPRESS_STRU_AUTH  =
        TABLES
          new_objects         = pos
          objectindx          = pos_idx
        EXCEPTIONS
          error_during_insert = 1
          no_authorization    = 2
          corr_exit           = 3
          OTHERS              = 4.
      IF sy-subrc <> 0.
* Implement suitable error handling here
        "Create Message for Error during dummy creation

      ELSE.
        "Dummy was created
        IF pos IS NOT INITIAL.
          LOOP AT pos ASSIGNING FIELD-SYMBOL(<pos_line>).
            MOVE-CORRESPONDING <pos_line> TO child.
            id_pos = <pos_line>-objid.
          ENDLOOP.

          CALL FUNCTION 'RH_READ_OBJECT'
            EXPORTING
              plvar     = '01'
              otype     = /sew/cl_int_constants=>orgunit
              objid     = orgeh
*             REALO     = ' '
*             ISTAT     = ' '
              begda     = begda
              endda     = endda
*             LANGU     = SY-LANGU
*             OINTERVAL = 'X'
*             STORE     = 'X'
*             CHECK_STRU_AUTH       = 'X'
*             READ_DB   = ' '
            IMPORTING
              obeg      = parent-begda
              oend      = parent-endda
*             OSTAT     =
              histo     = parent-histo
              short     = parent-short
              stext     = parent-stext
              tistat    = parent-istat
*             TLANGU    =
*             DISPLAY_TEXT          =
*         TABLES
*             EXISTENCE =
            EXCEPTIONS
              not_found = 1
              OTHERS    = 2.
          IF sy-subrc <> 0.
* Implement suitable error handling here
            "Error while reading Orgunit
          ELSE.
            parent-otype = /sew/cl_int_constants=>orgunit.
            parent-objid = orgeh.
            parent-plvar = '01'.

            "Create relation between S AND O
            me->maintain_relation(
            EXPORTING
              action = CONV #( /sew/cl_int_constants=>insert_rel )
              begda = begda
              endda = endda
              relat = '003'
              rsign = 'B'
              parent = parent "CONV #( orgeh )
              child = child "CONV #( <pos_line> )
            ).
          ENDIF.
        ENDIF.
      ENDIF.
      "Error because Orgunit is empty and no dummy position can be created
    ELSE.
    ENDIF.
  ENDMETHOD.


  METHOD maintain_relation.
    CALL FUNCTION 'RHOM_ALL_BUFFER_INIT'.

    CALL FUNCTION 'RHOM_MAINTAIN_RELATION_BUFF'
      EXPORTING
        act_fcode               = action
*       ACT_PLVAR               =
*       ACT_ISTAT               = '1'
        vbegda                  = begda
        vendda                  = endda
*       DEFIBEGDA               =
*       DEFIENDDA               =
        vprozt                  = '100'
*       VPRIOX                  =
*       VSEQNR                  =
        parent_object           = parent
*       ACT_INFTY               = /sew/cl_int_constants=>it1001
        act_rsign               = rsign
        act_relat               = relat
        child_object            = child
*       MOVE_FLAG               =
*       OLD_PARENT_OBJECT       =
*       CUT_DATE                =
*       HR_ACTION_INFO          =
        change_manager_relation = ' '
*     IMPORTING
*       VBEGDA                  =
*       VENDDA                  =
      EXCEPTIONS
        no_active_plvar         = 1
        no_authority            = 2
        write_error             = 3
        OTHERS                  = 4.
    IF sy-subrc <> 0.
* Implement suitable error handling here
      "Create Message for Error during maintain relation between S - O

    ELSE.
      "Success create relation
      CALL FUNCTION 'RHOM_WRITE_BUFFER_TO_DB'.
      COMMIT WORK AND WAIT.
    ENDIF.

  ENDMETHOD.


  METHOD perform_hire.
    DATA: prelp_tab      TYPE prelp_tab,
          prelp          TYPE prelp,
*          return_tab     TYPE hrpad_return_tab,
          bapipakey_tab  TYPE hrpad_bapipakey_tab,
          lr_structdescr TYPE REF TO cl_abap_structdescr,
          lr_tabledescr  TYPE REF TO cl_abap_tabledescr,
          lr_table       TYPE REF TO data,
          p0000          TYPE p0000,
          p0001          TYPE p0001,
          it_aendup      TYPE /sew/int_it_aeup,
          bukrs          TYPE bukrs.
    FIELD-SYMBOLS: <pnnnn>        TYPE any.
    DATA(it_aend_tmp) = VALUE /sew/tt_it_aend( FOR ls_it_aend IN it_aend_tab WHERE ( int_run = me->int_run AND
                                                                                     cloud_id = it_aend-cloud_id ) ( ls_it_aend ) ).




    "Not final yet
    LOOP AT it_aend_tmp INTO DATA(aend_tmp).
      CLEAR: prelp.
      MOVE-CORRESPONDING aend_tmp TO prelp.
      IF aend_tmp-infty = '0000'.
        DATA(begda) = aend_tmp-begda.
*        DATA(id_pos) = me->create_dummy_pos( begda = begda endda = /sew/cl_int_constants=>highdate  ).

        cl_hr_pnnnn_type_cast=>prelp_to_pnnnn( EXPORTING prelp = prelp
                                       IMPORTING pnnnn = p0000 ).
        ASSIGN COMPONENT /sew/cl_int_constants=>massn OF STRUCTURE p0000 TO FIELD-SYMBOL(<massn>).
        ASSIGN COMPONENT /sew/cl_int_constants=>massg OF STRUCTURE p0000 TO FIELD-SYMBOL(<massg>).
      ELSEIF aend_tmp-infty = '0001'.

        cl_hr_pnnnn_type_cast=>prelp_to_pnnnn( EXPORTING prelp = prelp
                                      IMPORTING pnnnn = p0001 ).
        ASSIGN COMPONENT 'PLANS' OF STRUCTURE p0001 TO FIELD-SYMBOL(<plans>).
        ASSIGN COMPONENT 'BUKRS' OF STRUCTURE p0001 TO FIELD-SYMBOL(<bukrs>).
        bukrs = <bukrs>.
*        <plans> = id_pos.
        cl_hr_pnnnn_type_cast=>pnnnn_to_prelp( EXPORTING pnnnn = p0001
                              IMPORTING prelp = prelp ).
      ELSEIF aend_tmp-infty = '0002'.
        DATA p0002 TYPE p0002.
        cl_hr_pnnnn_type_cast=>prelp_to_pnnnn( EXPORTING prelp = prelp
                                     IMPORTING pnnnn = p0002 ).
*        ASSIGN COMPONENT 'NATIO' OF STRUCTURE p0002 TO FIELD-SYMBOL(<natio>).
*        <natio> = 'AR'.
*                cl_hr_pnnnn_type_cast=>pnnnn_to_prelp( EXPORTING pnnnn = p0002
*                              IMPORTING prelp = prelp ).
      ENDIF.
      APPEND prelp TO prelp_tab.
    ENDLOOP.
    DATA pernr_n TYPE pernr_d.
    TRY.
        CALL FUNCTION 'HR_PAD_HIRE_EMPLOYEE'
          EXPORTING
            employeenumber  = pernr_n
            hiringdate      = begda
            actiontype      = <massn> " muss noch angepasst werden!
            reasonforaction = <massg> " muss noch angepasst werden!
            pnnnn_tab       = prelp_tab
            nocommit        = simu "No commit, only write to buffer, database entries only written when all entries processed correctly
          IMPORTING
            return_tab      = return_tab
            bapipakey_tab   = bapipakey_tab
            is_ok           = is_ok.
      CATCH cx_hrpa_missing_infty_data.
    ENDTRY.


    IF is_ok = abap_true.
      READ TABLE bapipakey_tab ASSIGNING FIELD-SYMBOL(<bapipakey>) INDEX 1.
      IF <bapipakey>-employeeno IS NOT INITIAL.
        pernr = <bapipakey>-employeeno.

        "Add IT_AENDUP entry for Hire to send PERNR to Cloud
        IF simu NE abap_true.
          DATA(it_aend_up) = NEW /sew/cl_int_it_aendup( ).
          it_aendup-pernr = pernr.
          it_aendup-cloud_id = it_aend-cloud_id.
          it_aendup-molga = it_aend-molga.
          it_aendup-int_run = /sew/cl_int_utility=>create_guid( ).
          it_aendup-legal_entity = bukrs. "it_aend-legal_entity.
          it_aendup-mandt = sy-mandt.
          GET TIME STAMP FIELD it_aendup-timestamp.
          it_aendup-begda = it_aend-begda.
          it_aendup-endda = it_aend-endda.
          it_aendup-aedtm = sy-datum.
          it_aendup-uname = sy-uname.
          it_aendup-status = /sew/cl_int_constants=>booking_status-initial.
          it_aend_up->save_entries( it_aendup = it_aendup ).
        ENDIF.
      ENDIF.
    ENDIF.

*    IF is_ok = abap_false.
*      DATA(error_handler) = NEW /sew/cl_int_error_handler( cloud_id = CONV #( it_aend-cloud_id ) aend_id = CONV #( it_aend-aend_id )
*                                                          int_run = me->int_run molga = me->molga ).

*      error_handler->add_message_pa( EXPORTING pernr = CONV #( it_aend-cloud_id ) int_run = me->int_run return_tab = return_tab begda = it_aend-begda endda = it_aend-endda ).
*      ELSE.
    "Success messages

*    ENDIF.
  ENDMETHOD.
ENDCLASS.
