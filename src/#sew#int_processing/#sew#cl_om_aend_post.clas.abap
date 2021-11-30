class /SEW/CL_OM_AEND_POST definition
  public
  create public .

public section.

  data DATES type RSDSSELOPT_T .
  data STATUS type RSDSSELOPT_T .
  data OM_AEND type /SEW/TT_OM_AEND .
  data OM_AEND_ERROR type /SEW/TT_OM_AEND .
  data OM_AEND_POST type /SEW/TT_OM_AEND .
  data OM_AEND_NOCHANGE type /SEW/TT_OM_AEND .
  data MESSAGE_HANDLER type ref to CL_HRPAY00_MESSAGE_HANDLER .
  data SKIPPED_OBJECTS type HRAHQ_PERNR_TABLE .
  data SIMU type BOOLEAN .
  data STATUS_HANDLER type ref to /SEW/CL_INT_STATUS_HANDLER .
  data BOOK_MANAGER_RELATION type BOOLE_D .

  methods CHECK_ACTIVE_RELAT
    importing
      !DATS type DATS
      !ORG type ORGEH
    exporting
      !RELAT_EXISTS type BOOLE_D .
  methods MAINTAIN_HRPXXXX
    importing
      !HRP_NEW type ANY
      !INFTY type INFTY
      !SUBTY type SUBTY
      !OTYPE type OTYPE
      !LANGU type LAISO
      !SAP_ID type HROBJID
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !PLVAR type PLVAR
      !IT_CHANGE type BOOLE_D
      !IT_CREATE type BOOLE_D
      !SIMU type BOOLE_D optional
      !ACTION type OKCODE optional
    exporting
      !RETURN type BAPIRET1
      !HRP_OLD type ANY .
  methods READ_HRPXXXX
    importing
      !INFTY type INFTY
      !SUBTY type SUBTY
      !OTYPE type OTYPE
      !LANGU type LAISO
      !SAP_ID type HROBJID
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !PLVAR type PLVAR
    exporting
      !RETURN type BAPIRET1
      !HRP_OLD type ANY .
  methods DELIMIT_HRP1000
    importing
      !INFTY type INFTY
      !HRP_OLD type ANY
      !DELIMIT_DATE type DATS
      !NEW_DATE type DATS
      !SIMU type BOOLE_D optional
    exporting
      !RETURN type BAPIRET1 .
  methods OBJECT_CREATION
    importing
      !SIMU type BOOLE_D optional
    exporting
      !RETURN type BAPIRET1
    changing
      !OM_AEND type /SEW/INT_OM_AEND .
  methods OBJECT_MODIFICATION
    importing
      !HRP_DATA type ANY
      !OM_AEND type /SEW/INT_OM_AEND
    exporting
      !RETURN type BAPIRET1
      !RETURN_TAB type HRPAD_RETURN_TAB .
  methods UPDATE_STATUS .
  methods CHECK_STATUS_RANGE
    importing
      !INITIAL type BOOLEAN
      !ERROR type BOOLEAN .
  methods GET_OM_AEND_DATA
    importing
      !OBJID type RSDSSELOPT_T
      !INTRUN type RSDSSELOPT_T .
  methods PREPARE_PROTOCOL .
  methods START_POST .
  methods CONSTRUCTOR
    importing
      !ALV type BOOLEAN optional
      !BATCH type BOOLEAN
      !SIMU type BOOLEAN
      !DATES type RSDSSELOPT_T optional .
  methods POST_OM_AEND
    importing
      !SIMU type BOOLEAN
    exporting
      !OM_AEND_POST type /SEW/TT_OM_AEND
      !OM_AEND_ERROR type /SEW/TT_OM_AEND
      !OM_AEND_NOCHANGE type /SEW/TT_OM_AEND
    changing
      !OM_AEND type /SEW/TT_OM_AEND
      !MESSAGE_HANDLER type ref to CL_HRPAY00_MESSAGE_HANDLER .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /SEW/CL_OM_AEND_POST IMPLEMENTATION.


  METHOD check_active_relat.
    DATA(check_date) = dats + 1.
    SELECT * FROM hrp1001 INTO TABLE @DATA(relations) WHERE begda LE @check_date AND endda GE @check_date
                                                        AND objid = @org AND rsign = 'B' AND relat = '003'.
    IF relations IS NOT INITIAL.
      relat_exists = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD CHECK_STATUS_RANGE.

    "Default both status
    status = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = /sew/cl_int_it_aend=>status_error )
                                 ( sign = 'I' option = 'EQ' low = /sew/cl_int_it_aend=>status_initial ) ).

    "In case only approved and canceled requests should be posted
    IF initial EQ abap_true.
      status = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = /sew/cl_int_it_aend=>status_initial ) ).
    ENDIF.

    "In case only failed requests should be posted
    IF error EQ abap_true.
      status = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = /sew/cl_int_it_aend=>status_error ) ).
    ENDIF.

  ENDMETHOD.


  METHOD CONSTRUCTOR.

    me->simu  = simu.
    me->dates = dates.

    "prepare protocol in case dialog is active and ALV is requested
    IF alv   EQ abap_true. "AND
*       batch EQ abap_false.
       message_handler = cl_hrpay00_message_handler=>get_message_handler( ).
    ENDIF.

  ENDMETHOD.


  METHOD delimit_hrp1000.

    DATA: lr_structdescr TYPE REF TO cl_abap_structdescr,
          lr_tabledescr  TYPE REF TO cl_abap_tabledescr,
          lr_table       TYPE REF TO data.
    FIELD-SYMBOLS: <hrp_old>     TYPE any,
                   <hrp_old_tab> TYPE STANDARD TABLE.
    ASSIGN hrp_old TO <hrp_old>.

    lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV #( 'P' && infty ) ).
    lr_tabledescr ?= cl_abap_tabledescr=>create( p_line_type = lr_structdescr ).
    CREATE DATA lr_table TYPE HANDLE lr_tabledescr.
    ASSIGN lr_table->* TO <hrp_old_tab>.
    APPEND <hrp_old> TO <hrp_old_tab>.


    ASSIGN COMPONENT /sew/cl_int_constants=>objid OF STRUCTURE <hrp_old> TO FIELD-SYMBOL(<objid>).
    ASSIGN COMPONENT /sew/cl_int_constants=>begda OF STRUCTURE <hrp_old> TO FIELD-SYMBOL(<begda>).
    ASSIGN COMPONENT /sew/cl_int_constants=>endda OF STRUCTURE <hrp_old> TO FIELD-SYMBOL(<endda>).

    IF simu = abap_true.
      DATA(begda_ext) = /sew/cl_int_utility=>get_external_date( date = <begda> ).
      DATA(endda_ext) = /sew/cl_int_utility=>get_external_date( date = <endda> ).
      CONCATENATE begda_ext '-' endda_ext INTO DATA(date) SEPARATED BY space.
      return = VALUE bapiret1( type = /sew/cl_int_constants=>success
                                 id = /sew/cl_int_constants=>msg_class_int
                                 number = /sew/cl_int_constants=>msg_no-m26
                                 message_v1 = <objid>
                                 message_v2 = infty
                                 message_v3 = date
*                                 message_v4 = date
).
    ELSE.
      CALL FUNCTION 'RH_CUT_OBJECT'
        EXPORTING
*         LOAD               = 'X'
          gdate              = delimit_date
          histo              = ' '
          gstat              = '1'
*         INVERT             = 'X'
          vtask              = 'D'
*         ORDER_FLG          = 'X'
*         COMMIT_FLG         = 'X'
          authy              = ' '
*         PPPAR_IMP          =
          cut_dependents     = 'X'
*         KEEP_LUPD          =
*         WORKF_ACTV         = 'X'
        TABLES
          i1000              = <hrp_old_tab>
*         ILFCODE            =
        EXCEPTIONS
          error_during_cut   = 1
          no_authorization   = 2
          gdate_before_begda = 3
          corr_exit          = 4
          OTHERS             = 5.


*    CALL FUNCTION 'RH_CUT_INFTY'
*      EXPORTING
**       LOAD               = 'X'
*        gdate              = delimit_date
*        histo              = ' '
**       DEL_SUCC           = ' '
*        vtask              = 'D'
**       ORDER_FLG          = 'X'
**       COMMIT_FLG         = 'X'
*        authy              = ' '
**       PPPAR_IMP          =
**       KEEP_LUPD          =
**       WORKF_ACTV         = 'X'
*      TABLES
*        innnn              = <hrp_old_tab>
**       ILFCODE            =
*      EXCEPTIONS
*        error_during_cut   = 1
*        no_authorization   = 2
*        gdate_before_begda = 3
*        cut_of_timco_one   = 4
*        corr_exit          = 5
*        OTHERS             = 6.

      IF sy-subrc <> 0.
* Implement suitable error handling here
        return = /sew/cl_int_utility=>map_sy_msg( msgid = sy-msgid msgty = sy-msgty msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 msgv4 = sy-msgv4 ).
*      /sew/cl_int_utility=>get_message( EXPORTING msgid = return-id msgno = return-number
*                                        IMPORTING message = return-message ).
        "Add messages
        IF return IS NOT INITIAL.
        ENDIF.
        "Success
      ELSE.

        return = VALUE bapiret1( type = /sew/cl_int_constants=>success
                             id = /sew/cl_int_constants=>msg_class_int
                             number = /sew/cl_int_constants=>msg_no-m15
                             message_v1 = <objid>
                             message_v2 = <begda>
                             message_v3 = <endda>
                             message_v4 = delimit_date
                           ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_om_aend_data.

    "get infotype changes
    om_aend = NEW /sew/cl_int_om_aend( )->get( objid  = objid
                                               intrun = intrun
                                               dates  = dates
                                               status = status ).

    CHECK om_aend IS NOT INITIAL.

    SORT om_aend BY sap_id timestamp infty ASCENDING.

  ENDMETHOD.


  METHOD maintain_hrpxxxx.
*    DATA: lr_structdescr TYPE REF TO cl_abap_structdescr,
*          lr_tabledescr  TYPE REF TO cl_abap_tabledescr,
*          lr_table       TYPE REF TO data.
*    FIELD-SYMBOLS:  <hrp_old_tab> TYPE STANDARD TABLE.
*
*    lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV #( 'P' && infty ) ).
*    lr_tabledescr ?= cl_abap_tabledescr=>create( p_line_type = lr_structdescr ).
*    CREATE DATA lr_table TYPE HANDLE lr_tabledescr.
*    ASSIGN lr_table->* TO <hrp_old_tab>.
*
*    CALL FUNCTION 'RH_READ_INFTY'
*      EXPORTING
**       AUTHORITY            = 'DISP'
*        with_stru_auth       = ' '
*        plvar                = plvar
*        otype                = otype
*        objid                = sap_id
*        infty                = infty
**       ISTAT                = ' '
**       EXTEND               = 'X'
*        subty                = subty
*        begda                = begda
*        endda                = endda
**       CONDITION            = '00000'
**       INFTB                = '1'
**       SORT                 = 'X'
**       VIA_T777D            = ' '
*      TABLES
*        innnn                = <hrp_old_tab>
**       OBJECTS              =
*      EXCEPTIONS
*        all_infty_with_subty = 1
*        nothing_found        = 2
*        no_objects           = 3
*        wrong_condition      = 4
*        wrong_parameters     = 5
*        OTHERS               = 6.
*
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*      return = /sew/cl_int_utility=>map_sy_msg( msgid = sy-msgid msgty = sy-msgty msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 msgv4 = sy-msgv4 ).
**        /sew/cl_int_utility=>get_message( EXPORTING msgid = return-id msgno = return-number
**                                          IMPORTING message = return-message ).
*    ELSE.
*      READ TABLE <hrp_old_tab> ASSIGNING FIELD-SYMBOL(<hrp_old>) INDEX 1.
*      "If IT is HRP1000 we need to get language specific entry
*      IF infty = /sew/cl_int_constants=>it1000.
*        IF <hrp_old> IS ASSIGNED.
*          UNASSIGN <hrp_old>.
*        ENDIF.
**        READ TABLE <hrp_old_tab> ASSIGNING <hrp_old> WITH KEY langu = langu.
*        LOOP AT <hrp_old_tab> ASSIGNING <hrp_old>.
*          ASSIGN COMPONENT 'LANGU' OF STRUCTURE <hrp_old> TO FIELD-SYMBOL(<langu>).
*          IF <langu> = langu.
*            hrp_old = <hrp_old>.
*            EXIT.
*          ENDIF.
*        ENDLOOP.
*      ELSE.
*        IF <hrp_old> IS ASSIGNED.
*          hrp_old = <hrp_old>.
*        ENDIF.
*      ENDIF.
*
*    ENDIF.
    DATA: delimit_date   TYPE dats,
          it             TYPE REF TO data,
          lr_structdescr TYPE REF TO cl_abap_structdescr,
          hrp_old_it     TYPE REF TO data,
          lr_tabledescr  TYPE REF TO cl_abap_tabledescr,
          lr_table_old   TYPE REF TO data,
          lr_table_new   TYPE REF TO data.

    FIELD-SYMBOLS: <hrp_new_tab> TYPE STANDARD TABLE,
                   <hrp_old>     TYPE any,
                   <hrp_new>     TYPE any,
                   <hrp_old_tab> TYPE STANDARD TABLE.
    ASSIGN hrp_new TO <hrp_new>.

    lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV #( 'P' && infty ) ).
    CREATE DATA hrp_old_it TYPE HANDLE lr_structdescr.
    ASSIGN hrp_old_it->* TO <hrp_old>.
    lr_tabledescr ?= cl_abap_tabledescr=>create( p_line_type = lr_structdescr ).
    CREATE DATA lr_table_old TYPE HANDLE lr_tabledescr.
    CREATE DATA lr_table_new TYPE HANDLE lr_tabledescr.
    ASSIGN lr_table_old->* TO <hrp_old_tab>.
    ASSIGN lr_table_new->* TO <hrp_new_tab>.

    IF simu = abap_true.
      DATA(begda_ext) = /sew/cl_int_utility=>get_external_date( date = begda ).
      DATA(endda_ext) = /sew/cl_int_utility=>get_external_date( date = endda ).
      CONCATENATE begda_ext '-' endda_ext INTO DATA(date) SEPARATED BY space.
      return = VALUE bapiret1( type = /sew/cl_int_constants=>success
                                 id = /sew/cl_int_constants=>msg_class_int
                                 number = /sew/cl_int_constants=>msg_no-m27
                                 message_v1 = sap_id
                                 message_v2 = infty
                                 message_v3 = date
*                                 message_v4 = date
).
    ELSE.
      "If IT already exists and it's no simple creation check if delimit needs to take place
      IF it_change = abap_true.
        "When other IT then HRP1000 and HRP1001 we need to delimit old record
        IF infty NE /sew/cl_int_constants=>it1000 AND infty NE /sew/cl_int_constants=>it1001.
          delimit_date = begda - 1.
          "Read old record
          me->read_hrpxxxx(
        EXPORTING
          begda = begda
          endda = begda
          infty = infty
          langu = langu
          otype = otype
          sap_id = sap_id
          subty = subty
          plvar = CONV #( /sew/cl_int_constants=>plvar )
          IMPORTING
            hrp_old = <hrp_old>
            return = return ).

          IF <hrp_old> IS INITIAL AND action = /sew/cl_int_constants=>infty_operation-om_delimit.
            return = VALUE bapiret1( type = /sew/cl_int_constants=>warning
                                     id = /sew/cl_int_constants=>msg_class_int
                                     number = /sew/cl_int_constants=>msg_no-m29
*                                     message_v1 = <om_aend>-infty
*                                     message_v2 = <om_aend>-begda
*                                     message_v3 = <om_aend>-endda
*                                     message_v4 = <om_aend>-sap_id
                                    ).
          ENDIF.

          ASSIGN COMPONENT /sew/cl_int_constants=>begda OF STRUCTURE <hrp_old> TO FIELD-SYMBOL(<begda_old>).
          "If begda old and begda are equal then there are two changes on the same day
          IF <begda_old> = begda.

            "Else old records needs to be delimited
          ELSE.
            APPEND <hrp_old> TO <hrp_old_tab>.
            CALL FUNCTION 'RH_CUT_INFTY'
              EXPORTING
*               LOAD               = 'X'
                gdate              = delimit_date
                histo              = ' '
*               DEL_SUCC           = ' '
                vtask              = 'D'
*               ORDER_FLG          = 'X'
*               COMMIT_FLG         = 'X'
                authy              = ' '
*               PPPAR_IMP          =
*               KEEP_LUPD          =
*               WORKF_ACTV         = 'X'
              TABLES
                innnn              = <hrp_old_tab>
*               ILFCODE            =
              EXCEPTIONS
                error_during_cut   = 1
                no_authorization   = 2
                gdate_before_begda = 3
                cut_of_timco_one   = 4
                corr_exit          = 5
                OTHERS             = 6.
          ENDIF.
        ENDIF.
      ENDIF.

      "Add new entry
      "Insert changes
      CONCATENATE 'P' infty INTO DATA(it_name).

      CREATE DATA it TYPE (it_name).
      ASSIGN it->* TO <hrp_new>.
      MOVE-CORRESPONDING hrp_new TO <hrp_new>.
      ASSIGN COMPONENT 'ISTAT' OF STRUCTURE <hrp_new> TO FIELD-SYMBOL(<istat>).
      <istat> = '1'.
      APPEND <hrp_new> TO <hrp_new_tab>.

      CALL FUNCTION 'RH_INSERT_INFTY'
        EXPORTING
          fcode               = 'INSE'
          vtask               = 'D'
*         ORDER_FLG           = 'X'
*         COMMIT_FLG          = 'X'
          authy               = ' '
*         PPPAR_IMP           =
*         OLD_TABNR           = ' '
*         REPID               = ' '
*         FORM                = ' '
*         KEEP_LUPD           =
*         WORKF_ACTV          = 'X'
        TABLES
          innnn               = <hrp_new_tab>
*         ILFCODE             =
        EXCEPTIONS
          no_authorization    = 1
          error_during_insert = 2
          repid_form_initial  = 3
          corr_exit           = 4
          begda_greater_endda = 5
          OTHERS              = 6.
      IF sy-subrc <> 0.
* Implement suitable error handling here
        return = /sew/cl_int_utility=>map_sy_msg( msgid = sy-msgid msgty = sy-msgty msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 msgv4 = sy-msgv4 ).
*      /sew/cl_int_utility=>get_message( EXPORTING msgid = return-id msgno = return-number
*                                        IMPORTING message = return-message ).
        "Add messages
        IF return IS NOT INITIAL.
        ENDIF.
      ELSE.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD object_creation.
    DATA: it        TYPE REF TO data,
          wplog     TYPE wplog,
          obj_struc TYPE p1000,
          obj       TYPE TABLE OF p1000,
          obj_idx   TYPE TABLE OF hrobject,
          om_aendup TYPE /sew/int_om_aeup.
    FIELD-SYMBOLS: <struk>    TYPE any.
    CONCATENATE 'P' om_aend-infty INTO DATA(it_name).

    CREATE DATA it TYPE (it_name).
    ASSIGN it->* TO <struk>.
    MOVE-CORRESPONDING om_aend TO wplog.
    cl_hr_pnnnn_type_cast=>wplog_to_pnnnn(
      EXPORTING
        wplog = wplog
      IMPORTING
        pnnnn = <struk> ).
*    SELECT SINGLE intca FROM t500l WHERE molga = @om_aend-molga INTO @DATA(country).
*    SELECT SINGLE spras FROM t001 WHERE land1 = @country INTO @DATA(spras).
    ASSIGN COMPONENT 'LANGU' OF STRUCTURE <struk> TO FIELD-SYMBOL(<langu>).
*    <langu> = spras.
    /sew/cl_int_utility=>get_spras_by_molga( EXPORTING molga = om_aend-molga
                                             IMPORTING spras = DATA(spras) langu = DATA(langu) ).
    <langu> = spras.
    APPEND INITIAL LINE TO obj ASSIGNING FIELD-SYMBOL(<obj_struc>).
    <obj_struc> = CORRESPONDING #( <struk> ).

*    CALL FUNCTION 'RHOM_ALL_BUFFER_INIT'.
    IF simu = abap_true.
      DATA(begda_ext) = /sew/cl_int_utility=>get_external_date( date = om_aend-begda ).
      DATA(endda_ext) = /sew/cl_int_utility=>get_external_date( date = om_aend-endda ).
      CONCATENATE begda_ext '-' endda_ext INTO DATA(date) SEPARATED BY space.
      return = VALUE bapiret1( type = /sew/cl_int_constants=>success
                                 id = /sew/cl_int_constants=>msg_class_int
                                 number = /sew/cl_int_constants=>msg_no-m25
                                 message_v1 = om_aend-infty
                                 message_v2 = om_aend-otype
                                 message_v3 = date
*                                 message_v4 = date
).
    ELSE.
      TRY.
          CALL FUNCTION 'RHOM_CREATE_OBJECTS_FROM_TAB'
            EXPORTING
              plvar               = '01'
              otype               = om_aend-otype "/sew/cl_int_constants=>position
              ostat               = '1'
              begda               = om_aend-begda
              endda               = om_aend-endda
              buffer_mode         = 'X'
*             SUPPRESS_STRU_AUTH  =
            TABLES
              new_objects         = obj
              objectindx          = obj_idx
            EXCEPTIONS
              error_during_insert = 1
              no_authorization    = 2
              corr_exit           = 3
              OTHERS              = 4.
          IF sy-subrc <> 0.
* Implement suitable error handling here
            "Create Message for Error during dummy creation

*          return-id = sy-msgid.
*          return-type = sy-msgty.
*          return-number = sy-msgno.
*          return-message_v1 = sy-msgv1.
*          return-message_v2 = sy-msgv2.
*          return-message_v3 = sy-msgv3.
*          return-message_v4 = sy-msgv4.
            return = /sew/cl_int_utility=>map_sy_msg( msgid = sy-msgid msgty = sy-msgty msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 msgv4 = sy-msgv4 ).
*          /sew/cl_int_utility=>get_message( EXPORTING msgid = return-id msgno = return-number
*                                            IMPORTING message = return-message ).

          ELSE.
*            CALL FUNCTION 'RHOM_WRITE_BUFFER_TO_DB'.
            READ TABLE obj ASSIGNING FIELD-SYMBOL(<obj_single>) INDEX 1.
            om_aend-sap_id = <obj_single>-objid.

            "Add OM_AENDUP entry for department creation to send objectid to Cloud
            IF simu NE abap_true.
              DATA(om_aend_up) = NEW /sew/cl_int_om_aendup( ).
              om_aendup-sap_id = om_aend-sap_id.
              om_aendup-cloud_id = om_aend-cloud_id.
              om_aendup-molga = om_aend-molga.
              om_aendup-int_run = /sew/cl_int_utility=>create_guid( ).
              om_aendup-mandt = sy-mandt.
              GET TIME STAMP FIELD om_aendup-timestamp.
              om_aendup-begda = om_aend-begda.
              om_aendup-endda = om_aend-endda.
              om_aendup-aedtm = sy-datum.
              om_aendup-uname = sy-uname.
              om_aendup-status = /sew/cl_int_constants=>booking_status-initial.
              om_aend_up->save_entries( om_aendup = om_aendup ).
            ENDIF.

            COMMIT WORK AND WAIT.
          ENDIF.
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD object_modification.
*    DATA: it        TYPE REF TO data,
*          wplog     TYPE wplog,
*          obj_struc TYPE p1000,
*          obj       TYPE TABLE OF p1000,
*          obj_idx   TYPE TABLE OF hrobject.
*    FIELD-SYMBOLS: <struk>    TYPE any.
*    CONCATENATE 'P' om_aend-infty INTO DATA(it_name).
*
*    CREATE DATA it TYPE (it_name).
*    ASSIGN it->* TO <struk>.
*    MOVE-CORRESPONDING om_aend TO wplog.
*    cl_hr_pnnnn_type_cast=>wplog_to_pnnnn(
*      EXPORTING
*        wplog = wplog
*      IMPORTING
*        pnnnn = <struk> ).
*    APPEND INITIAL LINE TO obj ASSIGNING FIELD-SYMBOL(<obj_struc>).
*    <obj_struc> = CORRESPONDING #( <struk> ).
*
*    CALL FUNCTION 'RHOM_ALL_BUFFER_INIT'.
*    TRY.
*        CALL FUNCTION 'RHOM_CREATE_OBJECTS_FROM_TAB'
*          EXPORTING
*            plvar               = '01'
*            otype               = om_aend-otype "/sew/cl_int_constants=>position
*            ostat               = '1'
*            begda               = om_aend-begda
*            endda               = om_aend-endda
**           BUFFER_MODE         =
**           SUPPRESS_STRU_AUTH  =
*          TABLES
*            new_objects         = obj
*            objectindx          = obj_idx
*          EXCEPTIONS
*            error_during_insert = 1
*            no_authorization    = 2
*            corr_exit           = 3
*            OTHERS              = 4.
*        IF sy-subrc <> 0.
** Implement suitable error handling here
*          "Create Message for Error during dummy creation
*
*          return-id = sy-msgid.
*          return-type = sy-msgty.
*          return-number = sy-msgno.
*          return-message_v1 = sy-msgv1.
*          return-message_v2 = sy-msgv2.
*          return-message_v3 = sy-msgv3.
*          return-message_v4 = sy-msgv4.
*          /sew/cl_int_utility=>get_message( EXPORTING msgid = return-id msgno = return-number
*                                            IMPORTING message = return-message ).
*
*        ELSE.
*          CALL FUNCTION 'RHOM_WRITE_BUFFER_TO_DB'.
**      COMMIT WORK AND WAIT.
*        ENDIF.
*    ENDTRY.

    DATA: it             TYPE REF TO data,
          wplog          TYPE wplog,
          obj_struc      TYPE p1000,
          obj            TYPE TABLE OF p1000,
          obj_idx        TYPE TABLE OF hrobject,
          wplog_tab      TYPE wplog_tab,
          it_tab         TYPE REF TO data,
          hrpad_return   TYPE hrpad_return,
          delimit_date   TYPE dats,
          lr_structdescr TYPE REF TO cl_abap_structdescr,
          lr_tabledescr  TYPE REF TO cl_abap_tabledescr,
          lr_table       TYPE REF TO data,
          check_date     TYPE dats.
    FIELD-SYMBOLS: <struk>     TYPE any,
                   <hrp_data>  TYPE STANDARD TABLE,
                   <ft_it_old> TYPE STANDARD TABLE.

    CONCATENATE 'P' om_aend-infty INTO DATA(it_name).

    CREATE DATA it_tab TYPE STANDARD TABLE OF (it_name).
    ASSIGN it_tab->* TO <hrp_data>.
    CREATE DATA it TYPE (it_name).
    ASSIGN it->* TO <struk>.
    MOVE-CORRESPONDING om_aend TO wplog.
    wplog-objid = om_aend-sap_id.
    wplog-plvar = '01'.
    SELECT SINGLE intca FROM t500l WHERE molga = @om_aend-molga INTO @DATA(country).
    SELECT SINGLE spras FROM t001 WHERE land1 = @country INTO @DATA(spras).
*      CALL FUNCTION 'CONVERSION_EXIT_ISOLA_OUTPUT'
*        EXPORTING
*          input         = spras
*       IMPORTING
*         OUTPUT        = wplog-
    .

    APPEND wplog TO wplog_tab.
    cl_hr_pnnnn_type_cast=>wplog_to_pnnnn_tab(
      EXPORTING
        wplog_tab = wplog_tab
      IMPORTING
        pnnnn_tab = <hrp_data> ).
    cl_hr_pnnnn_type_cast=>wplog_to_pnnnn(
      EXPORTING
        wplog = wplog
      IMPORTING
        pnnnn = <struk> ).
    APPEND INITIAL LINE TO obj ASSIGNING FIELD-SYMBOL(<obj_struc>).
    <obj_struc> = CORRESPONDING #( <struk> ).
    LOOP AT <hrp_data> ASSIGNING FIELD-SYMBOL(<hrp_struc>).
      ASSIGN COMPONENT 'GDATE' OF STRUCTURE <hrp_struc> TO FIELD-SYMBOL(<gdate>).
      CLEAR <gdate>.
      ASSIGN COMPONENT 'LANGU' OF STRUCTURE <hrp_struc> TO FIELD-SYMBOL(<langu>).
      <langu> = spras.
    ENDLOOP.

    "Read old IT
    check_date = om_aend-begda - 1.
    lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV #( 'P' && om_aend-infty ) ).
    lr_tabledescr ?= cl_abap_tabledescr=>create( p_line_type = lr_structdescr ).
    CREATE DATA lr_table TYPE HANDLE lr_tabledescr.
    ASSIGN lr_table->* TO <ft_it_old>.
    IF om_aend-infty = /sew/cl_int_constants=>it1000.
      CALL FUNCTION 'RH_READ_INFTY_1000'
        EXPORTING
*         AUTHORITY        = 'DISP'
          with_stru_auth   = ' '
          plvar            = '01'
          otype            = om_aend-otype
          objid            = om_aend-sap_id
*         ISTAT            = ' '
*         EXTEND           = 'X'
          begda            = check_date
          endda            = check_date
*         CONDITION        = '00000'
*         SORT             = 'X'
        TABLES
          i1000            = <ft_it_old>
*         OBJECTS          =
        EXCEPTIONS
          nothing_found    = 1
          wrong_condition  = 2
          wrong_parameters = 3
          OTHERS           = 4.
      IF sy-subrc <> 0.
* Implement suitable error handling here
        return = /sew/cl_int_utility=>map_sy_msg( msgid = sy-msgid msgty = sy-msgty msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 msgv4 = sy-msgv4 ).
*        /sew/cl_int_utility=>get_message( EXPORTING msgid = return-id msgno = return-number
*                                          IMPORTING message = return-message ).
        IF return IS NOT INITIAL.
          hrpad_return = CORRESPONDING #( return ).
          APPEND hrpad_return TO return_tab.
          CLEAR: return, hrpad_return.
        ENDIF.
      ENDIF.

    ELSE.

      CALL FUNCTION 'RH_READ_INFTY_NNNN'
        EXPORTING
*         AUTHORITY             = 'DISP'
          with_stru_auth        = ' '
          plvar                 = '01'
          otype                 = om_aend-otype
          objid                 = om_aend-sap_id
          infty                 = om_aend-infty
*         ISTAT                 = ' '
*         EXTEND                = 'X'
*         SUBTY                 = ' '
          begda                 = check_date
          endda                 = check_date
*         CONDITION             = '00000'
*         INFTB                 = '1'
*         SORT                  = 'X'
        TABLES
          innnn                 = <ft_it_old>
*         OBJECTS               =
        EXCEPTIONS
          nothing_found         = 1
          wrong_condition       = 2
          infotyp_not_supported = 3
          wrong_parameters      = 4
          OTHERS                = 5.
      IF sy-subrc <> 0.
* Implement suitable error handling here
        return = /sew/cl_int_utility=>map_sy_msg( msgid = sy-msgid msgty = sy-msgty msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 msgv4 = sy-msgv4 ).
*        /sew/cl_int_utility=>get_message( EXPORTING msgid = return-id msgno = return-number
*                                          IMPORTING message = return-message ).
        IF return IS NOT INITIAL.
          hrpad_return = CORRESPONDING #( return ).
          APPEND hrpad_return TO return_tab.
          CLEAR: return, hrpad_return.
        ENDIF.
      ENDIF.
    ENDIF.

    "Insert changes
    CALL FUNCTION 'RH_INSERT_INFTY'
      EXPORTING
        fcode               = 'INSE'
        vtask               = 'D'
*       ORDER_FLG           = 'X'
*       COMMIT_FLG          = 'X'
        authy               = ' '
*       PPPAR_IMP           =
*       OLD_TABNR           = ' '
*       REPID               = ' '
*       FORM                = ' '
*       KEEP_LUPD           =
*       WORKF_ACTV          = 'X'
      TABLES
        innnn               = <hrp_data>
*       ILFCODE             =
      EXCEPTIONS
        no_authorization    = 1
        error_during_insert = 2
        repid_form_initial  = 3
        corr_exit           = 4
        begda_greater_endda = 5
        OTHERS              = 6.
    IF sy-subrc <> 0.
* Implement suitable error handling here
      return = /sew/cl_int_utility=>map_sy_msg( msgid = sy-msgid msgty = sy-msgty msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 msgv4 = sy-msgv4 ).
*      /sew/cl_int_utility=>get_message( EXPORTING msgid = return-id msgno = return-number
*                                        IMPORTING message = return-message ).
      "Add messages
      IF return IS NOT INITIAL.
      ENDIF.
    ELSE.


    ENDIF.
    READ TABLE <ft_it_old> ASSIGNING FIELD-SYMBOL(<fs_it_old>) INDEX 1.
    "Delimit old IT if available
    IF <fs_it_old> IS ASSIGNED.
      IF <fs_it_old> IS NOT INITIAL.
        delimit_date = om_aend-begda - 1.
        ASSIGN COMPONENT 'BEGDA' OF STRUCTURE <fs_it_old> TO FIELD-SYMBOL(<fs_begda_old>).
        IF <fs_begda_old> NE om_aend-begda.
*          CALL FUNCTION 'RH_CUT_INFTY'
*            EXPORTING
**             LOAD               = 'X'
*              gdate              = check_date
*              histo              = ' '
**             DEL_SUCC           = ' '
*              vtask              = 'D'
**             ORDER_FLG          = 'X'
**             COMMIT_FLG         = 'X'
*              authy              = ' '
**             PPPAR_IMP          =
**             KEEP_LUPD          =
**             WORKF_ACTV         = 'X'
*            TABLES
*              innnn              = <ft_it_old>
**             ILFCODE            =
*            EXCEPTIONS
*              error_during_cut   = 1
*              no_authorization   = 2
*              gdate_before_begda = 3
*              cut_of_timco_one   = 4
*              corr_exit          = 5
*              OTHERS             = 6.

        ELSE.

*          CALL FUNCTION 'RH_DELETE_INFTY'
*            EXPORTING
**             LOAD                = 'X'
*              vtask               = 'D'
**             ORDER_FLG           = 'X'
**             COMMIT_FLG          = 'X'
*              authy               = ' '
**             PPPAR_IMP           =
**             KEEP_LUPD           =
**             WORKF_ACTV          = 'X'
*            TABLES
*              innnn               = <ft_it_old>
**             ILFCODE             =
*            EXCEPTIONS
*              error_during_delete = 1
*              no_authorization    = 2
*              delete_first_record = 3
*              corr_exit           = 4
*              OTHERS              = 5.


*          CALL FUNCTION 'RH_DELETE_OBJECT'
*            EXPORTING
*              plvar                        = '01'
*              otype                        = 'O'
*              objid                        = om_aend-sap_id
*              vtask                        = 'D'
**             ORDER_FLG                    = 'X'
**             COMMIT_FLG                   = 'X'
*              authy                        = ' '
**             CONFIRM                      = ' '
**             DELETE_1205_WFDID            = 'X'
**             DELETE_USER_PROFILES         = 'X'
**             DELETE_DEPENDENTS            = 'X'
**             KEEP_LUPD                    =
**             WORKF_ACTV                   = 'X'
**             NO_EXCEPT_FOREIGN_DATA       = ' '
** IMPORTING
**             CONFIRM_EXIT                 =
*            TABLES
*              del_objects                  = <ft_it_old>
**             ILFCODE                      =
*            EXCEPTIONS
*              error_during_delete          = 1
*              no_authorization             = 2
*              corr_exit                    = 3
*              buffer_upd_with_foreign_data = 4
*              OTHERS                       = 5.
        ENDIF.


        IF sy-subrc <> 0.
* Implement suitable error handling here
          "Error when delimiting old IT
          return = /sew/cl_int_utility=>map_sy_msg( msgid = sy-msgid msgty = sy-msgty msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 msgv4 = sy-msgv4 ).
*          /sew/cl_int_utility=>get_message( EXPORTING msgid = return-id msgno = return-number
*                                            IMPORTING message = return-message ).
          IF return IS NOT INITIAL.
            hrpad_return = CORRESPONDING #( return ).
            APPEND hrpad_return TO return_tab.
            CLEAR: return, hrpad_return.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD post_om_aend.

    DATA: sap_id          TYPE hrobjid,
          count           TYPE i VALUE 0,
          max             TYPE i VALUE 0,
          wplog           TYPE wplog,
          wplog_tab       TYPE wplog_tab,
*          hrpdata TYPE STANDARD TABLE,
          key             TYPE bapipakey,
          return          TYPE bapireturn1,
          it_tab          TYPE REF TO data,
          it              TYPE REF TO data,
          sap_id_error    TYPE rsdsselopt_t,
          om_aend_tmp     TYPE /sew/tt_om_aend,
          om_aend_del     TYPE /sew/tt_om_aend,
          supress_struk   TYPE pppar_exep,
          return_tab      TYPE hrpad_return_tab,
          no_change       TYPE boole_d,
          hrp_old_it      TYPE REF TO data,
          hrp_last_it     TYPE REF TO data,
          lr_structdescr  TYPE REF TO cl_abap_structdescr,
          delimit_date    TYPE dats,
          sap_id_buffer   TYPE hrobjid,
          object_creation TYPE boole_d,
          commit_flag     TYPE boole_d,
          begda_ext       TYPE /sew/dd_date_ext,
          endda_ext       TYPE /sew/dd_date_ext,
          exception       TYPE REF TO cx_root.

    FIELD-SYMBOLS: <struk>       TYPE any,
                   <infotype>    TYPE any,
                   <hrp_data>    TYPE STANDARD TABLE,
                   <om_aend_tmp> TYPE /sew/int_om_aend,
                   <hrp_old>     TYPE any,
                   <hrp_last>    TYPE any.

    CALL FUNCTION 'RHOM_ALL_BUFFER_INIT'.
    commit_flag = abap_true.
    IF simu = abap_true.
      commit_flag = abap_false.
    ENDIF.
    LOOP AT me->status_handler->om_aend ASSIGNING FIELD-SYMBOL(<om_aend>). "WHERE status = '01' OR status = '07'.
      IF me->book_manager_relation = abap_true.
        IF <om_aend>-subty NE /sew/cl_int_constants=>relations-manager.
          CONTINUE.
        ENDIF.
      ELSE.
        IF <om_aend>-subty = /sew/cl_int_constants=>relations-manager.
          count = count + 1.
          IF <om_aend>-sap_id IS INITIAL.
            IF sap_id_buffer IS NOT INITIAL.
              APPEND <om_aend> TO me->status_handler->del_om_aend.
              <om_aend>-sap_id = sap_id_buffer.
              APPEND <om_aend> TO om_aend_post.
            ENDIF.
          ENDIF.

          CONTINUE.
        ENDIF.
      ENDIF.
      me->status_handler->check_for_error(
         EXPORTING
            cloud_id = CONV #( <om_aend>-cloud_id )
            sap_id = CONV #( <om_aend>-sap_id )
            type = /sew/cl_int_constants=>orgunit
            int_run = <om_aend>-int_run
            ext_type = CONV #( <om_aend>-ext_type )
          IMPORTING
            has_error = DATA(has_error)
      ).
      IF has_error = abap_true.
        CONTINUE.
      ENDIF.

      CLEAR: om_aend_del, no_change.
      me->status_handler->aend_id = <om_aend>-aend_id.
      me->status_handler->begda = <om_aend>-begda.
      me->status_handler->endda = <om_aend>-endda.
      me->status_handler->molga = <om_aend>-molga.
      me->status_handler->cloud_id = <om_aend>-cloud_id.
      me->status_handler->sap_id = <om_aend>-sap_id.
      me->status_handler->int_run = <om_aend>-int_run.
      "if already an error for guid occurred go to next entry
      IF <om_aend>-cloud_id IN sap_id_error AND
         sap_id_error        IS NOT INITIAL.
*        IF <om_aend>-sap_id IS INITIAL.
        count = count + 1.
        CONTINUE.
*        ENDIF.
      ENDIF.

      IF sap_id_buffer NE <om_aend>-sap_id.
        CLEAR: sap_id_buffer, object_creation.
      ENDIF.

      IF <om_aend>-sap_id IS INITIAL.
        IF sap_id_buffer IS NOT INITIAL.
          <om_aend>-sap_id = sap_id_buffer.
        ENDIF.
      ENDIF.

      DATA(infty_operation) = NEW /sew/cl_int_it_operation( int_run = <om_aend>-int_run molga = <om_aend>-molga objid = <om_aend>-sap_id ).


*      DATA(error_handler) = NEW /sew/cl_int_error_handler( sap_id = CONV #( <om_aend>-sap_id ) cloud_id = CONV #( <om_aend>-cloud_id )
*                                                  aend_id = CONV #( <om_aend>-aend_id ) int_run = <om_aend>-int_run molga = <om_aend>-molga ).
      "default simulation status
      DATA(simu_tmp) = simu.

      "check for new content
      IF max EQ count.
        count = 0.
        CALL FUNCTION 'RHOM_ALL_BUFFER_INIT'.
        "get all om_aend entries with same sap_id run id
*        IF <om_aend>-action NE /sew/cl_int_constants=>hire.
        om_aend_tmp = VALUE /sew/tt_om_aend( FOR ls_om_aend IN me->status_handler->om_aend WHERE ( sap_id EQ <om_aend>-sap_id  AND cloud_id = <om_aend>-cloud_id ) ( ls_om_aend ) ).
*        ELSE.
*          om_aend_tmp = VALUE /sew/tt_om_aend( FOR ls_om_aend IN om_aend WHERE ( int_run = <om_aend>-int_run AND
*                                                                               cloud_id = <om_aend>-cloud_id ) ( ls_om_aend ) ).
*        ENDIF.
        max = lines( om_aend_tmp ).
      ENDIF.

      "in case more entries belong to actual integration run id
      IF max GT 1.
        simu_tmp = abap_true.
      ENDIF.

      "enqueue sap_id
      IF sap_id NE <om_aend>-sap_id OR
         sap_id IS INITIAL.

        "dequeue old sap_id
        IF sap_id IS NOT INITIAL.
*          CALL FUNCTION 'RH_PM_DEQUEUE'
*            EXPORTING
*              act_plvar = '01'
*              act_otype = <om_aend>-otype
*              act_objid = sap_id.
**            IMPORTING
**              return    = return.

*          CALL FUNCTION 'RHOM_DEQUEUE'
*            EXPORTING
*              plvar                    = '01'
*              otype                    = <om_aend>-otype
*              objid                    = <om_aend>-sap_id
*            EXCEPTIONS
*              dequeue_failed           = 1
*              object_changed_in_buffer = 2
*              OTHERS                   = 3.
*
*          "check for enqueue sap_id
*          IF sy-msgty EQ 'E'.
*            return = /sew/cl_int_utility=>map_sy_msg( msgid = sy-msgid msgty = sy-msgty msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 msgv4 = sy-msgv4 ).
*            /sew/cl_int_utility=>get_message( EXPORTING msgid = return-id msgno = return-number
*                                              IMPORTING message = return-message ).
*            "Add messages
**            error_handler->add_message_pa( EXPORTING sap_id = <om_aend>-sap_id int_run = <om_aend>-int_run return = return begda = <om_aend>-begda endda = <om_aend>-endda ).
**          /sew/cl_utils=>add_message_to_handler( EXPORTING iv_sap_id  = sap_id
**                                                           is_return = return
**                                                 CHANGING  MESSAGE_HANDLER = MESSAGE_HANDLER ).
*            CLEAR: sap_id, return.
*          ENDIF.
        ENDIF.

        IF <om_aend>-sap_id IS NOT INITIAL.
*          CALL FUNCTION 'RH_PM_ENQUEUE'
*            EXPORTING
*              act_plvar = '01'
*              act_otype = <om_aend>-otype
*              act_objid = <om_aend>-sap_id.
**            IMPORTING
**              return    = return.

*          CALL FUNCTION 'RHOM_ENQUEUE'
*            EXPORTING
*              plvar          = '01'
*              otype          = <om_aend>-otype
*              objid          = <om_aend>-sap_id
*            EXCEPTIONS
*              enqueue_failed = 1
*              OTHERS         = 2.
*
*
*
*          "check for enqueue sap_id
*          IF sy-msgty EQ 'E'.
*            return = /sew/cl_int_utility=>map_sy_msg( msgid = sy-msgid msgty = sy-msgty msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 msgv4 = sy-msgv4 ).
*            /sew/cl_int_utility=>get_message( EXPORTING msgid = return-id msgno = return-number
*                                              IMPORTING message = return-message ).
*            "Add messages
*
**          error_handler->add_message_pa( EXPORTING sap_id = <om_aend>-sap_id int_run = <om_aend>-int_run return = return begda = <om_aend>-begda endda = <om_aend>-endda ).
**        /sew/cl_utils=>add_message_to_handler( EXPORTING iv_sap_id  = <om_aend>-sap_id
**                                                         is_return = return
**                                               CHANGING  MESSAGE_HANDLER = MESSAGE_HANDLER ).
*            CLEAR: sap_id, return.
*
*            "collect lines in error output table
*            APPEND LINES OF om_aend_tmp TO om_aend_error.
*            APPEND VALUE #( sign = 'I' option = 'EQ' low = <om_aend>-sap_id ) TO sap_id_error.
*            count = max.
*
*            CONTINUE.
*          ENDIF.
        ENDIF.
        sap_id = <om_aend>-sap_id.
      ENDIF.

      "Get Objid -> if no objid exists and it is not HRP1000 record -> error
      IF <om_aend>-sap_id IS INITIAL AND <om_aend>-infty NE /sew/cl_int_constants=>it1000.
        /sew/cl_int_utility=>get_objid_by_cloudid( EXPORTING cloud_id = CONV #( <om_aend>-cloud_id ) begda = <om_aend>-begda endda = <om_aend>-endda IMPORTING objid = <om_aend>-sap_id ).
      ENDIF.

      IF ( <om_aend>-sap_id IS NOT INITIAL AND <om_aend>-infty NE /sew/cl_int_constants=>it1000 )
            OR ( <om_aend>-sap_id IS INITIAL AND <om_aend>-infty = /sew/cl_int_constants=>it1000 )
              OR ( <om_aend>-sap_id IS NOT INITIAL AND <om_aend>-infty = /sew/cl_int_constants=>it1000 ).

        "create infotype structure
        CONCATENATE 'P' <om_aend>-infty INTO DATA(it_name).
        CREATE DATA it_tab TYPE STANDARD TABLE OF (it_name).
        ASSIGN it_tab->* TO <hrp_data>.

        CREATE DATA it TYPE (it_name).
        ASSIGN it->* TO <struk>.

        IF <struk> IS ASSIGNED.
          count = count + 1.

          "get infotype data
          CLEAR: wplog_tab, <hrp_data>.
          MOVE-CORRESPONDING <om_aend> TO wplog.
          wplog-objid = <om_aend>-sap_id.
          wplog-plvar = '01'.
          APPEND wplog TO wplog_tab.
          cl_hr_pnnnn_type_cast=>wplog_to_pnnnn_tab(
            EXPORTING
              wplog_tab = wplog_tab
            IMPORTING
              pnnnn_tab = <hrp_data> ).

          cl_hr_pnnnn_type_cast=>wplog_to_pnnnn(
            EXPORTING
              wplog = wplog
            IMPORTING
              pnnnn = <struk> ).
*        supress_struk-suppstru = 'X'.

          "Get language key for molga
          /sew/cl_int_utility=>get_spras_by_molga( EXPORTING molga = <om_aend>-molga
                                                   IMPORTING spras = DATA(spras) langu = DATA(langu) ).
          lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV #( 'P' && <om_aend>-infty ) ).
          CREATE DATA hrp_old_it TYPE HANDLE lr_structdescr.
          ASSIGN hrp_old_it->* TO <hrp_old>.

          "me->read_hrpxxxx(
          infty_operation->read_hrpxxxx(
          EXPORTING
            begda = <om_aend>-endda
            endda = <om_aend>-endda
            infty = <om_aend>-infty
            langu = CONV #( spras )
            otype = <om_aend>-otype
            sap_id = <om_aend>-sap_id
            subty = <om_aend>-subty
            plvar = CONV #( /sew/cl_int_constants=>plvar )
            IMPORTING
              hrp_old = <hrp_old>
              return = return ).

          IF <om_aend>-infty = /sew/cl_int_constants=>it1000.
            ASSIGN COMPONENT 'GDATE' OF STRUCTURE <struk> TO FIELD-SYMBOL(<gdate>).
            CLEAR <gdate>.
*              SELECT SINGLE intca FROM t500l WHERE molga = @<om_aend>-molga INTO @DATA(country).
*              SELECT SINGLE spras FROM t001 WHERE land1 = @country INTO @DATA(spras).
            ASSIGN COMPONENT 'LANGU' OF STRUCTURE <struk> TO FIELD-SYMBOL(<langu>).
            <langu> = langu.
          ENDIF.
          DATA(infty_delta) = NEW /sew/cl_int_infty_delta( infty = <om_aend>-infty ).
          infty_delta->molga = <om_aend>-molga.
          infty_delta->check_infty_change_om( EXPORTING it_record = <struk> hrp_old = <hrp_old>
                                           IMPORTING it_record_upd = <struk> it_create = DATA(it_create)
                                                     it_change = DATA(it_change) fields = DATA(changed_fields) ). "return_tab = return_tab ).
          IF it_change = abap_true.
            <om_aend>-operation = /sew/cl_int_constants=>infty_operation-om_modify.
          ENDIF.
          IF it_create = abap_true.
            <om_aend>-operation = /sew/cl_int_constants=>infty_operation-om_insert.
          ENDIF.
          "Add messages
*        IF return_tab IS NOT INITIAL.
*
*        ENDIF.
          IF <om_aend>-operation = /sew/cl_int_constants=>infty_operation-om_delimit AND it_create = abap_true.
            CLEAR it_create.
          ENDIF.

          "If inactivation ignore no change and process
          IF it_change = abap_false AND it_create = abap_false AND <om_aend>-infty = /sew/cl_int_constants=>it1000.
            IF <om_aend>-department_status = 'I'.
              it_change = abap_true.
            ENDIF.
          ENDIF.

          IF it_change = abap_false AND it_create = abap_false.
            no_change = abap_true.
            begda_ext = /sew/cl_int_utility=>get_external_date( date = <om_aend>-begda ).
            endda_ext = /sew/cl_int_utility=>get_external_date( date = <om_aend>-endda ).
            return = VALUE bapiret1( type = /sew/cl_int_constants=>success
                                     id = /sew/cl_int_constants=>msg_class_int
                                     number = /sew/cl_int_constants=>msg_no-m23
                                     message_v1 = <om_aend>-infty
                                     message_v2 = begda_ext
                                     message_v3 = endda_ext
                                     message_v4 = <om_aend>-sap_id
                                    ).
          ELSE.
            "Maintain relations
            IF <om_aend>-infty = /sew/cl_int_constants=>it1001.
              ASSIGN COMPONENT /sew/cl_int_constants=>sobid OF STRUCTURE <struk> TO FIELD-SYMBOL(<sobid>).
              ASSIGN COMPONENT /sew/cl_int_constants=>sclas OF STRUCTURE <struk> TO FIELD-SYMBOL(<sclas>).
              ASSIGN COMPONENT /sew/cl_int_constants=>objid OF STRUCTURE <struk> TO FIELD-SYMBOL(<objid>).
*            IF <sclas> = /sew/cl_int_constants=>orgunit.
*              IF sap_id_buffer IS NOT INITIAL.
*                <sobid> = sap_id_buffer.
*              ENDIF.
*            ELSE.
*              IF sap_id_buffer IS NOT INITIAL.
*                <objid> = sap_id_buffer.
*              ENDIF.
*            ENDIF.
              IF <sobid> IS INITIAL.
                <sobid> = sap_id_buffer.
              ENDIF.
              IF <objid> IS INITIAL.
                <objid> = sap_id_buffer.
              ENDIF.

              IF <sobid> IS NOT INITIAL.
                "Build parent object
*              DATA(parent) = /sew/cl_int_utility=>build_object( begda = <om_aend>-begda endda = <om_aend>-endda objid = COND hrobjid( WHEN <sclas> = /sew/cl_int_constants=>orgunit THEN CONV #( <sobid> )
*                                                                     WHEN <sclas> = /sew/cl_int_constants=>costcenter THEN CONV #( <objid> ) )
*                                                 otype = COND otype( WHEN <sclas> = /sew/cl_int_constants=>orgunit AND <om_aend>-otype = /sew/cl_int_constants=>position THEN /sew/cl_int_constants=>position
*                                                                     WHEN <sclas> = /sew/cl_int_constants=>costcenter AND <om_aend>-otype = /sew/cl_int_constants=>orgunit THEN /sew/cl_int_constants=>orgunit
*                                                                     WHEN <sclas> = /sew/cl_int_constants=>orgunit AND <om_aend>-otype = /sew/cl_int_constants=>orgunit THEN /sew/cl_int_constants=>orgunit ) ).
*              "Build child object
*              DATA(child) = /sew/cl_int_utility=>build_object( begda = <om_aend>-begda endda = <om_aend>-endda objid = CONV #( <sobid> )
*                                                 otype = <sclas> bukrs = <om_aend>-legal_entity ).

                IF <om_aend>-subty = /sew/cl_int_constants=>relations-manager.
                  IF <sobid> = <objid>.
                    /sew/cl_int_utility=>get_pernr_by_cloudid( EXPORTING cloud_id = CONV #( <om_aend>-relat_obj ) begda = <om_aend>-begda endda = <om_aend>-endda IMPORTING pernr = DATA(manager_pernr) ).
                    /sew/cl_int_utility=>get_plans_it0001( EXPORTING pernr = manager_pernr begda = <om_aend>-begda endda = <om_aend>-endda IMPORTING plans = DATA(plans) ).
                    <sobid> = plans.
                    CLEAR: plans, manager_pernr.
                  ENDIF.
                ENDIF.

                DATA(parent) = /sew/cl_int_utility=>build_object( begda = <om_aend>-begda endda = <om_aend>-endda objid = <objid>
                                                   otype = <om_aend>-otype ).

                DATA(child) = /sew/cl_int_utility=>build_object( begda = <om_aend>-begda endda = <om_aend>-endda objid = CONV #( <sobid> )
                                                   otype = <sclas> bukrs = <om_aend>-legal_entity ).
                infty_operation->otype = <om_aend>-otype.
                "utility=>maintain_relation
                infty_operation->maintain_relation(
                  EXPORTING
                    action = <om_aend>-operation
                    begda = <om_aend>-begda
                    endda = <om_aend>-endda
                    relat = <om_aend>-subty+1(3)
                    rsign = <om_aend>-subty+0(1)
                    parent = CONV #( parent ) "CONV #( orgeh )
                    child = CONV #( child ) "CONV #( <pos_line> )
                    simu = simu
                    spras = spras
                    IMPORTING
                      return = return
                  ).
                CLEAR infty_operation->otype.
              ELSE.
                begda_ext = /sew/cl_int_utility=>get_external_date( date = <om_aend>-begda ).
                endda_ext = /sew/cl_int_utility=>get_external_date( date = <om_aend>-endda ).
                return = VALUE bapiret1( type = /sew/cl_int_constants=>success
                                           id = /sew/cl_int_constants=>msg_class_int
                                           number = /sew/cl_int_constants=>msg_no-m13
                                           message_v1 = <om_aend>-sap_id
                                           message_v2 = begda_ext
                                           message_v3 = endda_ext
*                                  message_v4 =
                                         ).
              ENDIF.
              "Add messages
              IF return IS NOT INITIAL.

                "Success
              ELSE.
                begda_ext = /sew/cl_int_utility=>get_external_date( date = <om_aend>-begda ).
                endda_ext = /sew/cl_int_utility=>get_external_date( date = <om_aend>-endda ).
                return = VALUE bapiret1( type = /sew/cl_int_constants=>success
                                           id = /sew/cl_int_constants=>msg_class_int
                                           number = /sew/cl_int_constants=>msg_no-m12
                                           message_v1 = <om_aend>-sap_id
                                           message_v2 = begda_ext
                                           message_v3 = endda_ext
*                                  message_v4 =
                                         ).

              ENDIF.
*          me->status_handler->add_log_message( aend_id = <om_aend>-aend_id bapiret1 = return hrpad_return = return_tab ).

              "Process all other HRP operations
            ELSE.
              "If its a creation and IT 1000 we need to call special HRP1000 creation logic
              IF <om_aend>-infty = /sew/cl_int_constants=>it1000 AND it_create = abap_true.
                "HRP1000 Creation
                IF <hrp_old> IS INITIAL.
                  APPEND <om_aend> TO om_aend_del.
                  me->object_creation(
                  EXPORTING
                    simu = simu
                    IMPORTING
                      return = return
                     CHANGING
                       om_aend = <om_aend> ).
                  "Add messages
                  IF return IS NOT INITIAL.
                    CLEAR om_aend_del.
                    "Success
                  ELSE.
                    begda_ext = /sew/cl_int_utility=>get_external_date( date = <om_aend>-begda ).
                    endda_ext = /sew/cl_int_utility=>get_external_date( date = <om_aend>-endda ).
                    return = VALUE bapiret1( type = /sew/cl_int_constants=>success
                                               id = /sew/cl_int_constants=>msg_class_int
                                               number = /sew/cl_int_constants=>msg_no-m8
                                               message_v1 = <om_aend>-sap_id
                                               message_v2 = begda_ext
                                               message_v3 = endda_ext
*                                  message_v4 =
                                             ).

                    "Buffer newly created sap id
                    sap_id_buffer = <om_aend>-sap_id.
                    object_creation = abap_true.

                  ENDIF.
                  me->status_handler->sap_id = <om_aend>-sap_id.
*            me->status_handler->add_log_message( aend_id = <om_aend>-aend_id bapiret1 = return hrpad_return = return_tab ).
                  IF return-type NE /sew/cl_int_constants=>error.
                    MODIFY om_aend FROM <om_aend> TRANSPORTING sap_id WHERE cloud_id = <om_aend>-cloud_id AND int_run = <om_aend>-int_run.
                  ENDIF.
                ENDIF.
                "Else we can call general IT maintain logic
              ELSE.
                "If OM_AEND records has inactivation flag set delimit for HRP1000
                IF <om_aend>-department_status = 'I'.
                  "Check if deactivation on same day as creation
                  CREATE DATA hrp_last_it TYPE HANDLE lr_structdescr.
                  ASSIGN hrp_last_it->* TO <hrp_last>.
                  delimit_date = <om_aend>-begda - 1.
                  infty_operation->read_hrpxxxx(
                  EXPORTING
                    begda = delimit_date
                    endda = delimit_date
                    infty = <om_aend>-infty
                    langu = CONV #( spras )
                    otype = <om_aend>-otype
                    sap_id = <om_aend>-sap_id
                    subty = <om_aend>-subty
                    plvar = CONV #( /sew/cl_int_constants=>plvar )
                    IMPORTING
                      hrp_old = <hrp_last>
                      return = return ).

                  IF <hrp_last> IS INITIAL.
                    delimit_date = <om_aend>-begda.
                    infty_operation->read_hrpxxxx(
                    EXPORTING
                      begda = delimit_date
                      endda = delimit_date
                      infty = <om_aend>-infty
                      langu = CONV #( spras )
                      otype = <om_aend>-otype
                      sap_id = <om_aend>-sap_id
                      subty = <om_aend>-subty
                      plvar = CONV #( /sew/cl_int_constants=>plvar )
                      IMPORTING
                        hrp_old = <hrp_last>
                        return = return ).
                  ENDIF.

                  IF <hrp_last> IS INITIAL.
                    return = VALUE bapiret1( type = /sew/cl_int_constants=>success
                    id = /sew/cl_int_constants=>msg_class_int
                    number = /sew/cl_int_constants=>msg_no-m28
                    message_v1 = <om_aend>-sap_id
                    message_v2 = spras
*                                             message_v3 = <om_aend>-endda
*                                  message_v4 =
                    ).
                  ELSE.
                    "If active relations exist do not delimit the object instead create error message
                    me->check_active_relat(
                    EXPORTING
                      org = <om_aend>-sap_id
                      dats = delimit_date
                      IMPORTING
                        relat_exists = DATA(relat_exists)
                    ).
                    IF relat_exists = abap_true.
                      return = VALUE bapiret1( type = /sew/cl_int_constants=>error
                      id = /sew/cl_int_constants=>msg_class_int
                      number = /sew/cl_int_constants=>msg_no-m30
                      message_v1 = <om_aend>-sap_id
                      message_v2 = delimit_date
*                                             message_v3 = <om_aend>-endda
*                                  message_v4 =
                      ).
                    ELSE.
                      me->delimit_hrp1000(
                      EXPORTING
                        infty = <om_aend>-infty
                        delimit_date = delimit_date
                        new_date = <om_aend>-begda
                        hrp_old = <hrp_old>
                        simu = simu
                        IMPORTING
                          return = return ).
                    ENDIF.
                  ENDIF.
                  "Else Proceed with gerneral IT maintain logic
                ELSE.
                  TRY.
                      infty_operation->maintain_hrpxxxx(
                     EXPORTING
                       begda = <om_aend>-begda
                       endda = <om_aend>-endda
                       hrp_new = <struk>
                       infty = <om_aend>-infty
                       it_change = it_change
                       it_create = it_create
                       langu = CONV #( spras )"langu
                       otype = <om_aend>-otype
                       plvar = CONV #( /sew/cl_int_constants=>plvar )
                       sap_id = <om_aend>-sap_id
                       subty = <om_aend>-subty
                       simu = simu
                       action = <om_aend>-operation
                       IMPORTING
                         return = return ).
                    CATCH cx_hrpa_invalid_parameter INTO exception.
*                          return = /sew/cl_int_utility=>map_sy_msg( msgid = sy-msgid msgty = sy-msgty msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 msgv4 = sy-msgv4 ).
                      return-type = 'E'.
                      return-id = '/SEW/HCM_INTEGRATION'.
                      return-message_v1 = exception->get_text( ).
                      return-number = /sew/cl_int_constants=>msg_no-m35.
                  ENDTRY.
                ENDIF.

                "Success
                IF return IS INITIAL.
                  begda_ext = /sew/cl_int_utility=>get_external_date( date = <om_aend>-begda ).
                  endda_ext = /sew/cl_int_utility=>get_external_date( date = <om_aend>-endda ).
                  return = VALUE bapiret1( type = /sew/cl_int_constants=>success
                                           id = /sew/cl_int_constants=>msg_class_int
                                           number = /sew/cl_int_constants=>msg_no-m9
                                           message_v1 = <om_aend>-infty
                                           message_v2 = begda_ext
                                           message_v3 = endda_ext
                                           message_v4 = <om_aend>-sap_id
                                          ).

                  "Error
                ELSE.
                  "no change
                  IF return-id = '29'.
                    CLEAR return.
                    no_change = abap_true.
                  ENDIF.
                ENDIF.
              ENDIF.

            ENDIF.
            me->status_handler->get_changed_fields( otype = <om_aend>-otype fields = changed_fields infty = <om_aend>-infty it_record = <struk> aend_id = <om_aend>-aend_id
                                simulation = simu_tmp ).
            me->status_handler->add_log_message( aend_id = <om_aend>-aend_id bapiret1 = return hrpad_return = return_tab ).
          ENDIF.
          "check for operation errors
          IF return-type NE /sew/cl_int_constants=>success.

*          error_handler->add_message_pa( EXPORTING sap_id = <om_aend>-sap_id int_run = <om_aend>-int_run return = return begda = <om_aend>-begda endda = <om_aend>-endda ).
*        /sew/cl_utils=>add_message_to_handler( EXPORTING iv_sap_id  = <om_aend>-sap_id
*                                                         is_return = return
*                                               CHANGING  MESSAGE_HANDLER = MESSAGE_HANDLER ).
            CLEAR: key, return.

            "set error status
            <om_aend>-status = /sew/cl_int_constants=>booking_status-error.
            IF <om_aend>-sap_id IS INITIAL.
              MODIFY om_aend_tmp FROM <om_aend> TRANSPORTING status WHERE status NE /sew/cl_int_constants=>booking_status-error.
              APPEND LINES OF om_aend_tmp TO om_aend_error.
              DELETE om_aend_tmp WHERE int_run = <om_aend>-int_run AND cloud_id = <om_aend>-cloud_id AND aend_id = <om_aend>-aend_id.
            ELSE.
              MODIFY om_aend_tmp FROM <om_aend> TRANSPORTING status WHERE status NE /sew/cl_int_constants=>booking_status-error.
              APPEND LINES OF om_aend_tmp TO om_aend_error.
              READ TABLE om_aend_tmp ASSIGNING <om_aend_tmp> WITH KEY int_run = <om_aend>-int_run cloud_id = <om_aend>-cloud_id aend_id = <om_aend>-aend_id.
              IF <om_aend_tmp> IS ASSIGNED.
*                APPEND <om_aend_tmp> TO om_aend_error.
                IF <om_aend_tmp>-sap_id IS INITIAL.
                  APPEND <om_aend_tmp> TO me->status_handler->del_om_aend.
                ENDIF.
                DELETE om_aend_tmp WHERE int_run = <om_aend>-int_run AND cloud_id = <om_aend>-cloud_id AND aend_id = <om_aend>-aend_id.
              ENDIF.
            ENDIF.
            APPEND VALUE #( sign = 'I' option = 'EQ' low = <om_aend>-cloud_id ) TO sap_id_error.


            "in case several om_aend entries belong to same int_run, rollback for all infotype operations of int_run is needed
            IF max GT 1.
              IF <om_aend>-sap_id IS INITIAL.
                count = max.
                ROLLBACK WORK.
                CALL FUNCTION 'RHOM_ALL_BUFFER_INIT'.
              ELSE.
*              count = count + 1.
              ENDIF.
            ENDIF.
            CONTINUE.
          ENDIF.

          "in case several om_aend entries belong to same int_run, commit for all infotype operations of int_run is needed
          IF max EQ count.
            IF no_change NE abap_true.
              IF om_aend_del IS NOT INITIAL.
                APPEND LINES OF om_aend_del TO me->status_handler->del_om_aend.
              ELSE.
                IF <om_aend_tmp> IS ASSIGNED.
                  CLEAR: <om_aend_tmp>.
                ENDIF.
                READ TABLE om_aend_tmp ASSIGNING <om_aend_tmp> WITH KEY int_run = <om_aend>-int_run cloud_id = <om_aend>-cloud_id aend_id = <om_aend>-aend_id.
                IF <om_aend_tmp> IS ASSIGNED.
                  IF <om_aend_tmp>-sap_id IS INITIAL.
                    APPEND <om_aend_tmp> TO me->status_handler->del_om_aend.
                  ENDIF.
                ENDIF.
              ENDIF.
              APPEND <om_aend> TO om_aend_post.
              DELETE om_aend_tmp WHERE int_run = <om_aend>-int_run AND cloud_id = <om_aend>-cloud_id AND aend_id = <om_aend>-aend_id.
            ELSE.
              READ TABLE om_aend_tmp ASSIGNING <om_aend_tmp> WITH KEY int_run = <om_aend>-int_run cloud_id = <om_aend>-cloud_id aend_id = <om_aend>-aend_id.
              IF <om_aend_tmp> IS ASSIGNED.
                IF <om_aend_tmp>-sap_id IS INITIAL.
                  APPEND <om_aend_tmp> TO me->status_handler->del_om_aend.
                ENDIF.
              ENDIF.
              APPEND <om_aend> TO om_aend_nochange.
              DELETE om_aend_tmp WHERE int_run = <om_aend>-int_run AND cloud_id = <om_aend>-cloud_id AND aend_id = <om_aend>-aend_id.
            ENDIF.
            "set success status
*          <om_aend>-status = /sew/cl_int_constants=>booking_status-success.
*
*          MODIFY om_aend_tmp FROM <om_aend> TRANSPORTING status WHERE status NE /sew/cl_int_constants=>booking_status-success.
*
*          APPEND LINES OF om_aend_tmp TO om_aend_post.
            count = max.

            "do commit only in case simulation is inactive
            CHECK simu EQ abap_false.
            CALL FUNCTION 'RHOM_WRITE_BUFFER_TO_DB'.
*      COMMIT WORK AND WAIT.
*          CALL FUNCTION 'RH_UPDATE_DATABASE'
*            EXPORTING
*              vtask     = 'D'
*            EXCEPTIONS
*              corr_exit = 1
*              OTHERS    = 2.
*
*          IF sy-subrc <> 0.
*            RETURN.
*          ENDIF.
            COMMIT WORK.
*            CALL FUNCTION 'RHOM_ALL_BUFFER_INIT'.
          ELSE.
            IF no_change NE abap_true.
              IF om_aend_del IS NOT INITIAL.
                APPEND LINES OF om_aend_del TO me->status_handler->del_om_aend.
              ELSE.
                IF <om_aend_tmp> IS ASSIGNED.
                  CLEAR: <om_aend_tmp>.
                ENDIF.
                READ TABLE om_aend_tmp ASSIGNING <om_aend_tmp> WITH KEY int_run = <om_aend>-int_run cloud_id = <om_aend>-cloud_id aend_id = <om_aend>-aend_id.
                IF <om_aend_tmp> IS ASSIGNED.
                  IF <om_aend_tmp>-sap_id IS INITIAL.
                    APPEND <om_aend_tmp> TO me->status_handler->del_om_aend.
                  ENDIF.
                ENDIF.
              ENDIF.
              APPEND <om_aend> TO om_aend_post.
              DELETE om_aend_tmp WHERE int_run = <om_aend>-int_run AND cloud_id = <om_aend>-cloud_id AND aend_id = <om_aend>-aend_id.
            ELSE.
              READ TABLE om_aend_tmp ASSIGNING <om_aend_tmp> WITH KEY int_run = <om_aend>-int_run cloud_id = <om_aend>-cloud_id aend_id = <om_aend>-aend_id.
              IF <om_aend_tmp> IS ASSIGNED.
                IF <om_aend_tmp>-sap_id IS INITIAL.
                  APPEND <om_aend_tmp> TO me->status_handler->del_om_aend.
                ENDIF.
              ENDIF.
              APPEND <om_aend> TO om_aend_nochange.
              DELETE om_aend_tmp WHERE int_run = <om_aend>-int_run AND cloud_id = <om_aend>-cloud_id AND aend_id = <om_aend>-aend_id.
            ENDIF.
            "set success status
*          <om_aend>-status = /sew/cl_int_constants=>booking_status-success.
*          MODIFY om_aend_tmp FROM <om_aend> TRANSPORTING status WHERE status NE /sew/cl_int_constants=>booking_status-success.

*          APPEND LINES OF om_aend_tmp TO om_aend_post.
*          count = count + 1.
          ENDIF.
        ENDIF.
      ELSE.
        count = count + 1.
        MODIFY om_aend_tmp FROM <om_aend> TRANSPORTING status WHERE status NE /sew/cl_int_constants=>booking_status-error.
        APPEND LINES OF om_aend_tmp TO om_aend_error.
        DELETE om_aend_tmp WHERE int_run = <om_aend>-int_run AND cloud_id = <om_aend>-cloud_id AND aend_id = <om_aend>-aend_id.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = <om_aend>-cloud_id ) TO sap_id_error.
        return = VALUE bapiret1( type = /sew/cl_int_constants=>error
                         id = /sew/cl_int_constants=>msg_class_int
                         number = /sew/cl_int_constants=>msg_no-m34
                         message_v1 = <om_aend>-cloud_id
*                         message_v2 = begda_ext
*                         message_v3 = endda_ext
*                         message_v4 = <om_aend>-sap_id
                        ).
        me->status_handler->add_log_message( aend_id = <om_aend>-aend_id bapiret1 = return hrpad_return = return_tab ).
      ENDIF.
      CLEAR: changed_fields, return.
    ENDLOOP.

    "dequeue last sap_id
    IF sap_id IS NOT INITIAL.
*      CALL FUNCTION 'RH_PM_DEQUEUE'
*        EXPORTING
*          act_plvar = '01'
*          act_otype = <om_aend>-otype
*          act_objid = sap_id.
**        IMPORTING
**          return    = return.

*      CALL FUNCTION 'RHOM_DEQUEUE'
*        EXPORTING
*          plvar                    = '01'
*          otype                    = <om_aend>-otype
*          objid                    = <om_aend>-sap_id
*        EXCEPTIONS
*          dequeue_failed           = 1
*          object_changed_in_buffer = 2
*          OTHERS                   = 3.
*
*      "check for enqueue sap_id
*      IF sy-msgty EQ 'E'.
*        return = /sew/cl_int_utility=>map_sy_msg( msgid = sy-msgid msgty = sy-msgty msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 msgv4 = sy-msgv4 ).
*        /sew/cl_int_utility=>get_message( EXPORTING msgid = return-id msgno = return-number
*                                          IMPORTING message = return-message ).
**        error_handler->add_message_pa( EXPORTING sap_id = sap_id int_run = <om_aend>-int_run return = return begda = <om_aend>-begda endda = <om_aend>-endda ).
**        /sew/cl_utils=>add_message_to_handler( EXPORTING iv_sap_id  = sap_id
**                                                         is_return = return
**                                               CHANGING  message_handler = message_handler ).
**        CLEAR: sap_id, return.
*      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD prepare_protocol.

    "Check message handler
    IF message_handler IS BOUND.

      "prepare layout options
      DATA(layout) = VALUE slis_layout_alv( zebra = 'X'
                                               colwidth_optimize = 'X' ).

      DATA: root_node TYPE hrpad_pal_node_key VALUE 'ROOT'.

      "Define structure of successful and failed IT_AEND entries ALV table
      message_handler->if_hrpay00_pal_services~create_fcat( EXPORTING i_structure_name = '/SEW/INT_OM_AEND'
                                                            IMPORTING et_fcat          = DATA(fcat_om_aend) ).

      "add post table
*      IF om_aend_post IS NOT INITIAL.
*        message_handler->if_hrpay00_pal_services~add_table(
*          EXPORTING
*            i_parent_node_key = root_node
*            it_fcat           = fcat_om_aend
*            it_append_table   = om_aend_post
*            is_layout         = layout
*            i_node_txt        = COND string( WHEN simu EQ abap_true
*                                             THEN TEXT-005
*                                             ELSE TEXT-001 ) ).
*      ENDIF.
*
*      "add error table
*      IF om_aend_error IS NOT INITIAL.
*        message_handler->if_hrpay00_pal_services~add_table(
*          EXPORTING
*            i_parent_node_key = root_node
*            it_fcat           = fcat_om_aend
*            it_append_table   = om_aend_error
*            is_layout         = layout
*            i_node_txt        = COND string( WHEN simu EQ abap_true
*                                             THEN TEXT-006
*                                             ELSE TEXT-002 ) ).
*      ENDIF.

      IF status_handler->new_om_aend IS NOT INITIAL.
        message_handler->if_hrpay00_pal_services~add_table(
          EXPORTING
            i_parent_node_key = root_node
            it_fcat           = fcat_om_aend
            it_append_table   = status_handler->new_om_aend
            is_layout         = layout
            i_node_txt        = COND string( WHEN simu EQ abap_true
                                             THEN TEXT-009
                                             ELSE TEXT-010 ) ).
      ENDIF.

      "add skipped pernr's
      IF skipped_objects IS NOT INITIAL.
        message_handler->if_hrpay00_pal_services~add_table(
        EXPORTING
          i_parent_node_key = root_node
          it_append_table   = skipped_objects
          is_layout         = layout
          i_node_txt        = COND string( WHEN simu EQ abap_true
                                             THEN TEXT-008
                                             ELSE TEXT-004 ) ).
      ENDIF.

      "add message table
      message_handler->if_hrpay00_pal_services~add_messages(
        EXPORTING
          i_parent_node_key = root_node
      ).

      "show pals
      CALL METHOD message_handler->display_pal.

    ENDIF.

  ENDMETHOD.


  METHOD read_hrpxxxx.
    DATA: lr_structdescr TYPE REF TO cl_abap_structdescr,
          lr_tabledescr  TYPE REF TO cl_abap_tabledescr,
          lr_table       TYPE REF TO data.
    FIELD-SYMBOLS:  <hrp_old_tab> TYPE STANDARD TABLE.

    lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV #( 'P' && infty ) ).
    lr_tabledescr ?= cl_abap_tabledescr=>create( p_line_type = lr_structdescr ).
    CREATE DATA lr_table TYPE HANDLE lr_tabledescr.
    ASSIGN lr_table->* TO <hrp_old_tab>.

    CALL FUNCTION 'RH_READ_INFTY'
      EXPORTING
*       AUTHORITY            = 'DISP'
        with_stru_auth       = ' '
        plvar                = plvar
        otype                = otype
        objid                = sap_id
        infty                = infty
*       ISTAT                = ' '
*       EXTEND               = 'X'
        subty                = subty
        begda                = begda
        endda                = endda
*       CONDITION            = '00000'
*       INFTB                = '1'
*       SORT                 = 'X'
*       VIA_T777D            = ' '
      TABLES
        innnn                = <hrp_old_tab>
*       OBJECTS              =
      EXCEPTIONS
        all_infty_with_subty = 1
        nothing_found        = 2
        no_objects           = 3
        wrong_condition      = 4
        wrong_parameters     = 5
        OTHERS               = 6.

    IF sy-subrc <> 0.
* Implement suitable error handling here
*      return = /sew/cl_int_utility=>map_sy_msg( msgid = sy-msgid msgty = sy-msgty msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 msgv4 = sy-msgv4 ).

    ENDIF.
*    ELSE.
    IF <hrp_old_tab> IS NOT INITIAL.
      READ TABLE <hrp_old_tab> ASSIGNING FIELD-SYMBOL(<hrp_old>) INDEX 1.
      "If IT is HRP1000 we need to get language specific entry
      IF infty = /sew/cl_int_constants=>it1000.
        IF <hrp_old> IS ASSIGNED.
          UNASSIGN <hrp_old>.
        ENDIF.
*        READ TABLE <hrp_old_tab> ASSIGNING <hrp_old> WITH KEY langu = langu.
        LOOP AT <hrp_old_tab> ASSIGNING <hrp_old>.
          ASSIGN COMPONENT 'LANGU' OF STRUCTURE <hrp_old> TO FIELD-SYMBOL(<langu>).
          IF <langu> = langu.
            hrp_old = <hrp_old>.
            EXIT.
          ENDIF.
        ENDLOOP.
      ELSE.
        IF <hrp_old> IS ASSIGNED.
          hrp_old = <hrp_old>.
        ENDIF.
      ENDIF.
    ENDIF.

*    ENDIF.
  ENDMETHOD.


  METHOD start_post.

    "call post
*    DATA(om_aend_obj) = NEW /sew/cl_int_om_aend( ).
    me->post_om_aend(
      EXPORTING
        simu            = simu
      IMPORTING
        om_aend_post    = om_aend_post
        om_aend_error   = om_aend_error
        om_aend_nochange = om_aend_nochange
      CHANGING
        om_aend         = me->status_handler->om_aend
        message_handler = message_handler ).

    "store success status /SEW/INT_IT_AEND
*    DATA(is_ok) = om_aend_obj->save_entries( om_aend = om_aend_post ).
*
*    "store error status /SEW/INT_IT_AEND
*    IF is_ok EQ abap_true.
*      is_ok = om_aend_obj->save_entries( om_aend = om_aend_error ).
*    ENDIF.

    "puffer status update
    me->update_status( ).
    "persist status update
    DATA(is_ok) = me->status_handler->persist_data( simu = simu source = /sew/cl_int_constants=>poster ).

    "Check simulation status and update status of ZTPT_REQ_HEADER
    IF simu  EQ abap_true OR
       is_ok EQ abap_false.
      ROLLBACK WORK.
    ENDIF.

  ENDMETHOD.


  METHOD update_status.
    "Update Success
    LOOP AT me->om_aend_post ASSIGNING FIELD-SYMBOL(<om_aend_post>).
      me->status_handler->set_status( aend_id = <om_aend_post>-aend_id status = /sew/cl_int_constants=>booking_status-success ).
    ENDLOOP.
    "Update Error
    LOOP AT me->om_aend_error ASSIGNING FIELD-SYMBOL(<om_aend_error>).
      me->status_handler->set_status( aend_id = <om_aend_error>-aend_id status = /sew/cl_int_constants=>booking_status-error ).
    ENDLOOP.
    "Update No Change
    LOOP AT me->om_aend_nochange ASSIGNING FIELD-SYMBOL(<om_aend_nochange>).
      me->status_handler->set_status( aend_id = <om_aend_nochange>-aend_id status = /sew/cl_int_constants=>booking_status-nochange ).
    ENDLOOP.
    "Set simulated status if simulation run
    IF simu = abap_true.
      me->status_handler->simu_om_aend = me->status_handler->simu_om_aend.
      LOOP AT me->status_handler->simu_om_aend ASSIGNING FIELD-SYMBOL(<simu_om_aend>).
        <simu_om_aend>-status = /sew/cl_int_constants=>booking_status-simulated.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
