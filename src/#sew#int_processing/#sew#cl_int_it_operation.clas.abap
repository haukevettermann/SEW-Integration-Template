class /SEW/CL_INT_IT_OPERATION definition
  public
  final
  create public .

public section.

  data PERNR type PERNR .
  data MOLGA type MOLGA .
  data INT_RUN type GUID_32 .
  data OBJID type HROBJID .
  data OTYPE type OTYPE .

  methods UPDATE_PAXXXX_DCIF
    importing
      !DATA type ANY
      !PERNR type P_PERNR optional
      !BEGDA type SY-DATUM optional
      !ENDDA type SY-DATUM optional
      !INFTY type INFTY
    exporting
      !IS_OK type BOOLE_D
      !MESSAGES type HRPAD_MESSAGE_TAB .
  methods CREATE_ACTION_DCIF
    importing
      !DATA type ANY
      !PERNR type P_PERNR optional
      !BEGDA type SY-DATUM optional
      !ENDDA type SY-DATUM optional
      !INFTY type INFTY
    exporting
      !IS_OK type BOOLE_D
      !MESSAGES type HRPAD_MESSAGE_TAB .
  methods DELIMIT_HRPXXXX
    importing
      !RECORD type ANY
      !INFTY type INFTY
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !DELIMIT_DATE type DATS
      !SIMU type BOOLE_D optional
    exporting
      !RETURN type BAPIRET1 .
  methods CREATE_PAXXXX_DCIF
    importing
      !IV_DATA type ANY
      !IV_ENTITY type CHAR50 optional .
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
  methods DELETE_PAXXXX_DCIF
    importing
      !IV_PERNR type P_PERNR optional
      !IV_BEGDA type SY-DATUM optional
      !IV_ENDDA type SY-DATUM optional
      !IV_ENTITY type CHAR50 optional
      !IV_DATA type ANY optional
    exporting
      !EV_MESSAGE type BAPI_MSG
      !ER_MESSAGE_LIST type ref to CL_HRPA_MESSAGE_LIST .
  methods MAINTAIN_RELATION
    importing
      !ACTION type OKCODE
      !BEGDA type DATS
      !ENDDA type DATS
      !PARENT type OBJEC
      !CHILD type OBJEC
      !RSIGN type RSIGN
      !RELAT type RELAT
      !SIMU type BOOLE_D
      !SPRAS type SPRAS
    exporting
      !RETURN type BAPIRET1 .
  methods READ_PAXXXX_DCIF
    importing
      !IV_INFTY type INFTY
      !IV_SUBTY type SUBTY optional
      !IV_BEGDA type SY-DATUM optional
      !IV_ENDDA type SY-DATUM optional
      !IV_PERNR type P_PERNR optional
      !EV_MESSAGE type BAPI_MSG
      !ER_MESSAGE_LIST type ref to CL_HRPA_MESSAGE_LIST
      !IV_DATA type ANY optional
      !IV_ENTITY type CHAR50 optional
    exporting
      !RETURN_TAB type HRPAD_RETURN_TAB
      !RECORD_TAB type STANDARD TABLE .
  methods MAINTAIN_TEVEN_HC
    importing
      !PERNR type PERNR_D
      !SIMU type BOOLE_D
      !BEGDA type BEGDA
      !ENDDA type ENDDA
    returning
      value(RETURN) type HRPAD_RETURN .
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
      !RSIGN type RSIGN optional
      !RELAT type RELAT optional
    exporting
      !RETURN type BAPIRET1
      !HRP_OLD type ANY .
  methods CREATE_DUMMY_POS
    importing
      !BEGDA type DATS
      !ENDDA type DATS
      !ORGEH type HROBJID
      !LANGU type LAISO
      !SIMU type BOOLE_D
    returning
      value(ID_POS) type HROBJID .
  methods CONSTRUCTOR
    importing
      !PERNR type PERNR_D optional
      !MOLGA type MOLGA optional
      !INT_RUN type GUID_32 optional
      !OBJID type HROBJID optional
      !OTYPE type OTYPE optional .
  methods READ_TEVEN
    importing
      !BEGDA type DATS
      !ENDDA type DATS
      !PERNR type PERNR_D
      !SIMU type BOOLE_D
    exporting
      !RETURN_TAB type HRPAD_RETURN_TAB
      !RECORD_MORE_TAB type PTM_MT_MORE
      !RECORD_TAB type PTM_MT .
  methods READ_PAXXXX
    importing
      !INFTY type INFTY
      !SUBTY type SUBTY optional
      !BEGDA type DATS
      !ENDDA type DATS
      !PERNR type PERNR_D
      !SIMU type BOOLE_D
    exporting
      !RETURN_TAB type HRPAD_RETURN_TAB
      !RECORD_TAB type STANDARD TABLE .
  methods PERFORM_REVERSE_TERMINATION
    importing
      !RECORD type ANY
      !ACTION type ACTIO
      !INFTY type INFTY
      !SUBTY type SUBTY optional
      !BEGDA type DATS
      !ENDDA type DATS
      !PERNR type PERNR_D
      !SIMU type BOOLE_D
    exporting
      !RETURN_TAB type HRPAD_RETURN_TAB
      !RECORD_TAB type STANDARD TABLE
      !IS_REV_TERM type BOOLE_D .
  methods PERFORM_HIRE
    importing
      !IT_AEND type /SEW/INT_IT_AEND
      !IT_AEND_TAB type /SEW/TT_IT_AEND
      !IT_AEND_TAB_REL type /SEW/TT_IT_AEND optional
      !SIMU type BOOLE_D
    exporting
      !IS_OK type BOOLE_D
      !RETURN_TAB type HRPAD_RETURN_TAB
      !PERNR type PERNR_D .
  methods PERFORM_CANCEL_HIRE
    importing
      !IT_AEND type /SEW/INT_IT_AEND
      !SIMU type BOOLE_D
      !RECORD type ANY
    exporting
      !IS_OK type BOOLE_D
      !RETURN_TAB type HRPAD_RETURN_TAB .
  methods MODIFY_PAXXXX
    importing
      !RECORD type ANY
      !INFTY type INFTY
      !SUBTY type SUBTY
      !BEGDA type DATS
      !ENDDA type DATS
      !PERNR type PERNR_D
      !DELIMIT_DATE type DATS optional
      !SIMU type BOOLE_D
    exporting
      !RETURN type BAPIRET1
      !RETURN_TAB type HRPAD_RETURN_TAB .
  methods MAINTAIN_PAXXXX
    importing
      !RECORD type ANY
      !INFTY type INFTY
      !SUBTY type SUBTY
      !BEGDA type DATS
      !ENDDA type DATS
      !PERNR type PERNR_D
      !OPERATION type ACTIO
      !NUMBER type SEQNR optional
      !SIMU type BOOLE_D
    exporting
      !RETURN type BAPIRET1
      !RETURN_TAB type HRPAD_RETURN_TAB .
  methods MAINTAIN_PA0001
    importing
      value(P0001) type P0001
      !INFTY type INFTY
      !SUBTY type SUBTY
      !BEGDA type DATS
      !ENDDA type DATS
      !PERNR type PERNR_D
      !NUMBER type SEQNR optional
      !SIMU type BOOLE_D
    exporting
      !RETURN type BAPIRET1
      !RETURN_TAB type HRPAD_RETURN_TAB .
  methods HANDLE_REVERSE_TERMINATION
    importing
      !INFTY type INFTY
      !SUBTY type SUBTY optional
      !BEGDA type DATS
      !ENDDA type DATS
      !PERNR type PERNR_D
      !SIMU type BOOLE_D
    exporting
      !RETURN_TAB type HRPAD_RETURN_TAB
      !RECORD_TAB type STANDARD TABLE .
  methods HANDLE_HIREDATA_CHANGE
    importing
      !INFTY type INFTY
      !SUBTY type SUBTY optional
      !BEGDA type DATS
      !ENDDA type DATS
      !PERNR type PERNR_D
      !SIMU type BOOLE_D
    exporting
      !RETURN_TAB type HRPAD_RETURN_TAB
      !RECORD_TAB type STANDARD TABLE .
  methods DELIMIT_PAXXXX
    importing
      !RECORD type ANY
      !INFTY type INFTY
      !SUBTY type SUBTY
      !BEGDA type DATS
      !ENDDA type DATS
      !PERNR type PERNR_D
      !DELIMIT_DATE type DATS
      !SIMU type BOOLE_D
    exporting
      !RETURN type BAPIRET1
      !RETURN_TAB type HRPAD_RETURN_TAB .
  methods DELETE_PAXXXX
    importing
      !RECORD type ANY
      !INFTY type INFTY
      !SUBTY type SUBTY
      !BEGDA type DATS
      !ENDDA type DATS
      !PERNR type PERNR_D
      !DELIMIT_DATE type DATS
      !SIMU type BOOLE_D
    exporting
      !RETURN type BAPIRET1
      !RETURN_TAB type HRPAD_RETURN_TAB .
  methods CHECK_REVERSE_TERMINATION
    importing
      !BEGDA type DATS
      !ENDDA type DATS
      !PERNR type PERNR_D
      !SIMU type BOOLE_D
    exporting
      !RETURN_TAB type HRPAD_RETURN_TAB
      !RECORD_TAB type STANDARD TABLE
      !REVERSE_TERMINATION type BOOLE_D .
protected section.

  data G_STARTDATE_0002 type BEGDA .
private section.
ENDCLASS.



CLASS /SEW/CL_INT_IT_OPERATION IMPLEMENTATION.


  METHOD check_reverse_termination.

    DATA: ret      LIKE LINE OF return_tab,
          return   TYPE bapiret1,
          tab_0000 TYPE STANDARD TABLE OF p0000,
          tab_0001 TYPE STANDARD TABLE OF p0001.
*          action         TYPE actio.

    "Only perform reverse termination block if current record being processed is IT0000 and action not equal termination
*    IF infty = /sew/cl_int_constants=>it0000 AND action NE /sew/cl_int_constants=>termination.
    "Read IT0000 with time interval of new action
    me->read_paxxxx( EXPORTING begda = endda
                               endda = endda
                               infty = CONV #( /sew/cl_int_constants=>it0000 )
                               pernr = pernr
                               simu  = simu
                     IMPORTING return_tab = return_tab
                               record_tab = tab_0000 ).
    READ TABLE tab_0000 ASSIGNING FIELD-SYMBOL(<record_0000>) INDEX 1.
    IF <record_0000> IS ASSIGNED AND <record_0000> IS NOT INITIAL.
      "If current action is termination, reverse termination needs to take place else exit reverse termination block
      IF <record_0000>-massn = /sew/cl_int_constants=>termination.
        reverse_termination = abap_true.
      ENDIF.
    ENDIF.
*    ENDIF.
  ENDMETHOD.


  METHOD CONSTRUCTOR.
    me->pernr = pernr.
    me->molga = molga.
    me->int_run = int_run.
    me->objid = objid.
    me->otype = otype.
  ENDMETHOD.


  METHOD create_action_dcif.

    DATA: it                     TYPE REF TO data,
          lv_pernr               TYPE p_pernr,
          lv_begda               TYPE begda,
          lv_endda               TYPE endda,
          lt_prelp               TYPE hrpad_prelp_tab,
          lr_infty_reader        TYPE REF TO if_hrpa_read_infotype,
          lr_message_list        TYPE REF TO cl_hrpa_message_list,
          lr_masterdata_bl       TYPE REF TO if_hrpa_masterdata_bl,
          container              TYPE REF TO if_hrpa_infty_container,
          old_container          TYPE REF TO cl_hrpa_infotype_container,
          new_container          TYPE REF TO if_hrpa_infty_container,
          new_infotype_container TYPE REF TO cl_hrpa_infotype_container,
          infotype_ref           TYPE REF TO data,
          lv_is_ok               TYPE boole_d,
          lv_dummy               TYPE string,
          ls_msg                 TYPE symsg,
          lv_massg               TYPE massg,
          lv_massn               TYPE massn,
          ls_message             TYPE hrpad_message,
          entity_ref             TYPE REF TO data,
          data_ref               TYPE REF TO data,
          pnnnn_ref              TYPE REF TO data,
          container_tab          TYPE hrpad_infty_container_tab,
          container_if           TYPE hrpad_infty_container_ref,
          t777d                  TYPE t777d,
          lv_has_error           TYPE boole_d,
          lv_count               TYPE i,
          lv_offset              TYPE i,
          lt_messages            TYPE hrpad_message_tab,
          lv_cut_date_temp       TYPE begda,
          lr_message             TYPE REF TO cl_hrpa_message_list,
          ls_update_mode         TYPE hrpad_update_mode.


    FIELD-SYMBOLS: <pshdr>    TYPE pshdr,
                   <pnnnn>    TYPE any,
                   <ptnnnn>   TYPE ANY TABLE,
                   <pskey>    TYPE pskey,
                   <persk>    TYPE persk,
                   <pxxxx>    TYPE any,
                   <struc>    TYPE any,
                   <fs_pnnnn> TYPE ANY TABLE,
                   <fs_endda> TYPE p0001-endda,
                   <fs_begda> TYPE p0001-begda.


    CONCATENATE 'P' infty INTO DATA(it_name).
    CREATE DATA it TYPE (it_name).
    ASSIGN it->* TO <struc>.
    ASSIGN data TO <struc>.
*    ASSIGN COMPONENT 'PERNR' OF STRUCTURE <struc> TO <pernr>.

    ASSIGN COMPONENT 'PSKEY' OF STRUCTURE <struc> TO <pskey>.

*    TRY.
    CREATE OBJECT lr_message.

    CALL METHOD cl_hrpa_masterdata_factory=>get_business_logic
      IMPORTING
        business_logic = lr_masterdata_bl.

    TRY.
        CALL METHOD cl_hrpa_read_infotype=>get_instance
          IMPORTING
            infotype_reader = lr_infty_reader.
      CATCH cx_hrpa_violated_assertion.
*
    ENDTRY.

    CHECK lr_masterdata_bl IS BOUND.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = begda
        days      = '1'
        months    = '0'
        signum    = '-'
        years     = '0'
      IMPORTING
        calc_date = lv_cut_date_temp.

*   Read old (existing) entry
    CALL METHOD lr_masterdata_bl->read
      EXPORTING
        tclas           = 'A'
        infty           = infty
        pernr           = pernr
        begda           = begda
        endda           = endda
        no_auth_check   = space
        message_handler = lr_message
      IMPORTING
        container_tab   = container_tab
        is_ok           = lv_is_ok.

    IF lv_is_ok EQ abap_true.
      DESCRIBE TABLE container_tab LINES lv_count.

      IF lv_count GT 0.
        READ TABLE container_tab INTO container INDEX lv_count.
      ENDIF.
    ENDIF.

    IF lv_is_ok = abap_false.
      CALL METHOD lr_message->get_abend_list
        IMPORTING
          messages = lt_messages.

      messages = CORRESPONDING #( BASE ( messages ) lt_messages ).

      IF lt_messages IS INITIAL.
        CALL METHOD lr_message->get_error_list
          IMPORTING
            messages = lt_messages.

        messages = CORRESPONDING #( BASE ( messages ) lt_messages ).

      ENDIF.
      RETURN.
    ENDIF.

*   Old entry
    old_container ?= container.

    TRY.
        CALL METHOD old_container->if_hrpa_infty_container_data~primary_record_ref
          IMPORTING
            pnnnn_ref = infotype_ref.
      CATCH cx_hrpa_violated_assertion.
    ENDTRY.

*   Transfer old infotype key to write Measure
    ASSIGN infotype_ref->* TO <pxxxx>.
    IF sy-subrc IS NOT INITIAL.
      is_ok = abap_false.
    ENDIF.
*      IF <pxxxx> IS ASSIGNED.
*        ASSIGN COMPONENT 'PSKEY' OF STRUCTURE <pxxxx> TO <pskey>.
*        IF sy-subrc IS NOT INITIAL.
*          is_ok = abap_false.
*        ENDIF.
*        MOVE-CORRESPONDING <pxxxx> TO <pskey>.
*      ENDIF.

*      MOVE <pxxxx> TO ls_pa0000.

*      me->read_future_infotype( EXPORTING iv_pernr = iv_pernr iv_infty = '0000' iv_begda = iv_begda IMPORTING is_infty_future = ls_0000_fut it_infty_future = lt_0000_fut ).


*   New enddate is highdate
*      IF iv_endda IS INITIAL.
*        IF ls_0000_fut-begda IS NOT INITIAL AND ls_0000_fut-begda NE ls_pa0000-begda.
*          ls_pa0000-endda = ls_0000_fut-begda - 1.
*        ELSE.
*          ls_pa0000-endda = '99991231'.
*        ENDIF.
*      ELSE.
*        IF ls_0000_fut-begda IS NOT INITIAL AND ls_0000_fut-begda NE ls_pa0000-begda.
*          ls_pa0000-endda = ls_0000_fut-begda - 1.
*        ELSE.
*          ls_pa0000-endda = iv_endda.
*        ENDIF.
*      ENDIF.


*      IF <pskey> IS ASSIGNED.
*        MOVE-CORRESPONDING ls_pa0000 TO <pskey>.
*      ENDIF.

    READ TABLE container_tab INTO container_if  INDEX 1.
    new_infotype_container ?= container_if.
    new_infotype_container ?= new_infotype_container->modify_key( <pskey> ).
    new_infotype_container ?= new_infotype_container->modify_primary_record( <struc> ).
    new_container ?= new_infotype_container.

    ASSIGN COMPONENT 'MASSN' OF STRUCTURE <struc> TO FIELD-SYMBOL(<massn>).
    ASSIGN COMPONENT 'MASSG' OF STRUCTURE <struc> TO FIELD-SYMBOL(<massg>).

    ls_update_mode-no_retroactivity = abap_true.
    CALL METHOD lr_masterdata_bl->insert
      EXPORTING
        massn           = <massn>
        massg           = <massg>
        message_handler = lr_message
        no_auth_check   = space
        update_mode     = ls_update_mode
      IMPORTING
        is_ok           = lv_is_ok
      CHANGING
        container       = new_container.

    CALL METHOD lr_message->get_message_list
      IMPORTING
        messages = lt_messages.

    messages = CORRESPONDING #( BASE ( messages ) lt_messages ).

    IF lv_is_ok = abap_false.

      RETURN.

    ELSE.

    ENDIF.
  ENDMETHOD.


  METHOD create_dummy_pos.
    DATA: pos_idx   TYPE TABLE OF hrobject,
          pos_struc TYPE p1000,
          pos       TYPE TABLE OF p1000,
          child     TYPE objec,
          parent    TYPE objec,
          return    TYPE bapiret1.
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
              spras = CONV #( langu )
              simu = simu
               IMPORTING
                 return = return
            ).
            IF return-type NE /sew/cl_int_constants=>error AND simu NE abap_true.
              "Success create relation
              CALL FUNCTION 'RHOM_WRITE_BUFFER_TO_DB'.
              COMMIT WORK AND WAIT.
*              SUBMIT rhinte20 AND RETURN
*                WITH pchplvar = /sew/cl_int_constants=>plvar
*                WITH pchotype = /sew/cl_int_constants=>position
*                WITH pchobjid-low = id_pos
*                WITH selec EQ abap_true.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      "Error because Orgunit is empty and no dummy position can be created
    ELSE.
    ENDIF.
  ENDMETHOD.


  METHOD CREATE_PAXXXX_DCIF.
*********************************************
* CREATE INFOTYPE (DCIF)
*********************************************


    DATA: lv_pernr               TYPE p_pernr,
          lv_begda               TYPE begda,
          lv_endda               TYPE endda,
          lc_hcm_globalhr_person TYPE REF TO /sew/cl_hcm_globalhr_person,
          lt_prelp               TYPE hrpad_prelp_tab,
          lr_infty_reader        TYPE REF TO if_hrpa_read_infotype,
          lr_message_list        TYPE REF TO cl_hrpa_message_list,
          lr_masterdata_bl       TYPE REF TO if_hrpa_masterdata_bl,
          container              TYPE REF TO if_hrpa_infty_container,
          old_container          TYPE REF TO cl_hrpa_infotype_container,
          new_container          TYPE REF TO if_hrpa_infty_container,
          new_infotype_container TYPE REF TO cl_hrpa_infotype_container,
          infotype_ref           TYPE REF TO data,
          lv_is_ok               TYPE boole_d,
          lv_dummy               TYPE string,
          ls_msg                 TYPE symsg,
          entity_ref             TYPE REF TO data,
          data_ref               TYPE REF TO data,
          pnnnn_ref              TYPE REF TO data,
          container_tab          TYPE hrpad_infty_container_tab,
          container_if           TYPE hrpad_infty_container_ref,
          t777d                  TYPE t777d,
          lv_has_error           TYPE boole_d,
          lv_count               TYPE i,
          lv_offset              TYPE i,
          lv_infotype            TYPE infty,
          lv_entity              TYPE char50,
          lt_messages            TYPE hrpad_message_tab.

    FIELD-SYMBOLS:  <pshdr>    TYPE pshdr,
                    <pnnnn>    TYPE any,
                    <ptnnnn>   TYPE ANY TABLE,
                    <pskey>    TYPE pskey,
                    <pxxxx>    TYPE any,
                    <entity>   TYPE any,
                    <data>     TYPE any,
                    <struc>    TYPE any,
                    <fs_pnnnn> TYPE any,
                    <fs_pernr> TYPE p_pernr,
                    <fs_infty> TYPE prelp-infty,
                    <fs_subty> TYPE p0001-subty,
                    <fs_objps> TYPE p0001-objps,
                    <fs_sprps> TYPE p0001-sprps,
                    <fs_seqnr> TYPE p0001-seqnr,
                    <fs_begda> TYPE p0001-begda,
                    <fs_endda> TYPE p0001-endda.

    TRY.
        CREATE OBJECT lr_message_list.

        CALL METHOD cl_hrpa_masterdata_factory=>get_business_logic
          IMPORTING
            business_logic = lr_masterdata_bl.

        TRY.
            CALL METHOD cl_hrpa_read_infotype=>get_instance
              IMPORTING
                infotype_reader = lr_infty_reader.
            .
          CATCH cx_hrpa_violated_assertion .
        ENDTRY.

        CREATE DATA entity_ref TYPE (iv_entity).

        ASSIGN entity_ref->* TO <entity>.

        CLEAR lv_entity.
        MOVE iv_entity TO lv_entity.

        lv_offset = strlen( lv_entity ).
        lv_offset = lv_offset - 4.

        lv_entity = lv_entity+lv_offset.

        MOVE lv_entity TO lv_infotype.
        t777d = cl_hr_t777d=>read( infty = lv_infotype ).
        CREATE DATA infotype_ref TYPE (t777d-ppnnn).
        ASSIGN infotype_ref->* TO <pxxxx>.

        ASSIGN infotype_ref->* TO <pnnnn> CASTING LIKE <pxxxx>.
        <pnnnn> = <pxxxx>.
        ASSIGN <pnnnn> TO <pshdr> CASTING.
        <pshdr>-infty = lv_infotype.

        ASSIGN infotype_ref->* TO <struc> CASTING LIKE <pxxxx>.
        ASSIGN COMPONENT 'PSKEY' OF STRUCTURE <struc> TO <pskey>.
        ASSIGN COMPONENT 'BEGDA' OF STRUCTURE <struc> TO <fs_begda>.
        ASSIGN COMPONENT 'ENDDA' OF STRUCTURE <struc> TO <fs_endda>.

        IF <fs_endda> >= sy-datum.
          <pskey>-begda = <fs_endda>.
          <pskey>-endda = '99991231'.
        ELSE.
          <pskey>-begda = <fs_begda>.
          <pskey>-endda = <fs_endda>.
        ENDIF.

        CALL METHOD lr_masterdata_bl->get_infty_container
          EXPORTING
            tclas           = 'A'
            pskey           = <pskey>
            no_auth_check   = abap_true
            message_handler = lr_message_list
          IMPORTING
            container       = container_if.

        new_infotype_container ?= container_if.
        new_infotype_container ?= new_infotype_container->modify_key( <pskey> ).

        new_infotype_container ?= new_infotype_container->modify_primary_record( <struc> ).
        new_container ?= new_infotype_container.

        CALL METHOD lr_masterdata_bl->insert
          EXPORTING
*           massn           = '01'
*           massg           = '01'
            no_auth_check   = space
            message_handler = lr_message_list
          IMPORTING
            is_ok           = lv_is_ok
          CHANGING
            container       = new_container.
        old_container ?= new_container.

        IF lr_message_list->has_error( ) = abap_true OR
           lr_message_list->has_abend( ) = abap_true.
          lv_has_error = abap_true.
        ELSE.
          lv_has_error = abap_false.
        ENDIF.

        IF lv_has_error = abap_true.
          CALL METHOD lr_message_list->get_abend_list
            IMPORTING
              messages = lt_messages.

          IF lt_messages[] IS INITIAL.
            CALL METHOD lr_message_list->get_error_list
              IMPORTING
                messages = lt_messages.
          ENDIF.

          CALL METHOD lr_masterdata_bl->if_hrpa_buffer_control~initialize.
        ELSE.
          CALL METHOD lr_masterdata_bl->flush
            EXPORTING
              no_commit = ' '.
        ENDIF.

      CATCH /iwbep/cx_mgw_busi_exception.
      CATCH /iwbep/cx_mgw_tech_exception.
    ENDTRY.

  ENDMETHOD.


  METHOD delete_paxxxx.

    DATA: lr_structdescr TYPE REF TO cl_abap_structdescr,
          record_old     TYPE REF TO data,
          lr_tabledescr  TYPE REF TO cl_abap_tabledescr,
          lr_table_old   TYPE REF TO data,
          lr_table_new   TYPE REF TO data,
          ret            LIKE LINE OF return_tab.

    FIELD-SYMBOLS: <record_new_tab> TYPE STANDARD TABLE,
                   <record_old>     TYPE any,
                   <record_new>     TYPE any,
                   <record_old_tab> TYPE STANDARD TABLE.
    ASSIGN record TO <record_new>.

    lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV #( 'P' && infty ) ).
    CREATE DATA record_old TYPE HANDLE lr_structdescr.
    ASSIGN record_old->* TO <record_old>.
    lr_tabledescr ?= cl_abap_tabledescr=>create( p_line_type = lr_structdescr ).
    CREATE DATA lr_table_old TYPE HANDLE lr_tabledescr.
*    CREATE DATA lr_table_new TYPE HANDLE lr_tabledescr.
    ASSIGN lr_table_old->* TO <record_old_tab>.
*    ASSIGN lr_table_new->* TO <record_new_tab>.

    me->read_paxxxx(
    EXPORTING
      begda = begda
      endda = endda
      infty = infty
      pernr = pernr
      subty = subty
      simu = simu
      IMPORTING
        return_tab = return_tab
        record_tab = <record_old_tab>
      ).

*    CALL FUNCTION 'HR_READ_INFOTYPE'
*      EXPORTING
**       TCLAS     = 'A'
*        pernr     = pernr
*        infty     = infty
*        begda     = begda
*        endda     = endda
**       SPRPS     = '*'
**       BYPASS_BUFFER = ' '
**       LEGACY_MODE   = ' '
**     IMPORTING
*      TABLES
*        infty_tab = <record_old_tab>
** EXCEPTIONS
**       INFTY_NOT_FOUND       = 1
**       INVALID_INPUT = 2
**       OTHERS    = 3
*      .

    LOOP AT <record_old_tab> ASSIGNING FIELD-SYMBOL(<fs_record_old>).
      CLEAR: ret, return.
      IF <fs_record_old> IS ASSIGNED.
        ASSIGN COMPONENT /sew/cl_int_constants=>endda OF STRUCTURE <fs_record_old> TO FIELD-SYMBOL(<fs_endda_old>).
        ASSIGN COMPONENT /sew/cl_int_constants=>begda OF STRUCTURE <fs_record_old> TO FIELD-SYMBOL(<fs_begda_old>).
*        IF <fs_endda_old> GT delimit_date.

        "Delete old record
        me->maintain_paxxxx(
        EXPORTING
          begda = <fs_begda_old>
          endda = <fs_endda_old>
          pernr = pernr
          infty = infty
          subty = subty
          record = <fs_record_old> "<record_new> "<fs_record_old>
          operation = /sew/cl_int_constants=>infty_operation-pa_delete
          simu = simu
          IMPORTING
            return = return
          ).

*          <fs_endda_old> = delimit_date.

*          me->maintain_paxxxx(
*          EXPORTING
*            begda = <fs_begda_old>
*            endda = <fs_endda_old>
*            pernr = pernr
*            infty = infty
*            subty = subty
*            record = <record_new>
*            operation = /sew/cl_int_constants=>infty_operation-pa_insert
*            simu = simu
*            IMPORTING
*              return = return
*            ).


        IF return IS NOT INITIAL."-type = /sew/cl_int_constants=>error.
          MOVE-CORRESPONDING return TO ret.

        ELSE.
          ret = VALUE hrpad_return( type = /sew/cl_int_constants=>success
                   id = /sew/cl_int_constants=>msg_class_int
                   number = /sew/cl_int_constants=>msg_no-m17
                   message_v1 = pernr
                   message_v2 = infty
                   message_v3 = subty
                   message_v4 = delimit_date
                 ).
        ENDIF.
        APPEND ret TO return_tab.
*        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD delete_paxxxx_dcif.


    DATA: lv_pernr               TYPE p_pernr,
          lv_begda               TYPE begda,
          lv_endda               TYPE endda,
          lc_hcm_globalhr_person TYPE REF TO /sew/cl_hcm_globalhr_person,
          lt_prelp               TYPE hrpad_prelp_tab,
          lr_infty_reader        TYPE REF TO if_hrpa_read_infotype,
          lr_message_list        TYPE REF TO cl_hrpa_message_list,
          lr_masterdata_bl       TYPE REF TO if_hrpa_masterdata_bl,
          container              TYPE REF TO if_hrpa_infty_container,
          old_container          TYPE REF TO cl_hrpa_infotype_container,
          new_container          TYPE REF TO if_hrpa_infty_container,
          new_infotype_container TYPE REF TO cl_hrpa_infotype_container,
          infotype_ref           TYPE REF TO data,
          lv_is_ok               TYPE boole_d,
          lv_dummy               TYPE string,
          ls_msg                 TYPE symsg,
          lv_massg               TYPE massg,
          lv_massn               TYPE massn,
          lv_begend              TYPE date,
          lv_endbeg              TYPE date,
          entity_ref             TYPE REF TO data,
          data_ref               TYPE REF TO data,
          pnnnn_ref              TYPE REF TO data,
          container_tab          TYPE hrpad_infty_container_tab,
          container_if           TYPE hrpad_infty_container_ref,
          t777d                  TYPE t777d,
          lv_has_error           TYPE boole_d,
          lv_count               TYPE i,
          lv_offset              TYPE i,
          lv_infotype            TYPE infty,
          lv_entity              TYPE char50,
          lt_messages            TYPE hrpad_message_tab,
          ls_pa0000              TYPE p0000,
          ls_pa0001              TYPE p0001,
          ls_pa0050              TYPE p0050,
          ls_pa0105              TYPE p0105.

    FIELD-SYMBOLS: <pshdr>    TYPE pshdr,
                   <pnnnn>    TYPE any,
                   <ptnnnn>   TYPE ANY TABLE,
                   <pskey>    TYPE pskey,
                   <pxxxx>    TYPE any,
                   <entity>   TYPE any,
                   <data>     TYPE p0000,
                   <struc>    TYPE any,
                   <fs_pnnnn> TYPE ANY TABLE,
                   <fs_endda> TYPE p0001-endda,
                   <fs_begda> TYPE p0001-begda.

    TRY.

        CREATE OBJECT lr_message_list.

        CALL METHOD cl_hrpa_masterdata_factory=>get_business_logic
          IMPORTING
            business_logic = lr_masterdata_bl.

        TRY.
            CALL METHOD cl_hrpa_read_infotype=>get_instance
              IMPORTING
                infotype_reader = lr_infty_reader.
          CATCH cx_hrpa_violated_assertion .
        ENDTRY.

        CLEAR ls_pa0001.
        CALL METHOD lr_masterdata_bl->read
          EXPORTING
            tclas           = 'A'
            infty           = '0001'
            pernr           = iv_pernr
            begda           = iv_begda
            endda           = iv_begda
            no_auth_check   = space
            message_handler = lr_message_list
          IMPORTING
            container_tab   = container_tab
            is_ok           = lv_is_ok.

        IF lv_is_ok EQ abap_true.
          DESCRIBE TABLE container_tab LINES lv_count.

          IF lv_count GT 0.
            READ TABLE container_tab INTO container INDEX lv_count.
          ENDIF.
        ENDIF.
        IF container_tab IS NOT INITIAL.
          old_container ?= container.

          TRY.
              CALL METHOD old_container->if_hrpa_infty_container_data~primary_record_ref
                IMPORTING
                  pnnnn_ref = infotype_ref.
            CATCH cx_hrpa_violated_assertion .
          ENDTRY.

          UNASSIGN <pxxxx>.

          ASSIGN infotype_ref->* TO <pxxxx>.

          MOVE <pxxxx> TO ls_pa0001.
        ENDIF.

        CHECK lr_masterdata_bl IS BOUND.

        CALL METHOD lr_masterdata_bl->read
          EXPORTING
            tclas           = 'A'
            infty           = '0000'
            pernr           = iv_pernr
            begda           = iv_begda
            endda           = iv_endda
            no_auth_check   = space
            message_handler = er_message_list
          IMPORTING
            container_tab   = container_tab
            is_ok           = lv_is_ok.

        IF lv_is_ok EQ abap_true.
          DESCRIBE TABLE container_tab LINES lv_count.

          IF lv_count GT 0.
            READ TABLE container_tab INTO container INDEX lv_count.
          ENDIF.
        ENDIF.

        IF lv_is_ok = abap_false.
          CALL METHOD lr_message_list->get_abend_list
            IMPORTING
              messages = lt_messages.

          IF lt_messages IS INITIAL.
            CALL METHOD lr_message_list->get_error_list
              IMPORTING
                messages = lt_messages.
          ENDIF.
          RETURN.
        ENDIF.

        old_container ?= container.

        TRY.
            CALL METHOD old_container->if_hrpa_infty_container_data~primary_record_ref
              IMPORTING
                pnnnn_ref = infotype_ref.
          CATCH cx_hrpa_violated_assertion .
        ENDTRY.

        ASSIGN infotype_ref->* TO <pxxxx>.
        IF <pxxxx> IS ASSIGNED.
          ASSIGN COMPONENT 'PSKEY' OF STRUCTURE <pxxxx> TO <pskey>.
          MOVE-CORRESPONDING <pxxxx> TO <pskey>.
        ENDIF.

        MOVE <pxxxx> TO ls_pa0000.
        ls_pa0000-begda = iv_begda.
        ls_pa0000-endda = iv_endda.
        ls_pa0000-massn = '03'.

        IF <pskey> IS ASSIGNED.
          MOVE-CORRESPONDING ls_pa0000 TO <pskey>.
        ENDIF.

        READ TABLE container_tab INTO container_if  INDEX 1.
        new_infotype_container ?= container_if.
        new_infotype_container ?= new_infotype_container->modify_key( <pskey> ).

        new_infotype_container ?= new_infotype_container->modify_primary_record( ls_pa0000 ).
        new_container ?= new_infotype_container.

        CALL METHOD lr_masterdata_bl->insert
          EXPORTING
            massn           = ls_pa0000-massn
            message_handler = er_message_list
            no_auth_check   = space
          IMPORTING
            is_ok           = lv_is_ok
          CHANGING
            container       = new_container.

*        IF lv_is_ok = abap_true.
*
*          CLEAR: container_tab, new_infotype_container, old_container.
*
** mod it0001
*          CALL METHOD lr_masterdata_bl->read
*            EXPORTING
*              tclas           = 'A'
*              infty           = '0001'
*              pernr           = iv_pernr
*              begda           = iv_begda
*              endda           = iv_begda
*              no_auth_check   = space
*              message_handler = lr_message_list
*            IMPORTING
*              container_tab   = container_tab
*              is_ok           = lv_is_ok.
*
*          IF lv_is_ok EQ abap_true.
*            DESCRIBE TABLE container_tab LINES lv_count.
*
*            IF lv_count GT 0.
*              READ TABLE container_tab INTO container INDEX lv_count.
*            ENDIF.
*          ENDIF.
*          IF container_tab IS NOT INITIAL.
*            old_container ?= container.
*
*            TRY.
*                CALL METHOD old_container->if_hrpa_infty_container_data~primary_record_ref
*                  IMPORTING
*                    pnnnn_ref = infotype_ref.
*              CATCH cx_hrpa_violated_assertion .
*            ENDTRY.
*
*            UNASSIGN <pxxxx>.
*
*            ASSIGN infotype_ref->* TO <pxxxx>.
*
*            MOVE <pxxxx> TO ls_pa0001.
*
*            ls_pa0001-begda = iv_begda.
*            ls_pa0001-endda = iv_endda.
*            ls_pa0001-persg = '6'.
*
*            IF <pxxxx> IS ASSIGNED.
*              ASSIGN COMPONENT 'PSKEY' OF STRUCTURE <pxxxx> TO <pskey>.
*              MOVE-CORRESPONDING  ls_pa0001 TO <pskey>.
*            ENDIF.
*
*            READ TABLE container_tab INTO container_if  INDEX 1.
*            new_infotype_container ?= container_if.
*            new_infotype_container ?= new_infotype_container->modify_key( <pskey> ).
*
*            new_infotype_container ?= new_infotype_container->modify_primary_record( ls_pa0001 ).
*            new_container ?= new_infotype_container.
*
*            CALL METHOD lr_masterdata_bl->modify
*              EXPORTING
*                old_container   = old_container
*                massn           = ls_pa0000-massn
*                message_handler = lr_message_list
*                no_auth_check   = space
*              IMPORTING
*                is_ok           = lv_is_ok
*              CHANGING
*                container       = new_container.
*          ENDIF.
*        ENDIF.

* mod it0050
        IF lv_is_ok = abap_true.
*--------------------------------------------------------------------*
* DE5SHAUK, 04.05.2018, MHP
* Not only change infotype, also delimit position
* Begin of change
*--------------------------------------------------------------------*
*         Read Position
          SELECT * FROM hrp1001
            INTO TABLE @DATA(t1001)
            WHERE otype = 'P'
            AND objid = @iv_pernr
            AND sclas = 'S'.
          IF sy-subrc IS INITIAL.
            LOOP AT t1001 INTO DATA(s1001).
              SELECT * FROM hrp1000
                INTO TABLE @DATA(t1000)
                WHERE otype = 'S'
                AND objid = @s1001-sobid.
*              AND objid = @ls_pa0001-plans.
*            AND objid = @ls_pa0001-plans.
              SORT t1000 BY begda ASCENDING.
              READ TABLE t1000 INTO DATA(s1000) INDEX 1.
*         Delimit Position
              IF s1000-begda = iv_begda.
                CALL FUNCTION 'RHOM_DELETE_OBJECT'
                  EXPORTING
                    plvar                  = '01'
                    otype                  = s1000-otype
                    objid                  = s1000-objid
                    delete_from_date       = iv_begda
                    delete_complete        = 'X'
                  EXCEPTIONS
                    error_during_delete    = 1
                    enqueue_error          = 2
                    wrong_delete_from_date = 3
                    otype_not_supported    = 4
                    OTHERS                 = 5.
                IF sy-subrc <> 0.
                  ev_message = 'Error during delete assignment, pleace contact your system administrator!'.
                ENDIF.
              ELSE.
                CALL FUNCTION 'RHOM_DELETE_OBJECT'
                  EXPORTING
                    plvar                  = '01'
                    otype                  = s1000-otype
                    objid                  = s1000-objid
                    delete_from_date       = iv_begda
                    delete_complete        = ''
                  EXCEPTIONS
                    error_during_delete    = 1
                    enqueue_error          = 2
                    wrong_delete_from_date = 3
                    otype_not_supported    = 4
                    OTHERS                 = 5.
                IF sy-subrc <> 0.
                  ev_message = 'Error during delete assignment, pleace contact your system administrator!'.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.
*--------------------------------------------------------------------*
* End of change
*--------------------------------------------------------------------*

          CLEAR: container_tab, new_infotype_container, old_container.

          CALL METHOD lr_masterdata_bl->read
            EXPORTING
              tclas           = 'A'
              infty           = '0050'
              pernr           = iv_pernr
              begda           = iv_begda
              endda           = iv_begda
              no_auth_check   = space
              message_handler = er_message_list
            IMPORTING
              container_tab   = container_tab
              is_ok           = lv_is_ok.

          IF lv_is_ok EQ abap_true.
            DESCRIBE TABLE container_tab LINES lv_count.

            IF lv_count GT 0.
              READ TABLE container_tab INTO container INDEX lv_count.
            ENDIF.
          ENDIF.

          IF container_tab IS NOT INITIAL.

            old_container ?= container.

            TRY.
                CALL METHOD old_container->if_hrpa_infty_container_data~primary_record_ref
                  IMPORTING
                    pnnnn_ref = infotype_ref.
              CATCH cx_hrpa_violated_assertion .
            ENDTRY.

            UNASSIGN <pxxxx>.

            ASSIGN infotype_ref->* TO <pxxxx>.

            MOVE <pxxxx> TO ls_pa0050.

            ls_pa0050-endda = iv_begda.

            IF <pxxxx> IS ASSIGNED.
              ASSIGN COMPONENT 'PSKEY' OF STRUCTURE <pxxxx> TO <pskey>.
              MOVE-CORRESPONDING ls_pa0050 TO <pskey>.
            ENDIF.

            READ TABLE container_tab INTO container_if  INDEX 1.
            new_infotype_container ?= container_if.
            new_infotype_container ?= new_infotype_container->modify_key( <pskey> ).

            new_infotype_container ?= new_infotype_container->modify_primary_record( ls_pa0050 ).
            new_container ?= new_infotype_container.

            CALL METHOD lr_masterdata_bl->modify
              EXPORTING
                old_container   = old_container
                massn           = ls_pa0000-massn
                message_handler = er_message_list
                no_auth_check   = space
              IMPORTING
                is_ok           = lv_is_ok
              CHANGING
                container       = new_container.

          ENDIF.
        ENDIF.
* mod 0105

        CLEAR: container_tab, new_infotype_container, old_container.

        IF lv_is_ok = abap_true.

          CLEAR: container_tab, new_infotype_container, old_container.

          CALL METHOD lr_masterdata_bl->read
            EXPORTING
              tclas           = 'A'
              infty           = '0105'
              pernr           = iv_pernr
              begda           = iv_begda
              endda           = iv_begda
              no_auth_check   = space
              message_handler = er_message_list
            IMPORTING
              container_tab   = container_tab
              is_ok           = lv_is_ok.

          IF lv_is_ok EQ abap_true.

            LOOP AT container_tab INTO container.

              old_container ?= container.

              TRY.
                  CALL METHOD old_container->if_hrpa_infty_container_data~primary_record_ref
                    IMPORTING
                      pnnnn_ref = infotype_ref.
                CATCH cx_hrpa_violated_assertion .
              ENDTRY.

              UNASSIGN <pxxxx>.

              ASSIGN infotype_ref->* TO <pxxxx>.

              MOVE <pxxxx> TO ls_pa0105.
              ls_pa0105-endda = iv_begda.

              IF <pxxxx> IS ASSIGNED.
                ASSIGN COMPONENT 'PSKEY' OF STRUCTURE <pxxxx> TO <pskey>.
                MOVE-CORRESPONDING ls_pa0105 TO <pskey>.
              ENDIF.

              new_infotype_container ?= container.
              new_infotype_container ?= new_infotype_container->modify_key( <pskey> ).

              new_infotype_container ?= new_infotype_container->modify_primary_record( ls_pa0105 ).
              new_container ?= new_infotype_container.

              CALL METHOD lr_masterdata_bl->modify
                EXPORTING
                  old_container   = old_container
                  massn           = ls_pa0000-massn
                  message_handler = er_message_list
                  no_auth_check   = space
                IMPORTING
                  is_ok           = lv_is_ok
                CHANGING
                  container       = new_container.
            ENDLOOP.
          ENDIF.
        ENDIF.

      CATCH /iwbep/cx_mgw_busi_exception.
      CATCH /iwbep/cx_mgw_tech_exception.
    ENDTRY.
  ENDMETHOD.


  METHOD delimit_hrpxxxx.

    DATA: lr_structdescr TYPE REF TO cl_abap_structdescr,
          hrp_old_it     TYPE REF TO data,
          lr_tabledescr  TYPE REF TO cl_abap_tabledescr,
          lr_table_old   TYPE REF TO data,
          lr_table_new   TYPE REF TO data.

    FIELD-SYMBOLS: <hrp_new_tab> TYPE STANDARD TABLE,
                   <hrp_old>     TYPE any,
                   <record>      TYPE any,
                   <hrp_old_tab> TYPE STANDARD TABLE.
    ASSIGN record TO <record>.

    lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV #( 'P' && infty ) ).
    lr_tabledescr ?= cl_abap_tabledescr=>create( p_line_type = lr_structdescr ).
    CREATE DATA lr_table_old TYPE HANDLE lr_tabledescr.
    ASSIGN lr_table_old->* TO <hrp_old_tab>.

    DATA(global_hr) = 'GLOB'.
    CLEAR: global_hr.
    SET PARAMETER ID 'GLOB' FIELD global_hr.
    APPEND <record> TO <hrp_old_tab>.
    CALL FUNCTION 'RH_CUT_INFTY'
      EXPORTING
*       LOAD               = 'X'
        gdate              = delimit_date
        histo              = ' '
*       DEL_SUCC           = ' '
        vtask              = 'V'
*       ORDER_FLG          = 'X'
*       COMMIT_FLG         = 'X'
        authy              = ' '
*       PPPAR_IMP          =
*       KEEP_LUPD          =
*       WORKF_ACTV         = 'X'
      TABLES
        innnn              = <hrp_old_tab>
*       ILFCODE            =
      EXCEPTIONS
        error_during_cut   = 1
        no_authorization   = 2
        gdate_before_begda = 3
        cut_of_timco_one   = 4
        corr_exit          = 5
        OTHERS             = 6.

    IF sy-subrc <> 0.
* Implement suitable error handling here
      return = /sew/cl_int_utility=>map_sy_msg( msgid = sy-msgid msgty = sy-msgty msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 msgv4 = sy-msgv4 ).

    ELSE.
    ENDIF.
*    global_hr = 'GLOB'.
*    SET PARAMETER ID 'GLOB' FIELD global_hr.
  ENDMETHOD.


  METHOD delimit_paxxxx.

    DATA: lr_structdescr TYPE REF TO cl_abap_structdescr,
          record_old     TYPE REF TO data,
          lr_tabledescr  TYPE REF TO cl_abap_tabledescr,
          lr_table_old   TYPE REF TO data,
          lr_table_new   TYPE REF TO data,
          ret            LIKE LINE OF return_tab.

    FIELD-SYMBOLS: <record_new_tab> TYPE STANDARD TABLE,
                   <record_old>     TYPE any,
                   <record_new>     TYPE any,
                   <record_old_tab> TYPE STANDARD TABLE.
    ASSIGN record TO <record_new>.

    lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV #( 'P' && infty ) ).
    CREATE DATA record_old TYPE HANDLE lr_structdescr.
    ASSIGN record_old->* TO <record_old>.
    lr_tabledescr ?= cl_abap_tabledescr=>create( p_line_type = lr_structdescr ).
    CREATE DATA lr_table_old TYPE HANDLE lr_tabledescr.
*    CREATE DATA lr_table_new TYPE HANDLE lr_tabledescr.
    ASSIGN lr_table_old->* TO <record_old_tab>.
*    ASSIGN lr_table_new->* TO <record_new_tab>.

    me->read_paxxxx(
    EXPORTING
      begda = begda
      endda = endda
      infty = infty
      pernr = pernr
      subty = subty
      simu = simu
      IMPORTING
        return_tab = return_tab
        record_tab = <record_old_tab>
      ).

*    CALL FUNCTION 'HR_READ_INFOTYPE'
*      EXPORTING
**       TCLAS     = 'A'
*        pernr     = pernr
*        infty     = infty
*        begda     = begda
*        endda     = endda
**       SPRPS     = '*'
**       BYPASS_BUFFER = ' '
**       LEGACY_MODE   = ' '
**     IMPORTING
*      TABLES
*        infty_tab = <record_old_tab>
** EXCEPTIONS
**       INFTY_NOT_FOUND       = 1
**       INVALID_INPUT = 2
**       OTHERS    = 3
*      .

    LOOP AT <record_old_tab> ASSIGNING FIELD-SYMBOL(<fs_record_old>).
      CLEAR: ret, return.
      IF <fs_record_old> IS ASSIGNED.
        ASSIGN COMPONENT /sew/cl_int_constants=>endda OF STRUCTURE <fs_record_old> TO FIELD-SYMBOL(<fs_endda_old>).
        ASSIGN COMPONENT /sew/cl_int_constants=>begda OF STRUCTURE <fs_record_old> TO FIELD-SYMBOL(<fs_begda_old>).
        DATA(date_puf) = delimit_date.
        date_puf = <fs_endda_old>.
        <fs_endda_old> = delimit_date.
*        IF <fs_endda_old> GT delimit_date.
        "Delete old record
        me->maintain_paxxxx(
        EXPORTING
          begda = <fs_begda_old>
          endda = date_puf "<fs_endda_old>
          pernr = pernr
          infty = infty
          subty = subty
          record = <fs_record_old>
          operation = /sew/cl_int_constants=>infty_operation-pa_modify
          simu = simu
          IMPORTING
            return = return
          ).

*          <fs_endda_old> = delimit_date.
*
*          me->maintain_paxxxx(
*          EXPORTING
*            begda = <fs_begda_old>
*            endda = <fs_endda_old>
*            pernr = pernr
*            infty = infty
*            subty = subty
*            record = <fs_record_old>
*            operation = /sew/cl_int_constants=>infty_operation-pa_insert
*            simu = simu
*            IMPORTING
*              return = return
*            ).

*          CALL FUNCTION 'HR_INFOTYPE_OPERATION'
*            EXPORTING
*              infty         = infty
**             recordnumber  = <it_aend>-seqnr
*              number        = pernr
*              subtype       = subty
*              validityend   = <fs_endda_old>
*              validitybegin = <fs_begda_old>
*              record        = <fs_record_old>
*              operation     = /sew/cl_int_constants=>infty_operation-pa_delimit
*              nocommit      = simu
*            IMPORTING
*              return        = return.
*        key           = key.
        IF return IS NOT INITIAL."-type = /sew/cl_int_constants=>error.
          MOVE-CORRESPONDING return TO ret.

        ELSE.
          ret = VALUE hrpad_return( type = /sew/cl_int_constants=>success
                   id = /sew/cl_int_constants=>msg_class_int
                   number = /sew/cl_int_constants=>msg_no-m17
                   message_v1 = pernr
                   message_v2 = infty
                   message_v3 = subty
                   message_v4 = delimit_date
                 ).
        ENDIF.
        APPEND ret TO return_tab.
*        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD handle_hiredata_change.

    DATA: lr_structdescr TYPE REF TO cl_abap_structdescr,
          record_old     TYPE REF TO data,
          lr_tabledescr  TYPE REF TO cl_abap_tabledescr,
          lr_table_old   TYPE REF TO data,
          lr_table       TYPE REF TO data,
          record_new     TYPE REF TO data,
          ret            LIKE LINE OF return_tab,
          return         TYPE bapiret1,
          tab_0302       TYPE STANDARD TABLE OF p0302,
          tab_0001       TYPE STANDARD TABLE OF p0001,
          action         TYPE actio.

    FIELD-SYMBOLS: <record_new_tab> TYPE STANDARD TABLE,
*                   <record_old>     TYPE any,
                   <record_new>     TYPE any,
                   <record_old_tab> TYPE STANDARD TABLE,
                   <record_tab>     TYPE STANDARD TABLE.
*    ASSIGN record TO <record_new>.

    DATA(molga) = /sew/cl_int_utility=>get_molga( pernr = pernr begda = begda endda = endda ).

**JMB20210617 delete start
*    SELECT * FROM /sew/int_infotyp INTO TABLE @DATA(it_cust) WHERE object = @/sew/cl_int_constants=>person AND molga = @molga.
*    DATA(infotypes) = it_cust.
*    SELECT * FROM /sew/int_infotyp INTO TABLE @it_cust WHERE object = @/sew/cl_int_constants=>person AND molga = '*'.
*    APPEND LINES OF it_cust TO infotypes.
*JMB20210617 delete end, insert start
    DATA(molga_r) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = molga )
                                        ( sign = 'I' option = 'EQ' low = '*'   ) ).
    SELECT * FROM /sew/int_infotyp INTO TABLE @DATA(infotypes) WHERE object EQ @/sew/cl_int_constants=>person AND
                                                                     molga  IN @molga_r.
*JMB20210617 insert end

    SORT infotypes ASCENDING BY infty.
    DELETE ADJACENT DUPLICATES FROM infotypes COMPARING infty.

    LOOP AT infotypes ASSIGNING FIELD-SYMBOL(<infotype>).
      lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV #( 'P' && <infotype>-infty ) ).

**JMB20210618 start insert - in case of IT2011 update relevant entries in TEVEN
*
      CASE <infotype>-infty.
        WHEN /sew/cl_int_constants=>it2011.
          ret = maintain_teven_hc( pernr = pernr
                                begda = begda
                                endda = endda
                                simu  = simu ).
          CHECK ret IS NOT INITIAL.
          APPEND ret TO return_tab.
          CLEAR ret.
          CONTINUE.
      ENDCASE.
*JMB20210618 end insert

      CREATE DATA record_old TYPE HANDLE lr_structdescr.

*    ASSIGN record_old->* TO <record_old>.
      lr_tabledescr ?= cl_abap_tabledescr=>create( p_line_type = lr_structdescr ).
      CREATE DATA lr_table_old TYPE HANDLE lr_tabledescr.
      CREATE DATA lr_table TYPE HANDLE lr_tabledescr.
      ASSIGN lr_table_old->* TO <record_old_tab>.
      ASSIGN lr_table->* TO <record_tab>.
      me->read_paxxxx( EXPORTING begda = begda
                                 endda = endda
                                 infty = <infotype>-infty
                                 pernr = pernr
                                 subty = subty
                                 simu  = simu
                       IMPORTING return_tab = return_tab
                                 record_tab = <record_old_tab> ).

      IF <record_old_tab> IS NOT INITIAL.
*        READ TABLE <record_old_tab> ASSIGNING FIELD-SYMBOL(<record_old>) INDEX 1.
        LOOP AT <record_old_tab> ASSIGNING FIELD-SYMBOL(<record_old>).
          "Build new IT
          CREATE DATA record_new TYPE HANDLE lr_structdescr.
          ASSIGN record_new->* TO <record_new>.
          <record_new> = CORRESPONDING #( <record_old> ).

          "Get relevant data from old record
          ASSIGN COMPONENT /sew/cl_int_constants=>begda OF STRUCTURE <record_old> TO FIELD-SYMBOL(<begda_old>).
          ASSIGN COMPONENT /sew/cl_int_constants=>endda OF STRUCTURE <record_old> TO FIELD-SYMBOL(<endda_old>).
          ASSIGN COMPONENT /sew/cl_int_constants=>begda OF STRUCTURE <record_new> TO FIELD-SYMBOL(<begda_new>).
*      ASSIGN COMPONENT 'STAT2' OF STRUCTURE <record_new> TO FIELD-SYMBOL(<stat_new>).
*      ASSIGN COMPONENT /sew/cl_int_constants=>massn OF STRUCTURE <record_new> TO FIELD-SYMBOL(<massn_new>).
*      <massn_new> = /sew/cl_int_constants=>termination.
*      <stat_new> = '0'.
          DATA dat TYPE begda.
          dat = <begda_new>.
          <begda_new> = begda.
*      <endda_new> = begda - 1.
          "old pernr needs to be terminated

**JMB20210617 start insert - in case of hire date change, IT0302 needs to be updated before IT0000
*
          IF <infotype>-infty EQ /sew/cl_int_constants=>it0000.
            me->read_paxxxx( EXPORTING begda = <begda_old>
                                       endda = <begda_old>
                                       infty = /sew/cl_int_constants=>it0302
                                       pernr = pernr
                                       simu  = simu
                             IMPORTING return_tab = return_tab
                                       record_tab = tab_0302 ).

            LOOP AT tab_0302 ASSIGNING FIELD-SYMBOL(<0302_entry>) WHERE massn IN /sew/cl_int_constants=>hire_range.

              <0302_entry>-begda   = <begda_new>.
              DATA(endda_old_0302) = <0302_entry>-endda.

              IF <0302_entry>-endda LT <begda_new>.
                <0302_entry>-endda = <begda_new>.
              ENDIF.

              maintain_paxxxx( EXPORTING begda     = <begda_old>
                                         endda     = endda_old_0302
                                         pernr     = pernr
                                         infty     = /sew/cl_int_constants=>it0302
                                         subty     = <0302_entry>-subty
                                         number    = <0302_entry>-seqnr
                                         record    = <0302_entry>
                                         operation = /sew/cl_int_constants=>infty_operation-pa_modify
                                         simu      = simu
                               IMPORTING return    = return ).
            ENDLOOP.
            CLEAR: tab_0302.
          ENDIF.
*JMB20210617 end insert

          IF <infotype>-infty EQ /sew/cl_int_constants=>it0000.
            "Read current position
            me->read_paxxxx( EXPORTING begda = <begda_old>
                                       endda = <begda_old>
                                       infty = CONV #( /sew/cl_int_constants=>it0001 )
                                       pernr = pernr
                                       simu  = simu
                             IMPORTING return_tab = return_tab
                                       record_tab = tab_0001 ).

            READ TABLE tab_0001 INTO DATA(p0001) INDEX 1.

            "Adjust begda of corresponding records regarding position
            SUBMIT rhbegda0 AND RETURN
            WITH pchplvar = /sew/cl_int_constants=>plvar
            WITH pchotype = /sew/cl_int_constants=>position
            WITH pchobjid-low = p0001-plans
            WITH old_beg = <begda_old>
            WITH new_beg = <begda_new>
            WITH enq EQ abap_true
            WITH anzeige EQ abap_false
            WITH test EQ abap_true.

            IF sy-subrc IS INITIAL.
              SUBMIT rhbegda0 AND RETURN
              WITH pchplvar = /sew/cl_int_constants=>plvar
              WITH pchotype = /sew/cl_int_constants=>position
              WITH pchobjid-low = p0001-plans
              WITH old_beg = <begda_old>
              WITH new_beg = <begda_new>
              WITH enq EQ abap_true
              WITH anzeige EQ abap_false
              WITH test EQ simu.
              IF sy-subrc IS INITIAL.
*              ASSIGN COMPONENT /sew/cl_int_constants=>plans OF STRUCTURE <record_new> TO FIELD-SYMBOL(<plans>).
                SELECT SINGLE * FROM t528b INTO @DATA(t528b) WHERE begda = @<begda_old> AND endda = @<endda_old> AND plans = @p0001-plans AND otype = @/sew/cl_int_constants=>position.
                IF sy-subrc IS INITIAL.
                  t528b-begda = <begda_new>.
                  MODIFY t528b FROM t528b.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
          IF <infotype>-infty EQ /sew/cl_int_constants=>it0050 OR <infotype>-infty EQ /sew/cl_int_constants=>it0105.
            action = /sew/cl_int_constants=>infty_operation-pa_copy.
          ELSE.
            action = /sew/cl_int_constants=>infty_operation-pa_modify.
          ENDIF.
          me->maintain_paxxxx( EXPORTING begda     = <begda_old>
                                         endda     = <endda_old>
                                         pernr     = pernr
                                         infty     = <infotype>-infty
                                         subty     = subty
*                                       number    = <record_new>-seqnr
                                         record    = <record_new>
                                         operation = action "/sew/cl_int_constants=>infty_operation-pa_modify
                                         simu      = simu
                               IMPORTING return    = return ).

          IF return-type NE /sew/cl_int_constants=>error.

            /sew/cl_int_utility=>get_spras_by_molga( EXPORTING molga = molga
            IMPORTING spras = DATA(spras) langu = DATA(langu) ).
            ret = VALUE hrpad_return( type       = /sew/cl_int_constants=>success
                                      id         = /sew/cl_int_constants=>msg_class_int
                                      number     = /sew/cl_int_constants=>msg_no-m22
                                      message_v1 = pernr
                                      message_v2 = <infotype>-infty
*                                   message_v3 = /sew/cl_int_utility=>read_massn_txt( massn = CONV #( /sew/cl_int_constants=>hire ) sprsl = spras )
*                                   message_v4 = delimit_date
                                     ).
          ELSE.
            ret = CORRESPONDING #( return ).
          ENDIF.

          APPEND ret TO return_tab.
          CLEAR: ret, return.
        ENDLOOP.
      ENDIF.
      CLEAR: ret, return.
    ENDLOOP.

    "If record tab is not initial then it is not an initial hire - hire was changed

  ENDMETHOD.


  METHOD HANDLE_REVERSE_TERMINATION.

    DATA: lr_structdescr TYPE REF TO cl_abap_structdescr,
          record_old     TYPE REF TO data,
          lr_tabledescr  TYPE REF TO cl_abap_tabledescr,
          lr_table_old   TYPE REF TO data,
          lr_table       TYPE REF TO data,
          record_new     TYPE REF TO data,
          ret            LIKE LINE OF return_tab,
          return         TYPE bapiret1,
          tab_0302       TYPE STANDARD TABLE OF p0302,
          tab_0001       TYPE STANDARD TABLE OF p0001,
          action         TYPE actio.

    FIELD-SYMBOLS: <record_new_tab> TYPE STANDARD TABLE,
*                   <record_old>     TYPE any,
                   <record_new>     TYPE any,
                   <record_old_tab> TYPE STANDARD TABLE,
                   <record_tab>     TYPE STANDARD TABLE.
*    ASSIGN record TO <record_new>.

    DATA(molga) = /sew/cl_int_utility=>get_molga( pernr = pernr begda = begda endda = endda ).

**JMB20210617 delete start
*    SELECT * FROM /sew/int_infotyp INTO TABLE @DATA(it_cust) WHERE object = @/sew/cl_int_constants=>person AND molga = @molga.
*    DATA(infotypes) = it_cust.
*    SELECT * FROM /sew/int_infotyp INTO TABLE @it_cust WHERE object = @/sew/cl_int_constants=>person AND molga = '*'.
*    APPEND LINES OF it_cust TO infotypes.
*JMB20210617 delete end, insert start
    DATA(molga_r) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = molga )
                                        ( sign = 'I' option = 'EQ' low = '*'   ) ).
    SELECT * FROM /sew/int_infotyp INTO TABLE @DATA(infotypes) WHERE object EQ @/sew/cl_int_constants=>person AND
                                                                     molga  IN @molga_r.
*JMB20210617 insert end

    SORT infotypes ASCENDING BY infty.
    DELETE ADJACENT DUPLICATES FROM infotypes COMPARING infty.

    LOOP AT infotypes ASSIGNING FIELD-SYMBOL(<infotype>).
      lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV #( 'P' && <infotype>-infty ) ).

**JMB20210618 start insert - in case of IT2011 update relevant entries in TEVEN
*
      CASE <infotype>-infty.
        WHEN /sew/cl_int_constants=>it2011.
          ret = maintain_teven_hc( pernr = pernr
                                begda = begda
                                endda = endda
                                simu  = simu ).
          CHECK ret IS NOT INITIAL.
          APPEND ret TO return_tab.
          CLEAR ret.
          CONTINUE.
      ENDCASE.
*JMB20210618 end insert

      CREATE DATA record_old TYPE HANDLE lr_structdescr.

*    ASSIGN record_old->* TO <record_old>.
      lr_tabledescr ?= cl_abap_tabledescr=>create( p_line_type = lr_structdescr ).
      CREATE DATA lr_table_old TYPE HANDLE lr_tabledescr.
      CREATE DATA lr_table TYPE HANDLE lr_tabledescr.
      ASSIGN lr_table_old->* TO <record_old_tab>.
      ASSIGN lr_table->* TO <record_tab>.
      me->read_paxxxx( EXPORTING begda = begda
                                 endda = endda
                                 infty = <infotype>-infty
                                 pernr = pernr
                                 subty = subty
                                 simu  = simu
                       IMPORTING return_tab = return_tab
                                 record_tab = <record_old_tab> ).

      IF <record_old_tab> IS NOT INITIAL.
*        READ TABLE <record_old_tab> ASSIGNING FIELD-SYMBOL(<record_old>) INDEX 1.
        LOOP AT <record_old_tab> ASSIGNING FIELD-SYMBOL(<record_old>).
          "Build new IT
          CREATE DATA record_new TYPE HANDLE lr_structdescr.
          ASSIGN record_new->* TO <record_new>.
          <record_new> = CORRESPONDING #( <record_old> ).

          "Get relevant data from old record
          ASSIGN COMPONENT /sew/cl_int_constants=>begda OF STRUCTURE <record_old> TO FIELD-SYMBOL(<begda_old>).
          ASSIGN COMPONENT /sew/cl_int_constants=>endda OF STRUCTURE <record_old> TO FIELD-SYMBOL(<endda_old>).
          ASSIGN COMPONENT /sew/cl_int_constants=>begda OF STRUCTURE <record_new> TO FIELD-SYMBOL(<begda_new>).
*      ASSIGN COMPONENT 'STAT2' OF STRUCTURE <record_new> TO FIELD-SYMBOL(<stat_new>).
*      ASSIGN COMPONENT /sew/cl_int_constants=>massn OF STRUCTURE <record_new> TO FIELD-SYMBOL(<massn_new>).
*      <massn_new> = /sew/cl_int_constants=>termination.
*      <stat_new> = '0'.
          DATA dat TYPE begda.
          dat = <begda_new>.
          <begda_new> = begda.
*      <endda_new> = begda - 1.
          "old pernr needs to be terminated

**JMB20210617 start insert - in case of hire date change, IT0302 needs to be updated before IT0000
*
          IF <infotype>-infty EQ /sew/cl_int_constants=>it0000.
            me->read_paxxxx( EXPORTING begda = <begda_old>
                                       endda = <begda_old>
                                       infty = /sew/cl_int_constants=>it0302
                                       pernr = pernr
                                       simu  = simu
                             IMPORTING return_tab = return_tab
                                       record_tab = tab_0302 ).

            LOOP AT tab_0302 ASSIGNING FIELD-SYMBOL(<0302_entry>).

              <0302_entry>-begda   = <begda_new>.
              DATA(endda_old_0302) = <0302_entry>-endda.

              IF <0302_entry>-endda LT <begda_new>.
                <0302_entry>-endda = <begda_new>.
              ENDIF.

              maintain_paxxxx( EXPORTING begda     = <begda_old>
                                         endda     = endda_old_0302
                                         pernr     = pernr
                                         infty     = /sew/cl_int_constants=>it0302
                                         subty     = <0302_entry>-subty
                                         number    = <0302_entry>-seqnr
                                         record    = <0302_entry>
                                         operation = /sew/cl_int_constants=>infty_operation-pa_modify
                                         simu      = simu
                               IMPORTING return    = return ).
            ENDLOOP.
            CLEAR: tab_0302.
          ENDIF.
*JMB20210617 end insert

          IF <infotype>-infty EQ /sew/cl_int_constants=>it0000.
            "Read current position
            me->read_paxxxx( EXPORTING begda = <begda_old>
                                       endda = <begda_old>
                                       infty = CONV #( /sew/cl_int_constants=>it0001 )
                                       pernr = pernr
                                       simu  = simu
                             IMPORTING return_tab = return_tab
                                       record_tab = tab_0001 ).

            READ TABLE tab_0001 INTO DATA(p0001) INDEX 1.

            "Adjust begda of corresponding records regarding position
            SUBMIT rhbegda0 AND RETURN
            WITH pchplvar = /sew/cl_int_constants=>plvar
            WITH pchotype = /sew/cl_int_constants=>position
            WITH pchobjid-low = p0001-plans
            WITH old_beg = <begda_old>
            WITH new_beg = <begda_new>
            WITH enq EQ abap_true
            WITH anzeige EQ abap_false
            WITH test EQ abap_true.

            IF sy-subrc IS INITIAL.
              SUBMIT rhbegda0 AND RETURN
              WITH pchplvar = /sew/cl_int_constants=>plvar
              WITH pchotype = /sew/cl_int_constants=>position
              WITH pchobjid-low = p0001-plans
              WITH old_beg = <begda_old>
              WITH new_beg = <begda_new>
              WITH enq EQ abap_true
              WITH anzeige EQ abap_false
              WITH test EQ simu.
              IF sy-subrc IS INITIAL.
*              ASSIGN COMPONENT /sew/cl_int_constants=>plans OF STRUCTURE <record_new> TO FIELD-SYMBOL(<plans>).
                SELECT SINGLE * FROM t528b INTO @DATA(t528b) WHERE begda = @<begda_old> AND endda = @<endda_old> AND plans = @p0001-plans AND otype = @/sew/cl_int_constants=>position.
                IF sy-subrc IS INITIAL.
                  t528b-begda = <begda_new>.
                  MODIFY t528b FROM t528b.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
          IF <infotype>-infty EQ /sew/cl_int_constants=>it0050 OR <infotype>-infty EQ /sew/cl_int_constants=>it0105.
            action = /sew/cl_int_constants=>infty_operation-pa_copy.
          ELSE.
            action = /sew/cl_int_constants=>infty_operation-pa_modify.
          ENDIF.
          me->maintain_paxxxx( EXPORTING begda     = <begda_old>
                                         endda     = <endda_old>
                                         pernr     = pernr
                                         infty     = <infotype>-infty
                                         subty     = subty
*                                       number    = <record_new>-seqnr
                                         record    = <record_new>
                                         operation = action "/sew/cl_int_constants=>infty_operation-pa_modify
                                         simu      = simu
                               IMPORTING return    = return ).

          IF return-type NE /sew/cl_int_constants=>error.

            /sew/cl_int_utility=>get_spras_by_molga( EXPORTING molga = molga
            IMPORTING spras = DATA(spras) langu = DATA(langu) ).
            ret = VALUE hrpad_return( type       = /sew/cl_int_constants=>success
                                      id         = /sew/cl_int_constants=>msg_class_int
                                      number     = /sew/cl_int_constants=>msg_no-m22
                                      message_v1 = pernr
                                      message_v2 = <infotype>-infty
*                                   message_v3 = /sew/cl_int_utility=>read_massn_txt( massn = CONV #( /sew/cl_int_constants=>hire ) sprsl = spras )
*                                   message_v4 = delimit_date
                                     ).
          ELSE.
            ret = CORRESPONDING #( return ).
          ENDIF.

          APPEND ret TO return_tab.
          CLEAR: ret, return.
        ENDLOOP.
      ENDIF.
      CLEAR: ret, return.
    ENDLOOP.

    "If record tab is not initial then it is not an initial hire - hire was changed

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

    DATA(global_hr) = 'GLOB'.
    SET PARAMETER ID 'GLOB' FIELD global_hr.
    IF infty NE /sew/cl_int_constants=>it1000.
      global_hr = ''.
      SET PARAMETER ID 'GLOB' FIELD global_hr.
    ENDIF.

    lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV #( 'P' && infty ) ).
    CREATE DATA hrp_old_it TYPE HANDLE lr_structdescr.
    ASSIGN hrp_old_it->* TO <hrp_old>.
    lr_tabledescr ?= cl_abap_tabledescr=>create( p_line_type = lr_structdescr ).
    CREATE DATA lr_table_old TYPE HANDLE lr_tabledescr.
    CREATE DATA lr_table_new TYPE HANDLE lr_tabledescr.
    ASSIGN lr_table_old->* TO <hrp_old_tab>.
    ASSIGN lr_table_new->* TO <hrp_new_tab>.

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

        ASSIGN COMPONENT /sew/cl_int_constants=>begda OF STRUCTURE <hrp_old> TO FIELD-SYMBOL(<begda_old>).
        "If begda old and begda are equal then there are two changes on the same day
        IF <begda_old> = begda.

          "Else old records needs to be delimited
        ELSE.
          APPEND <hrp_old> TO <hrp_old_tab>.
          CALL FUNCTION 'RH_CUT_INFTY'
            EXPORTING
*             LOAD               = 'X'
              gdate              = delimit_date
              histo              = ' '
*             DEL_SUCC           = ' '
              vtask              = 'D'
*             ORDER_FLG          = 'X'
*             COMMIT_FLG         = 'X'
              authy              = ' '
*             PPPAR_IMP          =
*             KEEP_LUPD          =
*             WORKF_ACTV         = 'X'
            TABLES
              innnn              = <hrp_old_tab>
*             ILFCODE            =
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
    APPEND <hrp_new> TO <hrp_new_tab>.
    CALL FUNCTION 'RH_INSERT_INFTY'
      EXPORTING
        fcode               = 'INSE'
        vtask               = 'V'
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
        innnn               = <hrp_new_tab>
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
    global_hr = ''.
    SET PARAMETER ID 'GLOB' FIELD global_hr.
  ENDMETHOD.


  METHOD maintain_pa0001.
    DATA: lr_message_list       TYPE REF TO cl_hrpa_message_list,
          lr_masterdata_bl      TYPE REF TO  if_hrpa_masterdata_bl,
          lr_container          TYPE REF TO  if_hrpa_infty_container,
          new_container         TYPE REF TO  if_hrpa_infty_container,
          lr_container_data     TYPE REF TO  if_hrpa_infty_container_data,
          lc_infotype_container TYPE REF TO cl_hrpa_infotype_container,
          lc_message_handler    TYPE REF TO cl_hrpay00_message_handler,
          container_tab         TYPE hrpad_infty_container_tab,
          container             TYPE REF TO if_hrpa_infty_container,
          old_container         TYPE REF TO cl_hrpa_infotype_container,
          infotype_ref          TYPE REF TO data,
          pskey                 TYPE pskey.
* Create
    TRY.
        CREATE OBJECT lr_message_list.

        p0001-pernr = pernr.
        p0001-infty = '0001'.
        p0001-subty = subty.

        CALL METHOD cl_hrpa_masterdata_factory=>get_business_logic
          IMPORTING
            business_logic = lr_masterdata_bl.

        MOVE p0001 TO pskey.

        CALL METHOD lr_masterdata_bl->get_infty_container
          EXPORTING
            tclas           = 'A'
            pskey           = pskey
            no_auth_check   = space
            message_handler = lr_message_list
          IMPORTING
            container       = lr_container
            is_ok           = DATA(ok).

        lc_infotype_container ?= lr_container.
        lc_infotype_container ?= lc_infotype_container->modify_key( pskey ).
        lc_infotype_container ?= lc_infotype_container->modify_primary_record( p0001 ).
        new_container ?= lc_infotype_container.

    ENDTRY.

    IF ok IS NOT INITIAL.

      CALL METHOD lr_masterdata_bl->read
        EXPORTING
          tclas           = 'A'
          infty           = '0001'
          pernr           = pernr
          begda           = begda
          endda           = endda
          no_auth_check   = space
          message_handler = lr_message_list
        IMPORTING
          container_tab   = container_tab
          is_ok           = ok.

      IF ok EQ abap_true.

        DESCRIBE TABLE container_tab LINES DATA(lv_count).

        IF lv_count GT 0.
*          READ TABLE container_tab INTO container INDEX lv_count - 1.
          READ TABLE container_tab INTO container INDEX lv_count.
        ENDIF.

        old_container ?= container.

        TRY.
            CALL METHOD old_container->if_hrpa_infty_container_data~primary_record_ref
              IMPORTING
                pnnnn_ref = infotype_ref.
          CATCH cx_hrpa_violated_assertion .
        ENDTRY.


        CALL METHOD lr_masterdata_bl->modify
          EXPORTING
            old_container   = old_container
            no_auth_check   = space
            message_handler = lr_message_list
          IMPORTING
            is_ok           = ok
          CHANGING
            container       = new_container.

      ENDIF.
    ENDIF.
    CALL FUNCTION '/SEW/FUB_GLOBHR_GET_ERROR_TXT'
      EXPORTING
        ir_message_list = lr_message_list
      IMPORTING
        return_tab      = return_tab.
    return = CORRESPONDING #( VALUE #( return_tab[ type = 'E' ] OPTIONAL ) ).
    IF return-type NE 'E'.
      CHECK simu EQ abap_false.
*      flush dcif
      CALL METHOD lr_masterdata_bl->flush
        EXPORTING
          no_commit = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD maintain_paxxxx.

    DATA: lr_structdescr TYPE REF TO cl_abap_structdescr,
          record_old     TYPE REF TO data,
          lr_tabledescr  TYPE REF TO cl_abap_tabledescr,
          lr_table_old   TYPE REF TO data,
          lr_table_new   TYPE REF TO data,
          ret            LIKE LINE OF return_tab.

    FIELD-SYMBOLS: <record_new_tab> TYPE STANDARD TABLE,
                   <record_old>     TYPE any,
                   <record_new>     TYPE any,
                   <record_old_tab> TYPE STANDARD TABLE.
*    ASSIGN record TO <record_new>.
*
*    lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV #( 'P' && infty ) ).
*    CREATE DATA record_old TYPE HANDLE lr_structdescr.
*    ASSIGN record_old->* TO <record_old>.
*    lr_tabledescr ?= cl_abap_tabledescr=>create( p_line_type = lr_structdescr ).
*    CREATE DATA lr_table_old TYPE HANDLE lr_tabledescr.
*    ASSIGN lr_table_old->* TO <record_old_tab>.
*    IF infty NE /sew/cl_int_constants=>it0001.
    CALL FUNCTION 'HR_INFOTYPE_OPERATION'
      EXPORTING
        infty         = infty
*       recordnumber  = <it_aend>-seqnr
        number        = pernr
        subtype       = subty
        validityend   = endda
        validitybegin = begda
        record        = record
        operation     = operation
        nocommit      = simu
      IMPORTING
        return        = return.
*    ELSE.
*      me->maintain_pa0001(
*        EXPORTING
*         p0001     = CORRESPONDING #( record )
*          infty      = infty                 " Infotype
*          subty      = subty                 " Subtype
*          begda      = begda                 " Field of type DATS
*          endda      = endda                 " Field of type DATS
*          pernr      = pernr                 " Personnel Number
*          simu       = simu                 " Data element for domain BOOLE: TRUE (='X') and FALSE (=' ')
*        IMPORTING
*          return_tab = return_tab ).
*      ret = CORRESPONDING #( return ).
*      APPEND ret TO return_tab.
*    ENDIF.

  ENDMETHOD.


  METHOD maintain_relation.
*    CALL FUNCTION 'RHOM_ALL_BUFFER_INIT'.
*
*    CALL FUNCTION 'RHOM_MAINTAIN_RELATION_BUFF'
*      EXPORTING
*        act_fcode               = action
**        act_plvar               = CONV #( /sew/cl_int_constants=>plvar )
**       ACT_ISTAT               = '1'
*        vbegda                  = begda
*        vendda                  = endda
**       DEFIBEGDA               =
**       DEFIENDDA               =
*        vprozt                  = '100'
**       VPRIOX                  =
**       VSEQNR                  =
*        parent_object           = parent
**       ACT_INFTY               = /sew/cl_int_constants=>it1001
*        act_rsign               = rsign
*        act_relat               = relat
*        child_object            = child
**       MOVE_FLAG               =
**       OLD_PARENT_OBJECT       =
**       CUT_DATE                =
**       HR_ACTION_INFO          =
*        change_manager_relation = ' '
**     IMPORTING
**       VBEGDA                  =
**       VENDDA                  =
*      EXCEPTIONS
*        no_active_plvar         = 1
*        no_authority            = 2
*        write_error             = 3
*        OTHERS                  = 4.
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*      "Create Message for Error during maintain relation between S - O
*
*    ELSE.
*      "Success create relation
*      CALL FUNCTION 'RHOM_WRITE_BUFFER_TO_DB'.
*      COMMIT WORK AND WAIT.
*    ENDIF.
    DATA: lr_structdescr TYPE REF TO cl_abap_structdescr,
          hrp_old_it     TYPE REF TO data,
          plvar          TYPE plvar,
          begda_new      TYPE dats,
          is_manager     TYPE boole_d.

    FIELD-SYMBOLS:  <hrp_old>     TYPE any.
    is_manager = abap_false.
*    CALL FUNCTION 'RHOM_ALL_BUFFER_INIT'.
    CONCATENATE rsign relat INTO DATA(subty).
    IF simu = abap_true.
      DATA(begda_ext) = /sew/cl_int_utility=>get_external_date( date = begda ).
      DATA(endda_ext) = /sew/cl_int_utility=>get_external_date( date = endda ).
      CONCATENATE begda_ext '-' endda_ext INTO DATA(date) SEPARATED BY space.
      return = VALUE bapiret1( type = /sew/cl_int_constants=>success
                                 id = /sew/cl_int_constants=>msg_class_int
                                 number = /sew/cl_int_constants=>msg_no-m24
                                 message_v1 = parent
                                 message_v2 = child
                                 message_v3 = subty
                                 message_v4 = date
).

    ELSE.

      "Relation to child can be earliest with begda of child object
      IF me->otype = /sew/cl_int_constants=>orgunit.

        lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV #( 'P' && /sew/cl_int_constants=>it1000 ) ).
        CREATE DATA hrp_old_it TYPE HANDLE lr_structdescr.
        ASSIGN hrp_old_it->* TO <hrp_old>.

        me->read_hrpxxxx(
        EXPORTING
          begda = begda
          endda = endda
          infty = CONV #( /sew/cl_int_constants=>it1000 )
          langu = CONV #( spras )
          otype = me->otype
          sap_id = child-objid
          subty = ''
          plvar = CONV #( /sew/cl_int_constants=>plvar )
          IMPORTING
            hrp_old = <hrp_old>
            return = return ).

        IF <hrp_old> IS NOT INITIAL.
          ASSIGN COMPONENT /sew/cl_int_constants=>begda OF STRUCTURE <hrp_old> TO FIELD-SYMBOL(<begda_old>).
          IF <begda_old> IS NOT INITIAL.
            begda_new = <begda_old>.
          ENDIF.
        ENDIF.
      ENDIF.

      IF begda_new IS INITIAL.
        begda_new = begda.
      ENDIF.

      "For B012 manager relation we need to delimit old relation
      IF subty = /sew/cl_int_constants=>relations-manager.
        is_manager = abap_true.
        CLEAR: <hrp_old>.
        lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV #( 'P' && /sew/cl_int_constants=>it1001 ) ).
        CREATE DATA hrp_old_it TYPE HANDLE lr_structdescr.
        ASSIGN hrp_old_it->* TO <hrp_old>.

        DATA(delimit_date) = begda.
        delimit_date = begda - 1.
        me->read_hrpxxxx(
        EXPORTING
          begda = delimit_date
          endda = delimit_date
          infty = CONV #( /sew/cl_int_constants=>it1001 )
          langu = CONV #( spras )
          otype = me->otype
          sap_id = parent-objid
          subty = CONV #( subty )
          plvar = CONV #( /sew/cl_int_constants=>plvar )
          rsign = rsign
          relat = relat
          IMPORTING
            hrp_old = <hrp_old>
            return = return ).

        ASSIGN COMPONENT /sew/cl_int_constants=>sobid OF STRUCTURE <hrp_old> TO FIELD-SYMBOL(<sobid_old>).
        IF child-objid NE <sobid_old>.
          me->delimit_hrpxxxx(
          EXPORTING
            record       = <hrp_old>
            begda        = begda
            endda        = endda
            infty        = CONV #( /sew/cl_int_constants=>it1001 )
            delimit_date = delimit_date
            IMPORTING
              return     = return
                              ).
*        ret = CORRESPONDING #( return ).
*        APPEND ret TO return_tab.
          CLEAR: return.
        ENDIF.
      ENDIF.

      plvar = /sew/cl_int_constants=>plvar.
      CALL FUNCTION 'RHOM_MAINTAIN_RELATION_BUFF'
        EXPORTING
          act_fcode               = action
          act_plvar               = plvar
          act_istat               = '1'
          vbegda                  = begda_new
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
          change_manager_relation = is_manager
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


METHOD MAINTAIN_TEVEN_HC.
  DATA: teven          TYPE tim_tmw_teven_tab,
        teven_old      TYPE TABLE OF teven,
        teven_new      TYPE TABLE OF teven,
        teven_more     TYPE TABLE OF teven_more,
        teven_more_new TYPE TABLE OF teven_more,
        begda_tmp      TYPE begda.

  begda_tmp = begda - 1.

  "get all time events older than new hire date
  CALL FUNCTION 'HR_TMW_DB_READ_TEVENT'
    EXPORTING
      pernr    = pernr
      fromdate = cl_hcp_global_constants=>c_lowdate
      todate   = begda_tmp
    IMPORTING
      result   = teven.

  CHECK teven IS NOT INITIAL.

  "collect all entries to delete
  LOOP AT teven ASSIGNING FIELD-SYMBOL(<teven>).
    APPEND <teven>-teven      TO teven_old.
    CHECK <teven>-teven_more IS NOT INITIAL.
    APPEND <teven>-teven_more TO teven_more.
  ENDLOOP.

  "Cancel all old entries
  CALL FUNCTION 'HR_TMW_DB_UPDATE_TEVENT'
    TABLES
      del_teven      = teven_old
      ins_teven      = teven_new
      del_teven_more = teven_more
      ins_teven_more = teven_more_new
    EXCEPTIONS
      insert_failed  = 1
      update_failed  = 2
      OTHERS         = 3.

  IF sy-subrc <> 0.
    return = VALUE #( type       = /sew/cl_int_constants=>error
                      id         = /sew/cl_int_constants=>msg_class_int
                      number     = /sew/cl_int_constants=>msg_no-m32
                      message_v1 = /sew/cl_int_constants=>it2011
                      message_v2 = pernr
                      message_v3 = begda ).
  ENDIF.

  "in case simulation is active, perform a rollback
  IF simu EQ abap_true.
    ROLLBACK WORK.
  ENDIF.

ENDMETHOD.


  METHOD modify_paxxxx.

    DATA: lr_structdescr TYPE REF TO cl_abap_structdescr,
          record_old     TYPE REF TO data,
          lr_tabledescr  TYPE REF TO cl_abap_tabledescr,
          lr_table_old   TYPE REF TO data,
          lr_table_new   TYPE REF TO data,
          ret            LIKE LINE OF return_tab.

    FIELD-SYMBOLS: <record_new_tab> TYPE STANDARD TABLE,
                   <record_old>     TYPE any,
                   <record_new>     TYPE any,
                   <record_old_tab> TYPE STANDARD TABLE.
    ASSIGN record TO <record_new>.

    lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV #( 'P' && infty ) ).
    CREATE DATA record_old TYPE HANDLE lr_structdescr.
    ASSIGN record_old->* TO <record_old>.
    lr_tabledescr ?= cl_abap_tabledescr=>create( p_line_type = lr_structdescr ).
    CREATE DATA lr_table_old TYPE HANDLE lr_tabledescr.
*    CREATE DATA lr_table_new TYPE HANDLE lr_tabledescr.
    ASSIGN lr_table_old->* TO <record_old_tab>.
*    ASSIGN lr_table_new->* TO <record_new_tab>.

    me->read_paxxxx(
    EXPORTING
      begda = begda
      endda = endda
      infty = infty
      pernr = pernr
      subty = subty
      simu = simu
      IMPORTING
        return_tab = return_tab
        record_tab = <record_old_tab>
      ).

*    CALL FUNCTION 'HR_READ_INFOTYPE'
*      EXPORTING
**       TCLAS     = 'A'
*        pernr     = pernr
*        infty     = infty
*        begda     = begda
*        endda     = endda
**       SPRPS     = '*'
**       BYPASS_BUFFER = ' '
**       LEGACY_MODE   = ' '
**     IMPORTING
*      TABLES
*        infty_tab = <record_old_tab>
** EXCEPTIONS
**       INFTY_NOT_FOUND       = 1
**       INVALID_INPUT = 2
**       OTHERS    = 3
*      .

    LOOP AT <record_old_tab> ASSIGNING FIELD-SYMBOL(<fs_record_old>).
      CLEAR: ret, return.
      IF <fs_record_old> IS ASSIGNED.
        ASSIGN COMPONENT /sew/cl_int_constants=>endda OF STRUCTURE <fs_record_old> TO FIELD-SYMBOL(<fs_endda_old>).
        ASSIGN COMPONENT /sew/cl_int_constants=>begda OF STRUCTURE <fs_record_old> TO FIELD-SYMBOL(<fs_begda_old>).
*        IF <fs_endda_old> GT delimit_date.

        "Delete old record
        me->maintain_paxxxx(
        EXPORTING
          begda = <fs_begda_old>
          endda = <fs_endda_old>
          pernr = pernr
          infty = infty
          subty = subty
          record = <record_new> "<fs_record_old>
          operation = /sew/cl_int_constants=>infty_operation-pa_modify
          simu = simu
          IMPORTING
            return = return
          ).

*          <fs_endda_old> = delimit_date.

*          me->maintain_paxxxx(
*          EXPORTING
*            begda = <fs_begda_old>
*            endda = <fs_endda_old>
*            pernr = pernr
*            infty = infty
*            subty = subty
*            record = <record_new>
*            operation = /sew/cl_int_constants=>infty_operation-pa_insert
*            simu = simu
*            IMPORTING
*              return = return
*            ).


        IF return IS NOT INITIAL."-type = /sew/cl_int_constants=>error.
          MOVE-CORRESPONDING return TO ret.

        ELSE.
          ret = VALUE hrpad_return( type = /sew/cl_int_constants=>success
                   id = /sew/cl_int_constants=>msg_class_int
                   number = /sew/cl_int_constants=>msg_no-m17
                   message_v1 = pernr
                   message_v2 = infty
                   message_v3 = subty
                   message_v4 = delimit_date
                 ).
        ENDIF.
        APPEND ret TO return_tab.
*        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD perform_cancel_hire.
    DATA: prelp_tab      TYPE prelp_tab,
          prelp          TYPE prelp,
*          return_tab     TYPE hrpad_return_tab,
          bapipakey_tab  TYPE hrpad_bapipakey_tab,
          lr_structdescr TYPE REF TO cl_abap_structdescr,
          lr_tabledescr  TYPE REF TO cl_abap_tabledescr,
          lr_table_old   TYPE REF TO data,
          lr_table_new   TYPE REF TO data,
          p0000          TYPE p0000,
          p0001          TYPE p0001,
          it_aendup      TYPE /sew/int_it_aeup,
          record_old     TYPE REF TO data,
          ret            LIKE LINE OF return_tab,
          return         TYPE bapiret1.

    FIELD-SYMBOLS: <pnnnn>          TYPE any,
                   <record_new_tab> TYPE STANDARD TABLE,
                   <record_old>     TYPE any,
                   <record_new>     TYPE any,
                   <record_old_tab> TYPE STANDARD TABLE.

    ASSIGN record TO <record_new>.

    "Modify current action to action ZZ
    me->modify_paxxxx(
      EXPORTING
        begda = it_aend-begda
        endda = it_aend-endda
        pernr = it_aend-pernr
        infty = it_aend-infty
        subty = it_aend-subty
        record = <record_new>
        simu = simu
        IMPORTING
          return_tab = return_tab
      ).
    IF line_exists( return_tab[ type = 'E' ] ). "IF return_tab IS INITIAL.
      is_ok = abap_false.

    ELSE.

      "Update IT0001 with default position

      lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV #( 'P' && /sew/cl_int_constants=>it0001 ) ).
      CREATE DATA record_old TYPE HANDLE lr_structdescr.
      ASSIGN record_old->* TO <record_old>.
      lr_tabledescr ?= cl_abap_tabledescr=>create( p_line_type = lr_structdescr ).
      CREATE DATA lr_table_old TYPE HANDLE lr_tabledescr.
*    CREATE DATA lr_table_new TYPE HANDLE lr_tabledescr.
      ASSIGN lr_table_old->* TO <record_old_tab>.
*    ASSIGN lr_table_new->* TO <record_new_tab>.

      me->read_paxxxx(
      EXPORTING
        begda = it_aend-begda
        endda = it_aend-endda
        infty = CONV #( /sew/cl_int_constants=>it0001 )
        pernr = it_aend-pernr
        subty = it_aend-subty
        simu = simu
        IMPORTING
          return_tab = return_tab
          record_tab = <record_old_tab>
        ).

      LOOP AT <record_old_tab> ASSIGNING FIELD-SYMBOL(<fs_record_old>).
*        CLEAR: ret, return.
        IF <fs_record_old> IS ASSIGNED.
          ASSIGN COMPONENT /sew/cl_int_constants=>endda OF STRUCTURE <fs_record_old> TO FIELD-SYMBOL(<fs_endda_old>).
          ASSIGN COMPONENT /sew/cl_int_constants=>begda OF STRUCTURE <fs_record_old> TO FIELD-SYMBOL(<fs_begda_old>).
          ASSIGN COMPONENT /sew/cl_int_constants=>plans OF STRUCTURE <fs_record_old> TO FIELD-SYMBOL(<fs_pos_old>).
*        IF <fs_endda_old> GT delimit_date.
          <fs_pos_old> = /sew/cl_int_constants=>default_pos.
          "Delete old record
          me->maintain_paxxxx(
          EXPORTING
            begda = <fs_begda_old>
            endda = <fs_endda_old>
            pernr = it_aend-pernr
            infty = CONV #( /sew/cl_int_constants=>it0001 )
            subty = it_aend-subty
            record = <fs_record_old> "<fs_record_old>
            operation = /sew/cl_int_constants=>infty_operation-pa_modify
            simu = simu
            IMPORTING
              return = return
            ).

*          <fs_endda_old> = delimit_date.

*          me->maintain_paxxxx(
*          EXPORTING
*            begda = <fs_begda_old>
*            endda = <fs_endda_old>
*            pernr = pernr
*            infty = infty
*            subty = subty
*            record = <record_new>
*            operation = /sew/cl_int_constants=>infty_operation-pa_insert
*            simu = simu
*            IMPORTING
*              return = return
*            ).


          IF return IS NOT INITIAL."-type = /sew/cl_int_constants=>error.
            MOVE-CORRESPONDING return TO ret.

          ELSE.
            ret = VALUE hrpad_return( type = /sew/cl_int_constants=>success
                     id = /sew/cl_int_constants=>msg_class_int
                     number = /sew/cl_int_constants=>msg_no-m17
                     message_v1 = it_aend-pernr
                     message_v2 = it_aend-infty
                     message_v3 = it_aend-subty
                     message_v4 = <fs_begda_old>
                   ).
          ENDIF.
          APPEND ret TO return_tab.
*        ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

*    IF is_ok = abap_false.
*      DATA(error_handler) = NEW /sew/cl_int_error_handler( cloud_id = CONV #( it_aend-cloud_id ) aend_id = CONV #( it_aend-aend_id )
*                                                          int_run = me->int_run molga = me->molga ).

*      error_handler->add_message_pa( EXPORTING pernr = CONV #( it_aend-cloud_id ) int_run = me->int_run return_tab = return_tab begda = it_aend-begda endda = it_aend-endda ).
*      ELSE.
    "Success messages

*    ENDIF.
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
          bukrs          TYPE bukrs,
          it_aend_tmp    TYPE /sew/tt_it_aend,
          pos_id         TYPE hrobjid,
          it0000_begda   TYPE dats,
          it0000_endda   TYPE dats.
    FIELD-SYMBOLS: <pnnnn>        TYPE any.
    CLEAR: it_aend_tmp, pos_id, it0000_begda.

    IF it_aend_tab_rel IS INITIAL.
      it_aend_tmp = VALUE /sew/tt_it_aend( FOR ls_it_aend IN it_aend_tab WHERE ( int_run = me->int_run AND
                                                                                       cloud_id = it_aend-cloud_id ) ( ls_it_aend ) ).

    ELSE.
      it_aend_tmp = it_aend_tab_rel.
    ENDIF.


    "Not final yet
    LOOP AT it_aend_tmp INTO DATA(aend_tmp) WHERE active NE 'I'.
      CLEAR: prelp.
      IF aend_tmp-pernr IS NOT INITIAL.
        CLEAR: aend_tmp-pernr.
      ENDIF.
      MOVE-CORRESPONDING aend_tmp TO prelp.
*      prelp-endda = /sew/cl_int_constants=>highdate.
      IF aend_tmp-infty = '0000'.
        it0000_begda = aend_tmp-begda.
        it0000_endda = aend_tmp-endda.
        cl_hr_pnnnn_type_cast=>prelp_to_pnnnn( EXPORTING prelp = prelp
                                       IMPORTING pnnnn = p0000 ).
        IF aend_tmp-action IN /sew/cl_int_constants=>hire_range.
          DATA(begda) = aend_tmp-begda.
          ASSIGN COMPONENT /sew/cl_int_constants=>massn OF STRUCTURE p0000 TO FIELD-SYMBOL(<massn>).
          ASSIGN COMPONENT /sew/cl_int_constants=>massg OF STRUCTURE p0000 TO FIELD-SYMBOL(<massg>).
        ENDIF.
      ENDIF.
      IF aend_tmp-infty = '0001'.
        cl_hr_pnnnn_type_cast=>prelp_to_pnnnn( EXPORTING prelp = prelp
                               IMPORTING pnnnn = p0001 ).
        cl_hr_pnnnn_type_cast=>pnnnn_to_prelp( EXPORTING pnnnn = p0001
                                IMPORTING prelp = prelp ).
      ENDIF.
      IF aend_tmp-infty = '0002'.
        DATA p0002 TYPE p0002.
        cl_hr_pnnnn_type_cast=>prelp_to_pnnnn( EXPORTING prelp = prelp
                               IMPORTING pnnnn = p0002 ).
        cl_hr_pnnnn_type_cast=>pnnnn_to_prelp( EXPORTING pnnnn = p0002
                                IMPORTING prelp = prelp ).
      ENDIF.
      IF aend_tmp-infty = '0006'.
        DATA p0006 TYPE p0006.
        cl_hr_pnnnn_type_cast=>prelp_to_pnnnn( EXPORTING prelp = prelp
                               IMPORTING pnnnn = p0006 ).
        cl_hr_pnnnn_type_cast=>pnnnn_to_prelp( EXPORTING pnnnn = p0006
                                IMPORTING prelp = prelp ).
      ENDIF.
      IF aend_tmp-infty = '0016'.
        DATA p0016 TYPE p0016.
        cl_hr_pnnnn_type_cast=>prelp_to_pnnnn( EXPORTING prelp = prelp
                               IMPORTING pnnnn = p0016 ).
        cl_hr_pnnnn_type_cast=>pnnnn_to_prelp( EXPORTING pnnnn = p0016
                                IMPORTING prelp = prelp ).
      ENDIF.
      IF aend_tmp-infty = '0105'.
        DATA p0105 TYPE p0105.
        cl_hr_pnnnn_type_cast=>prelp_to_pnnnn( EXPORTING prelp = prelp
                               IMPORTING pnnnn = p0105 ).
        cl_hr_pnnnn_type_cast=>pnnnn_to_prelp( EXPORTING pnnnn = p0105
                                IMPORTING prelp = prelp ).
      ENDIF.
      APPEND prelp TO prelp_tab.
    ENDLOOP.
    DATA pernr_n TYPE pernr_d.

    IF <massn> IS ASSIGNED AND <massn> IS NOT INITIAL.
      TRY.
          CALL FUNCTION 'HR_PAD_HIRE_EMPLOYEE'
            EXPORTING
              employeenumber  = pernr_n
              hiringdate      = begda
              actiontype      = <massn> " muss noch angepasst werden!
              reasonforaction = <massg> " muss noch angepasst werden!
              pnnnn_tab       = prelp_tab
              nocommit        = 'X' "No commit, only write to buffer, database entries only written when all entries processed correctly
            IMPORTING
              return_tab      = return_tab
              bapipakey_tab   = bapipakey_tab
              is_ok           = is_ok.
        CATCH cx_hrpa_missing_infty_data.
      ENDTRY.
    ELSE.
      TRY.
          CALL FUNCTION 'HR_PAD_HIRE_EMPLOYEE'
            EXPORTING
              employeenumber = pernr_n
              hiringdate     = begda
              actiontype     = /sew/cl_int_constants=>hire
*             reasonforaction = <massg> " muss noch angepasst werden!
              pnnnn_tab      = prelp_tab
              nocommit       = 'X' "No commit, only write to buffer, database entries only written when all entries processed correctly
            IMPORTING
              return_tab     = return_tab
              bapipakey_tab  = bapipakey_tab
              is_ok          = is_ok.
        CATCH cx_hrpa_missing_infty_data.
      ENDTRY.
    ENDIF.

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
          it_aendup-legal_entity = bukrs. "it_aend-legal_entity.
          it_aendup-int_run = /sew/cl_int_utility=>create_guid( ).
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

  ENDMETHOD.


  METHOD perform_reverse_termination.

    DATA: tab_0001     TYPE STANDARD TABLE OF p0001,
          return_maint TYPE bapiret1,
          ret          LIKE LINE OF return_tab.
    FIELD-SYMBOLS: <record_new> TYPE any.
    ASSIGN record TO <record_new>.
    "Only perform reverse termination block if current record being processed is IT0000 and action not equal termination
    IF infty = /sew/cl_int_constants=>it0000 AND action NE /sew/cl_int_constants=>termination AND action NE /sew/cl_int_constants=>rehire.
      CLEAR: is_rev_term.
      "Check if reverse termination is necessary (if current action is a termination)
      me->check_reverse_termination(
      EXPORTING
        begda = begda
        endda = endda
        pernr = pernr
        simu = simu
        IMPORTING
          reverse_termination = is_rev_term
      ).

      "Check for reverse termination is true - continue with reverse termination logic
      IF is_rev_term = abap_true.
        "Delete termination action
        me->delete_paxxxx(
        EXPORTING
          record = record
          begda = endda
          endda = endda
          delimit_date = endda
          infty = infty
          subty = subty
          pernr = pernr
          simu = simu
          IMPORTING
            return_tab = return_tab
        ).

        READ TABLE return_tab INTO DATA(return) WITH KEY type = /sew/cl_int_constants=>error.
        IF return IS INITIAL.
*          me->maintain_paxxxx(
*          EXPORTING
*            begda = begda
*            endda = endda
*            pernr = pernr
*            infty = infty
*            subty = subty
*            record = <record_new> "<fs_record_old>
*            operation = /sew/cl_int_constants=>infty_operation-pa_delete
*            simu = simu
*            IMPORTING
*              return = return_maint
*            ).
*          IF return_maint IS NOT INITIAL."-type = /sew/cl_int_constants=>error.
*            MOVE-CORRESPONDING return_maint TO ret.
*            APPEND ret TO return_tab.
*          ENDIF.
*        me->read_paxxxx( EXPORTING begda = begda
*                                  endda = begda
*                                  infty = /sew/cl_int_constants=>it0001
*                                  pernr = pernr
*                                  simu  = simu
*                        IMPORTING return_tab = return_tab
*                                  record_tab = tab_0001 ).

        ENDIF.
      ENDIF.
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

*    IF infty = /sew/cl_int_constants=>it1000.
      CALL FUNCTION 'RH_READ_INFTY'
        EXPORTING
*         AUTHORITY            = 'DISP'
          with_stru_auth       = ' '
          plvar                = plvar
          otype                = otype
          objid                = sap_id
          infty                = infty
*         ISTAT                = ' '
*         EXTEND               = 'X'
          subty                = subty
          begda                = begda
          endda                = endda
*         CONDITION            = '00000'
*         INFTB                = '1'
*         SORT                 = 'X'
*         VIA_T777D            = ' '
        TABLES
          innnn                = <hrp_old_tab>
*         OBJECTS              =
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
        ELSEIF infty = /sew/cl_int_constants=>it1001.
          IF <hrp_old> IS ASSIGNED.
            UNASSIGN <hrp_old>.
          ENDIF.
          IF rsign IS NOT INITIAL AND relat IS NOT INITIAL.
*        READ TABLE <hrp_old_tab> ASSIGNING <hrp_old> WITH KEY langu = langu.
            LOOP AT <hrp_old_tab> ASSIGNING <hrp_old>.
              ASSIGN COMPONENT 'RELAT' OF STRUCTURE <hrp_old> TO FIELD-SYMBOL(<relat>).
              ASSIGN COMPONENT 'RSIGN' OF STRUCTURE <hrp_old> TO FIELD-SYMBOL(<rsign>).
              IF <relat> = relat AND <rsign> = rsign.
                MOVE-CORRESPONDING <hrp_old> TO hrp_old.
*              hrp_old = <hrp_old>.
                EXIT.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ELSE.
          IF <hrp_old> IS ASSIGNED.
            hrp_old = <hrp_old>.
          ENDIF.
        ENDIF.
      ENDIF.
*    ENDIF.
*    ENDIF.
  ENDMETHOD.


  METHOD read_paxxxx.

    DATA: lr_structdescr TYPE REF TO cl_abap_structdescr,
          record_old     TYPE REF TO data,
          lr_tabledescr  TYPE REF TO cl_abap_tabledescr,
          lr_table_old   TYPE REF TO data,
          lr_table       TYPE REF TO data,
          ret            LIKE LINE OF return_tab,
          return         TYPE bapiret1.

    FIELD-SYMBOLS: <record_new_tab> TYPE STANDARD TABLE,
                   <record_old>     TYPE any,
                   <record_new>     TYPE any,
                   <record_old_tab> TYPE STANDARD TABLE,
                   <record_tab>     TYPE STANDARD TABLE.
*    ASSIGN record TO <record_new>.

    lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV #( 'P' && infty ) ).
    CREATE DATA record_old TYPE HANDLE lr_structdescr.
    ASSIGN record_old->* TO <record_old>.
    lr_tabledescr ?= cl_abap_tabledescr=>create( p_line_type = lr_structdescr ).
    CREATE DATA lr_table_old TYPE HANDLE lr_tabledescr.
    CREATE DATA lr_table TYPE HANDLE lr_tabledescr.
    ASSIGN lr_table_old->* TO <record_old_tab>.
    ASSIGN lr_table->* TO <record_tab>.

    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
*       TCLAS           = 'A'
        pernr           = pernr
        infty           = infty
        begda           = begda
        endda           = endda
*       SPRPS           = '*'
*       BYPASS_BUFFER   = ' '
*       LEGACY_MODE     = ' '
*     IMPORTING
      TABLES
        infty_tab       = <record_old_tab>
      EXCEPTIONS
        infty_not_found = 1
        invalid_input   = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      return = /sew/cl_int_utility=>map_sy_msg( msgid = sy-msgid msgty = sy-msgty msgno = sy-msgno
                                        msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 msgv4 = sy-msgv4 ).
      ret = CORRESPONDING #( return ).
      APPEND ret TO return_tab.
    ENDIF.

    IF subty IS NOT INITIAL.
      LOOP AT <record_old_tab> ASSIGNING FIELD-SYMBOL(<fs_record_old>).
        CLEAR: ret.
        IF <fs_record_old> IS ASSIGNED.
          ASSIGN COMPONENT /sew/cl_int_constants=>subty OF STRUCTURE <fs_record_old> TO FIELD-SYMBOL(<fs_subty_old>).
          IF <fs_subty_old> = subty.
            APPEND <fs_record_old> TO record_tab.
          ELSE.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ELSE.
      record_tab = <record_old_tab>.
    ENDIF.
  ENDMETHOD.


  METHOD read_paxxxx_dcif.


    DATA: lr_structdescr         TYPE REF TO cl_abap_structdescr,
          record_old             TYPE REF TO data,
          lr_tabledescr          TYPE REF TO cl_abap_tabledescr,
          lr_table_old           TYPE REF TO data,
          lr_table               TYPE REF TO data,
          ret                    LIKE LINE OF return_tab,
          return                 TYPE bapiret1,
          lr_message_list        TYPE REF TO cl_hrpa_message_list,
          lr_masterdata_bl       TYPE REF TO if_hrpa_masterdata_bl,
          lr_infty_reader        TYPE REF TO if_hrpa_read_infotype,
          container_tab          TYPE hrpad_infty_container_tab,
          lv_is_ok               TYPE boole_d,
          lv_count               TYPE i,
          container              TYPE REF TO if_hrpa_infty_container,
          old_container          TYPE REF TO cl_hrpa_infotype_container,
          new_container          TYPE REF TO if_hrpa_infty_container,
          container_if           TYPE hrpad_infty_container_ref,
          infotype_ref           TYPE REF TO data,
          new_infotype_container TYPE REF TO cl_hrpa_infotype_container,
          lt_messages            TYPE hrpad_message_tab.

    FIELD-SYMBOLS:
                   <pskey>          TYPE pskey.
*    ASSIGN record TO <record_new>.
    TRY.

        CREATE OBJECT lr_message_list.

        CALL METHOD cl_hrpa_masterdata_factory=>get_business_logic
          IMPORTING
            business_logic = lr_masterdata_bl.

        TRY.
            CALL METHOD cl_hrpa_read_infotype=>get_instance
              IMPORTING
                infotype_reader = lr_infty_reader.
          CATCH cx_hrpa_violated_assertion .
        ENDTRY.
        CALL METHOD lr_masterdata_bl->read
          EXPORTING
            tclas           = 'A'
            infty           = iv_infty
            pernr           = iv_pernr
            begda           = iv_begda
            endda           = iv_begda
            no_auth_check   = space
            message_handler = lr_message_list
          IMPORTING
            container_tab   = container_tab
            is_ok           = lv_is_ok.

        IF lv_is_ok EQ abap_true.
          DESCRIBE TABLE container_tab LINES lv_count.

          IF lv_count GT 0.
            READ TABLE container_tab INTO container INDEX lv_count.
          ENDIF.
        ENDIF.

        IF lv_is_ok = abap_false.
          CALL METHOD lr_message_list->get_abend_list
            IMPORTING
              messages = lt_messages.

          IF lt_messages IS INITIAL.
            CALL METHOD lr_message_list->get_error_list
              IMPORTING
                messages = lt_messages.
          ENDIF.
          RETURN.
        ENDIF.
        IF container_tab IS NOT INITIAL.
          old_container ?= container.
          TRY.
              CALL METHOD old_container->if_hrpa_infty_container_data~primary_record_ref
                IMPORTING
                  pnnnn_ref = infotype_ref.
            CATCH cx_hrpa_violated_assertion .
          ENDTRY.

          READ TABLE container_tab INTO container_if  INDEX 1.
          new_infotype_container ?= container_if.
          new_infotype_container ?= new_infotype_container->modify_key( <pskey> ).
*          new_infotype_container ?= new_infotype_container->modify_primary_record( ls_pa0000 ).
          new_container ?= new_infotype_container.
          ENDIF.

        CATCH /iwbep/cx_mgw_busi_exception.
        CATCH /iwbep/cx_mgw_tech_exception.
      ENDTRY.
    ENDMETHOD.


  METHOD read_teven.

    DATA: ret    LIKE LINE OF return_tab,
          return TYPE bapiret1.

    CALL FUNCTION 'HR_EVENT_READ'
      EXPORTING
        pernr   = pernr
        begda   = begda
        endda   = endda
      TABLES
        mt      = record_tab
        mt_more = record_more_tab.

    IF sy-subrc <> 0.
      return = /sew/cl_int_utility=>map_sy_msg( msgid = sy-msgid msgty = sy-msgty msgno = sy-msgno
                                        msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 msgv4 = sy-msgv4 ).
      ret = CORRESPONDING #( return ).
      APPEND ret TO return_tab.
    ENDIF.

  ENDMETHOD.


  METHOD update_paxxxx_dcif.
*    DATA: lv_pernr                    TYPE p_pernr,
*          lv_begda                    TYPE begda,
*          lv_endda                    TYPE endda,
*          lc_hcm_globalhr_person      TYPE REF TO /sew/cl_hcm_globalhr_person,
*          lt_prelp                    TYPE hrpad_prelp_tab,
*          lr_infty_reader             TYPE REF TO if_hrpa_read_infotype,
*          lr_message_list             TYPE REF TO cl_hrpa_message_list,
*          lr_masterdata_bl            TYPE REF TO if_hrpa_masterdata_bl,
*          container                   TYPE REF TO if_hrpa_infty_container,
*          container_0000              TYPE REF TO if_hrpa_infty_container,
*          container_0001              TYPE REF TO if_hrpa_infty_container,
*          old_container               TYPE REF TO cl_hrpa_infotype_container,
*          old_container_0000          TYPE REF TO cl_hrpa_infotype_container,
*          old_container_0001          TYPE REF TO cl_hrpa_infotype_container,
*          new_container               TYPE REF TO if_hrpa_infty_container,
*          new_infotype_container      TYPE REF TO cl_hrpa_infotype_container,
*          new_container_0000          TYPE REF TO if_hrpa_infty_container,
*          new_infotype_container_0000 TYPE REF TO cl_hrpa_infotype_container,
*          infotype_ref                TYPE REF TO data,
*          infotype_ref_0000           TYPE REF TO data,
*          new_container_0001          TYPE REF TO if_hrpa_infty_container,
*          new_infotype_container_0001 TYPE REF TO cl_hrpa_infotype_container,
*          infotype_ref_0001           TYPE REF TO data,
*          lv_is_ok                    TYPE boole_d,
*          lv_dummy                    TYPE string,
*          ls_msg                      TYPE symsg,
*          lv_massg                    TYPE massg,
*          lv_massn                    TYPE massn,
*          lv_begend                   TYPE date,
*          lv_endbeg                   TYPE date,
*          entity_ref                  TYPE REF TO data,
*          data_ref                    TYPE REF TO data,
*          pnnnn_ref                   TYPE REF TO data,
*          container_tab               TYPE hrpad_infty_container_tab,
*          container_tab_0000          TYPE hrpad_infty_container_tab,
*          container_if                TYPE hrpad_infty_container_ref,
*          container_if_0000           TYPE hrpad_infty_container_ref,
*          container_tab_0001          TYPE hrpad_infty_container_tab,
*          container_if_0001           TYPE hrpad_infty_container_ref,
*          t777d                       TYPE t777d,
*          lv_has_error                TYPE boole_d,
*          lv_count                    TYPE i,
*          lv_offset                   TYPE i,
*          lv_infotype                 TYPE infty,
*          lv_entity                   TYPE char50,
*          lo_message_handler          TYPE REF TO if_hrpa_message_handler,
*          lt_messages                 TYPE hrpad_message_tab,
*          lv_message                  TYPE bapi_msg,
*          lv_mess                     TYPE string,
*          lo_hrpa_assertion           TYPE REF TO cx_hrpa_violated_assertion,
*          ls_message                  TYPE hrpad_message,
*          lw_msg1                     TYPE symsg,
*          s_wd_return                 TYPE bapiret2,
*          ls_update_mode              TYPE hrpad_update_mode,
*          lt_it0001                   TYPE TABLE OF p0001,
*          ls_it0001                   TYPE p0001,
*          ls_hrp1001                  TYPE hrp1001,
*          ls_hrp1000                  TYPE hrp1000.
*
*    FIELD-SYMBOLS: <pshdr>      TYPE pshdr,
*                   <pnnnn>      TYPE any,
*                   <ptnnnn>     TYPE ANY TABLE,
*                   <pskey>      TYPE pskey,
*                   <pskey_0000> TYPE pskey,
*                   <pskey_0001> TYPE pskey,
*                   <pxxxx>      TYPE any,
*                   <entity>     TYPE any,
*                   <data>       TYPE any,
*                   <struc>      TYPE any,
*                   <fs_pnnnn>   TYPE ANY TABLE,
*                   <fs_endda>   TYPE p0001-endda,
*                   <fs_begda>   TYPE p0001-begda,
*                   <persk_old>  TYPE persk,
*                   <persk_new>  TYPE persk,
*                   <kostl_old>  TYPE kostl,
*                   <kostl_new>  TYPE kostl,
*                   <persg>      TYPE persg,
*                   <p0000>      TYPE p0000,
*                   <p0001>      TYPE p0001,
*                   <begda>      TYPE begda,
*                   <endda>      TYPE endda,
*                   <plans>      TYPE plans.
*
*    TRY.
*        CREATE OBJECT lr_message_list.
*        CREATE OBJECT lo_hrpa_assertion.
*
*        CALL METHOD cl_hrpa_masterdata_factory=>get_business_logic
*          IMPORTING
*            business_logic = lr_masterdata_bl.
*
*
*        TRY.
*            CALL METHOD cl_hrpa_read_infotype=>get_instance
*              IMPORTING
*                infotype_reader = lr_infty_reader.
*            .
*          CATCH cx_hrpa_violated_assertion .
*        ENDTRY.
*
** Get infotype
*        CREATE DATA entity_ref TYPE (iv_entity).
*
*        ASSIGN entity_ref->* TO <entity>.
*        CLEAR lv_entity.
*        MOVE iv_entity TO lv_entity.
*        lv_offset = strlen( lv_entity ).
*        lv_offset = lv_offset - 4.
*        lv_entity = lv_entity+lv_offset.
*        CLEAR lv_infotype.
*        MOVE lv_entity TO lv_infotype.
*        CONCATENATE 'p' lv_entity INTO lv_entity.
*        CREATE DATA pnnnn_ref TYPE TABLE OF (lv_entity).
*        ASSIGN pnnnn_ref->* TO <ptnnnn>.
*        ASSIGN pnnnn_ref->* TO <struc>.
*        ASSIGN iv_data TO <struc>.
*        CHECK lr_masterdata_bl IS BOUND.
*
*        IF lv_infotype = '0002'.
*          me->g_startdate_0002 = iv_begda.
*        ENDIF.
*
*        CALL METHOD lr_masterdata_bl->read
*          EXPORTING
*            tclas           = 'A'
*            infty           = lv_infotype
*            pernr           = iv_pernr
*            begda           = iv_begda
*            endda           = iv_endda
*            no_auth_check   = space
*            message_handler = lr_message_list
*          IMPORTING
*            container_tab   = container_tab
*            is_ok           = lv_is_ok.
*
*        IF lv_is_ok EQ abap_true.
*          DESCRIBE TABLE container_tab LINES lv_count.
*
*          IF lv_count GT 0.
*            READ TABLE container_tab INTO container INDEX lv_count.
*          ENDIF.
*        ENDIF.
*
*        IF lv_is_ok = abap_false.
*          CALL METHOD lr_message_list->get_abend_list
*            IMPORTING
*              messages = lt_messages.
*
*          IF lt_messages IS INITIAL.
*            CALL METHOD lr_message_list->get_error_list
*              IMPORTING
*                messages = lt_messages.
*          ENDIF.
*          RETURN.
*        ENDIF.
*
*        IF container IS NOT INITIAL.
*          old_container ?= container.
*
*          TRY.
*              CALL METHOD old_container->if_hrpa_infty_container_data~primary_record_ref
*                IMPORTING
*                  pnnnn_ref = infotype_ref.
*            CATCH cx_hrpa_violated_assertion .
*          ENDTRY.
*
*          ASSIGN infotype_ref->* TO <pxxxx>.
*          t777d = cl_hr_t777d=>read( infty = lv_infotype ).
*          CREATE DATA infotype_ref TYPE (t777d-ppnnn).
*          ASSIGN infotype_ref->* TO <pnnnn> CASTING LIKE <pxxxx>.
*          <pnnnn> = <pxxxx>.
*          ASSIGN <pnnnn> TO <pshdr> CASTING.
*          <pshdr>-infty = lv_infotype.
*          ASSIGN infotype_ref->* TO <data> CASTING LIKE <pxxxx>.
*          MOVE-CORRESPONDING <struc> TO <data>.
*
*        ELSE.
*          FIELD-SYMBOLS  <field> TYPE any.
*          CREATE DATA infotype_ref TYPE (lv_entity).
*          ASSIGN infotype_ref->* TO <data>.
*          MOVE-CORRESPONDING <struc> TO <data>.
*
*          ASSIGN COMPONENT 'INFTY' OF STRUCTURE <data> TO <field>.
*          <field> = lv_infotype.
*        ENDIF.
*
** save kostl for
*        ASSIGN COMPONENT 'KOSTL' OF STRUCTURE <pxxxx> TO <kostl_old>.
*        ASSIGN COMPONENT 'KOSTL' OF STRUCTURE <data> TO <kostl_new>.
**--------------------------------------------------------------------*
** DE5SHAUK, 04.05.2018
** In case of Change of Position, the old position should be delimited.
** Begin of change 1: Save Position for later check.
**--------------------------------------------------------------------*
*        ASSIGN COMPONENT 'PLANS' OF STRUCTURE <pxxxx> TO FIELD-SYMBOL(<plans_old>).
*        ASSIGN COMPONENT 'PLANS' OF STRUCTURE <data> TO FIELD-SYMBOL(<pans_new>).
**--------------------------------------------------------------------*
** End of change 1: Save Position for later check.
**--------------------------------------------------------------------*
*
***********************************************************************
** Change persk
*        ASSIGN COMPONENT 'PERSK' OF STRUCTURE <pxxxx> TO <persk_old>.
*        ASSIGN COMPONENT 'PERSK' OF STRUCTURE <data> TO <persk_new>.
*
*        IF <persk_old> IS  ASSIGNED AND <persk_new> IS ASSIGNED.
*          IF <persk_old> NE <persk_new>.
*            lv_massn = '02'.
*            lv_massg = <persk_new>.
** write IT0000
*            CALL METHOD lr_masterdata_bl->read
*              EXPORTING
*                tclas           = 'A'
*                infty           = '0000'
*                pernr           = iv_pernr
*                begda           = iv_begda
*                endda           = iv_endda
*                no_auth_check   = space
*                message_handler = lr_message_list
*              IMPORTING
*                container_tab   = container_tab_0000
*                is_ok           = lv_is_ok.
*
*            IF lv_is_ok EQ abap_true.
*              DESCRIBE TABLE container_tab_0000 LINES lv_count.
*
*              IF lv_count GT 0.
*                READ TABLE container_tab_0000 INTO container_0000 INDEX lv_count.
*              ENDIF.
*            ENDIF.
*            old_container_0000 ?= container_0000.
*            TRY.
*                CALL METHOD old_container_0000->if_hrpa_infty_container_data~primary_record_ref
*                  IMPORTING
*                    pnnnn_ref = infotype_ref_0000.
*              CATCH cx_hrpa_violated_assertion .
*            ENDTRY.
*            ASSIGN infotype_ref_0000->* TO <p0000>.
*            ASSIGN COMPONENT 'PSKEY' OF STRUCTURE <p0000> TO <pskey_0000>.
*
*            <p0000>-massn = '02'.
*            <p0000>-begda = iv_begda.
*            <p0000>-endda = iv_endda.
*
*            MOVE-CORRESPONDING <p0000> TO <pskey_0000>.
*
*            new_infotype_container_0000 ?= container_0000.
*            new_infotype_container_0000 ?= new_infotype_container_0000->modify_key( <pskey_0000> ).
*
*            new_infotype_container_0000 ?= new_infotype_container_0000->modify_primary_record( <p0000> ).
*            new_container_0000 ?= new_infotype_container_0000.
*
*            CALL METHOD lr_masterdata_bl->insert
*              EXPORTING
**               old_container   = old_container
*                massn           = lv_massn
*                massg           = lv_massg
*                update_mode     = ls_update_mode
*                message_handler = lr_message_list
*                no_auth_check   = space
*              IMPORTING
*                is_ok           = lv_is_ok
*              CHANGING
*                container       = new_container_0000.
*
*          ENDIF.
*        ENDIF.
*
***********************************************************************
*
*        ASSIGN COMPONENT 'PSKEY' OF STRUCTURE <data> TO <pskey>.
*        ASSIGN COMPONENT 'PERSG' OF STRUCTURE <data> TO <persg>.
*
*        lv_massn = '02'.
*
*        CALL METHOD lr_masterdata_bl->get_infty_container
*          EXPORTING
*            tclas           = 'A'
*            pskey           = <pskey>
*            no_auth_check   = abap_true
*            message_handler = lr_message_list
*          IMPORTING
*            container       = container_if.
*
*        new_infotype_container ?= container_if.
*        new_infotype_container ?= new_infotype_container->modify_key( <pskey> ).
*
*        new_infotype_container ?= new_infotype_container->modify_primary_record( <data> ).
*        new_container ?= new_infotype_container.
*
*        ls_update_mode-no_retroactivity = 'X'.
*
*        IF NOT lv_infotype = '0003' OR NOT lv_infotype = '0709'.
*
**--------------------------------------------------------------------* + fgawlik 12.04.2017
*          UNASSIGN: <begda>, <endda>.
*
*          CALL METHOD lr_masterdata_bl->read
*            EXPORTING
*              tclas           = 'A'
*              infty           = '0001'
*              pernr           = iv_pernr
*              begda           = '19000101'
*              endda           = '99991231'
*              no_auth_check   = space
*              message_handler = lr_message_list
*            IMPORTING
*              container_tab   = container_tab_0001
*              is_ok           = lv_is_ok.
*
*          IF lv_is_ok EQ abap_true.
*            CLEAR lv_count.
*            DESCRIBE TABLE container_tab_0001 LINES lv_count.
*            IF lv_count = 2. " Prüfung ob zwei Datensätze vorhanden sind, erst dann kann es sein das ein Fehler vorhanden ist
*              READ TABLE container_tab_0001 INTO container_0001 INDEX 1.
*              old_container_0001 ?= container_0001.
*              TRY.
*                  CALL METHOD old_container_0001->if_hrpa_infty_container_data~primary_record_ref
*                    IMPORTING
*                      pnnnn_ref = infotype_ref_0001.
*                CATCH cx_hrpa_violated_assertion .
*              ENDTRY.
*
*              ASSIGN infotype_ref_0001->* TO <p0001>.
*              ASSIGN COMPONENT 'BEGDA' OF STRUCTURE <p0001> TO <begda>.
*              ASSIGN COMPONENT 'ENDDA' OF STRUCTURE <p0001> TO <endda>.
*
** zu löschende Planstelle wegschreiben
*
*              UNASSIGN <plans>.
*              ASSIGN COMPONENT 'PLANS' OF STRUCTURE <p0001> TO <plans>.
*
*            ELSE. " falls nur ein Datensatz vorhanden ist, darf keine aktion stattfinden.
*
*            ENDIF.
*          ENDIF.
*
*          IF  <begda> IS ASSIGNED AND <endda> IS ASSIGNED.
*            IF <begda> = <endda>.
*              READ TABLE container_tab_0001 INTO container_0001 INDEX lv_count.
*              old_container_0001 ?= container_0001.
*              TRY.
*                  CALL METHOD old_container_0001->if_hrpa_infty_container_data~primary_record_ref
*                    IMPORTING
*                      pnnnn_ref = infotype_ref_0001.
*                CATCH cx_hrpa_violated_assertion .
*              ENDTRY.
*              ASSIGN infotype_ref_0001->* TO <p0001>.
*              ASSIGN COMPONENT 'PSKEY' OF STRUCTURE <p0001> TO <pskey_0001>.
*
*              <p0001>-begda = <begda>.
*
*              MOVE-CORRESPONDING <p0001> TO <pskey_0001>.
*
*              new_infotype_container_0001 ?= container_0001.
*              new_infotype_container_0001 ?= new_infotype_container_0001->modify_key( <pskey_0001> ).
*
*              new_infotype_container_0001 ?= new_infotype_container_0001->modify_primary_record( <p0001> ).
*              new_container_0001 ?= new_infotype_container_0001.
*
*              CALL METHOD lr_masterdata_bl->modify
*                EXPORTING
*                  old_container   = old_container_0001
*                  massn           = lv_massn
*                  massg           = lv_massg
*                  update_mode     = ls_update_mode
*                  message_handler = lr_message_list
*                  no_auth_check   = space
*                IMPORTING
*                  is_ok           = lv_is_ok
*                CHANGING
*                  container       = new_container_0001.
*
*              IF lv_is_ok IS NOT INITIAL AND <plans> IS ASSIGNED.
*
*                DATA           lv_user        TYPE sy-uname.
*                CALL FUNCTION 'IF_HR_ENQUEUE_OBJECT'
*                  EXPORTING
*                    plvar            = '01'
*                    otype            = 'S'
*                    objid            = <plans>
**                   ENQUEUE_ONCE     = ' '
*                  IMPORTING
*                    lock_user        = lv_user
*                  EXCEPTIONS
*                    enqueue_failed   = 1
*                    objid_is_initial = 2
*                    illegal_otype    = 3
*                    internal_error   = 4
*                    OTHERS           = 5.
*
*                IF sy-subrc <> 0.
*                  CONCATENATE 'Position is locked by User:' lv_user INTO ev_message.
*                  EXIT.
*                ENDIF.
*
*                CALL FUNCTION 'RHOM_DELETE_RELATIONS'
*                  EXPORTING
**                   PLVAR                  =
*                    otype                  = 'S'
*                    objid                  = <plans>
**                   delete_from_date       = <p0001>-begda
*                    delete_complete        = 'X'
*                  EXCEPTIONS
*                    no_active_plvar        = 1
*                    enqueue_error          = 2
*                    delete_error           = 3
*                    wrong_delete_from_date = 4
*                    OTHERS                 = 5.
*
*                IF sy-subrc = 0.
*
*                  CALL FUNCTION 'RHOM_DELETE_OBJECT'
*                    EXPORTING
*                      plvar                  = '01'
*                      otype                  = 'S'
*                      objid                  = <plans>
**                     delete_from_date       = iv_cut_date
*                      delete_complete        = 'X'
*                    EXCEPTIONS
*                      error_during_delete    = 1
*                      enqueue_error          = 2
*                      wrong_delete_from_date = 3
*                      otype_not_supported    = 4
*                      OTHERS                 = 5.
**
*                  IF sy-subrc <> 0.
*                    ev_message = 'Error during delete assignment, pleace contact your system administrator!'.
*                  ENDIF.
*                ENDIF.
*              ENDIF.
*            ELSE.
*
*              TRY.
*                  CALL METHOD lr_masterdata_bl->insert
*                    EXPORTING
**                     old_container   = old_container
*                      massn           = lv_massn
*                      massg           = lv_massg
*                      update_mode     = ls_update_mode
*                      message_handler = lr_message_list
*                      no_auth_check   = space
*                    IMPORTING
*                      is_ok           = lv_is_ok
*                    CHANGING
*                      container       = new_container.
*                CATCH cx_hrpa_violated_assertion .
*              ENDTRY.
*
*              IF  lv_is_ok = abap_false.
*                CALL METHOD lr_masterdata_bl->modify
*                  EXPORTING
*                    old_container   = new_container
*                    massn           = lv_massn
*                    massg           = lv_massg
*                    update_mode     = ls_update_mode
*                    message_handler = lr_message_list
*                    no_auth_check   = space
*                  IMPORTING
*                    is_ok           = lv_is_ok
*                  CHANGING
*                    container       = new_container.
*              ENDIF.
*            ENDIF.
*          ELSE.
**--------------------------------------------------------------------* + fgawlik 12.04.2017
*            TRY.
*                CALL METHOD lr_masterdata_bl->insert
*                  EXPORTING
**                   old_container   = old_container
*                    massn           = lv_massn
*                    massg           = lv_massg
*                    update_mode     = ls_update_mode
*                    message_handler = lr_message_list
*                    no_auth_check   = space
*                  IMPORTING
*                    is_ok           = lv_is_ok
*                  CHANGING
*                    container       = new_container.
*              CATCH cx_hrpa_violated_assertion .
*            ENDTRY.
*
*            IF  lv_is_ok = abap_false.
*              CALL METHOD lr_masterdata_bl->modify
*                EXPORTING
*                  old_container   = new_container
*                  massn           = lv_massn
*                  massg           = lv_massg
*                  update_mode     = ls_update_mode
*                  message_handler = lr_message_list
*                  no_auth_check   = space
*                IMPORTING
*                  is_ok           = lv_is_ok
*                CHANGING
*                  container       = new_container.
*            ENDIF.
*          ENDIF.
*** change costcenter
*          IF <kostl_old> IS ASSIGNED AND <kostl_new> IS ASSIGNED.
*            IF <kostl_old> NE <kostl_new>.
*              DATA: ls_parent_orgunit TYPE objec,
*                    ls_child_orgunit  TYPE objec,
*                    lv_kokrs          TYPE kokrs,
*                    lt_existence      TYPE TABLE OF hroexist,
*                    lv_objid          TYPE plog-objid,
*                    lv_objid_temp     TYPE wplog-objid.
*
*              UNASSIGN <plans>.
*              ASSIGN COMPONENT 'PLANS' OF STRUCTURE <data> TO <plans>.
*
*              CLEAR: lv_objid_temp.
*
*              IF <plans> IS ASSIGNED.
*                lv_objid_temp = <plans>.
*
*                CALL FUNCTION 'RH_GET_CONTROLLING_AREA'
*                  EXPORTING
*                    plvar         = '01'
*                    otype         = 'S '
*                    objid         = lv_objid_temp
*                    find_kokrs_up = 'X'
*                  IMPORTING
*                    kokrs         = lv_kokrs
*                  EXCEPTIONS
*                    not_found     = 1
*                    OTHERS        = 2.
*
*                IF sy-subrc <> 0.
** Implement suitable error handling here
*                ENDIF.
*
*                CONCATENATE <kostl_new> lv_kokrs INTO ls_child_orgunit-realo.
*                CALL FUNCTION 'RH_READ_OBJECT'
*                  EXPORTING
*                    begda     = iv_begda
*                    endda     = iv_endda
*                    realo     = ls_child_orgunit-realo
*                    otype     = 'K '
*                    plvar     = '01'
*                  IMPORTING
*                    ostat     = ls_child_orgunit-istat
*                    obeg      = ls_child_orgunit-begda
*                    oend      = ls_child_orgunit-endda
*                    short     = ls_child_orgunit-short
*                    stext     = ls_child_orgunit-stext
*                  EXCEPTIONS
*                    not_found = 1.
*
*                ls_child_orgunit-otype = 'K '.
*                ls_child_orgunit-short = <kostl_new>.
*                ls_child_orgunit-stext = <kostl_new>.
*
*
*                lv_objid = <plans>.
*                CALL FUNCTION 'RH_READ_OBJECT'
*                  EXPORTING
*                    plvar           = '01'
*                    otype           = 'S '
*                    objid           = lv_objid
*                    begda           = lv_begda
*                    endda           = lv_endda
*                    check_stru_auth = 'X'
*                  IMPORTING
*                    obeg            = ls_parent_orgunit-begda
*                    oend            = ls_parent_orgunit-endda
*                    histo           = ls_parent_orgunit-histo
*                    short           = ls_parent_orgunit-short
*                    stext           = ls_parent_orgunit-stext
*                    tistat          = ls_parent_orgunit-istat
*                  TABLES
*                    existence       = lt_existence
*                  EXCEPTIONS
*                    not_found       = 1
*                    OTHERS          = 2.
*
*                ls_parent_orgunit-plvar = '01'.
*                ls_parent_orgunit-otype = 'S '.
*                ls_parent_orgunit-objid = <plans>.
*                ls_parent_orgunit-realo = <plans>.
*
**** Maintain Buffer for child relation
*                CALL FUNCTION 'RHOM_MAINTAIN_RELATION_BUFF'
*                  EXPORTING
*                    act_fcode       = 'INSE'
*                    vbegda          = iv_begda
*                    vendda          = iv_endda
*                    parent_object   = ls_parent_orgunit
*                    act_infty       = '1001'
*                    act_rsign       = 'A'
*                    act_relat       = '011'
*                    child_object    = ls_child_orgunit
*                  EXCEPTIONS
*                    no_active_plvar = 1
*                    no_authority    = 2
*                    write_error     = 3
*                    OTHERS          = 4.
*
*                CASE sy-subrc.
*                  WHEN 1.
*                    ev_message = 'No active planning version!'.
*                  WHEN 2.
*                    ev_message = 'No authority!'.
*                  WHEN 3.
*                    ev_message = 'Write error while updating infotype'.
*                  WHEN 4.
*                    ev_message = 'Internal error!'.
*                ENDCASE.
*              ENDIF.
*            ENDIF.
*          ENDIF.
*
*          IF lr_message_list->has_error( ) = abap_true.
*            TRY.
*                CALL METHOD lr_message_list->get_error_list
*                  IMPORTING
*                    messages = lt_messages.
*
*                IF lt_messages IS NOT INITIAL.
*                  LOOP AT lt_messages INTO ls_message.
*                    MOVE-CORRESPONDING ls_message TO lw_msg1.
*                    CALL FUNCTION 'BALW_BAPIRETURN_GET2'
*                      EXPORTING
*                        type   = lw_msg1-msgty
*                        cl     = lw_msg1-msgid
*                        number = lw_msg1-msgno
*                        par1   = lw_msg1-msgv1
*                        par2   = lw_msg1-msgv2
*                        par3   = lw_msg1-msgv3
*                        par4   = lw_msg1-msgv4
*                      IMPORTING
*                        return = s_wd_return.
*
*                    ev_message = s_wd_return-message.
*
*                  ENDLOOP.
*                ENDIF.
*              CATCH cx_hrpa_violated_assertion .
*            ENDTRY.
*          ENDIF.
**--------------------------------------------------------------------*
** DE5SHAUK, 04.05.2018
** In case of Change of Position, the old position should be delimited.
** Begin of change 1: Delimit Position and Relations.
**--------------------------------------------------------------------*
**         Read Position
*          IF ( <plans_old> IS ASSIGNED AND <plans> IS ASSIGNED ) AND <plans> NE <plans_old>.
*            SELECT * FROM hrp1000
*              INTO TABLE @DATA(t1000)
*              WHERE otype = 'S'
*              AND objid = @<plans_old>.
*            SORT t1000 BY begda ASCENDING.
*            READ TABLE t1000 INTO DATA(s1000) INDEX 1.
**         Delimit Position
*            IF s1000-begda = iv_begda.
*              CALL FUNCTION 'RHOM_DELETE_OBJECT'
*                EXPORTING
*                  plvar                  = '01'
*                  otype                  = s1000-otype
*                  objid                  = s1000-objid
*                  delete_from_date       = iv_begda
*                  delete_complete        = 'X'
*                EXCEPTIONS
*                  error_during_delete    = 1
*                  enqueue_error          = 2
*                  wrong_delete_from_date = 3
*                  otype_not_supported    = 4
*                  OTHERS                 = 5.
*              IF sy-subrc <> 0.
*                ev_message = 'Error during delete assignment, pleace contact your system administrator!'.
*              ENDIF.
*            ELSE.
*              CALL FUNCTION 'RHOM_DELETE_OBJECT'
*                EXPORTING
*                  plvar                  = '01'
*                  otype                  = s1000-otype
*                  objid                  = s1000-objid
*                  delete_from_date       = iv_begda
*                  delete_complete        = ''
*                EXCEPTIONS
*                  error_during_delete    = 1
*                  enqueue_error          = 2
*                  wrong_delete_from_date = 3
*                  otype_not_supported    = 4
*                  OTHERS                 = 5.
*              IF sy-subrc <> 0.
*                ev_message = 'Error during delete assignment, pleace contact your system administrator!'.
*              ENDIF.
*            ENDIF.
*          ENDIF.
**--------------------------------------------------------------------*
** End of change 1: Delimit Position and Relations.
**--------------------------------------------------------------------*
*        ENDIF.
**        ENDIF. "DE5RETST, 20160811 (MHP)
*
*
*
*      CATCH /iwbep/cx_mgw_busi_exception.
*      CATCH /iwbep/cx_mgw_tech_exception.
*    ENDTRY.
*
*    er_message_list = lr_message_list.
    DATA: it                    TYPE REF TO data,
          lr_message_list       TYPE REF TO cl_hrpa_message_list,
          lr_masterdata_bl      TYPE REF TO if_hrpa_masterdata_bl,
          lr_container          TYPE REF TO if_hrpa_infty_container,
          old_container         TYPE REF TO if_hrpa_infty_container,
          new_container         TYPE REF TO if_hrpa_infty_container,
          lr_container_data     TYPE REF TO if_hrpa_infty_container_data,
          lc_infotype_container TYPE REF TO cl_hrpa_infotype_container,
          lc_message_handler    TYPE REF TO cl_hrpay00_message_handler,
          ls_pskey              TYPE pskey,
          lv_ok                 TYPE xfeld,
          ls_update_mode        TYPE hrpad_update_mode,
          ls_message            LIKE LINE OF messages,
          lt_messages           TYPE hrpad_message_tab,
          lv_message            TYPE char50,
          t777d                 TYPE t777d,
          infotype_ref          TYPE REF TO data
          .

    FIELD-SYMBOLS: <pshdr>    TYPE pshdr,
                   <pnnnn>    TYPE any,
                   <ptnnnn>   TYPE ANY TABLE,
                   <pskey>    TYPE pskey,
                   <pxxxx>    TYPE any,
                   <data>     TYPE any,
                   <struc>    TYPE any,
                   <fs_pnnnn> TYPE any,
                   <fs_begda> TYPE p0001-begda,
                   <fs_endda> TYPE p0001-endda,
                   <pernr>    TYPE pernr_d.

    CONCATENATE 'P' infty INTO DATA(it_name).
    CREATE DATA it TYPE (it_name).
    ASSIGN it->* TO <struc>.
    ASSIGN data TO <struc>.
    ASSIGN COMPONENT 'PERNR' OF STRUCTURE <struc> TO <pernr>.
*    <pernr> = '200673'.

*    t777d = cl_hr_t777d=>read( infty = infty ).
*    CREATE DATA infotype_ref TYPE (t777d-ppnnn).
*    ASSIGN infotype_ref->* TO <pxxxx>.
*
*    ASSIGN infotype_ref->* TO <pnnnn> CASTING LIKE <pxxxx>.
*    <pnnnn> = <pxxxx>.
*    ASSIGN <pnnnn> TO <pshdr> CASTING.
*    <pshdr>-infty = infty.
*
*    ASSIGN infotype_ref->* TO <struc> CASTING LIKE <pxxxx>.
    ASSIGN COMPONENT 'PSKEY' OF STRUCTURE <struc> TO <pskey>.
*    ASSIGN COMPONENT 'BEGDA' OF STRUCTURE <struc> TO <fs_begda>.
*    ASSIGN COMPONENT 'ENDDA' OF STRUCTURE <struc> TO <fs_endda>.

* Create
    TRY.
        CLEAR ls_pskey.

        CREATE OBJECT lr_message_list.

*        MOVE <struc> TO ls_pskey.

        CALL METHOD cl_hrpa_masterdata_factory=>get_business_logic
          IMPORTING
            business_logic = lr_masterdata_bl.

        CALL METHOD lr_masterdata_bl->get_infty_container
          EXPORTING
            tclas           = 'A'
            pskey           = <pskey>
            no_auth_check   = space
            message_handler = lr_message_list
          IMPORTING
            container       = lr_container
            is_ok           = is_ok.

        lc_infotype_container ?= lr_container.
        lc_infotype_container ?= lc_infotype_container->modify_key( <pskey> ).
        lc_infotype_container ?= lc_infotype_container->modify_primary_record( <struc> ).
        new_container         ?= lc_infotype_container.

      CATCH cx_root.

        ls_message-msgty = 'E'.
        ls_message-msgid = 'ZHR_PA_INT'.
        ls_message-msgno = 001. "Success
        APPEND ls_message TO messages.
        CLEAR: ls_message, lv_message.

    ENDTRY.

    IF is_ok IS NOT INITIAL.
*      ls_update_mode-no_retroactivity = 'X'.
      CALL METHOD lr_masterdata_bl->insert
        EXPORTING
*         old_container   = old_container
          no_auth_check   = space
          update_mode     = ls_update_mode
          message_handler = lr_message_list
        IMPORTING
          is_ok           = is_ok
        CHANGING
          container       = new_container.
    ENDIF.

    lr_message_list->get_message_list(
    IMPORTING
      messages = lt_messages    " HR Stammdaten: Meldungsliste
  ).
    messages = CORRESPONDING #( BASE ( messages ) lt_messages ).
    IF lr_message_list->has_error( ).
      EXIT.
    ELSE.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
