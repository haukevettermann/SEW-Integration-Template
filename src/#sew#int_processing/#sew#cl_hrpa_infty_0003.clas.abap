class /SEW/CL_HRPA_INFTY_0003 definition
  public
  inheriting from CL_HRPA_INFTY_NNNN
  final
  create public .

*"* public components of class /SEW/CL_HRPA_INFTY_0003
*"* do not include other source files here!!!
public section.
  type-pools PAR .

  class-methods SPECIAL_MODE_SET
    raising
      CX_HRPA_VIOLATED_ASSERTION .
  class-methods SPECIAL_MODE_RESET
    raising
      CX_HRPA_VIOLATED_ASSERTION .
  class-methods SPECIAL_MODE_GET
    returning
      value(EV_SPECIAL_MODE) type BOOLE_D .
protected section.
*"* protected components of class /SEW/CL_HRPA_INFTY_0003
*"* do not include other source files here!!!

  methods SPECIFIC_COMPUTATIONS
    importing
      !TCLAS type TCLAS
      !MESSAGE_HANDLER type ref to IF_HRPA_MESSAGE_HANDLER
    exporting
      !IS_OK type BOOLE_D
    changing
      !P0003 type P0003
    raising
      CX_HRPA_VIOLATED_ASSERTION .
  methods PAYROLL_LOCK
    importing
      !TCLAS type TCLAS
      !MOLGA type MOLGA
      !PERNR type P_PERNR
      !ABRSP type ABRSP
      !MESSAGE_HANDLER type ref to IF_HRPA_MESSAGE_HANDLER
    exporting
      !IS_OK type BOOLE_D
    raising
      CX_HRPA_VIOLATED_ASSERTION .
  methods CHECK_ABWD
    importing
      !TCLAS type TCLAS
      !PERNR type P_PERNR
      !ABWD1 type ABWD1
      !ABWD2 type ABWD2
      !MESSAGE_HANDLER type ref to IF_HRPA_MESSAGE_HANDLER
    exporting
      !IS_OK type BOOLE_D
    raising
      CX_HRPA_VIOLATED_ASSERTION .
  methods CHECK_PRDAT
    importing
      !TCLAS type TCLAS
      !P0003 type P0003
      !MESSAGE_HANDLER type ref to IF_HRPA_MESSAGE_HANDLER
    exporting
      !IS_OK type BOOLE_D
    raising
      CX_HRPA_VIOLATED_ASSERTION .
  methods CHECK_ARCHIVE
    importing
      !TCLAS type TCLAS
      !PERNR type P_PERNR
      !CHECK_DATE type BEGDA
      !FIELD_NAME type C
      !MESSAGE_HANDLER type ref to IF_HRPA_MESSAGE_HANDLER
    exporting
      !IS_OK type BOOLE_D
    raising
      CX_HRPA_VIOLATED_ASSERTION .
  methods CHECK_PRTEV
    importing
      !TCLAS type TCLAS
      !P0003 type P0003
      !MESSAGE_HANDLER type ref to IF_HRPA_MESSAGE_HANDLER
    exporting
      !IS_OK type BOOLE_D
    raising
      CX_HRPA_VIOLATED_ASSERTION .
  methods CHECK_RRDAT
    importing
      !TCLAS type TCLAS
      !PERNR type P_PERNR
      !RRDAT type RRDAT
      !MESSAGE_HANDLER type ref to IF_HRPA_MESSAGE_HANDLER
    exporting
      !IS_OK type BOOLE_D
    raising
      CX_HRPA_VIOLATED_ASSERTION .
  methods CHECK_ABRDT
    importing
      !PERNR type P_PERNR
      !ABRDT type ABRDT
      !MESSAGE_HANDLER type ref to IF_HRPA_MESSAGE_HANDLER
    exporting
      !IS_OK type BOOLE_D
    raising
      CX_HRPA_VIOLATED_ASSERTION .
  methods FILTER_PAY_TIME_RELEVANCE
    importing
      !IV_FIELD type FIELDNAME
      !IT_0283_ALL_RECORD type HRPA_T_P0283
    returning
      value(RT_0283_FILTER) type HRPA_T_P0283 .

  methods SPECIFIC_INSERT_COMPUTATIONS
    redefinition .
  methods SPECIFIC_MODIFY_COMPUTATIONS
    redefinition .
  methods VERSIONID
    redefinition .
private section.
*"* private components of class /SEW/CL_HRPA_INFTY_0003
*"* do not include other source files here!!!

  class-data A_EXTENDED_CHANGES type BOOLE_D .
ENDCLASS.



CLASS /SEW/CL_HRPA_INFTY_0003 IMPLEMENTATION.


METHOD CHECK_ABRDT.

  DATA msg         TYPE symsg.
  DATA field_list  TYPE hrpad_field_tab.
  DATA dummy(1)    TYPE c.                                  "#EC NEEDED

  DATA cd_payroll_until TYPE p0003-abrdt.


  is_ok = true.

* get payroll-until from cluster
  CALL FUNCTION 'CD_GET_INFO'
       EXPORTING
            persnr             = pernr
            no_authority_check = 'X'
       IMPORTING
            payroll_until      = cd_payroll_until
*           highest_paydt      =
       EXCEPTIONS
            no_record_found    = 1
            error_message      = 2
            OTHERS             = 3.
  IF sy-subrc <> 0.
    CLEAR cd_payroll_until.
  ENDIF.

  IF abrdt <> cd_payroll_until.
    APPEND 'P0003-ABRDT' TO field_list.
    MESSAGE w133(rp) WITH abrdt cd_payroll_until INTO dummy.
    MOVE-CORRESPONDING sy TO msg.
    CALL METHOD message_handler->add_message
      EXPORTING
        message    = msg
        field_list = field_list
        cause      = if_hrpa_message_handler=>infotype_specific.

  ENDIF.

ENDMETHOD.


METHOD CHECK_ABWD.

  DATA data_exists TYPE boole_d.
  DATA msg         TYPE symsg.
  DATA field_list  TYPE hrpad_field_tab.
  DATA dummy(1)    TYPE c.             "#EC NEEDED

  DATA p0000       TYPE p0000.
  DATA abwd2_after TYPE p0003-abwd2.


  is_ok = true.

* check for ABWD1
  IF abwd1 IS NOT INITIAL.
    CALL METHOD a_read_infotype->read_single
      EXPORTING
        tclas         = tclas
        pernr         = pernr
        infty         = '0000'
        subty         = space
        objps         = space
        sprps         = if_hrpa_read_infotype=>unlocked
        begda         = abwd1
        endda         = abwd1
        mode          = if_hrpa_read_infotype=>last_intersecting_record
        no_auth_check = true
      IMPORTING
        pnnnn         = p0000
        data_exists   = data_exists.

    IF data_exists = false.
      is_ok = false.
*     "&1" liegt vor Einstellung
      MESSAGE e127(rp) WITH 'abrechnen bis'(ab1) INTO dummy.
      MOVE-CORRESPONDING sy TO msg.
      APPEND 'P0003-ABWD1' TO field_list.
      CALL METHOD message_handler->add_message
        EXPORTING
          message    = msg
          field_list = field_list
          cause      = if_hrpa_message_handler=>infotype_specific.
    ENDIF.
*   check if abwd1 is in an inactive period
    IF p0000-stat2 <> '0'.
      is_ok = false.
*     &1 darf nur in nichtaktivem Zeitraum liegen
      MESSAGE e112(rp) WITH 'abrechnen bis'(ab1) abwd1 INTO dummy.
      MOVE-CORRESPONDING sy TO msg.
      APPEND 'P0003-ABWD1' TO field_list.
      CALL METHOD message_handler->add_message
        EXPORTING
          message    = msg
          field_list = field_list
          cause      = if_hrpa_message_handler=>infotype_specific.
    ENDIF.
  ENDIF.

* check ABWD2
  IF abwd2 IS NOT INITIAL AND abwd2 < high_date.
    abwd2_after = abwd2 + 1.
    CALL METHOD a_read_infotype->read_single
      EXPORTING
        tclas         = tclas
        pernr         = pernr
        infty         = '0000'
        subty         = space
        objps         = space
        sprps         = if_hrpa_read_infotype=>unlocked
        begda         = abwd2_after
        endda         = abwd2_after
        mode          = if_hrpa_read_infotype=>last_intersecting_record
        no_auth_check = true
      IMPORTING
        pnnnn         = p0000
        data_exists   = data_exists.

    IF data_exists = false.
      is_ok = false.
*     "&1" liegt vor Einstellung
      MESSAGE e127(rp) WITH 'Tag vor "nicht mehr abrechnen"'(ab2) INTO dummy.
      MOVE-CORRESPONDING sy TO msg.
      APPEND 'P0003-ABWD2' TO field_list.
      CALL METHOD message_handler->add_message
        EXPORTING
          message    = msg
          field_list = field_list
          cause      = if_hrpa_message_handler=>infotype_specific.
    ENDIF.
*   check if abwd2 is in an inactive period
    IF p0000-stat2 <> '0'.
      is_ok = false.
*     &1 darf nur in nichtaktivem Zeitraum liegen
      MESSAGE e112(rp) WITH 'Tag vor "nicht mehr abrechnen"  '(ab2)
                             abwd2_after INTO dummy.
      MOVE-CORRESPONDING sy TO msg.
      APPEND 'P0003-ABWD2' TO field_list.
      CALL METHOD message_handler->add_message
        EXPORTING
          message    = msg
          field_list = field_list
          cause      = if_hrpa_message_handler=>infotype_specific.
    ENDIF.
*   check ABWD1 against ABWD2: ABWD2 must not be lower than ABWD1
*   and both have to be valid
    IF abwd1 IS NOT INITIAL.
      IF abwd2 < abwd1.
        is_ok = false.
*       "nicht mehr abrechnen" darf nicht vor "abrechnen bis" liegen
        MESSAGE e114(rp) INTO dummy.
        MOVE-CORRESPONDING sy TO msg.
        APPEND 'P0003-ABWD1' TO field_list.
        APPEND 'P0003-ABWD2' TO field_list.
        CALL METHOD message_handler->add_message
          EXPORTING
            message    = msg
            field_list = field_list
            cause      = if_hrpa_message_handler=>infotype_specific.
      ENDIF.
    ENDIF.                             " ...-abwd1 is initial
  ENDIF.                               " ...-abwd2 is initial.

ENDMETHOD.


METHOD CHECK_ARCHIVE .

  DATA data_exists TYPE boole_d.
  DATA msg         TYPE symsg.
  DATA field_list  TYPE hrpad_field_tab.
  DATA dummy(1)    TYPE c.        "#EC NEEDED

  DATA p0283       TYPE p0283.
  DATA lt_0283_flt TYPE TABLE OF p0283.
  DATA lt_0283_all TYPE TABLE OF p0283.
  DATA prelp_tab   TYPE hrpad_prelp_tab.

  DATA max_pr_arch TYPE p0283-prdaa.
  DATA max_begda   TYPE p0283-begda.
  DATA max_object  TYPE p0283-subty.

  DATA lv_date_before_arch TYPE d.
  DATA lv_date_after_arch  TYPE d.

  FIELD-SYMBOLS <l_pr>  TYPE begda.

  is_ok = true.

* check date if it is before date of last archiving
  CALL METHOD a_read_infotype->read
    EXPORTING
      tclas         = tclas
      pernr         = pernr
      infty         = par_infty_arc
      subty         = '*'
      objps         = '*'
      sprps         = if_hrpa_read_infotype=>unlocked
      begda         = low_date
      endda         = high_date
      no_auth_check = true
    IMPORTING
      infotype_tab  = prelp_tab
      data_exists   = data_exists.

  CALL METHOD cl_hr_pnnnn_type_cast=>prelp_to_pnnnn_tab
    EXPORTING
      prelp_tab = prelp_tab    " Table of Generic Infotypes
    IMPORTING
      pnnnn_tab = lt_0283_all.

  CALL METHOD filter_pay_time_relevance                       "EHA2670074
    EXPORTING
      iv_field              = field_name
      it_0283_all_record    = lt_0283_all
    RECEIVING
      rt_0283_filter        = lt_0283_flt.

* further processing only if archive data exists
  CHECK data_exists = true.

  CASE field_name.
    WHEN 'P0003-PRDAT'.
      ASSIGN p0283-prdaa TO <l_pr>.
    WHEN 'P0003-PRTEV'.
      ASSIGN p0283-prtea TO <l_pr>.
    WHEN OTHERS.
      RAISE EXCEPTION TYPE cx_hrpa_violated_precondition.
  ENDCASE.

* find max. PRDAT from IT0283 (PRDAA: PRDAT as set by archiving)
  LOOP AT lt_0283_flt INTO p0283.
    CASE field_name.
      WHEN 'P0003-PRDAT'.
        lv_date_before_arch = p0283-oprda.
        lv_date_after_arch  = p0283-prdaa.
      WHEN 'P0003-PRTEV'.
        lv_date_before_arch = p0283-oprte.
        lv_date_after_arch  = p0283-prtea.
    ENDCASE.
    IF    lv_date_before_arch = lv_date_after_arch            "EHA2785825
      AND check_date > p0283-endda.
      CONTINUE.
    ENDIF.
    IF <l_pr> > max_pr_arch.
      max_pr_arch = <l_pr>.
      max_object  = p0283-subty.
      max_begda   = p0283-begda.
    ENDIF.
  ENDLOOP.

  IF max_pr_arch > check_date.
    is_ok = false.
    APPEND field_name TO field_list.
*   Änderung nicht möglich, da Datum durch Archivierung gesetzt (Subtyp &1)
    MESSAGE e237(rp)  WITH max_object max_pr_arch max_begda INTO dummy.
    MOVE-CORRESPONDING sy TO msg.
*   field list is given as parameter
    CALL METHOD message_handler->add_message
      EXPORTING
        message    = msg
        field_list = field_list
        cause      = if_hrpa_message_handler=>infotype_specific.
  ENDIF.

ENDMETHOD.


METHOD CHECK_PRDAT.

  DATA data_exists TYPE boole_d.
  DATA msg         TYPE symsg.
  DATA field_list  TYPE hrpad_field_tab.
  DATA dummy(1)    TYPE c.           "#EC NEEDED
  DATA molga       TYPE molga.

  DATA day_before_prdat TYPE begda.


  is_ok = true.

  CHECK p0003-prdat IS NOT INITIAL.

  CALL METHOD me->check_archive
    EXPORTING
      tclas           = tclas
      pernr           = p0003-pernr
      check_date      = p0003-prdat
      field_name      = 'P0003-PRDAT'
      message_handler = message_handler
    IMPORTING
      is_ok           = is_ok.

  CHECK is_ok = true.

  IF p0003-rrdat IS NOT INITIAL AND
     p0003-prdat > p0003-rrdat.
    is_ok = false.
    APPEND 'P0003-PRDAT' TO field_list.
    APPEND 'P0003-RRDAT' TO field_list.
*   Persönliche tiefste Rückrechnung liegt nach dem Datum "&1"
    MESSAGE e132(rp) WITH 'Früh.Änd.Stamm.'(grr) INTO dummy.
    MOVE-CORRESPONDING sy TO msg.
    CALL METHOD message_handler->add_message
      EXPORTING
        message    = msg
        field_list = field_list
        cause      = if_hrpa_message_handler=>infotype_specific.
  ENDIF.

  CHECK is_ok = true.

* check if date is before first hire
  CALL METHOD a_read_infotype->read_single
    EXPORTING
      tclas         = tclas
      pernr         = p0003-pernr
      infty         = '0000'
      subty         = space
      objps         = space
      sprps         = if_hrpa_read_infotype=>unlocked
      begda         = p0003-prdat
      endda         = p0003-prdat
      mode          = if_hrpa_read_infotype=>last_intersecting_record
      no_auth_check = true
    IMPORTING
      data_exists   = data_exists.

  IF data_exists = false.
    is_ok = false.
    APPEND 'P0003-PRDAT' TO field_list.
*   "&1" liegt vor Einstellung
    MESSAGE e127(rp) WITH 'Pers.tiefste Rückrechnung'(vos) INTO dummy.
    MOVE-CORRESPONDING sy TO msg.
    CALL METHOD message_handler->add_message
      EXPORTING
        message    = msg
        field_list = field_list
        cause      = if_hrpa_message_handler=>infotype_specific.
  ENDIF.

  CHECK is_ok = true.

* check if date is after date 'Accounted to'
  day_before_prdat = p0003-prdat - 1.

  IF day_before_prdat > p0003-abrdt AND
     p0003-abrdt IS NOT INITIAL.
    APPEND 'P0003-PRDAT' TO field_list.
    APPEND 'P0003-ABRDT' TO field_list.
*   Persönliche tiefste Rückrechnung liegt nach dem Datum "&1"
    MESSAGE w132(rp) WITH 'abgerechnet bis'(abr) INTO dummy.
    MOVE-CORRESPONDING sy TO msg.
    CALL METHOD message_handler->add_message
      EXPORTING
        message    = msg
        field_list = field_list
        cause      = if_hrpa_message_handler=>infotype_specific.
  ENDIF.

* check for bonus payments: only for US/JP/CA
  molga = molga( tclas = tclas
                 pernr = p0003-pernr
                 begda = p0003-begda ).

  CHECK molga = '07' OR molga = '10' OR molga = '22'.

  IF p0003-rcbon IS NOT INITIAL AND
     p0003-prdat > p0003-rcbon.
    is_ok = false.
    APPEND 'P0003-PRDAT' TO field_list.
    APPEND 'P0003-RCBON' TO field_list.
*   Persönliche tiefste Rückrechnung liegt nach dem Datum "&1"
    MESSAGE e132(rp) WITH 'Früh.Änd.Stamm.Bonus.'(gbo) INTO dummy.
    MOVE-CORRESPONDING sy TO msg.
    CALL METHOD message_handler->add_message
      EXPORTING
        message    = msg
        field_list = field_list
        cause      = if_hrpa_message_handler=>infotype_specific.
  ENDIF.

ENDMETHOD.


METHOD CHECK_PRTEV .

  DATA data_exists TYPE boole_d.
  DATA msg         TYPE symsg.
  DATA field_list  TYPE hrpad_field_tab.
  DATA dummy(1)    TYPE c.          "#EC NEEDED


  is_ok = true.

  CHECK p0003-prtev IS NOT INITIAL.

  CALL METHOD me->check_archive
    EXPORTING
      tclas           = tclas
      pernr           = p0003-pernr
      check_date      = p0003-prtev
      field_name      = 'P0003-PRTEV'
      message_handler = message_handler
    IMPORTING
      is_ok           = is_ok.

  CHECK is_ok = true.

  IF p0003-bderr IS NOT INITIAL AND
     p0003-prtev > p0003-bderr.
    is_ok = false.
*   "Pers.tiefste Rückrechnung Zeit" liegt nach "Rückrechn.BDE"
    MESSAGE e149(rp) INTO dummy.
    MOVE-CORRESPONDING sy TO msg.
    APPEND 'P0003-PRTEV' TO field_list.
    APPEND 'P0003-BDERR' TO field_list.
    CALL METHOD message_handler->add_message
      EXPORTING
        message    = msg
        field_list = field_list
        cause      = if_hrpa_message_handler=>infotype_specific.
  ENDIF.

  CHECK is_ok = true.

* todo: evtl. das folgende Stück coding in eine Methode verpacken
* und zweimal rufen: Problem sind hierbei die Textsymbole

* check before first hire
  CALL METHOD a_read_infotype->read_single
    EXPORTING
      tclas         = tclas
      pernr         = p0003-pernr
      infty         = '0000'
      subty         = space
      objps         = space
      sprps         = if_hrpa_read_infotype=>unlocked
      begda         = p0003-prtev                          "MELN1244339
      endda         = p0003-prtev
      mode          = if_hrpa_read_infotype=>last_intersecting_record
      no_auth_check = true
    IMPORTING
      data_exists   = data_exists.

  IF data_exists = false.
    is_ok = false.
    APPEND 'P0003-PRTEV' TO field_list.
*   "&1" liegt vor Einstellung
    MESSAGE e127(rp) WITH 'Pers. tiefste Rückrechnung Zeit'(voz) INTO dummy.
    MOVE-CORRESPONDING sy TO msg.
    CALL METHOD message_handler->add_message
      EXPORTING
        message    = msg
        field_list = field_list
        cause      = if_hrpa_message_handler=>infotype_specific.
  ENDIF.

ENDMETHOD.


METHOD CHECK_RRDAT.

  DATA msg         TYPE symsg.
  DATA field_list  TYPE hrpad_field_tab.
  DATA dummy(1)    TYPE c.                                  "#EC NEEDED

  DATA p0001       TYPE p0001.
  DATA twm_begda   TYPE t549q-begda.


  is_ok = true.

  CHECK rrdat IS NOT INITIAL.

* get the current Payroll Area
  p0001 = p0001( tclas = tclas
                 pernr = pernr
                 begda = rrdat ).

  IF p0001 IS INITIAL.
    is_ok = false.
*   "&1" liegt vor Einstellung
    MESSAGE e127(rp) WITH 'Rückrechnungsdatum'(rrd) INTO dummy.
    MOVE-CORRESPONDING sy TO msg.
    APPEND 'P0003-RRDAT' TO field_list.
    CALL METHOD message_handler->add_message
      EXPORTING
        message    = msg
        field_list = field_list
        cause      = if_hrpa_message_handler=>infotype_specific.
  ENDIF.

  CHECK is_ok = true.

* get the period of the corresponding payroll control record
  CALL FUNCTION 'PA03_PCR_READ'
    EXPORTING
      f_abkrs               = p0001-abkrs
    IMPORTING
      f_twm_date            = twm_begda
    EXCEPTIONS
      abkrs_no_accounting   = 1
      pcr_does_not_exist    = 2
      abkrs_does_not_exist  = 3
      period_does_not_exist = 4
      OTHERS                = 5.
* SY-SUBRC = 1: "No payroll relevant" is ok, RRDAT can be changed
  IF sy-subrc >= 2.
    MOVE-CORRESPONDING sy TO msg.
    is_ok = false.
    CALL METHOD message_handler->add_message
      EXPORTING
        message = msg
        cause   = if_hrpa_message_handler=>infotype_specific.
  ENDIF.

  CHECK is_ok = true.

  IF twm_begda > rrdat.
*   Die Rückrechn.Abrechn. darf nicht vor dem TWM (&) des Abkrs...
    MESSAGE e128(rp) WITH twm_begda INTO dummy.
    MOVE-CORRESPONDING sy TO msg.
    APPEND 'P0003-RRDAT' TO field_list.
    CALL METHOD message_handler->add_message
      EXPORTING
        message    = msg
        field_list = field_list
        cause      = if_hrpa_message_handler=>infotype_specific.
    is_ok = false.
  ENDIF.

ENDMETHOD.


  METHOD FILTER_PAY_TIME_RELEVANCE.                                       ""EHA2670074

    DATA: lr_hrarch_configurations TYPE REF TO cl_hrarch_configurations,
          lv_archive_object        TYPE objct_tr01,
          lt_infty_tab             TYPE infty_tab,
          lv_payroll_relevant      TYPE boolean,
          lv_time_relevant         TYPE boolean.
    DATA  lt_ao_subgrp             TYPE arch_pa_t_grp_so.
    DATA  lt_obj_config            TYPE arch_pa_t_config.

    FIELD-SYMBOLS:  <fs0283>       TYPE p0283.
    FIELD-SYMBOLS   <ls_ao_subgrp> TYPE arch_pa_s_grp_so.
    FIELD-SYMBOLS   <ls_obj_config> TYPE arch_pa_s_config.
    FIELD-SYMBOLS   <ls_infty_grp> TYPE arch_pa_s_infty_grp.

* This method will filter IT0283 by payroll or time relevant
* When we modify the payroll retro date, there is not need to check IT0283 created for time and vice versa

    CLEAR rt_0283_filter[].

    IF iv_field <> 'P0003-PRDAT' AND iv_field <>  'P0003-PRTEV'.
      RETURN.
    ENDIF.

    LOOP AT IT_0283_ALL_RECORD ASSIGNING <fs0283>
                                   WHERE state = par_state_techarch OR
                                         state = par_state_archived OR
                                         state = par_state_to_be_destroyed OR
                                         state = par_state_prep_supp_dest  OR
                                         state = par_state_destroy.

      CLEAR lt_ao_subgrp.
      CLEAR lt_obj_config.
      CLEAR lt_infty_tab.

      IF    ( <fs0283>-subty = '0001'    "PA_CALC
        AND iv_field = 'P0003-PRTEV' ) OR
            ( <fs0283>-subty = '0005'    "PA_TIME
        AND iv_field = 'P0003-PRDAT' ).
        CONTINUE.
      ENDIF.

      lv_archive_object = cl_hrarch_t77paarc_subty=>read_archive_object( <fs0283>-subty ).

      IF <fs0283>-stgrp IS INITIAL.
        CALL METHOD cl_hrarch_configurations=>get_instance
        EXPORTING
          iv_obj_name        = lv_archive_object
          iv_load_add_struc  = abap_true
        IMPORTING
          eo_config_instance = lr_hrarch_configurations.

      CALL METHOD lr_hrarch_configurations->get_object_infotype
        IMPORTING
          et_infty = lt_infty_tab.
      ELSE.
        APPEND INITIAL LINE TO lt_ao_subgrp ASSIGNING <ls_ao_subgrp>.
        <ls_ao_subgrp>-sign = 'I'.
        <ls_ao_subgrp>-option = 'EQ'.
        <ls_ao_subgrp>-low = <fs0283>-stgrp.
        CALL METHOD cl_hrarch_configurations=>get_instance
          EXPORTING
            iv_obj_name        = lv_archive_object
            it_ao_subgrp       = lt_ao_subgrp
            iv_load_add_struc  = abap_true
          IMPORTING
            eo_config_instance = lr_hrarch_configurations.
        CALL METHOD lr_hrarch_configurations->get_data_obj_config
          IMPORTING
            et_obj_config = lt_obj_config.
        LOOP AT lt_obj_config ASSIGNING <ls_obj_config>.
          LOOP AT <ls_obj_config>-infty_grp ASSIGNING <ls_infty_grp>.
            APPEND <ls_infty_grp>-infty TO lt_infty_tab.
          ENDLOOP.
          SORT lt_infty_tab.
          DELETE ADJACENT DUPLICATES FROM lt_infty_tab.
        ENDLOOP.
      ENDIF.

      "For objects like PA_CALC, PA_TIME and PA_CALC... there is no infotype defined
      " By default it is relevant
      IF lt_infty_tab[] IS INITIAL.
        APPEND <fs0283> TO rt_0283_filter[].
        CONTINUE.
      ENDIF.

      " Check if infotype is relevant
      CALL METHOD cl_hrarch_retro_calculations=>get_payroll_time_relevant
        EXPORTING
          it_infty            = lt_infty_tab
        IMPORTING
          ev_payroll_relevant = lv_payroll_relevant
          ev_time_relevant    = lv_time_relevant.

      IF iv_field = 'P0003-PRDAT' AND lv_payroll_relevant = 'X'.
        APPEND <fs0283> TO rt_0283_filter[].
      ELSEIF iv_field = 'P0003-PRTEV' AND lv_time_relevant = 'X'.
        APPEND <fs0283> TO rt_0283_filter[].
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


METHOD PAYROLL_LOCK.

  DATA msg           TYPE symsg.
  DATA field_list    TYPE hrpad_field_tab.
  DATA dummy(1)      TYPE c.                                "#EC NEEDED
  DATA p0001         TYPE p0001.


  DATA pcr_state        TYPE t569v-state.
  DATA pcr_endda        TYPE t549q-endda.
  DATA cd_payroll_until TYPE p0003-abrdt.


  is_ok = true.

* locking Pernr via A_EXTENDED_CHANGES (corresponds transaction PU03)
* should be possible without checking the payroll control record
  CHECK a_extended_changes IS INITIAL.

  CHECK abrsp IS NOT INITIAL.
* get payroll-until from cluster
  CALL FUNCTION 'CD_GET_INFO'
       EXPORTING
            persnr             = pernr
            no_authority_check = 'X'
       IMPORTING
            payroll_until      = cd_payroll_until
*            highest_paydt     =
       EXCEPTIONS
            no_record_found = 1
            error_message   = 2
            OTHERS          = 3.
  IF sy-subrc <> 0.
    CLEAR cd_payroll_until.
  ENDIF.

  CHECK cd_payroll_until IS NOT INITIAL.

  p0001 = p0001( tclas = tclas
                 pernr = pernr
                 begda = cd_payroll_until ).

  IF p0001 IS NOT INITIAL.
*   get the period of the corresponding payroll control record
    CALL FUNCTION 'PA03_PCR_READ'
      EXPORTING
        f_abkrs               = p0001-abkrs
      IMPORTING
        f_state               = pcr_state
        f_current_endda       = pcr_endda
      EXCEPTIONS
        abkrs_no_accounting   = 1
        pcr_does_not_exist    = 2
        abkrs_does_not_exist  = 3
        period_does_not_exist = 4
        OTHERS                = 5.
    IF sy-subrc <> 0.
      MOVE-CORRESPONDING sy TO msg.
      is_ok = false.
      CALL METHOD message_handler->add_message
        EXPORTING
          message = msg
          cause   = if_hrpa_message_handler=>infotype_specific.
      RETURN.
    ENDIF.
    CASE pcr_state.
      WHEN '0'.    " new created
      WHEN '3'.    " exit payroll
      WHEN '9'.    " deleted
        is_ok = false.
*       Personalnummer kann für die Abrechnung nicht gesperrt werden
        MESSAGE e591(54) INTO dummy.
        MOVE-CORRESPONDING sy TO msg.
        APPEND 'P0003-ABRSP' TO field_list.
        CALL METHOD message_handler->add_message
          EXPORTING
            message    = msg
            field_list = field_list
            cause      = if_hrpa_message_handler=>infotype_specific.
      WHEN OTHERS.   " check, if there exist payroll results
        IF pcr_endda <= cd_payroll_until.
          is_ok = false.
*         Personalnummer kann nicht für die Abrechnung gesperrt werden
          MESSAGE e590(54) INTO dummy.
          MOVE-CORRESPONDING sy TO msg.
          APPEND 'P0003-ABRSP' TO field_list.
          CALL METHOD message_handler->add_message
            EXPORTING
              message    = msg
              field_list = field_list
              cause      = if_hrpa_message_handler=>infotype_specific.
        ENDIF.
    ENDCASE.
  ENDIF.
ENDMETHOD.


METHOD SPECIAL_MODE_GET.
  ev_special_mode = a_extended_changes.
ENDMETHOD.


method SPECIAL_MODE_RESET.

* reset the flag for extended mode
  a_extended_changes = false.

endmethod.


METHOD SPECIAL_MODE_SET.

* This flag is internally used to indicate that
* extended changes on infotype 0003 are allowed.
* this complies with the special authorization
* check of transaction PU03
* E.g.: change of field RRDAT, ABRDT, ...
  a_extended_changes = true.

ENDMETHOD.


METHOD SPECIFIC_COMPUTATIONS .

  DATA msg           TYPE symsg.
  DATA field_list    TYPE hrpad_field_tab.
  DATA dummy(1)      TYPE c.                                "#EC NEEDED

  DATA molga             TYPE molga.


  is_ok = true.

* check box values are checked
  IF p0003-koabr CN ' X'.
    is_ok = false.
    MESSAGE e002(00) INTO dummy.
    MOVE-CORRESPONDING sy TO msg.
    APPEND 'P0003-KOABR' TO field_list.
    CALL METHOD message_handler->add_message
      EXPORTING
        message    = msg
        field_list = field_list
        cause      = if_hrpa_message_handler=>infotype_specific.
  ENDIF.

  CHECK is_ok = true.

* general checks
  CALL METHOD me->payroll_lock
    EXPORTING
      tclas           = tclas
      molga           = molga
      pernr           = p0003-pernr
      abrsp           = p0003-abrsp
      message_handler = message_handler
    IMPORTING
      is_ok           = is_ok.

  CHECK is_ok = true.

  CALL METHOD me->check_abwd
    EXPORTING
      tclas           = tclas
      pernr           = p0003-pernr
      abwd1           = p0003-abwd1
      abwd2           = p0003-abwd2
      message_handler = message_handler
    IMPORTING
      is_ok           = is_ok.

  CHECK is_ok = true.

  CALL METHOD me->check_prdat
    EXPORTING
      tclas           = tclas
      p0003           = p0003
      message_handler = message_handler
    IMPORTING
      is_ok           = is_ok.

  CHECK is_ok = true.

  CALL METHOD me->check_prtev
    EXPORTING
      tclas           = tclas
      p0003           = p0003
      message_handler = message_handler
    IMPORTING
      is_ok           = is_ok.

  CHECK is_ok = true.

  CALL METHOD me->check_rrdat
    EXPORTING
      tclas           = tclas
      pernr           = p0003-pernr
      rrdat           = p0003-rrdat
      message_handler = message_handler
    IMPORTING
      is_ok           = is_ok.

  CHECK is_ok = true.

  CALL METHOD me->check_abrdt
    EXPORTING
      pernr           = p0003-pernr
      abrdt           = p0003-abrdt
      message_handler = message_handler
    IMPORTING
      is_ok           = is_ok.

ENDMETHOD.


METHOD specific_insert_computations .

  DATA data_exists TYPE boole_d.
  DATA p0001       TYPE p0001.
  DATA molga       TYPE molga.
  DATA pme04       TYPE pme04.

  FIELD-SYMBOLS <p0003>  TYPE p0003.


  ASSIGN pnnnn TO <p0003> CASTING.

* check if there is already an existing infotype 0003
* if so, then the INSERT has to be prevented !
  CALL METHOD a_read_infotype->read_single
    EXPORTING
      tclas         = tclas
      pernr         = <p0003>-pernr
      infty         = '0003'
      subty         = space
      objps         = space
      sprps         = unlocked
      begda         = low_date
      endda         = high_date
      mode          = if_hrpa_read_infotype=>last_intersecting_record
      no_auth_check = true
    IMPORTING
      data_exists   = data_exists.
  IF data_exists = true.
    RAISE EXCEPTION TYPE cx_hrpa_violated_precondition.
  ENDIF.

* initialization of the infotype 0003
  <p0003>-dat00 = sy-datum.
  <p0003>-uhr00 = sy-uzeit.

  p0001 = p0001( tclas = tclas
                 pernr = <p0003>-pernr
                 begda = low_date ).

  molga = molga( tclas = tclas
                 pernr = <p0003>-pernr
                 begda = low_date ).

  IF p0001 IS INITIAL OR molga IS INITIAL.
*   if there is no p0001 / molga
*   we cannot determine VIEKN !
*    RAISE EXCEPTION TYPE cx_hrpa_violated_precondition.
    is_ok = abap_false.
    data(msg) = VALUE symsg( msgty = 'E' msgid = '/SEW/INTEGRATION' msgno = 031 ).
    message_handler->add_message(
      EXPORTING
        message      = msg
        cause        = if_hrpa_message_handler=>infotype_specific ).
    exit.
  ENDIF.

  pme04-tclas = tclas.
  pme04-bukrs = p0001-bukrs.
  pme04-werks = p0001-werks.
  pme04-persg = p0001-persg.
  pme04-persk = p0001-persk.
  pme04-molga = molga.

  CALL METHOD cl_hrpa_feature=>get_value
    EXPORTING
      feature       = 'IVWID'
      struc_content = pme04
    IMPORTING
      return_value  = <p0003>-viekn.


  CALL METHOD me->specific_computations
    EXPORTING
      tclas           = tclas
      message_handler = message_handler
    IMPORTING
      is_ok           = is_ok
    CHANGING
      p0003           = <p0003>.

ENDMETHOD.


METHOD SPECIFIC_MODIFY_COMPUTATIONS.

  FIELD-SYMBOLS <p0003>     TYPE p0003.
  FIELD-SYMBOLS <old_p0003> TYPE p0003.


  ASSIGN pnnnn TO <p0003> CASTING.
  ASSIGN old_pnnnn TO <old_p0003> CASTING.

* changes on the following fields are only allowed
* when using extended mode (transaction PU03)
  IF a_extended_changes = false.
    IF <p0003>-abrdt <> <old_p0003>-abrdt.
      <p0003>-abrdt = <old_p0003>-abrdt.
    ENDIF.
    IF <p0003>-rrdat <> <old_p0003>-rrdat.
      <p0003>-rrdat = <old_p0003>-rrdat.
    ENDIF.
    IF <p0003>-koabr <> <old_p0003>-koabr.
      <p0003>-koabr = <old_p0003>-koabr.
    ENDIF.
    IF <p0003>-pkgab <> <old_p0003>-pkgab.
      <p0003>-pkgab = <old_p0003>-pkgab.
    ENDIF.
    IF <p0003>-bderr <> <old_p0003>-bderr.
      <p0003>-bderr = <old_p0003>-bderr.
    ENDIF.
    IF <p0003>-kobde <> <old_p0003>-kobde.
      <p0003>-kobde = <old_p0003>-kobde.
    ENDIF.
    IF <p0003>-rcbon <> <old_p0003>-rcbon.
      <p0003>-rcbon = <old_p0003>-rcbon.
    ENDIF.
  ENDIF.

* fields dat00 and uhr00 must never change !
* (except they were SPACE before and are now set to the correct initial value)
  IF     <p0003>-dat00 <> <old_p0003>-dat00 AND
   NOT ( <p0003>-dat00 IS INITIAL AND                      "MELN2344496
         <old_p0003>-dat00 = space ).
    <p0003>-dat00 = <old_p0003>-dat00.
  ENDIF.
  IF     <p0003>-uhr00 <> <old_p0003>-uhr00 AND
   NOT ( <p0003>-uhr00 IS INITIAL AND
         <old_p0003>-uhr00 = space ).
    <p0003>-uhr00 = <old_p0003>-uhr00.
  ENDIF.

  CALL METHOD me->specific_computations
    EXPORTING
      tclas           = tclas
      message_handler = message_handler
    IMPORTING
      is_ok           = is_ok
    CHANGING
      p0003           = <p0003>.

ENDMETHOD.


METHOD VERSIONID .

* IT0003 has no country versions
  CLEAR cooked_versionid.

ENDMETHOD.
ENDCLASS.
