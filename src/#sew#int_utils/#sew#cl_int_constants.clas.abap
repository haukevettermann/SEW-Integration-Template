class /SEW/CL_INT_CONSTANTS definition
  public
  final
  create public .

public section.

  constants EMP type STRING value 'EMP' ##NO_TEXT.
  constants CWK type STRING value 'CWK' ##NO_TEXT.
  constants PEN type STRING value 'PEN' ##NO_TEXT.
  constants EMPLOYEE type STRING value 'Employee' ##NO_TEXT.
  constants CONTINGENT_WORKER type STRING value 'Contingent Worker' ##NO_TEXT.
  constants EXTERNAL_CONSULTANTS type STRING value 'External Consultants' ##NO_TEXT.
  constants VACATION_HELPER type STRING value 'Intern/Vacation Helper/Working Student' ##NO_TEXT.
  constants APPRENTICE type STRING value 'Apprentice' ##NO_TEXT.
  constants LABOUR_AGENCY type STRING value 'Temp Labour Agency' ##NO_TEXT.
  constants RETIREE type STRING value 'Retiree' ##NO_TEXT.
  constants:
    BEGIN OF cofu_mandant.
    CONSTANTS germany     TYPE mandt VALUE '102'.
    CONSTANTS france      TYPE mandt VALUE '005'.
    CONSTANTS netherlands TYPE mandt VALUE '190'.
    CONSTANTS newzealand  TYPE mandt VALUE '122'.
    CONSTANTS australia   TYPE mandt VALUE '120'.
    CONSTANTS austria     TYPE mandt VALUE '400'.
    CONSTANTS italy       TYPE mandt VALUE '400'.
    CONSTANTS END OF cofu_mandant .
  constants:
    BEGIN OF booking_status.
    CONSTANTS initial TYPE char2 VALUE '01'.
    CONSTANTS success TYPE char2 VALUE '02'.
    CONSTANTS error TYPE char2 VALUE '03'.
    CONSTANTS oracle TYPE char2 VALUE '04'.
    CONSTANTS nochange TYPE char2 VALUE '05'.
    CONSTANTS manual TYPE char2 VALUE '06'.
    CONSTANTS markasdone TYPE char2 VALUE '07'.
    CONSTANTS running TYPE char2 VALUE '08'.
    CONSTANTS error_des TYPE char2 VALUE '09'.
    CONSTANTS simulated TYPE char2 VALUE '10'.
    CONSTANTS pernr_locked TYPE char2 VALUE '11'.
    CONSTANTS END OF booking_status .
  constants:
    BEGIN OF infty_operation.
    CONSTANTS pa_insert TYPE actio VALUE 'INS'.
    CONSTANTS pa_copy TYPE actio VALUE 'COP'.
    CONSTANTS pa_insert_action TYPE actio VALUE 'INSS'.
    CONSTANTS pa_modify TYPE actio VALUE 'MOD'.
    CONSTANTS pa_delete TYPE actio VALUE 'DEL'.
    CONSTANTS pa_delimit TYPE actio VALUE 'LIS9'.
    CONSTANTS om_insert TYPE okcode VALUE 'INSE'.
    CONSTANTS om_modify TYPE okcode VALUE 'AEND'.
    CONSTANTS om_delete TYPE okcode VALUE 'DEL'.
    CONSTANTS om_delimit TYPE okcode VALUE 'CUT'.
    CONSTANTS tev_insert TYPE action VALUE 'INS'.
    CONSTANTS tev_modify TYPE actio VALUE 'UPD'.
    CONSTANTS tev_cancel TYPE action VALUE 'CAN'.
    CONSTANTS END OF infty_operation .
  constants:
    BEGIN OF oracle_assign_status_code.
    CONSTANTS inactive_process TYPE string VALUE 'INACTIVE_PROCESS'.
    CONSTANTS suspend_process  TYPE string VALUE 'SUSPEND_PROCESS'.
    CONSTANTS active_process   TYPE string VALUE 'ACTIVE_PROCESS'.
    CONSTANTS END OF oracle_assign_status_code .
  constants:
    BEGIN OF relations.
    CONSTANTS manager TYPE subty VALUE 'B012'.
    CONSTANTS cost_center TYPE subty VALUE 'A011'.
    CONSTANTS END OF relations .
  constants SOBID type STRING value 'SOBID' ##NO_TEXT.
  constants PERNR type STRING value 'PERNR' ##NO_TEXT.
  constants BEGDA type STRING value 'BEGDA' ##NO_TEXT.
  constants ENDDA type STRING value 'ENDDA' ##NO_TEXT.
  constants MASSG type STRING value 'MASSG' ##NO_TEXT.
  constants MASSN type STRING value 'MASSN' ##NO_TEXT.
  constants INFTY type STRING value 'INFTY' ##NO_TEXT.
  constants LANGU type STRING value 'LANGU' ##NO_TEXT.
  constants SUBTY type STRING value 'SUBTY' ##NO_TEXT.
  constants XPATH_BEGDA type STRING value '/StartDate' ##NO_TEXT.
  constants XPATH_ENDDA type STRING value '/EndDate' ##NO_TEXT.
  constants PERSON type OTYPE value 'P' ##NO_TEXT.
  constants ORGUNIT type OTYPE value 'O' ##NO_TEXT.
  constants POSITION type OTYPE value 'S' ##NO_TEXT.
  constants COSTCENTER type OTYPE value 'K' ##NO_TEXT.
  constants MSG_CLASS_INT type STRING value '/SEW/HCM_INTEGRATION' ##NO_TEXT.
  constants ERROR type CHAR1 value 'E' ##NO_TEXT.
  constants WARNING type CHAR1 value 'I' ##NO_TEXT.
  constants SUCCESS type CHAR1 value 'S' ##NO_TEXT.
  constants:
    BEGIN OF range.
    CONSTANTS sign TYPE string VALUE 'I' ##NO_TEXT.
    CONSTANTS option_eq TYPE string VALUE 'EQ' ##NO_TEXT.
    CONSTANTS END OF range .
  constants XML_NODE_PERSON type STRING value PERSON ##NO_TEXT.
  constants STARTDATE type STRING value 'StartDate' ##NO_TEXT.
  constants DEFAULT_POS type STRING value '99999999' ##NO_TEXT.
  constants ENDDATE type STRING value 'EndDate' ##NO_TEXT.
  constants HIGHDATE type DATS value '99991231' ##NO_TEXT.
  constants LOWDATE type DATS value '19000101' ##NO_TEXT.
  constants CANCEL_HIRE type STRING value 'ZZ' ##NO_TEXT.
  constants HIRE type STRING value '01' ##NO_TEXT.
  constants DATE_TYPE_HIRE type STRING value '99' ##NO_TEXT.
  constants HIRE_DATE_CHANGE type STRING value 'HC' ##NO_TEXT.
  constants REHIRE type STRING value '04' ##NO_TEXT.
  constants TERMINATION type STRING value '03' ##NO_TEXT.
  constants ORG_CHANGE type STRING value '02' ##NO_TEXT.
  constants ITM01 type STRING value 'm01' ##NO_TEXT.
  constants ITMM type STRING value 'mm' ##NO_TEXT.
  constants IT1M0 type STRING value '1m0' ##NO_TEXT.
  constants IT1M1 type STRING value '1m1' ##NO_TEXT.
  constants PLANS type STRING value 'PLANS' ##NO_TEXT.
  constants BDEGR type STRING value 'BDEGR' ##NO_TEXT.
  constants GRAWG type STRING value 'GRAWG' ##NO_TEXT.
*  constants PLANS type STRING value 'PLANS' ##NO_TEXT.
  constants KOSTL type STRING value 'KOSTL' ##NO_TEXT.
  constants ORGEH type STRING value 'ORGEH' ##NO_TEXT.
  constants INSERT_REL type STRING value 'INSE' ##NO_TEXT.
  constants HIGHDATE_ORACLE type DATS value '47121231' ##NO_TEXT.
  constants INTEGRATION_RUN_ID_PATH type STRING value '//RequestId' ##NO_TEXT.
  constants COUNTRY_CODE_PATH type STRING value '//Country' ##NO_TEXT.
  constants COUNTRY_CODE_PATH_RPT type STRING value '//COUNTRY' ##NO_TEXT.
  constants COUNTRY_CODE_PATH_OM type STRING value '/CostCenterData/Entity' ##NO_TEXT.
  constants COMPANY_CODE_PATH type STRING value '//EmploymentData/CompanyCode' ##NO_TEXT.
  constants OTYPE type STRING value 'OTYPE' ##NO_TEXT.
  constants INFTY_INS type ACTIO value 'INS' ##NO_TEXT.
  constants INFTY_DEL type ACTIO value 'DEL' ##NO_TEXT.
  constants INFTY_MOD type ACTIO value 'MOD' ##NO_TEXT.
  constants OBJID type STRING value 'OBJID' ##NO_TEXT.
  constants STAR type STRING value '*' ##NO_TEXT.
  constants:
    BEGIN OF folders.
    CONSTANTS employmentdata TYPE string VALUE '//EmploymentData'.
    CONSTANTS jobinformation TYPE string VALUE '//JobInformation'.
    CONSTANTS END OF folders .
  constants PRIMARY type STRING value '//Primary' ##NO_TEXT.
  constants M1 type CHAR3 value '001' ##NO_TEXT.
  constants M2 type CHAR3 value '002' ##NO_TEXT.
  constants PRIMARYASSIGNMENT type STRING value '//PrimaryAssignment' ##NO_TEXT.
  constants M3 type CHAR3 value '003' ##NO_TEXT.
  constants:
*  constants STATUS_INITIAL type /SEW/DD_STATUS value '01' ##NO_TEXT.
*  constants STATUS_ERROR type /SEW/DD_STATUS value '03' ##NO_TEXT.
*  constants STATUS_SUCCESS type /SEW/DD_STATUS value '02' ##NO_TEXT.
*    CONSTANTS:
*    BEGIN OF non_relevant_actions.
*    CONSTANTS hire TYPE char20 VALUE 'HIRE'.
*    CONSTANTS END OF non_relevant_actions .
    BEGIN OF msg_no.
    CONSTANTS m1 TYPE char3 VALUE '001'.
    CONSTANTS m2 TYPE char3 VALUE '002'.
    CONSTANTS m3 TYPE char3 VALUE '003'.
    CONSTANTS m4 TYPE char3 VALUE '004'.
    CONSTANTS m5 TYPE char3 VALUE '005'.
    CONSTANTS m6 TYPE char3 VALUE '006'.
    CONSTANTS m7 TYPE char3 VALUE '007'.
    CONSTANTS m8 TYPE char3 VALUE '008'.
    CONSTANTS m9 TYPE char3 VALUE '009'.
    CONSTANTS m10 TYPE char3 VALUE '010'.
    CONSTANTS m11 TYPE char3 VALUE '011'.
    CONSTANTS m12 TYPE char3 VALUE '012'.
    CONSTANTS m13 TYPE char3 VALUE '013'.
    CONSTANTS m14 TYPE char3 VALUE '014'.
    CONSTANTS m15 TYPE char3 VALUE '015'.
    CONSTANTS m16 TYPE char3 VALUE '016'.
    CONSTANTS m17 TYPE char3 VALUE '017'.
    CONSTANTS m18 TYPE char3 VALUE '018'.
    CONSTANTS m19 TYPE char3 VALUE '019'.
    CONSTANTS m20 TYPE char3 VALUE '020'.
    CONSTANTS m21 TYPE char3 VALUE '021'.
    CONSTANTS m22 TYPE char3 VALUE '022'.
    CONSTANTS m23 TYPE char3 VALUE '023'.
    CONSTANTS m24 TYPE char3 VALUE '024'.
    CONSTANTS m25 TYPE char3 VALUE '025'.
    CONSTANTS m26 TYPE char3 VALUE '026'.
    CONSTANTS m27 TYPE char3 VALUE '027'.
    CONSTANTS m28 TYPE char3 VALUE '028'.
    CONSTANTS m29 TYPE char3 VALUE '029'.
    CONSTANTS m30 TYPE char3 VALUE '030'.
    CONSTANTS m31 TYPE char3 VALUE '031'.
    CONSTANTS m32 TYPE char3 VALUE '032'.
    CONSTANTS m33 TYPE char3 VALUE '033'.
    CONSTANTS m34 TYPE char3 VALUE '034'.
    CONSTANTS m35 TYPE char3 VALUE '035'.
    CONSTANTS m36 TYPE char3 VALUE '036'.
    CONSTANTS m37 TYPE char3 VALUE '037'.
    CONSTANTS m38 TYPE char3 VALUE '038'.
    CONSTANTS m39 TYPE char3 VALUE '039'.
    CONSTANTS m40 TYPE char3 VALUE '040'.
    CONSTANTS m41 TYPE char3 VALUE '041'.
    CONSTANTS m42 TYPE char3 VALUE '042'.
    CONSTANTS m43 TYPE char3 VALUE '043'.
    CONSTANTS m44 TYPE char3 VALUE '044'.
    CONSTANTS m45 TYPE char3 VALUE '045'.
    CONSTANTS END OF msg_no .
  constants IT0000 type STRING value '0000' ##NO_TEXT.
  constants M4 type CHAR3 value '004' ##NO_TEXT.
  constants IT0002 type STRING value '0002' ##NO_TEXT.
  constants IT0001 type STRING value '0001' ##NO_TEXT.
  constants M5 type CHAR3 value '005' ##NO_TEXT.
  constants IT1000 type STRING value '1000' ##NO_TEXT.
  constants M6 type CHAR3 value '006' ##NO_TEXT.
  constants M7 type CHAR3 value '007' ##NO_TEXT.
  constants IT1001 type STRING value '1001' ##NO_TEXT.
  constants IT0105 type STRING value '0105' ##NO_TEXT.
  constants M8 type CHAR3 value '008' ##NO_TEXT.
  constants M9 type CHAR3 value '009' ##NO_TEXT.
  constants SERIALIZER type STRING value 'S' ##NO_TEXT.
  constants M10 type CHAR3 value '010' ##NO_TEXT.
  constants POSTER type STRING value 'P' ##NO_TEXT.
  constants COCKPIT type STRING value 'C' ##NO_TEXT.
  constants M11 type CHAR3 value '011' ##NO_TEXT.
  constants M12 type CHAR3 value '012' ##NO_TEXT.
  constants PLVAR type STRING value '01' ##NO_TEXT.
  constants ZAUSW type STRING value 'Time Device Badge ID' ##NO_TEXT.
  constants M13 type CHAR3 value '013' ##NO_TEXT.
  constants BEFORE type STRING value 'BEFORE' ##NO_TEXT.
  constants M14 type CHAR3 value '014' ##NO_TEXT.
  constants AFTER type STRING value 'AFTER' ##NO_TEXT.
  constants M15 type CHAR3 value '015' ##NO_TEXT.
  constants M16 type CHAR3 value '016' ##NO_TEXT.
  constants SCLAS type STRING value 'SCLAS' ##NO_TEXT.
  constants M17 type CHAR3 value '017' ##NO_TEXT.
  constants M18 type CHAR3 value '018' ##NO_TEXT.
  constants M19 type CHAR3 value '019' ##NO_TEXT.
  constants M20 type CHAR3 value '020' ##NO_TEXT.
  constants M21 type CHAR3 value '021' ##NO_TEXT.
  constants M22 type CHAR3 value '022' ##NO_TEXT.
  constants M23 type CHAR3 value '023' ##NO_TEXT.
  constants M24 type CHAR3 value '024' ##NO_TEXT.
  constants M25 type CHAR3 value '025' ##NO_TEXT.
  constants M26 type CHAR3 value '026' ##NO_TEXT.
  constants M27 type CHAR3 value '027' ##NO_TEXT.
  constants M28 type CHAR3 value '028' ##NO_TEXT.
  constants M29 type CHAR3 value '029' ##NO_TEXT.
  constants M30 type CHAR3 value '030' ##NO_TEXT.
  constants M31 type CHAR3 value '031' ##NO_TEXT.
  constants M32 type CHAR3 value '032' ##NO_TEXT.
  constants:
    BEGIN OF fields.
    CONSTANTS hire_date TYPE string VALUE 'StartDate'.
    CONSTANTS termination_date TYPE string VALUE 'EndDate'.
    CONSTANTS ass_stat TYPE string VALUE 'AssStatusType'.
    CONSTANTS action_code TYPE string VALUE 'ActionCode'.
    CONSTANTS END OF fields .
  constants WEB_CLOCK_ID_ORACLE type STRING value 'Web Clock' ##NO_TEXT.
  constants WEB_CLOCK_ID_SAP type TERID value '9999' ##NO_TEXT.
  constants IT0050 type STRING value '0050' ##NO_TEXT.
  constants IT2011 type STRING value '2011' ##NO_TEXT.
  constants IT2001 type STRING value '2001' ##NO_TEXT.
  constants IT2002 type STRING value '2002' ##NO_TEXT.
  constants IT2006 type STRING value '2006' ##NO_TEXT.
  constants IT2007 type STRING value '2007' ##NO_TEXT.
  constants USER2 type STRING value 'USER2' ##NO_TEXT.
  constants IT0302 type INFTY value '0302' ##NO_TEXT.
  constants LDATE type STRING value 'LDATE' ##NO_TEXT.
  constants PERSK type STRING value 'PERSK' ##NO_TEXT.
  constants PERSG type STRING value 'PERSG' ##NO_TEXT.
  constants ABSENCE type OTYPE value 'AS' ##NO_TEXT.
  constants DEL_ABSENCE type OTYPE value 'DS' ##NO_TEXT.
  constants ATTENDANCE type OTYPE value 'AT' ##NO_TEXT.
  constants DEL_ATTENDANCE type OTYPE value 'DT' ##NO_TEXT.
  constants TIME_EVENTS type OTYPE value 'TV' ##NO_TEXT.
  constants QUOTA type OTYPE value 'QU' ##NO_TEXT.
  constants BALANCE type OTYPE value 'BN' ##NO_TEXT.
  constants COGL type STRING value 'COGL' ##NO_TEXT.
  constants COFU type STRING value 'COFU' ##NO_TEXT.
  class-data ACTION_MAP type /SDF/CALM_RANGE_TT .
  class-data HIRE_RANGE type /SDF/CALM_RANGE_TT .
  class-data TERMINATION_RANGE type /SDF/CALM_RANGE_TT .
  class-data ORGCHANGE_RANGE type /SDF/CALM_RANGE_TT .
  class-data REHIRE_RANGE type /SDF/CALM_RANGE_TT .

  class-methods NON_RELEVANT_ACTION
    returning
      value(RANGE) type /SDF/CALM_RANGE_TT .
  class-methods SET_ACTION_RANGES
    importing
      !MOLGA type MOLGA .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /SEW/CL_INT_CONSTANTS IMPLEMENTATION.


  METHOD non_relevant_action.
    range = VALUE #( ( sign = 'I' option = 'EQ' low = 'HIRE' )
                     ( sign = 'I' option = 'EQ' low = 'ADD_PEN_WKR' )
                     ( sign = 'I' option = 'CP' low = 'HIRE' )
                     ( sign = 'I' option = 'CP' low = 'RESIG' )
                     ( sign = 'I' option = 'CP' low = 'TERM' )
                     ( sign = 'I' option = 'EQ' low = 'HI' )
                     ( sign = 'I' option = 'EQ' low = 'RESIGNATION' )
                     ( sign = 'I' option = 'EQ' low = 'REDUCTION_FORCE' )
                     ( sign = 'I' option = 'EQ' low = 'TERMINATION' )
                     ( sign = 'I' option = 'EQ' low = 'TERMINATE_PLACEMENT' ) ).
  ENDMETHOD.


  METHOD set_action_ranges.

    SELECT * FROM /sew/int_map_act INTO TABLE @DATA(map_act) WHERE molga = @molga.

    LOOP AT map_act ASSIGNING FIELD-SYMBOL(<map_act_line>).
      IF <map_act_line>-is_hire = abap_true.
        /sew/cl_int_constants=>hire_range = VALUE #( ( sign = 'I' option = 'EQ' low = <map_act_line>-action ) ).
      ELSEIF <map_act_line>-is_orgchange = abap_true.
        /sew/cl_int_constants=>orgchange_range = VALUE #( ( sign = 'I' option = 'EQ' low = <map_act_line>-action ) ).
      ELSEIF <map_act_line>-is_termination = abap_true.
        /sew/cl_int_constants=>termination_range = VALUE #( ( sign = 'I' option = 'EQ' low = <map_act_line>-action ) ).
      ELSEIF <map_act_line>-is_rehire = abap_true.
        /sew/cl_int_constants=>rehire_range = VALUE #( ( sign = 'I' option = 'EQ' low = <map_act_line>-action ) ).
      ENDIF.
    ENDLOOP.
*    range = VALUE #( ( sign = 'I' option = 'EQ' low = 'HIRE' )
*                     ( sign = 'I' option = 'CP' low = 'HIRE' )
*                     ( sign = 'I' option = 'CP' low = 'RESIG' )
*                     ( sign = 'I' option = 'CP' low = 'TERM' )
*                     ( sign = 'I' option = 'EQ' low = 'HI' )
*                     ( sign = 'I' option = 'EQ' low = 'RESIGNATION' )
*                     ( sign = 'I' option = 'EQ' low = 'REDUCTION_FORCE' )
*                     ( sign = 'I' option = 'EQ' low = 'TERMINATION' )
*                     ( sign = 'I' option = 'EQ' low = 'TERMINATE_PLACEMENT' ) ).
  ENDMETHOD.
ENDCLASS.
