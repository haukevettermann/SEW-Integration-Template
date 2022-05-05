class /SEW/CL_INT_ACTION_HANDLER definition
  public
  final
  create public .

public section.

  class-methods PROCESS_01
    importing
      !CLOUD_ID type /SEW/DD_OBJECTID
      !ACTION type /SEW/INT_IT_AEND
    exporting
      !HAS_ERROR type BOOLE_D
    changing
      !NEW_IT_AEND type /SEW/TT_IT_AEND .
  class-methods PROCESS_03
    importing
      !CLOUD_ID type /SEW/DD_OBJECTID
      !ACTION type /SEW/INT_IT_AEND
    exporting
      !HAS_ERROR type BOOLE_D
    changing
      !NEW_IT_AEND type /SEW/TT_IT_AEND .
  class-methods PROCESS_GENERAL
    importing
      !CLOUD_ID type /SEW/DD_OBJECTID
      !ACTION type /SEW/INT_IT_AEND
    exporting
      !HAS_ERROR type BOOLE_D
    changing
      !NEW_IT_AEND type /SEW/TT_IT_AEND .
  class-methods PROCESS_HC
    importing
      !CLOUD_ID type /SEW/DD_OBJECTID
      !ACTION type /SEW/INT_IT_AEND
    exporting
      !HAS_ERROR type BOOLE_D
    changing
      !NEW_IT_AEND type /SEW/TT_IT_AEND .
  class-methods PROCESS_MASSN_MOLGA
    importing
      !CLOUD_ID type /SEW/DD_OBJECTID
      !ACTION type /SEW/INT_IT_AEND
    exporting
      !HAS_ERROR type BOOLE_D
    changing
      !NEW_IT_AEND type /SEW/TT_IT_AEND .
  class-methods PROCESS_RT
    importing
      !CLOUD_ID type /SEW/DD_OBJECTID
      !ACTION type /SEW/INT_IT_AEND
    exporting
      !HAS_ERROR type BOOLE_D
    changing
      !NEW_IT_AEND type /SEW/TT_IT_AEND .
  class-methods PROCESS_Z1
    importing
      !CLOUD_ID type /SEW/DD_OBJECTID
      !ACTION type /SEW/INT_IT_AEND
    exporting
      !HAS_ERROR type BOOLE_D
    changing
      !NEW_IT_AEND type /SEW/TT_IT_AEND .
  class-methods PROCESS_Z4
    importing
      !CLOUD_ID type /SEW/DD_OBJECTID
      !ACTION type /SEW/INT_IT_AEND
    exporting
      !HAS_ERROR type BOOLE_D
    changing
      !NEW_IT_AEND type /SEW/TT_IT_AEND .
  class-methods PROCESS_Z5
    importing
      !CLOUD_ID type /SEW/DD_OBJECTID
      !ACTION type /SEW/INT_IT_AEND
    exporting
      !HAS_ERROR type BOOLE_D
    changing
      !NEW_IT_AEND type /SEW/TT_IT_AEND .
  class-methods PROCESS_ZZ
    importing
      !CLOUD_ID type /SEW/DD_OBJECTID
      !ACTION type /SEW/INT_IT_AEND
    exporting
      !HAS_ERROR type BOOLE_D
    changing
      !NEW_IT_AEND type /SEW/TT_IT_AEND .
  class-methods PROCESS_ACTION
    importing
      !CLOUD_ID type /SEW/DD_OBJECTID
    exporting
      !HAS_ERROR type BOOLE_D
    changing
      !NEW_IT_AEND type /SEW/TT_IT_AEND .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS /SEW/CL_INT_ACTION_HANDLER IMPLEMENTATION.


  METHOD process_01.
    DATA: classdescr TYPE REF TO cl_abap_classdescr.
    classdescr ?= cl_abap_typedescr=>describe_by_name( '/SEW/CL_INT_ACTION_HANDLER' ).
    DATA(method_molga) = |PROCESS_| && action-action && action-molga.
    READ TABLE classdescr->methods WITH KEY name = method_molga TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL."Molga spefic method availlable
      CALL METHOD /sew/cl_int_action_handler=>(method_molga)
        EXPORTING
          action      = action
        IMPORTING
          has_error   = has_error
        CHANGING
          new_it_aend = new_it_aend.
    ELSE." Logic if not molga specific.
      DATA:
            p0001 TYPE p0001.
*   Process hire logic
*   Check if hire is delimited by Org change -> if that is the case change hire to highdate.
      READ TABLE new_it_aend WITH KEY infty = /sew/cl_int_constants=>it0000 action = /sew/cl_int_constants=>org_change TRANSPORTING NO FIELDS.
      IF sy-subrc IS INITIAL.
        DATA(org_change_exists) = abap_true.
        IF action-endda NE /sew/cl_int_constants=>highdate.
          READ TABLE new_it_aend WITH KEY infty = /sew/cl_int_constants=>it0000 action = /sew/cl_int_constants=>hire ASSIGNING FIELD-SYMBOL(<hire_action>).
          <hire_action>-endda = /sew/cl_int_constants=>highdate.
        ENDIF.
      ENDIF.
*   Loop at timeslices with same begda where action is initial.
      LOOP AT new_it_aend ASSIGNING FIELD-SYMBOL(<infty>) WHERE begda = action-begda AND action IS INITIAL.
        READ TABLE new_it_aend WITH KEY infty = <infty>-infty action = action-action TRANSPORTING NO FIELDS.
        IF sy-subrc IS INITIAL.
          CONTINUE.
        ELSE.
          <infty>-action = action-action.
*       Set initial end date to end date of hire (should be highdate). End dated infotypes might otherwise cause issues when processing.
          IF <infty>-endda NE action-endda.
            <infty>-endda = action-endda.
          ENDIF.
        ENDIF.
      ENDLOOP.
*   Check if hire is delimited without further orgchange ->
      IF action-endda NE /sew/cl_int_constants=>highdate AND org_change_exists NE abap_true.
        APPEND INITIAL LINE TO new_it_aend ASSIGNING FIELD-SYMBOL(<termination>).
        <termination>-pernr = action-pernr.
        <termination>-cloud_id = action-cloud_id.
        <termination>-cloud_pernr = action-cloud_pernr.
        <termination>-aend_id = action-aend_id.
        <termination>-begda = action-endda + 1.
        <termination>-endda = /sew/cl_int_constants=>highdate.
        <termination>-action = /sew/cl_int_constants=>termination.
      ENDIF.

      LOOP AT new_it_aend ASSIGNING FIELD-SYMBOL(<new_it_entry>) WHERE infty NE /sew/cl_int_constants=>it0000 AND cloud_id = cloud_id.
        READ TABLE new_it_aend INTO DATA(infty_0000) WITH KEY infty = /sew/cl_int_constants=>it0000 cloud_id = cloud_id.
        IF <new_it_entry>-begda LT infty_0000-begda.
          <new_it_entry>-begda = infty_0000-begda.
          IF <new_it_entry>-endda LT <new_it_entry>-begda.
            DELETE new_it_aend WHERE aend_id = <new_it_entry>-aend_id.
          ENDIF.
        ELSE.
          CONTINUE.
        ENDIF.
      ENDLOOP.

*   SEW does not use the position interface, therefore dummy position needs to be created in case of hire.
      UNASSIGN <infty>.
      READ TABLE new_it_aend WITH KEY infty = /sew/cl_int_constants=>it0001 action = action-action ASSIGNING <infty>.
      IF sy-subrc IS INITIAL.
        /sew/cl_int_utility=>get_spras_by_molga( EXPORTING molga = <infty>-molga
                                                 IMPORTING spras = DATA(spras)
                                                           langu = DATA(langu) ).

        DATA(infty_operation) = NEW /sew/cl_int_it_operation( int_run = <infty>-int_run
                                                              molga   = action-molga ).
*   Create dummy position and add to infotyp
        DATA(prelp) = CORRESPONDING /sew/prelp( <infty> ).
        /sew/cl_int_type_cast=>prelp_to_pnnnn(
          EXPORTING
            prelp = CONV #( prelp )
          IMPORTING
            pnnnn = p0001 ).
        p0001-plans = infty_operation->create_dummy_pos( orgeh = p0001-orgeh
                                                          begda = action-begda
                                                          endda = action-endda
                                                          langu = CONV #( spras )
                                                          simu = /sew/cl_int_statics=>test_run ).
        "Transfer data to prelp
        /sew/cl_int_infty_proc_xml=>pa_to_prelp( EXPORTING infotype = p0001
                                                           aend_id  = <infty>-aend_id
                                                 IMPORTING prelp    = prelp ).
        <infty>-data1 = prelp-data1.
        <infty>-data2 = prelp-data2.
        <infty>-data3 = prelp-data3.
        <infty>-data4 = prelp-data4.
        <infty>-data5 = prelp-data5.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD process_03.
*   Process termination
    DATA: classdescr TYPE REF TO cl_abap_classdescr.
    classdescr ?= cl_abap_typedescr=>describe_by_name( '/SEW/CL_INT_ACTION_HANDLER' ).
    DATA(method_molga) = |PROCESS_| && action-action && action-molga.
    READ TABLE classdescr->methods WITH KEY name = method_molga TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL."Molga spefici method availlable
      CALL METHOD /sew/cl_int_action_handler=>(method_molga)
        EXPORTING
          action      = action
        IMPORTING
          has_error   = has_error
        CHANGING
          new_it_aend = new_it_aend.
    ELSE." Logic if not molga specific.
      "In case of Termination the termination date must be set for each IT for delimiting
*      READ TABLE new_it_aend WITH KEY action = /sew/cl_int_constants=>termination ASSIGNING FIELD-SYMBOL(<term_action>).
*      IF sy-subrc IS INITIAL.
*        LOOP AT new_it_aend ASSIGNING FIELD-SYMBOL(<action>) WHERE infty = /sew/cl_int_constants=>it0105.
**       Delimit IT0105 with begda of termination + 1 day.
*          <action>-endda = <term_action>-begda + 1.
*          <action>-action = /sew/cl_int_constants=>termination.
*        ENDLOOP.
*        IF <term_action>-pernr IS INITIAL.
*          SELECT SINGLE pernr FROM pa9400 INTO @DATA(pernr) WHERE oracleid = @<term_action>-cloud_id.
*          IF sy-subrc IS INITIAL.
*            LOOP AT new_it_aend ASSIGNING <action>.
*              <action>-pernr = pernr.
*            ENDLOOP.
*          ENDIF.
*        ENDIF.
*      ENDIF.

      LOOP AT new_it_aend ASSIGNING FIELD-SYMBOL(<term_action>) WHERE cloud_id = cloud_id. "WHERE action IN /sew/cl_int_constants=>termination_range.
        IF <term_action>-action IN /sew/cl_int_constants=>termination_range AND <term_action>-active NE 'I'.
          IF <term_action>-infty = /sew/cl_int_constants=>it0000.
            DATA(term_begda) = <term_action>-begda.
            DATA(term_endda) = <term_action>-endda.
          ENDIF.
          LOOP AT new_it_aend ASSIGNING FIELD-SYMBOL(<action>) WHERE infty = /sew/cl_int_constants=>it0105 OR infty = /sew/cl_int_constants=>it0050 AND cloud_id = cloud_id.
*       Delimit IT0105 with begda of termination + 1 day.
            IF <action>-begda LE term_begda AND <action>-endda GE term_begda.
              <action>-endda = <term_action>-begda - 1.
            ENDIF.
            <action>-action = <term_action>-action.
          ENDLOOP.

          IF <term_action>-pernr IS INITIAL.
            SELECT SINGLE pernr FROM pa9400 INTO @DATA(pernr) WHERE oracleid = @<term_action>-cloud_id.
            IF sy-subrc IS INITIAL.
              LOOP AT new_it_aend ASSIGNING <action> WHERE cloud_id = cloud_id.
                <action>-pernr = pernr.
              ENDLOOP.
            ENDIF.
          ENDIF.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD process_action.
    DATA: type_descr        TYPE REF TO cl_abap_typedescr,
          classdescr        TYPE REF TO cl_abap_classdescr,
          custom_classdescr TYPE REF TO cl_abap_classdescr.
    classdescr ?= cl_abap_typedescr=>describe_by_name( '/SEW/CL_INT_ACTION_HANDLER' ).
    DATA(custom_actionhandler) = /sew/cl_int_general_settings=>get_actionhandler( ).
*   Get exception class description
    CALL METHOD cl_abap_classdescr=>describe_by_name
      EXPORTING
        p_name         = custom_actionhandler
      RECEIVING
        p_descr_ref    = type_descr
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2.
    custom_classdescr ?= type_descr.
    DATA(actions) = VALUE /sew/tt_it_aend( FOR action IN new_it_aend WHERE ( infty = /sew/cl_int_constants=>it0000 ) ( action ) ).
    LOOP AT actions ASSIGNING FIELD-SYMBOL(<action>).
      DATA(method) = |PROCESS_| && <action>-action.
      IF custom_classdescr IS BOUND.
        READ TABLE custom_classdescr->methods WITH KEY name = method TRANSPORTING NO FIELDS.
      ELSE.
        sy-subrc = 4.
      ENDIF.
      IF sy-subrc IS INITIAL.
        CALL METHOD (custom_actionhandler)=>(method)
          EXPORTING
            cloud_id    = cloud_id
            action      = <action>
          IMPORTING
            has_error   = has_error
          CHANGING
            new_it_aend = new_it_aend.
      ELSE.
        READ TABLE classdescr->methods WITH KEY name = method TRANSPORTING NO FIELDS.
        IF sy-subrc IS INITIAL.
          CALL METHOD /sew/cl_int_action_handler=>(method)
            EXPORTING
              cloud_id    = cloud_id
              action      = <action>
            IMPORTING
              has_error   = has_error
            CHANGING
              new_it_aend = new_it_aend.
        ELSE.
          /sew/cl_int_action_handler=>process_general(
            EXPORTING
              cloud_id = cloud_id
              action   = <action>
            IMPORTING
              has_error   = has_error
            CHANGING
              new_it_aend = new_it_aend  ).
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD PROCESS_GENERAL.
*   Loop at timeslices with same begda where action is initial.
    LOOP AT new_it_aend ASSIGNING FIELD-SYMBOL(<action>) WHERE begda = action-begda AND action IS INITIAL AND cloud_id = cloud_id.
      <action>-action = action-action.
    ENDLOOP.
  ENDMETHOD.


  METHOD process_hc.
    DATA: classdescr TYPE REF TO cl_abap_classdescr,
          infty_buff TYPE infty,
          subty_buff TYPE subty,
          count_buff TYPE i.
    classdescr ?= cl_abap_typedescr=>describe_by_name( '/SEW/CL_INT_ACTION_HANDLER' ).
    DATA(method_molga) = |PROCESS_| && action-action && action-molga.
    READ TABLE classdescr->methods WITH KEY name = method_molga TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL."Molga spefic method availlable
      CALL METHOD /sew/cl_int_action_handler=>(method_molga)
        EXPORTING
          action      = action
        IMPORTING
          has_error   = has_error
        CHANGING
          new_it_aend = new_it_aend.
    ELSE." Logic if not molga specific.
      DATA:
            p0041 TYPE p0041.
*   Process hire logic
*   Check if hire is delimited by Org change -> if that is the case change hire to highdate.
      LOOP AT new_it_aend ASSIGNING FIELD-SYMBOL(<it_aend>) WHERE infty = /sew/cl_int_constants=>it0000 AND action IN /sew/cl_int_constants=>orgchange_range AND cloud_id = cloud_id.
        DATA(org_change_exists) = abap_true.
        IF action-endda NE /sew/cl_int_constants=>highdate.
          LOOP AT new_it_aend ASSIGNING FIELD-SYMBOL(<hire_action>) WHERE infty = /sew/cl_int_constants=>it0000 AND action IN /sew/cl_int_constants=>hire_range AND cloud_id = cloud_id.
            <hire_action>-endda = /sew/cl_int_constants=>highdate.
          ENDLOOP.
*          READ TABLE new_it_aend WITH KEY infty = /sew/cl_int_constants=>it0000 action = /sew/cl_int_constants=>hire ASSIGNING FIELD-SYMBOL(<hire_action>).
*          <hire_action>-endda = /sew/cl_int_constants=>highdate.
        ENDIF.
      ENDLOOP.
*      READ TABLE new_it_aend WITH KEY infty = /sew/cl_int_constants=>it0000 action = /sew/cl_int_constants=>org_change TRANSPORTING NO FIELDS.
*      IF sy-subrc IS INITIAL.
*        DATA(org_change_exists) = abap_true.
*        IF action-endda NE /sew/cl_int_constants=>highdate.
*          READ TABLE new_it_aend WITH KEY infty = /sew/cl_int_constants=>it0000 action = /sew/cl_int_constants=>hire ASSIGNING FIELD-SYMBOL(<hire_action>).
*          <hire_action>-endda = /sew/cl_int_constants=>highdate.
*        ENDIF.
*      ENDIF.
*   Loop at timeslices with same begda where action is initial.
      LOOP AT new_it_aend ASSIGNING FIELD-SYMBOL(<infty>) WHERE begda = action-begda AND action IS INITIAL AND cloud_id = cloud_id.
        READ TABLE new_it_aend WITH KEY infty = <infty>-infty action = action-action cloud_id = cloud_id TRANSPORTING NO FIELDS.
        IF sy-subrc IS INITIAL.
          CONTINUE.
        ELSE.
          IF <infty>-infty = /sew/cl_int_constants=>it0000 OR <infty>-infty = /sew/cl_int_constants=>it0001 OR <infty>-infty = /sew/cl_int_constants=>it0041.
            <infty>-action = action-action.
          ENDIF.
*       Set initial end date to end date of hire (should be highdate). End dated infotypes might otherwise cause issues when processing.
          IF <infty>-endda NE action-endda.
            <infty>-endda = action-endda.
          ENDIF.
        ENDIF.
      ENDLOOP.
*   Check if hire is delimited without further orgchange ->
*      IF action-endda NE /sew/cl_int_constants=>highdate AND org_change_exists NE abap_true.
*        APPEND INITIAL LINE TO new_it_aend ASSIGNING FIELD-SYMBOL(<termination>).
*        <termination>-pernr = action-pernr.
*        <termination>-cloud_id = action-cloud_id.
*        <termination>-cloud_pernr = action-cloud_pernr.
*        <termination>-aend_id = action-aend_id.
*        <termination>-begda = action-endda + 1.
*        <termination>-endda = /sew/cl_int_constants=>highdate.
*        <termination>-action = /sew/cl_int_constants=>termination.
*      ENDIF.
*      Pending Worker process comes always with date when process was executed in Oracle - change hire date to actual hire date
      READ TABLE new_it_aend INTO DATA(infty_0001) WITH KEY infty = /sew/cl_int_constants=>it0001 cloud_id = cloud_id.
      count_buff = 0.
      LOOP AT new_it_aend ASSIGNING FIELD-SYMBOL(<new_it_entry>) WHERE infty NE /sew/cl_int_constants=>it0000 AND cloud_id = cloud_id.
        READ TABLE new_it_aend INTO DATA(infty_0000) WITH KEY infty = /sew/cl_int_constants=>it0000 cloud_id = cloud_id.
        IF <new_it_entry>-infty = '9400'.
          CONTINUE.
        ENDIF.
        IF infty_buff IS INITIAL.
          count_buff = count_buff + 1.
          infty_buff = <new_it_entry>-infty.
          subty_buff = <new_it_entry>-subty.
        ELSE.
          IF infty_buff = <new_it_entry>-infty AND subty_buff = <new_it_entry>-subty.
            CONTINUE.
          ELSE.
            IF infty_buff NE <new_it_entry>-infty AND subty_buff NE <new_it_entry>-subty.
              CLEAR: infty_buff, count_buff.
            ELSEIF infty_buff = <new_it_entry>-infty AND subty_buff NE <new_it_entry>-subty.
              subty_buff = <new_it_entry>-subty.
            ENDIF.
          ENDIF.
        ENDIF.
*        IF <new_it_entry>-begda LT infty_0000-begda.
*        IF <new_it_entry>-begda = infty_0001-begda.
        <new_it_entry>-begda = infty_0000-begda.
        <new_it_entry>-action = action-action.
        IF <new_it_entry>-endda LT <new_it_entry>-begda.
          DELETE new_it_aend WHERE aend_id = <new_it_entry>-aend_id.
        ENDIF.
*        ELSE.
        CONTINUE.
*        ENDIF.
      ENDLOOP.
*   SEW does not use the position interface, therefore dummy position needs to be created in case of hire.
      UNASSIGN <infty>.
      READ TABLE new_it_aend WITH KEY infty = /sew/cl_int_constants=>it0041 action = action-action cloud_id = cloud_id ASSIGNING <infty>.
      IF sy-subrc IS INITIAL.
*        /sew/cl_int_utility=>get_spras_by_molga( EXPORTING molga = <infty>-molga
*                                                 IMPORTING spras = DATA(spras)
*                                                           langu = DATA(langu) ).
*
*        DATA(infty_operation) = NEW /sew/cl_int_it_operation( int_run = <infty>-int_run
*                                                              molga   = action-molga ).
**   Create dummy position and add to infotyp
        DATA(prelp) = CORRESPONDING /sew/prelp( <infty> ).
        /sew/cl_int_type_cast=>prelp_to_pnnnn(
          EXPORTING
            prelp = CONV #( prelp )
          IMPORTING
            pnnnn = p0041 ).
*        p0001-plans = infty_operation->create_dummy_pos( orgeh = p0001-orgeh
*                                                          begda = action-begda
*                                                          endda = action-endda
*                                                          langu = CONV #( spras )
*                                                          simu = /sew/cl_int_statics=>test_run ).
*
*        p0001-otype = /sew/cl_int_constants=>position.
*
        p0041-dat01 = <infty>-begda.
*        "Transfer data to prelp
        /sew/cl_int_infty_proc_xml=>pa_to_prelp( EXPORTING infotype = p0041
                                                           aend_id  = <infty>-aend_id
                                                 IMPORTING prelp    = prelp ).
        <infty>-data1 = prelp-data1.
        <infty>-data2 = prelp-data2.
        <infty>-data3 = prelp-data3.
        <infty>-data4 = prelp-data4.
        <infty>-data5 = prelp-data5.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD PROCESS_MASSN_MOLGA.
    "Template for molga-specific action handling
    DATA: classdescr TYPE REF TO cl_abap_classdescr.
    classdescr ?= cl_abap_typedescr=>describe_by_name( '/SEW/CL_INT_ACTION_HANDLER' ).
    DATA(method_molga) = |PROCESS_| && action-action && action-molga.
    READ TABLE classdescr->methods WITH KEY name = method_molga TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL."Molga spefici method availlable
      CALL METHOD /SEW/CL_INT_ACTION_HANDLER=>(method_molga)
        EXPORTING
          action      = action
        IMPORTING
          has_error   = has_error
        CHANGING
          new_it_aend = new_it_aend.
    ELSE." Logic if not molga specific.

    ENDIF.
  ENDMETHOD.


  METHOD process_rt.
    DATA: classdescr TYPE REF TO cl_abap_classdescr.
    classdescr ?= cl_abap_typedescr=>describe_by_name( '/SEW/CL_INT_ACTION_HANDLER' ).
    DATA(method_molga) = |PROCESS_| && action-action && action-molga.
    READ TABLE classdescr->methods WITH KEY name = method_molga TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL."Molga spefic method availlable
      CALL METHOD /sew/cl_int_action_handler=>(method_molga)
        EXPORTING
          action      = action
        IMPORTING
          has_error   = has_error
        CHANGING
          new_it_aend = new_it_aend.
    ELSE." Logic if not molga specific.
      DATA:
            p0001 TYPE p0001.

      READ TABLE new_it_aend WITH KEY infty = /sew/cl_int_constants=>it0000 action = action-action cloud_id = cloud_id ASSIGNING FIELD-SYMBOL(<infty>).
*      CLEAR: <infty>-action.

*   SEW does not use the position interface, therefore dummy position needs to be created in case of hire.
      UNASSIGN <infty>.
      READ TABLE new_it_aend WITH KEY infty = /sew/cl_int_constants=>it0001 cloud_id = cloud_id ASSIGNING <infty>.
      IF sy-subrc IS INITIAL.
        /sew/cl_int_utility=>get_spras_by_molga( EXPORTING molga = <infty>-molga
                                                 IMPORTING spras = DATA(spras)
                                                           langu = DATA(langu) ).

        DATA(infty_operation) = NEW /sew/cl_int_it_operation( int_run = <infty>-int_run
                                                              molga   = action-molga ).
*   Create dummy position and add to infotyp
        DATA(prelp) = CORRESPONDING /sew/prelp( <infty> ).
        /sew/cl_int_type_cast=>prelp_to_pnnnn(
          EXPORTING
            prelp = CONV #( prelp )
          IMPORTING
            pnnnn = p0001 ).
        p0001-plans = infty_operation->create_dummy_pos( orgeh = p0001-orgeh
                                                          begda = action-begda
                                                          endda = action-endda
                                                          langu = CONV #( spras )
                                                          simu = /sew/cl_int_statics=>test_run ).

        p0001-otype = /sew/cl_int_constants=>position.

        "Transfer data to prelp
        /sew/cl_int_infty_proc_xml=>pa_to_prelp( EXPORTING infotype = p0001
                                                           aend_id  = <infty>-aend_id
                                                 IMPORTING prelp    = prelp ).
        <infty>-data1 = prelp-data1.
        <infty>-data2 = prelp-data2.
        <infty>-data3 = prelp-data3.
        <infty>-data4 = prelp-data4.
        <infty>-data5 = prelp-data5.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD process_z1.
    DATA: classdescr TYPE REF TO cl_abap_classdescr.
    classdescr ?= cl_abap_typedescr=>describe_by_name( '/SEW/CL_INT_ACTION_HANDLER' ).
    DATA(method_molga) = |PROCESS_| && action-action && action-molga.
    READ TABLE classdescr->methods WITH KEY name = method_molga TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL."Molga spefic method availlable
      CALL METHOD /sew/cl_int_action_handler=>(method_molga)
        EXPORTING
          action      = action
        IMPORTING
          has_error   = has_error
        CHANGING
          new_it_aend = new_it_aend.
    ELSE." Logic if not molga specific.
      DATA:
            p0001 TYPE p0001.
*   Process hire logic
*   Check if hire is delimited by Org change -> if that is the case change hire to highdate.
      LOOP AT new_it_aend ASSIGNING FIELD-SYMBOL(<it_aend>) WHERE infty = /sew/cl_int_constants=>it0000 AND action IN /sew/cl_int_constants=>orgchange_range AND cloud_id = cloud_id.
        DATA(org_change_exists) = abap_true.
        IF action-endda NE /sew/cl_int_constants=>highdate.
          LOOP AT new_it_aend ASSIGNING FIELD-SYMBOL(<hire_action>) WHERE infty = /sew/cl_int_constants=>it0000 AND action IN /sew/cl_int_constants=>hire_range AND cloud_id = cloud_id.
            <hire_action>-endda = /sew/cl_int_constants=>highdate.
          ENDLOOP.
*          READ TABLE new_it_aend WITH KEY infty = /sew/cl_int_constants=>it0000 action = /sew/cl_int_constants=>hire ASSIGNING FIELD-SYMBOL(<hire_action>).
*          <hire_action>-endda = /sew/cl_int_constants=>highdate.
        ENDIF.
      ENDLOOP.

*      READ TABLE new_it_aend WITH KEY infty = /sew/cl_int_constants=>it0000 action = /sew/cl_int_constants=>org_change TRANSPORTING NO FIELDS.
*      IF sy-subrc IS INITIAL.
*        DATA(org_change_exists) = abap_true.
*        IF action-endda NE /sew/cl_int_constants=>highdate.
*          READ TABLE new_it_aend WITH KEY infty = /sew/cl_int_constants=>it0000 action = /sew/cl_int_constants=>hire ASSIGNING FIELD-SYMBOL(<hire_action>).
*          <hire_action>-endda = /sew/cl_int_constants=>highdate.
*        ENDIF.
*      ENDIF.
*   Loop at timeslices with same begda where action is initial.
      LOOP AT new_it_aend ASSIGNING FIELD-SYMBOL(<infty>) WHERE begda = action-begda AND action IS INITIAL AND cloud_id = cloud_id.
        READ TABLE new_it_aend WITH KEY infty = <infty>-infty action = action-action cloud_id = cloud_id TRANSPORTING NO FIELDS.
        IF sy-subrc IS INITIAL.
          CONTINUE.
        ELSE.
          IF <infty>-infty = /sew/cl_int_constants=>it0000 OR <infty>-infty = /sew/cl_int_constants=>it0001 OR <infty>-infty = /sew/cl_int_constants=>it0041.
            <infty>-action = action-action.
          ENDIF.
*       Set initial end date to end date of hire (should be highdate). End dated infotypes might otherwise cause issues when processing.
          IF <infty>-endda NE action-endda.
            <infty>-endda = action-endda.
          ENDIF.
        ENDIF.
      ENDLOOP.
*   Check if hire is delimited without further orgchange ->
      IF action-endda NE /sew/cl_int_constants=>highdate AND org_change_exists NE abap_true.
        APPEND INITIAL LINE TO new_it_aend ASSIGNING FIELD-SYMBOL(<termination>).
        <termination>-pernr = action-pernr.
        <termination>-cloud_id = action-cloud_id.
        <termination>-cloud_pernr = action-cloud_pernr.
        <termination>-aend_id = action-aend_id.
        <termination>-begda = action-endda + 1.
        <termination>-endda = /sew/cl_int_constants=>highdate.
        <termination>-action = /sew/cl_int_constants=>termination.
      ENDIF.
*      Pending Worker process comes always with date when process was executed in Oracle - change hire date to actual hire date
      LOOP AT new_it_aend ASSIGNING FIELD-SYMBOL(<new_it_entry>) WHERE infty NE /sew/cl_int_constants=>it0000 AND cloud_id = cloud_id.
        READ TABLE new_it_aend INTO DATA(infty_0000) WITH KEY infty = /sew/cl_int_constants=>it0000 cloud_id = cloud_id.
        IF <new_it_entry>-begda LT infty_0000-begda.
          <new_it_entry>-begda = infty_0000-begda.
          IF <new_it_entry>-endda LT <new_it_entry>-begda.
            DELETE new_it_aend WHERE aend_id = <new_it_entry>-aend_id.
          ENDIF.
        ELSE.
          CONTINUE.
        ENDIF.
      ENDLOOP.
*   SEW does not use the position interface, therefore dummy position needs to be created in case of hire.
      UNASSIGN <infty>.
      READ TABLE new_it_aend WITH KEY infty = /sew/cl_int_constants=>it0001 action = action-action cloud_id = cloud_id ASSIGNING <infty>.
      IF sy-subrc IS INITIAL.
        /sew/cl_int_utility=>get_spras_by_molga( EXPORTING molga = <infty>-molga
                                                 IMPORTING spras = DATA(spras)
                                                           langu = DATA(langu) ).

        DATA(infty_operation) = NEW /sew/cl_int_it_operation( int_run = <infty>-int_run
                                                              molga   = action-molga ).
*   Create dummy position and add to infotyp
        DATA(prelp) = CORRESPONDING /sew/prelp( <infty> ).
        /sew/cl_int_type_cast=>prelp_to_pnnnn(
          EXPORTING
            prelp = CONV #( prelp )
          IMPORTING
            pnnnn = p0001 ).
        p0001-plans = infty_operation->create_dummy_pos( orgeh = p0001-orgeh
                                                          begda = action-begda
                                                          endda = action-endda
                                                          langu = CONV #( spras )
                                                          simu = /sew/cl_int_statics=>test_run ).

        p0001-otype = /sew/cl_int_constants=>position.

        "Transfer data to prelp
        /sew/cl_int_infty_proc_xml=>pa_to_prelp( EXPORTING infotype = p0001
                                                           aend_id  = <infty>-aend_id
                                                 IMPORTING prelp    = prelp ).
        <infty>-data1 = prelp-data1.
        <infty>-data2 = prelp-data2.
        <infty>-data3 = prelp-data3.
        <infty>-data4 = prelp-data4.
        <infty>-data5 = prelp-data5.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD process_z4.
*   Process termination
    DATA: classdescr TYPE REF TO cl_abap_classdescr.
    classdescr ?= cl_abap_typedescr=>describe_by_name( '/SEW/CL_INT_ACTION_HANDLER' ).
    DATA(method_molga) = |PROCESS_| && action-action && action-molga.
    READ TABLE classdescr->methods WITH KEY name = method_molga TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL."Molga spefici method availlable
      CALL METHOD /sew/cl_int_action_handler=>(method_molga)
        EXPORTING
          action      = action
        IMPORTING
          has_error   = has_error
        CHANGING
          new_it_aend = new_it_aend.
    ELSE." Logic if not molga specific.
      "In case of Termination the termination date must be set for each IT for delimiting
      LOOP AT new_it_aend ASSIGNING FIELD-SYMBOL(<term_action>) WHERE cloud_id = cloud_id. "WHERE action IN /sew/cl_int_constants=>termination_range.
        IF <term_action>-action IN /sew/cl_int_constants=>termination_range AND <term_action>-active NE 'I'.
          IF <term_action>-infty = /sew/cl_int_constants=>it0000.
            DATA(term_begda) = <term_action>-begda.
            DATA(term_endda) = <term_action>-endda.
          ENDIF.
          LOOP AT new_it_aend ASSIGNING FIELD-SYMBOL(<action>) WHERE infty = /sew/cl_int_constants=>it0105 OR infty = /sew/cl_int_constants=>it0050 AND cloud_id = cloud_id.
*       Delimit IT0105 with begda of termination + 1 day.
            IF <action>-begda LE term_begda AND <action>-endda GE term_begda.
              <action>-endda = <term_action>-begda - 1.
            ENDIF.
            <action>-action = <term_action>-action.
          ENDLOOP.

          IF <term_action>-pernr IS INITIAL.
            SELECT SINGLE pernr FROM pa9400 INTO @DATA(pernr) WHERE oracleid = @<term_action>-cloud_id.
            IF sy-subrc IS INITIAL.
              LOOP AT new_it_aend ASSIGNING <action> WHERE cloud_id = cloud_id.
                <action>-pernr = pernr.
              ENDLOOP.
            ENDIF.
          ENDIF.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD process_z5.
    DATA: classdescr TYPE REF TO cl_abap_classdescr.
    classdescr ?= cl_abap_typedescr=>describe_by_name( '/SEW/CL_INT_ACTION_HANDLER' ).
    DATA(method_molga) = |PROCESS_| && action-action && action-molga.
    READ TABLE classdescr->methods WITH KEY name = method_molga TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL."Molga spefic method availlable
      CALL METHOD /sew/cl_int_action_handler=>(method_molga)
        EXPORTING
          action      = action
        IMPORTING
          has_error   = has_error
        CHANGING
          new_it_aend = new_it_aend.
    ELSE." Logic if not molga specific.
      DATA:
            p0001 TYPE p0001.
*   Process hire logic
*   Check if hire is delimited by Org change -> if that is the case change hire to highdate.
*      LOOP AT new_it_aend ASSIGNING FIELD-SYMBOL(<it_aend>) WHERE infty = /sew/cl_int_constants=>it0000 AND action IN /sew/cl_int_constants=>orgchange_range.
*        DATA(org_change_exists) = abap_true.
*        IF action-endda NE /sew/cl_int_constants=>highdate.
*          LOOP AT new_it_aend ASSIGNING FIELD-SYMBOL(<hire_action>) WHERE infty = /sew/cl_int_constants=>it0000 AND action IN /sew/cl_int_constants=>hire_range.
*            <hire_action>-endda = /sew/cl_int_constants=>highdate.
*          ENDLOOP.
**          READ TABLE new_it_aend WITH KEY infty = /sew/cl_int_constants=>it0000 action = /sew/cl_int_constants=>hire ASSIGNING FIELD-SYMBOL(<hire_action>).
**          <hire_action>-endda = /sew/cl_int_constants=>highdate.
*        ENDIF.
*      ENDLOOP.


*   Loop at timeslices with same begda where action is initial.
*      LOOP AT new_it_aend ASSIGNING FIELD-SYMBOL(<infty>) WHERE begda = action-begda AND action IS INITIAL.
*        READ TABLE new_it_aend WITH KEY infty = <infty>-infty action = action-action TRANSPORTING NO FIELDS.
*        IF sy-subrc IS INITIAL.
*          CONTINUE.
*        ELSE.
*          IF <infty>-infty = /sew/cl_int_constants=>it0000 OR <infty>-infty = /sew/cl_int_constants=>it0001 OR <infty>-infty = /sew/cl_int_constants=>it0041.
*            <infty>-action = action-action.
*          ENDIF.
**       Set initial end date to end date of hire (should be highdate). End dated infotypes might otherwise cause issues when processing.
*          IF <infty>-endda NE action-endda.
*            <infty>-endda = action-endda.
*          ENDIF.
*        ENDIF.
*      ENDLOOP.

      LOOP AT new_it_aend ASSIGNING FIELD-SYMBOL(<infty>) WHERE active IS INITIAL AND cloud_id = cloud_id.
        IF <infty>-infty = /sew/cl_int_constants=>it0000 AND <infty>-action IN /sew/cl_int_constants=>termination_range.
          <infty>-active = 'I'.
        ENDIF.
        IF <infty>-action IN /sew/cl_int_constants=>termination_range.
          CLEAR: <infty>-action.
        ENDIF.
        IF <infty>-infty = /sew/cl_int_constants=>it0001 AND <infty>-begda = action-begda.
          <infty>-action = action-action.
        ENDIF.
      ENDLOOP.

*   Check if hire is delimited without further orgchange ->
*      IF action-endda NE /sew/cl_int_constants=>highdate AND org_change_exists NE abap_true.
*        APPEND INITIAL LINE TO new_it_aend ASSIGNING FIELD-SYMBOL(<termination>).
*        <termination>-pernr = action-pernr.
*        <termination>-cloud_id = action-cloud_id.
*        <termination>-cloud_pernr = action-cloud_pernr.
*        <termination>-aend_id = action-aend_id.
*        <termination>-begda = action-endda + 1.
*        <termination>-endda = /sew/cl_int_constants=>highdate.
*        <termination>-action = /sew/cl_int_constants=>termination.
*      ENDIF.
*      Pending Worker process comes always with date when process was executed in Oracle - change hire date to actual hire date
*      LOOP AT new_it_aend ASSIGNING FIELD-SYMBOL(<new_it_entry>) WHERE infty NE /sew/cl_int_constants=>it0000.
*        READ TABLE new_it_aend INTO DATA(infty_0000) WITH KEY infty = /sew/cl_int_constants=>it0000.
*        IF <new_it_entry>-begda LT infty_0000-begda.
*          <new_it_entry>-begda = infty_0000-begda.
*          IF <new_it_entry>-endda LT <new_it_entry>-begda.
*            DELETE new_it_aend WHERE aend_id = <new_it_entry>-aend_id.
*          ENDIF.
*        ELSE.
*          CONTINUE.
*        ENDIF.
*      ENDLOOP.
*   SEW does not use the position interface, therefore dummy position needs to be created in case of hire.
      UNASSIGN <infty>.
      READ TABLE new_it_aend WITH KEY infty = /sew/cl_int_constants=>it0001 action = action-action cloud_id = cloud_id ASSIGNING <infty>.
      IF sy-subrc IS INITIAL.
        /sew/cl_int_utility=>get_spras_by_molga( EXPORTING molga = <infty>-molga
                                                 IMPORTING spras = DATA(spras)
                                                           langu = DATA(langu) ).

        DATA(infty_operation) = NEW /sew/cl_int_it_operation( int_run = <infty>-int_run
                                                              molga   = action-molga ).
*   Create dummy position and add to infotyp
        DATA(prelp) = CORRESPONDING /sew/prelp( <infty> ).
        /sew/cl_int_type_cast=>prelp_to_pnnnn(
          EXPORTING
            prelp = CONV #( prelp )
          IMPORTING
            pnnnn = p0001 ).
        p0001-plans = infty_operation->create_dummy_pos( orgeh = p0001-orgeh
                                                          begda = action-begda
                                                          endda = action-endda
                                                          langu = CONV #( spras )
                                                          simu = /sew/cl_int_statics=>test_run ).

        p0001-otype = /sew/cl_int_constants=>position.

        "Transfer data to prelp
        /sew/cl_int_infty_proc_xml=>pa_to_prelp( EXPORTING infotype = p0001
                                                           aend_id  = <infty>-aend_id
                                                 IMPORTING prelp    = prelp ).
        <infty>-data1 = prelp-data1.
        <infty>-data2 = prelp-data2.
        <infty>-data3 = prelp-data3.
        <infty>-data4 = prelp-data4.
        <infty>-data5 = prelp-data5.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD process_zz.
*   Process no show
    DATA: classdescr TYPE REF TO cl_abap_classdescr.
    classdescr ?= cl_abap_typedescr=>describe_by_name( '/SEW/CL_INT_ACTION_HANDLER' ).
    DATA(method_molga) = |PROCESS_| && action-action && action-molga.
    READ TABLE classdescr->methods WITH KEY name = method_molga TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL."Molga spefici method availlable
      CALL METHOD /sew/cl_int_action_handler=>(method_molga)
        EXPORTING
          action      = action
        IMPORTING
          has_error   = has_error
        CHANGING
          new_it_aend = new_it_aend.
    ELSE." Logic if not molga specific.
      "In case of Termination the termination date must be set for each IT for delimiting
      LOOP AT new_it_aend ASSIGNING FIELD-SYMBOL(<term_action>) WHERE cloud_id = cloud_id. "WHERE action IN /sew/cl_int_constants=>termination_range.
        IF <term_action>-action = 'ZZ'." AND <term_action>-active NE 'I'.
          IF <term_action>-infty = /sew/cl_int_constants=>it0000.
            DATA(term_begda) = <term_action>-begda.
            DATA(term_endda) = <term_action>-endda.
          ENDIF.
          IF <term_action>-infty NE /sew/cl_int_constants=>it0000.

          ENDIF.
          LOOP AT new_it_aend ASSIGNING FIELD-SYMBOL(<action>) WHERE infty = /sew/cl_int_constants=>it0105 OR infty = /sew/cl_int_constants=>it0050 AND cloud_id = cloud_id.
*       Delimit IT0105 with begda of termination + 1 day.
            IF <action>-begda LE term_begda AND <action>-endda GE term_begda.
              <action>-endda = <term_action>-begda - 1.
            ENDIF.
            <action>-action = <term_action>-action.
          ENDLOOP.

          IF <term_action>-pernr IS INITIAL.
            SELECT SINGLE pernr FROM pa9400 INTO @DATA(pernr) WHERE oracleid = @<term_action>-cloud_id.
            IF sy-subrc IS INITIAL.
              LOOP AT new_it_aend ASSIGNING <action> WHERE cloud_id = cloud_id.
                <action>-pernr = pernr.
              ENDLOOP.
            ENDIF.
          ENDIF.
          EXIT.
        ENDIF.
      ENDLOOP.

      LOOP AT new_it_aend ASSIGNING FIELD-SYMBOL(<adjust_date>) WHERE begda = term_begda AND cloud_id = cloud_id.
        <adjust_date>-begda = term_begda - 1.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
