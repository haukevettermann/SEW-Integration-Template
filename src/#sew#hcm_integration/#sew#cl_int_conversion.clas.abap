class /SEW/CL_INT_CONVERSION definition
  public
  final
  create public .

public section.

  data MOLGA type MOLGA .

  methods CONSTRUCTOR
    importing
      !MOLGA type MOLGA optional .
  class-methods CONVERT_ALLDF
    importing
      !VALUE_IN type /SEW/DD_VALUE
    returning
      value(VALUE_OUT) type /SEW/DD_VALUE .
  class-methods CONVERT_AWORG
    importing
      !VALUE_IN type /SEW/DD_VALUE
    returning
      value(VALUE_OUT) type /SEW/DD_VALUE .
  class-methods CONVERT_AWREF
    importing
      !VALUE_IN type /SEW/DD_VALUE
    returning
      value(VALUE_OUT) type /SEW/DD_VALUE .
  class-methods GET_INSTANCE
    importing
      !MOLGA type MOLGA optional
    returning
      value(INSTANCE) type ref to /SEW/CL_INT_CONVERSION .
  class-methods CONVERT_QUONR
    importing
      !VALUE_IN type /SEW/DD_VALUE
    returning
      value(VALUE_OUT) type /SEW/DD_VALUE .
  class-methods CONVERT_PDC_USRUP
    importing
      !VALUE_IN type /SEW/DD_VALUE
    returning
      value(VALUE_OUT) type /SEW/DD_VALUE .
  class-methods CONVERT_KTART
    importing
      !VALUE_IN type /SEW/DD_VALUE
    returning
      value(VALUE_OUT) type /SEW/DD_VALUE .
  class-methods CONVERT_VTKEN
    importing
      !VALUE_IN type /SEW/DD_VALUE
    returning
      value(VALUE_OUT) type /SEW/DD_VALUE .
  class-methods CONVERT_FLAG1
    importing
      !VALUE_IN type /SEW/DD_VALUE
    returning
      value(VALUE_OUT) type /SEW/DD_VALUE .
  class-methods CONVERT_TIME_TO_SAP_TIME
    importing
      !VALUE_IN type /SEW/DD_VALUE
    returning
      value(VALUE_OUT) type /SEW/DD_VALUE .
  class-methods CONVERT_DATE_ORACLE_OM
    importing
      !VALUE_IN type /SEW/DD_VALUE
    returning
      value(VALUE_OUT) type /SEW/DD_VALUE .
  class-methods CONVERT_DATE_ORACLE
    importing
      !VALUE_IN type /SEW/DD_VALUE
    returning
      value(VALUE_OUT) type /SEW/DD_VALUE .
  class-methods CONVERT_DATETIME_TO_TIME
    importing
      !VALUE_IN type /SEW/DD_VALUE
    returning
      value(VALUE_OUT) type /SEW/DD_VALUE .
  class-methods CONVERT_DATETIME_TO_DATE
    importing
      !VALUE_IN type /SEW/DD_VALUE
    returning
      value(VALUE_OUT) type /SEW/DD_VALUE .
  class-methods CONVERT_DATE
    importing
      !VALUE_IN type /SEW/DD_VALUE
    returning
      value(VALUE_OUT) type /SEW/DD_VALUE .
  class-methods CONVERT_COUNTRY_CODE
    importing
      !VALUE_IN type /SEW/DD_VALUE
    returning
      value(VALUE_OUT) type /SEW/DD_VALUE .
  class-methods CONVERT_POS
    importing
      !VALUE_IN type /SEW/DD_VALUE
    returning
      value(VALUE_OUT) type /SEW/DD_VALUE .
  class-methods CONVERT_TRFST
    importing
      !VALUE_IN type /SEW/DD_VALUE
    returning
      value(VALUE_OUT) type /SEW/DD_VALUE .
  class-methods CONVERT_TRFGR
    importing
      !VALUE_IN type /SEW/DD_VALUE
    returning
      value(VALUE_OUT) type /SEW/DD_VALUE .
  class-methods CONVERT_TRFGB
    importing
      !VALUE_IN type /SEW/DD_VALUE
    returning
      value(VALUE_OUT) type /SEW/DD_VALUE .
  class-methods CONVERT_TRFAR
    importing
      !VALUE_IN type /SEW/DD_VALUE
    returning
      value(VALUE_OUT) type /SEW/DD_VALUE .
  class-methods CONVERT_TERMINALID
    importing
      !VALUE_IN type /SEW/DD_VALUE
    returning
      value(VALUE_OUT) type /SEW/DD_VALUE .
  class-methods CONVERT_WERKS
    importing
      !VALUE_IN type /SEW/DD_VALUE
    returning
      value(VALUE_OUT) type /SEW/DD_VALUE .
  class-methods CONVERT_SOBID
    importing
      !VALUE_IN type /SEW/DD_VALUE
    returning
      value(VALUE_OUT) type /SEW/DD_VALUE .
  class-methods CONVERT_PA_CLOUDID
    importing
      !VALUE_IN type /SEW/DD_VALUE
    returning
      value(VALUE_OUT) type /SEW/DD_VALUE .
  class-methods CONVERT_OM_CLOUDID
    importing
      !VALUE_IN type /SEW/DD_VALUE
    returning
      value(VALUE_OUT) type /SEW/DD_VALUE .
  class-methods CONVERT_KST01
    importing
      !VALUE_IN type /SEW/DD_VALUE
    returning
      value(VALUE_OUT) type /SEW/DD_VALUE .
  class-methods CONVERT_KPR01
    importing
      !VALUE_IN type /SEW/DD_VALUE
    returning
      value(VALUE_OUT) type /SEW/DD_VALUE .
  class-methods CONVERT_KOSTL
    importing
      !VALUE_IN type /SEW/DD_VALUE
    returning
      value(VALUE_OUT) type /SEW/DD_VALUE .
  class-methods CONVERT_KOKRS
    importing
      !VALUE_IN type /SEW/DD_VALUE
    returning
      value(VALUE_OUT) type /SEW/DD_VALUE .
  class-methods CONVERT_KBU01
    importing
      !VALUE_IN type /SEW/DD_VALUE
    returning
      value(VALUE_OUT) type /SEW/DD_VALUE .
  class-methods CONVERT_BTRTL
    importing
      !VALUE_IN type /SEW/DD_VALUE
    returning
      value(VALUE_OUT) type /SEW/DD_VALUE .
  class-methods CONVERT_BDEGR
    importing
      !VALUE_IN type /SEW/DD_VALUE
    returning
      value(VALUE_OUT) type /SEW/DD_VALUE .
protected section.
private section.

  class-data INSTANCE type ref to /SEW/CL_INT_CONVERSION .
ENDCLASS.



CLASS /SEW/CL_INT_CONVERSION IMPLEMENTATION.


METHOD constructor.

  me->molga = molga.

ENDMETHOD.


METHOD convert_alldf.

  CHECK value_in IS NOT INITIAL.

  value_out = abap_false.
  CHECK value_in EQ 'Y'.
  value_out = abap_true.

ENDMETHOD.


METHOD CONVERT_AWORG.

  CHECK value_in IS NOT INITIAL.

  DATA(length) = strlen( value_in ).

  CHECK length GT 10.
  DATA(last_char) = length - 10.

  value_out = value_in+10(last_char).

ENDMETHOD.


METHOD convert_awref.

  CHECK value_in IS NOT INITIAL.

  DATA(length) = strlen( value_in ).

  value_out = value_in.
  CHECK length GT 10.

  value_out = value_in+0(10).

ENDMETHOD.


  METHOD convert_bdegr.
* TODO
    CHECK value_in IS NOT INITIAL.
*    value_out = value_in+0(4).

    DATA(l_offset) = strlen( value_in ) - 3.
    IF l_offset > 0.
      value_out = value_in+l_offset(3).
    ENDIF.

  ENDMETHOD.


  METHOD convert_btrtl.
* TODO
    CHECK value_in IS NOT INITIAL.
    value_out = value_in+4(4).
  ENDMETHOD.


  METHOD convert_country_code.
    SELECT SINGLE molga FROM t500l INTO @value_out WHERE intca EQ @value_in.
*    TEST PURPOSE
      value_out = '01'.
  ENDMETHOD.


  METHOD convert_date.
    CHECK value_in IS NOT INITIAL.
    value_out = value_in.
*   Check if it is already in SAP Format
    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                      = CONV dats( value_out )
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS                    = 2.
    CHECK sy-subrc IS NOT INITIAL.
    value_out = CONV dats( value_out+0(4) && value_out+5(2) && value_out+8(2) ).
    IF CONV dats( value_out ) = /sew/cl_int_constants=>highdate_oracle.
      value_out = /sew/cl_int_constants=>highdate.
    ENDIF.
  ENDMETHOD.


  METHOD CONVERT_DATETIME_TO_DATE.
    CHECK value_in IS NOT INITIAL.
    value_out = value_in.
*   Check if it is already in SAP Format
    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                      = CONV dats( value_out )
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS                    = 2.
    CHECK sy-subrc IS NOT INITIAL.
    value_out = CONV dats( value_out+0(4) && value_out+5(2) && value_out+8(2) ).
    IF CONV dats( value_out ) = /sew/cl_int_constants=>highdate_oracle.
      value_out = /sew/cl_int_constants=>highdate.
    ENDIF.
  ENDMETHOD.


  METHOD convert_datetime_to_time.
    CHECK value_in IS NOT INITIAL.
    value_out = value_in.
    value_out = CONV tims( value_out+11(2) && value_out+14(2) && value_out+17(2) ).

  ENDMETHOD.


  METHOD CONVERT_DATE_ORACLE.
    CHECK value_in IS NOT INITIAL.
    value_out = |{ value_in+0(4) }/{ value_in+4(2) }/{ value_in+6(2) } 00:00:00|.
  ENDMETHOD.


  METHOD CONVERT_DATE_ORACLE_OM.
    CHECK value_in IS NOT INITIAL.
    value_out = |{ value_in+0(4) }/{ value_in+4(2) }/{ value_in+6(2) } |.
  ENDMETHOD.


METHOD convert_flag1.
  CLEAR: value_out.
  CHECK value_in IS NOT INITIAL.
  CHECK value_in EQ 'ORA_WITHDRAWN'.
  value_out = abap_true.
ENDMETHOD.


  METHOD convert_kbu01.
    DATA:
          kostl TYPE p0001-kostl.
    SPLIT value_in AT '-' INTO DATA(split1) DATA(split2).

    value_out = split1.
  ENDMETHOD.


  METHOD convert_kokrs.
    DATA:
          kostl TYPE p0001-kostl.
    SPLIT value_in AT '-' INTO DATA(split1) DATA(split2).
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = split2
*      IMPORTING
*        output = kostl.
    value_out = split1.
  ENDMETHOD.


  METHOD convert_kostl.
    DATA:
          kostl TYPE p0001-kostl.
    SPLIT value_in AT '-' INTO DATA(split1) DATA(split2).
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = split2
      IMPORTING
        output = kostl.
    value_out = kostl.
  ENDMETHOD.


  METHOD convert_kpr01.
*    DATA:
*          kostl TYPE p0001-kostl.
*    SPLIT value_in AT '-' INTO DATA(split1) DATA(split2).
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = split2
*      IMPORTING
*        output = kostl.
*    value_out = kostl.

    value_out = value_in * 100.
    REPLACE ALL OCCURRENCES OF '.' IN value_out WITH ','.

  ENDMETHOD.


  METHOD CONVERT_KST01.
    DATA:
          kostl TYPE p0001-kostl.
    SPLIT value_in AT '-' INTO DATA(split1) DATA(split2).
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = split2
      IMPORTING
        output = kostl.
    value_out = kostl.
  ENDMETHOD.


METHOD CONVERT_KTART.

  CHECK value_in IS NOT INITIAL.

  DATA(molga) = get_instance( )->molga.

  SELECT SINGLE ktart FROM /sew/int_abk_pln INTO value_out WHERE molga    EQ molga AND
                                                                 planname EQ value_in AND
                                                                 endda    GE sy-datum AND
                                                                 begda    LE sy-datum.
ENDMETHOD.


  METHOD convert_om_cloudid.
* TODO
    CHECK value_in IS NOT INITIAL.
    SELECT SINGLE objid FROM hrp9401 INTO value_out WHERE oracleid = value_in. "AND begda LE field-endda AND endda GE field-begda.
  ENDMETHOD.


  METHOD convert_pa_cloudid.
* TODO
    CHECK value_in IS NOT INITIAL.
    SELECT SINGLE pernr FROM pa9400 INTO value_out WHERE oraclepernr = value_in. "AND begda LE field-endda AND endda GE field-begda.
    IF value_out IS INITIAL.
      SELECT SINGLE pernr FROM pa9400 INTO value_out WHERE pernr = value_in.
      IF value_out IS INITIAL.
        value_out = value_in.
      ENDIF.
    ENDIF.
  ENDMETHOD.


METHOD convert_pdc_usrup.
  "needed for WebClock events
  IF value_in EQ 'ORA_HWM_IN'.
    value_out = 'P10'.
  ENDIF.

  IF value_in EQ 'ORA_HWM_OUT'.
    value_out = 'P20'.
  ENDIF.
ENDMETHOD.


  METHOD convert_pos.
    CHECK value_in IS NOT INITIAL.
    SELECT SINGLE objid FROM hrp9401 INTO value_out WHERE oracleid = value_in.
  ENDMETHOD.


METHOD convert_quonr.

  CHECK value_in IS NOT INITIAL.

  SPLIT value_in AT '_' INTO DATA(pernr) DATA(quonr).

  CHECK quonr IS NOT INITIAL.

  value_out = quonr.

ENDMETHOD.


  METHOD convert_sobid.
* TODO
    value_out = value_in.
    REPLACE ALL OCCURRENCES OF '-' IN value_out WITH ''.
  ENDMETHOD.


  METHOD convert_terminalid.
* TODO
    CHECK value_in IS NOT INITIAL.
    value_out = value_in.

    IF value_in EQ /sew/cl_int_constants=>web_clock_id_oracle.
      value_out = /sew/cl_int_constants=>web_clock_id_sap.
    ENDIF.

  ENDMETHOD.


METHOD convert_time_to_sap_time.
  CHECK value_in IS NOT INITIAL.
  value_out = value_in.
  REPLACE ALL OCCURRENCES OF ':' IN value_out WITH ''.

  "in case milliseconds is missing
  CHECK strlen( value_out ) LT 6.
  CONCATENATE value_out '00' INTO value_out.

ENDMETHOD.


  METHOD convert_trfar.
    DATA:
          kostl TYPE p0001-kostl.
    CHECK value_in IS NOT INITIAL.
    SPLIT value_in AT '_' INTO DATA(split1) DATA(split2).
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = split2
*      IMPORTING
*        output = kostl.
    value_out = split1.
  ENDMETHOD.


  METHOD convert_trfgb.
    DATA:
          kostl TYPE p0001-kostl.
    CHECK value_in IS NOT INITIAL.
    SPLIT value_in AT '_' INTO DATA(split1) DATA(split2).
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = split2
*      IMPORTING
*        output = kostl.
    value_out = split2.
  ENDMETHOD.


  METHOD convert_trfgr.
    CHECK value_in IS NOT INITIAL.
    value_out = value_in.
    REPLACE '.' IN value_out WITH ','.

  ENDMETHOD.


  METHOD convert_trfst.

    CHECK value_in IS NOT INITIAL.
    IF value_in NE 'NVT'.
      value_out = '9'.
    ELSE.
      value_out = 'DELETED'.
    ENDIF.
  ENDMETHOD.


METHOD CONVERT_VTKEN.

  CHECK value_in IS NOT INITIAL.

  value_out = abap_false.
  CHECK value_in EQ 'Y'.
  value_out = abap_true.

ENDMETHOD.


  METHOD CONVERT_WERKS.
* TODO
    CHECK value_in IS NOT INITIAL.
    value_out = value_in+0(4).
  ENDMETHOD.


METHOD get_instance.
  IF /sew/cl_int_conversion=>instance IS BOUND.
    instance = /sew/cl_int_conversion=>instance.
  ELSE.
    CREATE OBJECT /sew/cl_int_conversion=>instance EXPORTING molga = molga.
    instance = /sew/cl_int_conversion=>instance.
  ENDIF.
ENDMETHOD.
ENDCLASS.
