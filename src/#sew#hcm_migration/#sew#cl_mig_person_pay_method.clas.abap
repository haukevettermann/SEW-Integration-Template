class /SEW/CL_MIG_PERSON_PAY_METHOD definition
  public
  create public .

public section.

  data VP_PERSON_PAY type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  constants PERSON_PAYMENT_METHOD type STRING value 'PersonalPaymentMethod' ##NO_TEXT.
  data P0009 type P0009_TAB .
  data BNKA type BF_BNKA .
  data PERNR type RSDSSELOPT_T .
  data BEGDA type BEGDA .
  data ENDDA type ENDDA .
  data COFU type BOOLEAN .
  data COGL type BOOLEAN .
  data MOLGA type RSDSSELOPT_T .
  data BANK_ACC type ref to /SEW/CL_MIG_EXT_BANK_ACC .
  data BANK_OWN type ref to /SEW/CL_MIG_EXT_BANK_OWN .

  methods MAP_MIG_COFU_VALUES
    importing
      !P0009 type P0009
    exporting
      !ORG_PAYMENT_METHOD type /SEW/DD_VALUE .
  methods CONSTRUCTOR
    importing
      !PERNR type RSDSSELOPT_T
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !COFU type BOOLEAN
      !COGL type BOOLEAN
      !MOLGA type RSDSSELOPT_T
      !COGU type BOOLEAN
      !BANK_ACC type ref to /SEW/CL_MIG_EXT_BANK_ACC
      !BANK_OWN type ref to /SEW/CL_MIG_EXT_BANK_OWN .
  methods CREATE_METADATA
    exporting
      value(METADATA_BANK) type STRING
    returning
      value(METADATA) type STRING .
  methods PROCEED_COFU_PERSON_PAY_METHOD
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !WORKER type ref to /SEW/CL_MIG_WORKER
    exporting
      !EXT_BANK_ACC type STRING
      !EXT_BANK_OWN type STRING
    returning
      value(DATA) type STRING .
protected section.
private section.

  data COGU type BOOLEAN .
  data MAPPING_FIELDS_ORG_PAY_METHOD type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPP_FI .
  data MAPPING_VALUES_ORG_PAY_METHOD type /SEW/CL_MIG_UTILS=>/SEW/TT_INT_MAPPING .

  methods GET_MAPPING_COFU_FIELDS .
  methods GET_MAPPING_COFU_VALUES .
  methods GET_COFU_DATA .
  methods GET_COGL_DATA .
  methods MAP_COFU_DATA
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
    exporting
      !EXT_BANK_ACC type STRING
      !EXT_BANK_OWN type STRING
    returning
      value(DATA) type STRING .
  methods MAP_COGL_DATA .
ENDCLASS.



CLASS /SEW/CL_MIG_PERSON_PAY_METHOD IMPLEMENTATION.


METHOD constructor.
  me->pernr = pernr.
  me->begda = begda.
  me->endda = endda.
  me->cofu  = cofu.
  me->cogu  = cogu.
  me->cogl  = cogl.
  me->molga = molga.
  me->bank_acc = bank_acc.
  me->bank_own = bank_own.

  IF cofu EQ abap_true OR
     cogu EQ abap_true.
    vp_person_pay = VALUE #( ( name = 1  value = /sew/cl_mig_utils=>merge )
                             ( name = 2  value = person_payment_method )
                             ( name = 3  value = 'SourceSystemId' )
                             ( name = 3  value = 'SourceSystemOwner' )
                             ( name = 3  value = 'LegislativeDataGroupName' )
                             ( name = 4  value = 'AssignmentNumber' )
                             ( name = 5  value = 'PersonalPaymentMethodCode' )
                             ( name = 6  value = 'EffectiveStartDate' )
                             ( name = 7  value = 'PaymentAmountType' )
                             ( name = 8  value = 'Amount' )
                             ( name = 9  value = 'ProcessingOrder' )
                             ( name = 10 value = 'OrganizationPaymentMethodCode' )
                             ( name = 11 value = 'Percentage' )
                             ( name = 16 value = 'BankNumber' )
                             ( name = 12 value = 'BankName' )
                             ( name = 13 value = 'BankBranchNumber' )
                             ( name = 17 value = 'BankBranchName' )
                             ( name = 14 value = 'BankCountryCode' )
                             ( name = 15 value = 'BankAccountNumber' ) ).
  ELSEIF cogl EQ abap_true.
    "Not needed for CoGl
  ENDIF.

ENDMETHOD.


METHOD create_metadata.

  DESCRIBE TABLE vp_person_pay LINES DATA(length).

  LOOP AT vp_person_pay ASSIGNING FIELD-SYMBOL(<person_pay>).

    "set METADATA title
    CASE <person_pay>-name.
      WHEN 1.
        CONCATENATE /sew/cl_mig_utils=>metadata /sew/cl_mig_utils=>separator INTO metadata.
        CONTINUE.
    ENDCASE.

    CONCATENATE metadata <person_pay>-value INTO metadata.

    "set separator
    CHECK length NE sy-tabix.
    CONCATENATE metadata /sew/cl_mig_utils=>separator INTO metadata.
  ENDLOOP.

ENDMETHOD.


METHOD get_cofu_data.

  "Get IT0009
  SELECT pernr,
         subty,
         begda,
         endda,
         waers,
         zweck,
         bankl,
         banks,
         bankn,
         iban INTO CORRESPONDING FIELDS OF TABLE @p0009 FROM pa0009 WHERE pernr IN @pernr AND
                                                                          begda LE @endda AND
                                                                          endda GE @begda.

  "collect bank keys
  DATA(bankl) = VALUE rsdsselopt_t( FOR <bankl> IN p0009 ( sign = 'I' option = 'EQ' low = <bankl>-bankl ) ).
  SORT bankl BY low.
  DELETE ADJACENT DUPLICATES FROM bankl COMPARING low.

  "Get BNKA
  SELECT bankl,
         banka,
         bnklz INTO CORRESPONDING FIELDS OF TABLE @bnka FROM bnka WHERE bankl IN @bankl.

ENDMETHOD.


METHOD get_cogl_data.
ENDMETHOD.


METHOD GET_MAPPING_COFU_FIELDS.

  "get mapping fields for title
  /sew/cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                   infty        = '0009'
                                                   sap_field    = 'SUBTY'
                                                   oracle_field = 'ORGPAYMETHOD'
                                                   export       = abap_true
                                         IMPORTING mapping_fields = mapping_fields_org_pay_method ).
ENDMETHOD.


METHOD GET_MAPPING_COFU_VALUES.

  "get mapping values for anred
  /sew/cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = '0009'
                                                   sap_field    = 'SUBTY'
                                                   oracle_field = 'ORGPAYMETHOD'
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_org_pay_method ).
ENDMETHOD.


METHOD map_cofu_data.
  DATA: bankbranchcode TYPE string,
        bankbranchname TYPE string,
        counter        TYPE string,
        bank_name      TYPE string,
        pernr_old      type rsdsselopt_t,
        count          TYPE i.

  DATA(sso) = 'SAP_' && sy-mandt.

  LOOP AT p0009 ASSIGNING FIELD-SYMBOL(<p0009>).
    count = count + 1.

    "get BankName
    READ TABLE bnka ASSIGNING FIELD-SYMBOL(<bnka>) WITH KEY bankl = <p0009>-bankl.

    DATA(begda_tmp) = /sew/cl_mig_utils=>convert_date( <p0009>-begda ).

    "get source id
    DATA(src_sys_id) = /sew/cl_mig_utils=>get_src_id( pernr = <p0009>-pernr
                                                      begda = <p0009>-begda
                                                      endda = <p0009>-endda
                                                      vp_src_id = vp_src_id ).

    CHECK src_sys_id IS NOT INITIAL.  "JMB20210811 I

    CHECK <bnka>  IS ASSIGNED.

    map_mig_cofu_values( EXPORTING p0009              = <p0009>
                         IMPORTING org_payment_method = DATA(org_pay_method) ).

    counter = CONV #( count ).
    CONDENSE counter.

    DATA(assignmentnumber) = 'E' && <p0009>-pernr.
    CONCATENATE assignmentnumber '_PPM_' counter INTO DATA(ssid).

    CLEAR: bank_name.
    CONCATENATE <p0009>-banks 'LDG' INTO DATA(leg_group) SEPARATED BY space.
    CONCATENATE <bnka>-banka '_' <bnka>-bankl(5) INTO bank_name.

    "JMB20220211 I - For Netherlands pass bank code
    IF sy-mandt EQ /sew/cl_int_constants=>cofu_mandant-netherlands.
      CONCATENATE <bnka>-banka '_' <bnka>-bnklz INTO bank_name.
    ENDIF.

    CLEAR: bankbranchcode, bankbranchname.

    "for Italy Bank branch code is needed
    IF '15' IN molga.
      bankbranchcode = <p0009>-bankl.
      bankbranchname = <bnka>-banka && '_' && <p0009>-bankl.

    ELSEIF sy-mandt EQ /sew/cl_int_constants=>cofu_mandant-netherlands OR
           '03'     IN molga .
      bankbranchcode = <bnka>-brnch.
      bankbranchname = <bnka>-banka && '_' && <bnka>-bnklz.
    ENDIF.

    DATA(personalpaycode) = conv string( <p0009>-zweck ).

    IF <p0009>-zweck IS INITIAL.
      personalpaycode = 'SALARY'.
    ENDIF.

    CONCATENATE /sew/cl_mig_utils=>merge
                person_payment_method
                ssid
                sso
                leg_group
                assignmentnumber
                personalpaycode
                begda_tmp
                'M'
                ''
                ''
                org_pay_method
                ''
                '' "<p0009>-bankl "JMB20220218 D - C400129651-6652
                bank_name
                bankbranchcode
                bankbranchname
                <p0009>-banks
                <p0009>-bankn
    INTO DATA(data_tmp) SEPARATED BY /sew/cl_mig_utils=>separator.
    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.

    ext_bank_acc = ext_bank_acc && cl_abap_char_utilities=>newline && bank_acc->map_cofu_data( bankname   = bank_name
                                                                                               bankbranch = bankbranchname
                                                                                               country    = <p0009>-banks
                                                                                               number     = <p0009>-bankn
                                                                                               currency   = <p0009>-waers
                                                                                               iban       = <p0009>-iban ).
    DATA(primary) = /sew/cl_mig_utils=>yes.
    IF <p0009>-pernr IN pernr_old AND
       pernr_old     IS NOT INITIAL.
      CLEAR primary.
    ENDIF.

    ext_bank_own = ext_bank_own && cl_abap_char_utilities=>newline && bank_own->map_cofu_data( pernr      = <p0009>-pernr
                                                                                               bankname   = bank_name
                                                                                               bankbranch = bankbranchname
                                                                                               country    = <p0009>-banks
                                                                                               number     = <p0009>-bankn
                                                                                               currency   = <p0009>-waers
                                                                                               primary    = primary ).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0009>-pernr ) to pernr_old.

  ENDLOOP.

ENDMETHOD.


  method MAP_COGL_DATA.
  endmethod.


METHOD map_mig_cofu_values.
  DATA: value_tmp TYPE /sew/dd_value.

  "Process ORG_PAY_METHOD mapping
  value_tmp = CONV #( p0009-subty ).
  /sew/cl_int_mapping=>process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = '0009'
      field_sap      = 'SUBTY'
      field_oracle   = 'ORGPAYMETHOD'
      mapping_fields = CONV #( mapping_fields_org_pay_method )
      mapping_values = CONV #( mapping_values_org_pay_method )
    CHANGING
      value          = value_tmp ).

  org_payment_method = value_tmp.
ENDMETHOD.


METHOD proceed_cofu_person_pay_method.
  get_cofu_data( ).
  get_mapping_cofu_fields( ).
  get_mapping_cofu_values( ).
  data = map_cofu_data( EXPORTING vp_src_id = vp_src_id
                        IMPORTING ext_bank_acc = ext_bank_acc
                                  ext_bank_own = ext_bank_own ).
ENDMETHOD.
ENDCLASS.
