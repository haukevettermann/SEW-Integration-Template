class /SEW/CL_MIG_EXT_BANK_ACC definition
  public
  create public .

public section.

  data VP_EXT_BANK type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  constants EXTERNAL_BANK_ACCOUNT type STRING value 'ExternalBankAccount' ##NO_TEXT.
  data PERNR type RSDSSELOPT_T .
  data BEGDA type BEGDA .
  data ENDDA type ENDDA .
  data COFU type BOOLEAN .
  data COGL type BOOLEAN .
  data MOLGA type RSDSSELOPT_T .
  data COGU type BOOLEAN .

  methods CONSTRUCTOR
    importing
      !PERNR type RSDSSELOPT_T
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !COGU type BOOLEAN
      !COFU type BOOLEAN
      !COGL type BOOLEAN
      !MOLGA type RSDSSELOPT_T .
  methods CREATE_METADATA
    returning
      value(METADATA) type STRING .
  methods MAP_COFU_DATA
    importing
      !BANKNAME type STRING
      !BANKBRANCH type STRING
      !COUNTRY type BANKS
      !NUMBER type BANKN
      !CURRENCY type PAD_WAERS
      !IBAN type IBAN
    returning
      value(BANK_ACC) type STRING .
protected section.
private section.
ENDCLASS.



CLASS /SEW/CL_MIG_EXT_BANK_ACC IMPLEMENTATION.


METHOD constructor.

  me->pernr = pernr.
  me->begda = begda.
  me->endda = endda.
  me->cofu  = cofu.
  me->cogu  = cogu.
  me->cogl  = cogl.
  me->molga = molga.

  IF cofu EQ abap_true OR
     cogu EQ abap_true.
    vp_ext_bank = VALUE #( ( name = 1  value = /sew/cl_mig_utils=>merge )
                           ( name = 2  value = external_bank_account )
                           ( name = 3  value = 'BankName' )
                           ( name = 3  value = 'BankBranchName' )
                           ( name = 3  value = 'CountryCode' )
                           ( name = 4  value = 'AccountNumber' )
                           ( name = 5  value = 'IBAN' )
                           ( name = 6  value = 'AccountName' )
                           ( name = 7  value = 'AccountType' )
                           ( name = 9  value = 'CurrencyCode' )
                           ( name = 10 value = 'SourceSystemId' )
                           ( name = 11 value = 'SourceSystemOwner' ) ).
  ELSEIF cogl EQ abap_true.
    "Not needed for CoGl
  ENDIF.


ENDMETHOD.


METHOD create_metadata.

  DESCRIBE TABLE vp_ext_bank LINES DATA(length).

  LOOP AT vp_ext_bank ASSIGNING FIELD-SYMBOL(<ext_bank>).

    "set METADATA title
    CASE <ext_bank>-name.
      WHEN 1.
        CONCATENATE /sew/cl_mig_utils=>metadata /sew/cl_mig_utils=>separator INTO metadata.
        CONTINUE.
    ENDCASE.

    CONCATENATE metadata <ext_bank>-value INTO metadata.

    "set separator
    CHECK length NE sy-tabix.
    CONCATENATE metadata /sew/cl_mig_utils=>separator INTO metadata.
  ENDLOOP.

ENDMETHOD.


METHOD map_cofu_data.

  CONCATENATE /sew/cl_mig_utils=>merge
              external_bank_account
              bankname
              bankbranch
              country
              number
              iban
              ''
              ''
              currency
              ''
              ''
  INTO bank_acc SEPARATED BY /sew/cl_mig_utils=>separator.

ENDMETHOD.
ENDCLASS.
