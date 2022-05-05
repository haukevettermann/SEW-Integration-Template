class /SEW/CL_MIG_CONTACT_PHONE definition
  public
  create public .

public section.

  data PERNR type RSDSSELOPT_T .
  data BEGDA type BEGDA .
  data ENDDA type ENDDA .
  data COFU type BOOLEAN .
  data COGL type BOOLEAN .
  data COGU type BOOLEAN .
  data MOLGA type RSDSSELOPT_T .
  data VP_PER_CONTACT_STRUCTURE type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  constants CONTACT_PHONE type STRING value 'ContactPhone' ##NO_TEXT.
  constants PER type STRING value 'PER' ##NO_TEXT.
  constants CONT type STRING value 'CONT' ##NO_TEXT.
  data VORW_UDATA type /SEW/CL_MIG_PERSON_PHONE=>/SEW/VORW_UDATA_LIST .

  methods CONSTRUCTOR
    importing
      !PERNR type RSDSSELOPT_T
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !COFU type BOOLEAN
      !COGL type BOOLEAN
      !MOLGA type RSDSSELOPT_T
      !COGU type BOOLEAN .
  methods PROCEED_COFU_PER_CONTACT_PHONE
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !WORKER type ref to /SEW/CL_MIG_WORKER
    returning
      value(DATA) type STRING .
  methods SPLIT_PHONE_NUMBER_COFU
    importing
      !NUMBER type STRING
      !LEG_CODE type LAND1
      !WERKS type PERSA optional
      !BTRTL type BTRTL optional
      !PHONE_TYPE type STRING
    exporting
      !COUNTRY_CODE type STRING
      !AREA_CODE type STRING
      !PHONE_NUMBER type STRING
      !EXTENSION type STRING .
  class-methods SPLIT_PHONE_NUMBER_COGL
    importing
      !NUMBER type STRING
      !LEG_CODE type LAND1
    exporting
      !COUNTRY_CODE type STRING
      !AREA_CODE type STRING
      !PHONE_NUMBER type STRING
      !EXTENSION type STRING .
  methods CREATE_METADATA
    returning
      value(METADATA) type STRING .
protected section.
private section.

  data P0021 type P0021_TAB .
  data P0001 type P0001_TAB .
  data LAND1_MAP type /IWBEP/T_MGW_NAME_VALUE_PAIR .

  methods GET_COFU_DATA .
  methods MAP_COFU_DATA
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
    returning
      value(DATA) type STRING .
ENDCLASS.



CLASS /SEW/CL_MIG_CONTACT_PHONE IMPLEMENTATION.


METHOD constructor.
  me->pernr = pernr.
  me->begda = begda.
  me->endda = endda.
  me->cofu  = cofu.
  me->cogl  = cogl.
  me->cogu  = cogu.
  me->molga = molga.

  IF cogu EQ abap_true.

  ELSEIF cofu EQ abap_true.

    vp_per_contact_structure = VALUE #( ( name = 1  value = /sew/cl_mig_utils=>merge )
                                        ( name = 2  value =  contact_phone )
                                        ( name = 4  value = 'PhoneType' )
                                        ( name = 5  value = 'DateFrom' )
                                        ( name = 6  value = 'DateTo' )
                                        ( name = 7  value = 'PersonId(SourceSystemId)' )
                                        ( name = 7  value = 'PersonNumber' )
                                        ( name = 8  value = 'PrimaryFlag' )
                                        ( name = 10 value = 'CountryCodeNumber' )
                                        ( name = 11 value = 'AreaCode' )
                                        ( name = 12 value = 'PhoneNumber' )
                                        ( name = 13 value = 'Extension' )
                                        ( name = 14 value = 'LegislationCode')
                                        ( name = 8  value = 'SourceSystemOwner' )
                                        ( name = 9  value = 'SourceSystemId' ) ).
  ELSEIF cogl EQ abap_true.
  ENDIF.

ENDMETHOD.


  METHOD CREATE_METADATA.

    DESCRIBE TABLE vp_per_contact_structure LINES DATA(length).

    LOOP AT vp_per_contact_structure ASSIGNING FIELD-SYMBOL(<person_contact_struc>).

      "set METADATA title
      CASE <person_contact_struc>-name.
        WHEN 1.
          CONCATENATE /sew/cl_mig_utils=>metadata /sew/cl_mig_utils=>separator INTO metadata.
          CONTINUE.
      ENDCASE.

      CONCATENATE metadata <person_contact_struc>-value INTO metadata.

      "set separator
      CHECK length NE sy-tabix.
      CONCATENATE metadata /sew/cl_mig_utils=>separator INTO metadata.

    ENDLOOP.


  ENDMETHOD.


METHOD get_cofu_data.
**JMB20211312 start insert - select only specific subtypes
*
  DATA: famsa TYPE rsdsselopt_t.
  CASE sy-mandt.
    WHEN /sew/cl_int_constants=>cofu_mandant-netherlands.
      famsa = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '1' )
                                  ( sign = 'I' option = 'EQ' low = '13' )
                                  ( sign = 'I' option = 'EQ' low = '15' ) ).
  ENDCASE.
*JMB20211312 insert end

  "Get IT0001
  SELECT pernr,
         begda,
         endda,
         werks,
         btrtl,
         bukrs INTO CORRESPONDING FIELDS OF TABLE @p0001 FROM pa0001 WHERE pernr IN @pernr AND
                                                                           begda LE @endda AND
                                                                           endda GE @begda.

  "get area code
  DATA(werks) = VALUE rsdsselopt_t( FOR <p0001> IN p0001 ( sign = 'I' option = 'EQ' low = <p0001>-werks ) ).
  DATA(btrtl) = VALUE rsdsselopt_t( FOR <p0001> IN p0001 ( sign = 'I' option = 'EQ' low = <p0001>-btrtl ) ).

  DELETE ADJACENT DUPLICATES FROM werks COMPARING low.
  DELETE ADJACENT DUPLICATES FROM btrtl COMPARING low.

  SELECT werks,
         btrtl,
         landesvorw,
         ortsvorw FROM /sew/vorw_udata INTO CORRESPONDING FIELDS OF TABLE @vorw_udata WHERE werks IN @werks AND
                                                                                            btrtl IN @btrtl.

  DATA(bukrs) = VALUE rsdsselopt_t( FOR <p0001> IN p0001 ( sign = 'I' option = 'EQ' low = <p0001>-bukrs ) ).
  SORT bukrs BY low.
  DELETE ADJACENT DUPLICATES FROM bukrs COMPARING low.
  land1_map = /sew/cl_mig_utils=>get_legislation_codes( bukrs ).

  "Read Infotype 0021
  SELECT pernr,
         begda,
         endda,
         zz_telnr,
         zz_telnr2
  INTO CORRESPONDING FIELDS OF TABLE @p0021 FROM pa0021 WHERE pernr IN @pernr
                                                              AND famsa IN @famsa
                                                              AND begda LE @endda
                                                              AND endda GE @begda.
ENDMETHOD.


METHOD map_cofu_data.

  DATA: src_id       TYPE string,
        phone_type   TYPE string,
        number       TYPE string,
        sys_id       TYPE string,
        country_code TYPE string,
        area_code    TYPE string,
        phone_number TYPE string,
        extension    TYPE string,
        land1        TYPE /iwbep/s_mgw_name_value_pair.

  CHECK p0021 IS NOT INITIAL.
  SORT p0021 BY pernr ASCENDING begda ASCENDING.

  DATA(check_pernr) = p0021[ 1 ]-pernr.
  DATA(count) = 0.

  sys_id = 'SAP_' && sy-mandt.

  LOOP AT p0021 ASSIGNING FIELD-SYMBOL(<p0021>).

    IF <p0021>-pernr NE check_pernr.
      count = 1.
      check_pernr = <p0021>-pernr.
    ELSE.
      count = count + 1.
    ENDIF.

    DATA(collect_all) = abap_false.

    WHILE collect_all NE abap_true.
      phone_type = 'H1'.
      number     = <p0021>-zz_telnr.

      IF <p0021>-zz_telnr IS INITIAL.
        phone_type = 'HM'.
        number       = <p0021>-zz_telnr2.
        collect_all = abap_true.
        CLEAR: <p0021>-zz_telnr2.
      ELSE.
        CLEAR: <p0021>-zz_telnr.
      ENDIF.

      IF number IS INITIAL.
        collect_all = abap_true.
        EXIT.
      ENDIF.

      "get legislationcode
      CLEAR land1.
      LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>) WHERE pernr EQ <p0021>-pernr AND
                                                          begda LE <p0021>-endda AND
                                                          endda GE <p0021>-begda.
        READ TABLE land1_map INTO land1 WITH KEY name = <p0001>-bukrs.
        EXIT.
      ENDLOOP.

      CHECK <p0001> IS ASSIGNED.

      split_phone_number_cofu( EXPORTING number       = number
                                         werks        = <p0001>-werks
                                         btrtl        = <p0001>-btrtl
                                         leg_code     = CONV #( land1-value )
                                         phone_type   = phone_type
                               IMPORTING country_code = country_code
                                         area_code    = area_code
                                         phone_number = phone_number
                                         extension    = extension ).

      "in case no country code and phone number is provided, skip entry
      CHECK country_code IS NOT INITIAL AND
            phone_number IS NOT INITIAL.

      DATA(start_date) = /sew/cl_mig_utils=>convert_date( <p0021>-begda ).

      DATA(count_str) = CONV string( count ).
      CONDENSE count_str.

      CONCATENATE per
                  cont
                  <p0021>-pernr
                  count_str
                  INTO DATA(person_id) SEPARATED BY '_'. "PER_CONT_00200518_1

      CONCATENATE contact_phone
                  <p0021>-pernr
                  phone_type
                  count_str
                  INTO src_id SEPARATED BY '_'.

      CONCATENATE /sew/cl_mig_utils=>merge
                  contact_phone
                  phone_type
                  start_date
                  ''
                  person_id
                  ''
                  ''
                  country_code
                  area_code
                  phone_number
                  extension
                  land1-value
                  sys_id
                  src_id
      INTO DATA(data_tmp) SEPARATED BY /sew/cl_mig_utils=>separator.
      CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
    ENDWHILE.
  ENDLOOP.
ENDMETHOD.


METHOD proceed_cofu_per_contact_phone.
  get_cofu_data( ).
  data = map_cofu_data( vp_src_id ).
ENDMETHOD.


METHOD split_phone_number_cofu.
  DATA: number_tmp TYPE string.
  CLEAR: country_code, area_code, phone_number, extension.

  CHECK number IS NOT INITIAL.

  "fallback: pass country code in phone number
  "fallback: pass country code in phone number
  IF strlen( number ) GE 3.
    country_code = number+0(3).
  ELSE.
    DATA(len_number) = strlen( number ).
    country_code = number+0(len_number).
  ENDIF.
  phone_number = number.
  REPLACE ALL OCCURRENCES OF country_code IN phone_number WITH ''.

  "check SAP mandant
  CASE sy-mandt.
    WHEN /sew/cl_int_constants=>cofu_mandant-germany.

      "for mobile phone pass fallback logic above
      CHECK phone_type NE 'WM'.

      READ TABLE vorw_udata ASSIGNING FIELD-SYMBOL(<vorw>) WITH KEY werks = werks
                                                                    btrtl = btrtl.
      CHECK <vorw> IS ASSIGNED.
      country_code = <vorw>-landesvorw.

      DATA(cc_tmp) = cl_hr_se_generic_tools=>get_country_predialing_code( leg_code ).

      REPLACE ALL OCCURRENCES OF '+' IN cc_tmp WITH ''.

      IF cc_tmp NE country_code AND
         cc_tmp IS NOT INITIAL.
        country_code = cc_tmp.
      ENDIF.

      area_code    = <vorw>-ortsvorw.
      phone_number = number.

    WHEN /sew/cl_int_constants=>cofu_mandant-netherlands OR
         /sew/cl_int_constants=>cofu_mandant-austria     OR
         /sew/cl_int_constants=>cofu_mandant-italy       OR
         /sew/cl_int_constants=>cofu_mandant-newzealand  OR
         /sew/cl_int_constants=>cofu_mandant-australia.

      split_phone_number_cogl( EXPORTING number   = number
                                         leg_code = leg_code
                               IMPORTING country_code = country_code
                                         area_code    = area_code
                                         phone_number = phone_number
                                         extension    = extension ).
  ENDCASE.

  REPLACE ALL OCCURRENCES OF '+' IN country_code WITH ''.

ENDMETHOD.


  METHOD SPLIT_PHONE_NUMBER_COGL.
    DATA: number_tmp TYPE string.
    TRY.
        country_code = segment( val = number index = 1 sep = '/' ).
        DATA(cc_tmp) = cl_hr_se_generic_tools=>get_country_predialing_code( leg_code ).

        IF cc_tmp NE country_code.
          country_code = cc_tmp.
        ENDIF.

        REPLACE ALL OCCURRENCES OF '+' IN country_code WITH ''.

        area_code    = segment( val = number index = 2 sep = '/' ).
        phone_number = segment( val = number index = 3 sep = '/' ).
        extension    = segment( val = number index = 4 sep = '/' ).

        DATA(leg_special) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = 'CA' )
                                                ( sign = 'I' option = 'EQ' low = 'US' ) ).

        CHECK leg_code IN leg_special.

        "in case of Canada or USA area code has to have 3 digits and phone number 7 digits
        DATA(area_length) = strlen( area_code ).
        DATA(number_length) = strlen( phone_number ).
        IF strlen( area_code ) NE 3.
          IF area_length GT 3.
            area_length = area_length - 3.
            number_tmp = area_code+3(area_length).
            area_code = area_code+0(3).

            CONCATENATE number_tmp phone_number INTO phone_number.

          ELSEIF area_length LT 3.
            DATA(char_need) = 3 - area_length.
            DATA(phone_len) = strlen( phone_number ).

            IF number_length GE char_need.
              number_tmp = phone_number+0(char_need).

              DATA(number_len) = strlen( number_tmp ).
              phone_len = phone_len - number_len.
              phone_number = phone_number+char_need(phone_len).

              CONCATENATE area_code number_tmp INTO area_code.
            ENDIF.

          ENDIF.
        ENDIF.

        DATA(extension_length) = strlen( extension ).
        number_length = strlen( phone_number ).
        IF number_length NE 7.
          IF number_length GT 7.
            number_length = number_length - 7.
            number_tmp   = phone_number+7(number_length).
            phone_number = phone_number+0(7).

            CONCATENATE number_tmp extension INTO extension.
          ELSEIF number_length LT 7.
            DATA(chr_need)  = 7 - number_length.
            DATA(ext_len) = strlen( extension ).

            IF extension_length GE chr_need.
              number_tmp = extension+0(chr_need).

              DATA(num_len) = strlen( number_tmp ).
              ext_len = ext_len - num_len.
              extension = extension+char_need(ext_len).

              CONCATENATE phone_number number_tmp INTO phone_number.
            ENDIF.
          ENDIF.
        ENDIF.



      CATCH cx_sy_strg_par_val.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
