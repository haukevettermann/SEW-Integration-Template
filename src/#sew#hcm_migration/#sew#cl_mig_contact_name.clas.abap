class /SEW/CL_MIG_CONTACT_NAME definition
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
  data VP_CONTACT_NAME_STRUCTURE type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  constants CONTACT_NAME type STRING value 'ContactName' ##NO_TEXT.
  constants GLOBAL type STRING value 'GLOBAL' ##NO_TEXT.
  constants PER type STRING value 'PER' ##NO_TEXT.
  constants CONT type STRING value 'CONT' ##NO_TEXT.
  constants NAME type STRING value 'NAME' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !PERNR type RSDSSELOPT_T
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !COFU type BOOLEAN
      !COGL type BOOLEAN
      !MOLGA type RSDSSELOPT_T
      !COGU type BOOLEAN .
  methods PROCEED_COFU_CON_NAME
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !WORKER type ref to /SEW/CL_MIG_WORKER
    returning
      value(DATA) type STRING .
  methods CREATE_METADATA
    returning
      value(METADATA) type STRING .
protected section.
private section.

  data P0021 type P0021_TAB .
  data P0002 type P0002_TAB .
  data P0001 type P0001_TAB .
  data LAND1_MAP type /IWBEP/T_MGW_NAME_VALUE_PAIR .

  methods GET_COFU_DATA .
  methods MAP_COFU_DATA
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
    returning
      value(DATA) type STRING .
ENDCLASS.



CLASS /SEW/CL_MIG_CONTACT_NAME IMPLEMENTATION.


  METHOD constructor.
    me->pernr = pernr.
    me->begda = begda.
    me->endda = endda.
    me->cofu  = cofu.
    me->cogu  = cogu.
    me->cogl  = cogl.
    me->molga = molga.

    IF cogl EQ abap_true.

    ELSEIF cofu EQ abap_true.
      vp_contact_name_structure = VALUE #( ( name = 1  value = /sew/cl_mig_utils=>merge )
                                           ( name = 2  value = contact_name )
                                           ( name = 3  value = 'PersonId(SourceSystemId)' )
                                           ( name = 4  value = 'EffectiveStartDate' )
                                           ( name = 5  value = 'EffectiveEndDate' )
                                           ( name = 6  value = 'LegislationCode' )
                                           ( name = 7  value = 'NameType' )
                                           ( name = 8  value = 'FirstName' )
                                           ( name = 9  value = 'MiddleNames' )
                                           ( name = 10 value = 'LastName' )
                                           ( name = 11 value = 'SourceSystemOwner' )
                                           ( name = 12 value = 'SourceSystemId' ) ).
    ELSEIF cogu EQ abap_true.

    ENDIF.
  ENDMETHOD.


  method CREATE_METADATA.

    DESCRIBE TABLE vp_contact_name_structure LINES DATA(length).

    LOOP AT vp_contact_name_structure ASSIGNING FIELD-SYMBOL(<contact_name_struc>).

      "set METADATA title
      CASE <contact_name_struc>-name.
        WHEN 1.
          CONCATENATE /sew/cl_mig_utils=>metadata /sew/cl_mig_utils=>separator INTO metadata.
          CONTINUE.
      ENDCASE.

      CONCATENATE metadata <contact_name_struc>-value INTO metadata.

      "set separator
      CHECK length NE sy-tabix.
      CONCATENATE metadata /sew/cl_mig_utils=>separator INTO metadata.
    ENDLOOP.


  endmethod.


  METHOD get_cofu_data.

    SELECT pernr,
           begda,
           endda,
           sprsl,
           nachn,
           vorna,
           name2,
           rufnm,
           anred,
           titel,
           fnamr,
           lnamr,
           vorsw,
           inits,
           vors2,
           knznm,
           titl2,
           midnm INTO CORRESPONDING FIELDS OF TABLE @p0002 FROM pa0002 WHERE pernr IN @pernr AND
                                                                             begda LE @endda AND
                                                                             endda GE @begda.

    SELECT pernr,
           begda,
           endda,
           famsa,
           seqnr,
           emrgn,
           subty,
           objps,
           fgbdt,
           zz_telnr,
           zz_telnr2,
           zz_art,
           favor,
           fanam,
           fanat,
           fnac2
    INTO CORRESPONDING FIELDS OF TABLE @p0021 FROM pa0021 WHERE pernr IN @pernr
                                                            AND begda LE @endda
                                                            AND endda GE @begda.

    "get BUKRS for LegislationCode
    SELECT pernr,
           begda,
           endda,
           bukrs INTO CORRESPONDING FIELDS OF TABLE @p0001 FROM pa0001 WHERE pernr IN @pernr AND
                                                                             begda LE @endda AND
                                                                             endda GE @begda.

    DATA(bukrs) = VALUE rsdsselopt_t( FOR <p0001> IN p0001 ( sign = 'I' option = 'EQ' low = <p0001>-bukrs ) ).
    SORT bukrs BY low.
    DELETE ADJACENT DUPLICATES FROM bukrs COMPARING low.
    land1_map = /sew/cl_mig_utils=>get_legislation_codes( bukrs ).
  ENDMETHOD.


  METHOD map_cofu_data.

    DATA: src_id TYPE string,
          sys_id TYPE string,
          land1  TYPE  /iwbep/s_mgw_name_value_pair.


    CHECK p0021 IS NOT INITIAL.
    SORT p0021 BY pernr ASCENDING begda ASCENDING.

    DATA(check_pernr) = p0021[ 1 ]-pernr.
    DATA(count) = 0.

    LOOP AT p0021 ASSIGNING FIELD-SYMBOL(<p0021>).

      IF <p0021>-pernr NE check_pernr.
        count = 1.
        check_pernr = <p0021>-pernr.
      ELSE.
        count = count + 1.
      ENDIF.

      DATA(count_str) = CONV string( count ).
      CONDENSE count_str.
**IFT20211119 Start delete
**
*      CLEAR land1.
*      LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>) WHERE pernr EQ <p0021>-pernr AND
*                                                          begda LE <p0021>-endda AND
*                                                          endda GE <p0021>-begda.
*
*        READ TABLE land1_map INTO land1 WITH KEY name = <p0001>-bukrs.
*        EXIT.
*      ENDLOOP.
*IFT20211119 End delete

      CONCATENATE per
                  cont
                  <p0021>-pernr
                  count_str
      INTO DATA(per_id) SEPARATED BY '_'.


      DATA(eff_start_date) = /sew/cl_mig_utils=>convert_date( <p0021>-begda ).
      sys_id = 'SAP_' && sy-mandt.
      src_id = per && '_' && cont && '_' && name && '_' &&  <p0021>-pernr && '_' && count_str.


      CONCATENATE /sew/cl_mig_utils=>merge              "                METADATA|                MERGE|
                  contact_name                          "                ContactName|             ContactName|
                  per_id                                "                PersonId(SourceSystemId)|PER_CONT_00200554|
                  eff_start_date                        "                EffectiveStartDate|      2018/09/26|
                  ''                                    "                EffectiveEndDate|        |
*                  land1-value "IFT20211119 D           "                LegislationCode|         NL|
                  <p0021>-fanat                         "                NameType|                GLOBAL|
                  global                                "                FirstName|               Ariane|
                  <p0021>-favor " FirstName             "                MiddleNames|             |
                  <p0021>-fnac2 " MiddleNames           "                LastName|                Draxler|
                  <p0021>-fanam " LastName              "                SourceSystemOwner|       SAP_190|
                  sys_id                                "                SourceSystemId           PER_CONT_NAME_00200554_1
                  src_id
      INTO DATA(data_tmp) SEPARATED BY /sew/cl_mig_utils=>separator.


      CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.

    ENDLOOP.

**IFT20211116 Start Deletion
*
*    LOOP AT p0002 ASSIGNING FIELD-SYMBOL(<p0002>).
*
*      CLEAR land1.
*      LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>) WHERE pernr EQ <p0002>-pernr AND
*                                                          begda LE <p0002>-endda AND
*                                                          endda GE <p0002>-begda.
*
*        READ TABLE land1_map INTO land1 WITH KEY name = <p0001>-bukrs.
*        EXIT.
*      ENDLOOP.
*
*      CONCATENATE per
*                  cont
*                  <p0002>-pernr
*      INTO DATA(per_id) SEPARATED BY '_'.
*
*      DATA(eff_start_date) = /sew/cl_mig_utils=>convert_date( <p0002>-begda ).
*
*      sys_id = 'SAP_' && sy-mandt.
*      src_id = per && '_' && cont && '_' && name && '_' &&  <p0002>-pernr.
*
*      CONCATENATE /sew/cl_mig_utils=>merge
*                  contact_name
*                  per_id
*                  eff_start_date
*                  ''
*                  land1-value
*                  global
*                  <p0002>-vorna " FirstName
*                  <p0002>-midnm " MiddleNames
*                  <p0002>-nachn " LastName
*                  sys_id
*                  src_id
*      INTO DATA(data_tmp) SEPARATED BY /sew/cl_mig_utils=>separator.
*
*
*      CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
*
*    ENDLOOP.
*IFT20211116 End Deletion
  ENDMETHOD.


  METHOD proceed_cofu_con_name.

    get_cofu_data( ).
*    /sew/cl_mig_utils=>update_begin_date( EXPORTING p0000 = worker->p0000
*                                           CHANGING p0002 = p0002 ).

    data = map_cofu_data( vp_src_id ).

  ENDMETHOD.
ENDCLASS.
