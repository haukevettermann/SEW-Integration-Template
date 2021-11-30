class /SEW/CL_MIG_CONTACT definition
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
  data P0002 type P0002_TAB .
  data VP_PER_CONTACT_STRUCTURE type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  constants CONTACT type STRING value 'Contact' ##NO_TEXT.
  constants PER type STRING value 'PER' ##NO_TEXT.
  constants CONT type STRING value 'CONT' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !PERNR type RSDSSELOPT_T
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !COFU type BOOLEAN
      !COGL type BOOLEAN
      !MOLGA type RSDSSELOPT_T
      !COGU type BOOLEAN .
  methods PROCEED_COFU_PER_CONTACT
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

  methods GET_COFU_DATA .
  methods MAP_COFU_DATA
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
    returning
      value(DATA) type STRING .
ENDCLASS.



CLASS /SEW/CL_MIG_CONTACT IMPLEMENTATION.


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
                                          ( name = 2  value =  contact )
                                          ( name = 3  value = 'EffectiveStartDate' )
                                          ( name = 4  value = 'EffectiveEndDate' )
                                          ( name = 5  value = 'PersonNumber' )
                                          ( name = 6  value = 'StartDate' )
                                          ( name = 7  value = 'DateOfBirth' )
                                          ( name = 8  value = 'SourceSystemOwner' )
                                          ( name = 9  value = 'SourceSystemId' ) ).
    ELSEIF cogl EQ abap_true.


    ENDIF.

  ENDMETHOD.


  METHOD create_metadata.

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
    " Read infotype 0002
    SELECT pernr,
           begda,
           endda,
           natio
           INTO CORRESPONDING FIELDS OF TABLE @p0002 FROM pa0002 WHERE pernr IN @pernr
                                                                   AND begda LE @endda
                                                                   AND endda GE @begda.

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
           zz_art
    INTO CORRESPONDING FIELDS OF TABLE @p0021 FROM pa0021 WHERE pernr IN @pernr
                                                                AND begda LE @endda
                                                                AND endda GE @begda.
  ENDMETHOD.


  METHOD map_cofu_data.

    DATA: src_id TYPE string,
          sys_id TYPE string.

    CHECK p0021 IS NOT INITIAL.
    SORT p0021 BY pernr ASCENDING begda ASCENDING.

    DATA(check_pernr) = p0021[ 1 ]-pernr.
    DATA(count) = 0.

* pernr | check | count
* 1234  | 1234  | 1
* 1234  | 1234  | 2
* 1234  | 1234  | 3
* 1234  | 1234  | 4
* 4321  | 1234  | 1 <- set count to 1, and set check_pernr to pernr

    LOOP AT p0021 ASSIGNING FIELD-SYMBOL(<p0021>).

      IF <p0021>-pernr NE check_pernr.
        count = 1.
        check_pernr = <p0021>-pernr.
      ELSE.
        count = count + 1.
      ENDIF.


      DATA(eff_start_date) = COND #( WHEN <p0021>-begda IS NOT INITIAL THEN  /sew/cl_mig_utils=>convert_date( <p0021>-begda )
                                     ELSE '' ).
      DATA(start_date) = COND #( WHEN <p0021>-begda IS NOT INITIAL THEN  /sew/cl_mig_utils=>convert_date( <p0021>-begda )
                                 ELSE '' ).

      DATA(date_of_birth) = COND #( WHEN <p0021>-fgbdt IS NOT INITIAL THEN  /sew/cl_mig_utils=>convert_date( <p0021>-fgbdt )
                                    ELSE '' ).
      DATA(count_str) = CONV string( count ).
      CONDENSE count_str.

      sys_id = 'SAP_' && sy-mandt.

      CONCATENATE per
                  cont
                  <p0021>-pernr
                  count_str
                  INTO src_id SEPARATED BY '_'. "PER_CONT_00200518_1

      CONCATENATE per
                  <p0021>-pernr
                  INTO DATA(tmp_pernr) SEPARATED BY '_'.



      CONCATENATE /sew/cl_mig_utils=>merge
                  contact
                  eff_start_date
                  ''
                  ''
                  start_date
                  date_of_birth
                  sys_id
                  src_id
      INTO DATA(data_tmp) SEPARATED BY /sew/cl_mig_utils=>separator.

      CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.

    ENDLOOP.

**IFT20211116 Start Deletion
*
*    LOOP AT p0002 ASSIGNING FIELD-SYMBOL(<p0002>).
*      DATA(eff_start_date) = COND #( WHEN <p0002>-begda IS NOT INITIAL THEN  /sew/cl_mig_utils=>convert_date( <p0002>-begda )
*                                     ELSE '' ).
*      DATA(start_date) = COND #( WHEN <p0002>-begda IS NOT INITIAL THEN  /sew/cl_mig_utils=>convert_date( <p0002>-begda )
*                                 ELSE '' ). " SY-DATUM???
*
*      DATA(date_of_birth) = COND #( WHEN <p0002>-gbdat IS NOT INITIAL THEN  /sew/cl_mig_utils=>convert_date( <p0002>-gbdat )
*                                    ELSE '' ).
*
*      sys_id = 'SAP_' && sy-mandt.
*      CONCATENATE per
*                  cont
*                  <p0002>-pernr
*                  INTO src_id SEPARATED BY '_'. "PER_CONT_00200518
*
*      CONCATENATE per
*                  <p0002>-pernr
*                  INTO DATA(tmp_pernr) SEPARATED BY '_'.
*
*      CONCATENATE /sew/cl_mig_utils=>merge
*                  contact
*                  eff_start_date
*                  ''
*                  tmp_pernr
*                  start_date
*                  date_of_birth
*                  sys_id
*                  src_id
*      INTO DATA(data_tmp) SEPARATED BY /sew/cl_mig_utils=>separator.
*
*      CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
*
*    ENDLOOP.
*IFT20211116 End Deletion

  ENDMETHOD.


  METHOD proceed_cofu_per_contact.

    get_cofu_data( ).
*    /sew/cl_mig_utils=>update_begin_date( EXPORTING p0000 = worker->p0000
*                                           CHANGING p0002 = p0002 ).

    data = map_cofu_data( vp_src_id ).

  ENDMETHOD.
ENDCLASS.
