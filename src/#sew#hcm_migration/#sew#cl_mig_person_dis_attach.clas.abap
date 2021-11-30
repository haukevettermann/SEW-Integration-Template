class /SEW/CL_MIG_PERSON_DIS_ATTACH definition
  public
  final
  create public .

public section.

  constants PERSON_DIS_ATTACHMENT type STRING value 'PersonDisabilityAttachment' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !PERNR type RSDSSELOPT_T
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !COFU type BOOLEAN
      !COGL type BOOLEAN
      !MOLGA type RSDSSELOPT_T
      !COGU type BOOLEAN .
protected section.
private section.

  data PERNR type RSDSSELOPT_T .
  data BEGDA type BEGDA .
  data ENDDA type ENDDA .
  data COFU type BOOLEAN .
  data COGL type BOOLEAN .
  data COGU type BOOLEAN .
  data MOLGA type RSDSSELOPT_T .
  data P0002 type P0002_TAB .
  data VP_PER_DIS_ATTACHMENT_STRUC type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  data P0004 type P0004_TAB .

  methods PROCEED_COFU_DIS_ATTACHMENT
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !WORKER type ref to /SEW/CL_MIG_WORKER
    returning
      value(DATA) type STRING .
  methods GET_COFU_DATA .
  methods MAP_COFU_DATA
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
    returning
      value(DATA) type STRING .
  methods CREATE_METADATA
    returning
      value(METADATA) type STRING .
ENDCLASS.



CLASS /SEW/CL_MIG_PERSON_DIS_ATTACH IMPLEMENTATION.


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
      vp_per_dis_attachment_struc = VALUE #( ( name = 1  value = /sew/cl_mig_utils=>merge )
                                             ( name = 2  value = person_dis_attachment )
                                             ( name = 3  value = 'PersonDisabilityAttachment' )
                                             ( name = 4  value = 'DisabilityCode' )
                                             ( name = 5  value = 'Title' )
                                             ( name = 6  value = 'DataTypeCode' )
                                             ( name = 7  value = 'File' )
                                             ( name = 8  value = 'FileName' )
                                             ( name = 9  value = 'URLorTextorFileName' ) ).
    ELSEIF cogu EQ abap_true.

    ENDIF.
  ENDMETHOD.


  METHOD create_metadata.

    DESCRIBE TABLE vp_per_dis_attachment_struc LINES DATA(length).

    LOOP AT vp_per_dis_attachment_struc ASSIGNING FIELD-SYMBOL(<per_dis_attachment>).

      CASE <per_dis_attachment>-name.
        WHEN 1.
          CONCATENATE /sew/cl_mig_utils=>metadata /sew/cl_mig_utils=>separator INTO metadata.
          CONTINUE.

      ENDCASE.

      CONCATENATE metadata <per_dis_attachment>-value INTO metadata.

      "set separator
      CHECK length NE sy-tabix.
      CONCATENATE metadata /sew/cl_mig_utils=>separator INTO metadata.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_cofu_data.
    " read infotype 0002
    SELECT pernr,
           begda,
           endda,
           natio
           INTO CORRESPONDING FIELDS OF TABLE @p0002 FROM pa0002 WHERE pernr IN @pernr
                                                                   AND begda LE @endda
                                                                   AND endda GE @begda.
    " read infotype 0004
    SELECT pernr,
           begda,
           endda,
           sbgru
           INTO CORRESPONDING FIELDS OF TABLE @p0004 FROM pa0004 WHERE pernr IN @pernr
                                                                   AND begda LE @endda
                                                                   AND endda GE @begda.



  ENDMETHOD.


  METHOD map_cofu_data.

    CHECK p0004 IS NOT INITIAL.
    SORT p0004 BY pernr ASCENDING begda ASCENDING.

    DATA(tmp_pernr) = p0004[ 1 ]-pernr.
    DATA(counter)   = 0.


    LOOP AT p0004 ASSIGNING FIELD-SYMBOL(<p0004>).
      IF tmp_pernr NE <p0004>-pernr.
        counter = 1.
        tmp_pernr = <p0004>-pernr.
      ELSE.
        counter = counter + 1.
      ENDIF.

      DATA(counter_str) = CONV string( counter ).
      CONDENSE counter_str.

      DATA(disability_code) = 'PER_DIS_' && <p0004>-pernr && '_' && counter_str. "IFT20211130 I

*   MIGRATION TODO Disability attachment

      CONCATENATE /sew/cl_mig_utils=>merge " MERGE
                  person_dis_attachment    "PersonDisabilityAttachment'
                  disability_code          "DisabilityCode
                  ''                       "Title
                  ''                       "DataTypeCode
                  ''                       "File
                  ''                       "FileName
                  ''                       "URLorTextorFileName
       INTO DATA(data_tmp).

      CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.

    ENDLOOP.
  ENDMETHOD.


  METHOD proceed_cofu_dis_attachment.

    get_cofu_data( ).
    data = map_cofu_data( vp_src_id ).
  ENDMETHOD.
ENDCLASS.
