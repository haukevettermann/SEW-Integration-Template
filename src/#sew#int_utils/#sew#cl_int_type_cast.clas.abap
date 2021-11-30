class /SEW/CL_INT_TYPE_CAST definition
  public
  final
  create public .

*"* public components of class /SEW/CL_INT_TYPE_CAST
*"* do not include other source files here!!!
public section.

  types:
    BEGIN OF /sew/wplog.
      INCLUDE TYPE /sew/wplog.
*    INCLUDE TYPE wplog.
*    TYPES aend_id TYPE guid_32.
    TYPES END OF /sew/wplog .
  types:
    /sew/tt_WPLOG_tab TYPE TABLE OF /sew/wplog .

  class-methods CLASS_CONSTRUCTOR .
  class-methods PNNNN_TO_PRELP
    importing
      !PNNNN type ANY
      !AEND_ID type GUID_32
    exporting
      !PRELP type /SEW/PRELP .
  class-methods PNNNN_TO_PRELP_TAB
    importing
      !PNNNN_TAB type TABLE
      !AEND_ID_TAB type /SEW/TT_VALUE
    exporting
      !PRELP_TAB type /SEW/TT_PRELP_TAB .
  class-methods PRELP_TO_PNNNN
    importing
      !PRELP type /SEW/PRELP
    exporting
      !PNNNN type ANY .
  class-methods PRELP_TO_PNNNN_TAB
    importing
      !PRELP_TAB type PRELP_TAB
    exporting
      !PNNNN_TAB type TABLE .
  class-methods PNNNN_TO_WPLOG
    importing
      !PNNNN type ANY
      !AEND_ID type GUID_32
    exporting
      !WPLOG type /SEW/WPLOG .
  class-methods PNNNN_TO_WPLOG_TAB
    importing
      !PNNNN_TAB type TABLE
      !AEND_ID_TAB type /SEW/TT_VALUE
    exporting
      !WPLOG_TAB type WPLOG_TAB .
  class-methods WPLOG_TO_PNNNN
    importing
      !WPLOG type WPLOG
    exporting
      !PNNNN type ANY .
  class-methods WPLOG_TO_PNNNN_TAB
    importing
      !WPLOG_TAB type /SEW/CL_INT_TYPE_CAST=>/SEW/TT_WPLOG_TAB
      !AEND_ID_TAB type /SEW/TT_VALUE
    exporting
      !PNNNN_TAB type TABLE .
  class-methods VIEW_TO_PRELP
    importing
      !PRIMARY_RECORD type ANY
      !SECONDARY_RECORD type ANY
    exporting
      !PRELP type PRELP .
  class-methods PRELP_TO_VIEW
    importing
      !PRELP type PRELP
      !SECONDARY_INFTY type INFTY
    exporting
      !PRIMARY_RECORD type ANY
      !SECONDARY_RECORD type ANY .
protected section.
*"* protected components of class /SEW/CL_INT_TYPE_CAST
*"* do not include other source files here!!!
private section.
*"* private components of class /SEW/CL_INT_TYPE_CAST
*"* do not include other source files here!!!

  class-data DATA_OFFSET_IBM type I .
ENDCLASS.



CLASS /SEW/CL_INT_TYPE_CAST IMPLEMENTATION.


METHOD CLASS_CONSTRUCTOR .

  CONSTANTS pskey TYPE pskey VALUE IS INITIAL.
  CONSTANTS pshd1 TYPE pshd1 VALUE IS INITIAL.

  DATA pskey_length_ibm TYPE i.
  DATA pshd1_length_ibm TYPE i.


  DESCRIBE FIELD pskey LENGTH pskey_length_ibm IN BYTE MODE.
  DESCRIBE FIELD pshd1 LENGTH pshd1_length_ibm IN BYTE MODE.

  data_offset_ibm = pskey_length_ibm + pshd1_length_ibm.
ENDMETHOD.


METHOD pnnnn_to_prelp .

  FIELD-SYMBOLS <prelp> TYPE c.

  ASSIGN pnnnn TO <prelp> CASTING.
  prelp = <prelp>.
  prelp-aend_id = aend_id.

ENDMETHOD.


METHOD pnnnn_to_prelp_tab.
  DATA prelp TYPE /sew/prelp.

  FIELD-SYMBOLS <prelp> TYPE c.
  FIELD-SYMBOLS <pnnnn> TYPE any.
  FIELD-SYMBOLS <prelp_new> TYPE /sew/prelp.

  CLEAR prelp_tab[].
  LOOP AT pnnnn_tab ASSIGNING <pnnnn>.
    ASSIGN <pnnnn> TO <prelp> CASTING.
    APPEND <prelp> TO prelp_tab.
  ENDLOOP.

  DATA(lines) = lines( aend_id_tab ).
  LOOP AT prelp_tab ASSIGNING <prelp_new>.
    READ TABLE aend_id_tab ASSIGNING FIELD-SYMBOL(<aend_id>) INDEX sy-tabix.
    <prelp_new>-aend_id = <aend_id>.
  ENDLOOP.
ENDMETHOD.


METHOD pnnnn_to_wplog.

  FIELD-SYMBOLS <wplog> TYPE c.

  ASSIGN pnnnn TO <wplog> CASTING.
  wplog = <wplog>.
  wplog-aend_id = aend_id.
ENDMETHOD.


METHOD PNNNN_TO_WPLOG_TAB .

  FIELD-SYMBOLS <wplog> TYPE c.
  FIELD-SYMBOLS <pnnnn> TYPE ANY.

  CLEAR wplog_tab[].
  LOOP AT pnnnn_tab ASSIGNING <pnnnn>.
    ASSIGN <pnnnn> to <wplog> CASTING.
    APPEND <wplog> TO wplog_tab.
  ENDLOOP.
ENDMETHOD.


METHOD PRELP_TO_PNNNN.

  FIELD-SYMBOLS <pnnnn> TYPE c.
  FIELD-SYMBOLS <pskey> TYPE pskey.
  FIELD-SYMBOLS <pshd1> TYPE pshd1.

  IF NOT prelp-psrelp IS INITIAL.
    ASSIGN pnnnn TO <pnnnn> CASTING.
    <pnnnn> = prelp.
  ELSE.
    CLEAR pnnnn.
    ASSIGN COMPONENT 'PSKEY' OF STRUCTURE pnnnn TO <pskey>.
    IF sy-subrc = 0.
      ASSIGN COMPONENT 'PSHD1' OF STRUCTURE pnnnn TO <pshd1>.
      <pskey> = prelp-pshdr-pskey.
      <pshd1> = prelp-pshdr-pshd1.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD PRELP_TO_PNNNN_TAB .

  FIELD-SYMBOLS <pnnnn>   TYPE any.
  FIELD-SYMBOLS <pskey>   TYPE pskey.
  FIELD-SYMBOLS <pshd1>   TYPE pshd1.
  FIELD-SYMBOLS <prelp_c> TYPE c.
  FIELD-SYMBOLS <prelp>   TYPE prelp.

  DATA pnnnn_ref TYPE REF TO data.


  CREATE DATA pnnnn_ref LIKE LINE OF pnnnn_tab.
  ASSIGN pnnnn_ref->* TO <pnnnn>.

  CLEAR pnnnn_tab[].
  LOOP AT prelp_tab ASSIGNING <prelp>.
    IF NOT <prelp>-psrelp IS INITIAL.
      ASSIGN pnnnn_ref->* TO <prelp_c> CASTING.
      <prelp_c> = <prelp>.
    ELSE.
      CLEAR <pnnnn>.
      ASSIGN COMPONENT 'PSKEY' OF STRUCTURE <pnnnn> TO <pskey>.
      IF sy-subrc = 0.
        ASSIGN COMPONENT 'PSHD1' OF STRUCTURE <pnnnn> TO <pshd1>.
        <pskey> = <prelp>-pshdr-pskey.
        <pshd1> = <prelp>-pshdr-pshd1.
      ENDIF.
    ENDIF.
    APPEND <pnnnn> TO pnnnn_tab.
  ENDLOOP.

ENDMETHOD.


METHOD PRELP_TO_VIEW .

* secondary pskey will be recovered from primary record except INFTY

  FIELD-SYMBOLS <primary_record>   TYPE x.
  FIELD-SYMBOLS <secondary_record> TYPE x.
  FIELD-SYMBOLS <prelp>            TYPE x.
  FIELD-SYMBOLS <pskey>            TYPE pskey.
  FIELD-SYMBOLS <pshdr>            TYPE pshdr.

  DATA primary_length   TYPE i.
  DATA secondary_length TYPE i.


  IF NOT prelp-psrelp IS INITIAL.
*   compute length of primary record
    DESCRIBE FIELD primary_record LENGTH primary_length     IN BYTE MODE.

*   compute length of data part of secondary record
    DESCRIBE FIELD secondary_record LENGTH secondary_length IN BYTE MODE.
    SUBTRACT data_offset_ibm FROM secondary_length.


    ASSIGN primary_record   TO <primary_record>   CASTING.
    ASSIGN secondary_record TO <secondary_record> CASTING.
    ASSIGN secondary_record TO <pskey>            CASTING.
    ASSIGN prelp            TO <prelp>            CASTING.

*   split prelp
    <primary_record> = <prelp>(primary_length).

    <secondary_record>(data_offset_ibm)                  = <prelp>(data_offset_ibm).
    <pskey>-infty                                        = secondary_infty.
    <secondary_record>+data_offset_ibm(secondary_length) = <prelp>+primary_length(secondary_length).

  ELSE.
    CLEAR: primary_record, secondary_record.

    ASSIGN primary_record TO <pshdr> CASTING.
    <pshdr> = prelp-pshdr.

    ASSIGN secondary_record TO <pshdr> CASTING.
    <pshdr> = prelp-pskey.
    <pshdr>-infty = secondary_infty.
  ENDIF.

ENDMETHOD.


METHOD VIEW_TO_PRELP.

* primary_record-pskey and secondary_record-pskey are assumed to be
* matching. if they do no match pskey of secondary_record will be
* lost.

  FIELD-SYMBOLS <primary_record>   TYPE x.
  FIELD-SYMBOLS <secondary_record> TYPE x.
  FIELD-SYMBOLS <prelp>            TYPE x.

  DATA primary_length   TYPE i.
  DATA secondary_length TYPE i.


* compute length of primary record
  DESCRIBE FIELD primary_record LENGTH primary_length     IN BYTE MODE.

* compute length of data part of secondary record
  DESCRIBE FIELD secondary_record LENGTH secondary_length IN BYTE MODE.
  SUBTRACT data_offset_ibm FROM secondary_length.


  ASSIGN primary_record   TO <primary_record>   CASTING.
  ASSIGN secondary_record TO <secondary_record> CASTING.
  ASSIGN prelp            TO <prelp>            CASTING.

* combine into prelp
  CLEAR prelp.
  <prelp>(primary_length)                  = <primary_record>.
  <prelp>+primary_length(secondary_length) = <secondary_record>+data_offset_ibm(secondary_length).

ENDMETHOD.


METHOD WPLOG_TO_PNNNN.

  FIELD-SYMBOLS <pnnnn> TYPE c.

  ASSIGN pnnnn TO <pnnnn> CASTING.
  <pnnnn> = wplog.
ENDMETHOD.


METHOD wplog_to_pnnnn_tab .

  DATA wplog TYPE /sew/cl_int_type_cast=>/sew/wplog.

  FIELD-SYMBOLS <pnnnn> TYPE any.
  FIELD-SYMBOLS <wplog> TYPE c.

  DATA pnnnn_ref TYPE REF TO data.


  CREATE DATA pnnnn_ref LIKE LINE OF pnnnn_tab.
  ASSIGN pnnnn_ref->* TO <pnnnn>.
  ASSIGN pnnnn_ref->* TO <wplog> CASTING.

  CLEAR pnnnn_tab[].
  LOOP AT wplog_tab INTO <wplog>.
    APPEND <pnnnn> TO pnnnn_tab.
  ENDLOOP.

  DATA(lines) = lines( aend_id_tab ).
  LOOP AT wplog_tab ASSIGNING FIELD-SYMBOL(<wplog_new>).
    READ TABLE aend_id_tab ASSIGNING FIELD-SYMBOL(<aend_id>) INDEX sy-tabix.
    <wplog_new>-aend_id = <aend_id>.
  ENDLOOP.
ENDMETHOD.
ENDCLASS.
