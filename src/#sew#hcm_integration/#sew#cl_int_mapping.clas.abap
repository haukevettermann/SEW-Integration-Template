class /SEW/CL_INT_MAPPING definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF t_mapping_fieldlist.
    TYPES field TYPE /sew/dd_value.
    TYPES END OF t_mapping_fieldlist .
  types:
    BEGIN OF t_mapping_valuelist.
    TYPES field TYPE /sew/dd_value.
    TYPES value TYPE /sew/dd_value.
    TYPES END OF t_mapping_valuelist .

  class-methods GET_SUBTY_2001
    importing
      !PERNR type PERNR_D
      !OBJID type HROBJID
      !BEGDA type DATS
      !ENDDA type DATS
      !INFTY type INFTY
      !SEQNR type SEQNR default 000
      !FIELD_SAP type /SEW/DD_FIELD
      !FIELD_ORACLE type /SEW/DD_FIELD
      !IMPORT type BOOLE_D
      !EXPORT type BOOLE_D
      !FIELDS type /SEW/CL_INT_INFTY_PROC_XML=>T_FIELDS
    exporting
      !VALUE_OUT type /SEW/DD_VALUE
      !MESSAGE type BAPIRET1
      !DO_SIMPLE type BOOLE_D .
  class-methods GET_AWART
    importing
      !PERNR type PERNR_D
      !OBJID type HROBJID
      !BEGDA type DATS
      !ENDDA type DATS
      !INFTY type INFTY
      !SEQNR type SEQNR default 000
      !FIELD_SAP type /SEW/DD_FIELD
      !FIELD_ORACLE type /SEW/DD_FIELD
      !IMPORT type BOOLE_D
      !EXPORT type BOOLE_D
      !FIELDS type /SEW/CL_INT_INFTY_PROC_XML=>T_FIELDS
    exporting
      !VALUE_OUT type /SEW/DD_VALUE
      !MESSAGE type BAPIRET1
      !DO_SIMPLE type BOOLE_D .
  class-methods GET_MAPPING_VALUES
    importing
      !MOLGA type RSDSSELOPT_T
      !INFTY type RSDSSELOPT_T optional
      !SAP_FIELD type RSDSSELOPT_T optional
      !ORACLE_FIELD type RSDSSELOPT_T optional
      !EXPORT type RSDSSELOPT_T optional
      !IMPORT type RSDSSELOPT_T optional
    exporting
      !MAPPING_VALUES type /SEW/CL_INT_CUSTOMIZING_XML=>T_CUSTOMIZING_MAPPING .
  class-methods PROCESS_MAPPING
    importing
      !PERNR type PERNR_D optional
      !OBJID type HROBJID optional
      !BEGDA type DATS optional
      !ENDDA type DATS optional
      !INFTY type INFTY
      !SEQNR type SEQNR default 000
      !FIELD_SAP type /SEW/DD_FIELD
      !FIELD_ORACLE type /SEW/DD_FIELD
      !MAPPING_FIELDS type /SEW/CL_INT_CUSTOMIZING_XML=>T_CUSTOMIZING_MAPPING_FIELDS
      !MAPPING_VALUES type /SEW/CL_INT_CUSTOMIZING_XML=>T_CUSTOMIZING_MAPPING optional
      !IMPORT type BOOLE_D
      !EXPORT type BOOLE_D
      !FIELDS type /SEW/CL_INT_INFTY_PROC_XML=>T_FIELDS optional
    exporting
      !MESSAGE type BAPIRET1
      !SKIP type BOOLE_D
    changing
      !VALUE type /SEW/DD_VALUE optional .
  class-methods GET_BTRTL
    importing
      !PERNR type PERNR_D optional
      !OBJID type HROBJID optional
      !BEGDA type DATS optional
      !ENDDA type DATS optional
      !INFTY type INFTY optional
      !SEQNR type SEQNR default 000
      !FIELD_SAP type /SEW/DD_FIELD optional
      !FIELD_ORACLE type /SEW/DD_FIELD optional
      !IMPORT type BOOLE_D
      value(EXPORT) type BOOLE_D
      !FIELDS type /SEW/CL_INT_INFTY_PROC_XML=>T_FIELDS
    exporting
      value(VALUE_OUT) type /SEW/DD_VALUE
      !MESSAGE type BAPIRET1
      !DO_SIMPLE type BOOLE_D .
  class-methods GET_BEGDA
    importing
      !PERNR type PERNR_D optional
      !OBJID type HROBJID optional
      !BEGDA type DATS optional
      !ENDDA type DATS optional
      !INFTY type INFTY optional
      !SEQNR type SEQNR default 000
      !FIELD_SAP type /SEW/DD_FIELD optional
      !FIELD_ORACLE type /SEW/DD_FIELD optional
      !IMPORT type BOOLE_D
      value(EXPORT) type BOOLE_D
      !FIELDS type /SEW/CL_INT_INFTY_PROC_XML=>T_FIELDS
    exporting
      value(VALUE_OUT) type /SEW/DD_VALUE
      !MESSAGE type BAPIRET1
      !DO_SIMPLE type BOOLE_D .
  class-methods GET_ENDDA
    importing
      !PERNR type PERNR_D optional
      !OBJID type HROBJID optional
      !BEGDA type DATS optional
      !ENDDA type DATS optional
      !INFTY type INFTY optional
      !SEQNR type SEQNR default 000
      !FIELD_SAP type /SEW/DD_FIELD optional
      !FIELD_ORACLE type /SEW/DD_FIELD optional
      !IMPORT type BOOLE_D
      value(EXPORT) type BOOLE_D
      !FIELDS type /SEW/CL_INT_INFTY_PROC_XML=>T_FIELDS
    exporting
      value(VALUE_OUT) type /SEW/DD_VALUE
      !MESSAGE type BAPIRET1
      !DO_SIMPLE type BOOLE_D .
  class-methods GET_MASSN
    importing
      !PERNR type PERNR_D optional
      !BEGDA type DATS optional
      !ENDDA type DATS optional
      !OBJID type HROBJID optional
      !INFTY type INFTY optional
      !SEQNR type SEQNR default 000
      !FIELD_SAP type /SEW/DD_FIELD optional
      !FIELD_ORACLE type /SEW/DD_FIELD optional
      !IMPORT type BOOLE_D
      value(EXPORT) type BOOLE_D
      !FIELDS type /SEW/CL_INT_INFTY_PROC_XML=>T_FIELDS
    exporting
      value(VALUE_OUT) type /SEW/DD_VALUE
      !MESSAGE type BAPIRET1
      !DO_SIMPLE type BOOLE_D .
  class-methods GET_STAT2
    importing
      !PERNR type PERNR_D optional
      !OBJID type HROBJID optional
      !BEGDA type DATS optional
      !ENDDA type DATS optional
      !INFTY type INFTY optional
      !SEQNR type SEQNR default 000
      !FIELD_SAP type /SEW/DD_FIELD optional
      !FIELD_ORACLE type /SEW/DD_FIELD optional
      !IMPORT type BOOLE_D
      value(EXPORT) type BOOLE_D
      !FIELDS type /SEW/CL_INT_INFTY_PROC_XML=>T_FIELDS
    exporting
      value(VALUE_OUT) type /SEW/DD_VALUE
      !MESSAGE type BAPIRET1
      !DO_SIMPLE type BOOLE_D .
  class-methods GET_OBJID
    importing
      !PERNR type PERNR_D optional
      !OBJID type HROBJID optional
      !BEGDA type DATS optional
      !ENDDA type DATS optional
      !INFTY type INFTY optional
      !SEQNR type SEQNR default 000
      !FIELD_SAP type /SEW/DD_FIELD optional
      !FIELD_ORACLE type /SEW/DD_FIELD optional
      !IMPORT type BOOLE_D
      value(EXPORT) type BOOLE_D
      !FIELDS type /SEW/CL_INT_INFTY_PROC_XML=>T_FIELDS
    exporting
      value(VALUE_OUT) type /SEW/DD_VALUE
      !MESSAGE type BAPIRET1
      !DO_SIMPLE type BOOLE_D .
  class-methods GET_ORGEH
    importing
      !PERNR type PERNR_D optional
      !OBJID type HROBJID optional
      !BEGDA type DATS optional
      !ENDDA type DATS optional
      !INFTY type INFTY optional
      !SEQNR type SEQNR default 000
      !FIELD_SAP type /SEW/DD_FIELD optional
      !FIELD_ORACLE type /SEW/DD_FIELD optional
      !IMPORT type BOOLE_D
      value(EXPORT) type BOOLE_D
      !FIELDS type /SEW/CL_INT_INFTY_PROC_XML=>T_FIELDS
    exporting
      value(VALUE_OUT) type /SEW/DD_VALUE
      !MESSAGE type BAPIRET1
      !DO_SIMPLE type BOOLE_D .
  class-methods GET_OTYPE
    importing
      !PERNR type PERNR_D optional
      !OBJID type HROBJID optional
      !INFTY type INFTY optional
      !SEQNR type SEQNR default 000
      !FIELD_SAP type /SEW/DD_FIELD optional
      !FIELD_ORACLE type /SEW/DD_FIELD optional
      !IMPORT type BOOLE_D
      value(EXPORT) type BOOLE_D
      !FIELDS type /SEW/CL_INT_INFTY_PROC_XML=>T_FIELDS
    exporting
      value(VALUE_OUT) type /SEW/DD_VALUE
      !MESSAGE type BAPIRET1
      !DO_SIMPLE type BOOLE_D .
  class-methods GET_PERSK_WORKER_TYPE
    importing
      !PERNR type PERNR_D optional
      !OBJID type HROBJID optional
      !BEGDA type DATS optional
      !ENDDA type DATS optional
      !INFTY type INFTY optional
      !SEQNR type SEQNR default 000
      !FIELD_SAP type /SEW/DD_FIELD optional
      !FIELD_ORACLE type /SEW/DD_FIELD optional
      !IMPORT type BOOLE_D
      value(EXPORT) type BOOLE_D
      !FIELDS type /SEW/CL_INT_INFTY_PROC_XML=>T_FIELDS
    exporting
      value(VALUE_OUT) type /SEW/DD_VALUE
      !MESSAGE type BAPIRET1
      !DO_SIMPLE type BOOLE_D .
  class-methods GET_PERSK_SYSTEM_PERSON
    importing
      !PERNR type PERNR_D optional
      !OBJID type HROBJID optional
      !BEGDA type DATS optional
      !ENDDA type DATS optional
      !INFTY type INFTY optional
      !SEQNR type SEQNR default 000
      !FIELD_SAP type /SEW/DD_FIELD optional
      !FIELD_ORACLE type /SEW/DD_FIELD optional
      !IMPORT type BOOLE_D
      value(EXPORT) type BOOLE_D
      !FIELDS type /SEW/CL_INT_INFTY_PROC_XML=>T_FIELDS
    exporting
      value(VALUE_OUT) type /SEW/DD_VALUE
      !MESSAGE type BAPIRET1
      !DO_SIMPLE type BOOLE_D .
  class-methods GET_PERSK_PERSON_TYPE
    importing
      !PERNR type PERNR_D optional
      !OBJID type HROBJID optional
      !BEGDA type DATS optional
      !ENDDA type DATS optional
      !INFTY type INFTY optional
      !SEQNR type SEQNR default 000
      !FIELD_SAP type /SEW/DD_FIELD optional
      !FIELD_ORACLE type /SEW/DD_FIELD optional
      !IMPORT type BOOLE_D
      value(EXPORT) type BOOLE_D
      !FIELDS type /SEW/CL_INT_INFTY_PROC_XML=>T_FIELDS
    exporting
      value(VALUE_OUT) type /SEW/DD_VALUE
      !MESSAGE type BAPIRET1
      !DO_SIMPLE type BOOLE_D .
  class-methods GET_PERSK_ASSIGN_TYPE
    importing
      !PERNR type PERNR_D optional
      !OBJID type HROBJID optional
      !BEGDA type DATS optional
      !ENDDA type DATS optional
      !INFTY type INFTY optional
      !SEQNR type SEQNR default 000
      !FIELD_SAP type /SEW/DD_FIELD optional
      !FIELD_ORACLE type /SEW/DD_FIELD optional
      !IMPORT type BOOLE_D
      value(EXPORT) type BOOLE_D
      !FIELDS type /SEW/CL_INT_INFTY_PROC_XML=>T_FIELDS
    exporting
      value(VALUE_OUT) type /SEW/DD_VALUE
      !MESSAGE type BAPIRET1
      !DO_SIMPLE type BOOLE_D .
  class-methods GET_PERSK_ASSIGN_PERSON_TYPE
    importing
      !PERNR type PERNR_D optional
      !OBJID type HROBJID optional
      !BEGDA type DATS optional
      !ENDDA type DATS optional
      !INFTY type INFTY optional
      !SEQNR type SEQNR default 000
      !FIELD_SAP type /SEW/DD_FIELD optional
      !FIELD_ORACLE type /SEW/DD_FIELD optional
      !IMPORT type BOOLE_D
      value(EXPORT) type BOOLE_D
      !FIELDS type /SEW/CL_INT_INFTY_PROC_XML=>T_FIELDS
    exporting
      value(VALUE_OUT) type /SEW/DD_VALUE
      !MESSAGE type BAPIRET1
      !DO_SIMPLE type BOOLE_D .
  class-methods GET_PERSK_ASSIGN_CATEGORY
    importing
      !PERNR type PERNR_D optional
      !OBJID type HROBJID optional
      !BEGDA type DATS optional
      !ENDDA type DATS optional
      !INFTY type INFTY optional
      !SEQNR type SEQNR default 000
      !FIELD_SAP type /SEW/DD_FIELD optional
      !FIELD_ORACLE type /SEW/DD_FIELD optional
      !IMPORT type BOOLE_D
      value(EXPORT) type BOOLE_D
      !FIELDS type /SEW/CL_INT_INFTY_PROC_XML=>T_FIELDS
    exporting
      value(VALUE_OUT) type /SEW/DD_VALUE
      !MESSAGE type BAPIRET1
      !DO_SIMPLE type BOOLE_D .
  class-methods GET_PERSK
    importing
      !PERNR type PERNR_D optional
      !OBJID type HROBJID optional
      !BEGDA type DATS optional
      !ENDDA type DATS optional
      !INFTY type INFTY optional
      !SEQNR type SEQNR default 000
      !FIELD_SAP type /SEW/DD_FIELD optional
      !FIELD_ORACLE type /SEW/DD_FIELD optional
      !IMPORT type BOOLE_D
      value(EXPORT) type BOOLE_D
      !FIELDS type /SEW/CL_INT_INFTY_PROC_XML=>T_FIELDS
    exporting
      value(VALUE_OUT) type /SEW/DD_VALUE
      !MESSAGE type BAPIRET1
      !DO_SIMPLE type BOOLE_D .
  class-methods GET_RELAT
    importing
      !PERNR type PERNR_D optional
      !OBJID type HROBJID optional
      !BEGDA type DATS optional
      !ENDDA type DATS optional
      !INFTY type INFTY optional
      !SEQNR type SEQNR default 000
      !FIELD_SAP type /SEW/DD_FIELD optional
      !FIELD_ORACLE type /SEW/DD_FIELD optional
      !IMPORT type BOOLE_D
      value(EXPORT) type BOOLE_D
      !FIELDS type /SEW/CL_INT_INFTY_PROC_XML=>T_FIELDS
    exporting
      value(VALUE_OUT) type /SEW/DD_VALUE
      !MESSAGE type BAPIRET1
      !DO_SIMPLE type BOOLE_D .
  class-methods GET_RSIGN
    importing
      !PERNR type PERNR_D optional
      !OBJID type HROBJID optional
      !BEGDA type DATS optional
      !ENDDA type DATS optional
      !INFTY type INFTY optional
      !SEQNR type SEQNR default 000
      !FIELD_SAP type /SEW/DD_FIELD optional
      !FIELD_ORACLE type /SEW/DD_FIELD optional
      !IMPORT type BOOLE_D
      value(EXPORT) type BOOLE_D
      !FIELDS type /SEW/CL_INT_INFTY_PROC_XML=>T_FIELDS
    exporting
      value(VALUE_OUT) type /SEW/DD_VALUE
      !MESSAGE type BAPIRET1
      !DO_SIMPLE type BOOLE_D .
  class-methods GET_SOBID
    importing
      !PERNR type PERNR_D optional
      !OBJID type HROBJID optional
      !BEGDA type DATS optional
      !ENDDA type DATS optional
      !INFTY type INFTY optional
      !SEQNR type SEQNR default 000
      !FIELD_SAP type /SEW/DD_FIELD optional
      !FIELD_ORACLE type /SEW/DD_FIELD optional
      !IMPORT type BOOLE_D
      value(EXPORT) type BOOLE_D
      !FIELDS type /SEW/CL_INT_INFTY_PROC_XML=>T_FIELDS
    exporting
      value(VALUE_OUT) type /SEW/DD_VALUE
      !MESSAGE type BAPIRET1
      !DO_SIMPLE type BOOLE_D .
  class-methods GET_NUM01
    importing
      !PERNR type PERNR_D optional
      !OBJID type HROBJID optional
      !BEGDA type DATS optional
      !ENDDA type DATS optional
      !INFTY type INFTY optional
      !SEQNR type SEQNR default 000
      !FIELD_SAP type /SEW/DD_FIELD optional
      !FIELD_ORACLE type /SEW/DD_FIELD optional
      !IMPORT type BOOLE_D
      value(EXPORT) type BOOLE_D
      !FIELDS type /SEW/CL_INT_INFTY_PROC_XML=>T_FIELDS
    exporting
      value(VALUE_OUT) type /SEW/DD_VALUE
      !MESSAGE type BAPIRET1
      !DO_SIMPLE type BOOLE_D .
  class-methods GET_SUBTY
    importing
      !PERNR type PERNR_D optional
      !OBJID type HROBJID optional
      !BEGDA type DATS optional
      !ENDDA type DATS optional
      !INFTY type INFTY optional
      !SEQNR type SEQNR default 000
      !FIELD_SAP type /SEW/DD_FIELD optional
      !FIELD_ORACLE type /SEW/DD_FIELD optional
      !IMPORT type BOOLE_D
      value(EXPORT) type BOOLE_D
      !FIELDS type /SEW/CL_INT_INFTY_PROC_XML=>T_FIELDS
    exporting
      value(VALUE_OUT) type /SEW/DD_VALUE
      !MESSAGE type BAPIRET1
      !DO_SIMPLE type BOOLE_D .
  class-methods GET_TELNR
    importing
      !PERNR type PERNR_D optional
      !OBJID type HROBJID optional
      !BEGDA type DATS optional
      !ENDDA type DATS optional
      !INFTY type INFTY optional
      !SEQNR type SEQNR default 000
      !FIELD_SAP type /SEW/DD_FIELD optional
      !FIELD_ORACLE type /SEW/DD_FIELD optional
      !IMPORT type BOOLE_D
      value(EXPORT) type BOOLE_D
      !FIELDS type /SEW/CL_INT_INFTY_PROC_XML=>T_FIELDS
    exporting
      value(VALUE_OUT) type /SEW/DD_VALUE
      !MESSAGE type BAPIRET1
      !DO_SIMPLE type BOOLE_D .
  class-methods GET_SUBTY_2006
    importing
      !PERNR type PERNR_D optional
      !OBJID type HROBJID optional
      !BEGDA type DATS optional
      !ENDDA type DATS optional
      !INFTY type INFTY optional
      !SEQNR type SEQNR default 000
      !FIELD_SAP type /SEW/DD_FIELD optional
      !FIELD_ORACLE type /SEW/DD_FIELD optional
      !IMPORT type BOOLE_D
      value(EXPORT) type BOOLE_D
      !FIELDS type /SEW/CL_INT_INFTY_PROC_XML=>T_FIELDS
    exporting
      value(VALUE_OUT) type /SEW/DD_VALUE
      !MESSAGE type BAPIRET1
      !DO_SIMPLE type BOOLE_D .
  class-methods GET_ENAME
    importing
      !PERNR type PERNR_D optional
      !OBJID type HROBJID optional
      !BEGDA type DATS optional
      !ENDDA type DATS optional
      !INFTY type INFTY optional
      !SEQNR type SEQNR default 000
      !FIELD_SAP type /SEW/DD_FIELD optional
      !FIELD_ORACLE type /SEW/DD_FIELD optional
      !IMPORT type BOOLE_D
      value(EXPORT) type BOOLE_D
      !FIELDS type /SEW/CL_INT_INFTY_PROC_XML=>T_FIELDS
    exporting
      value(VALUE_OUT) type /SEW/DD_VALUE
      !MESSAGE type BAPIRET1
      !DO_SIMPLE type BOOLE_D .
  class-methods GET_USRID
    importing
      !PERNR type PERNR_D optional
      !OBJID type HROBJID optional
      !BEGDA type DATS optional
      !ENDDA type DATS optional
      !INFTY type INFTY optional
      !SEQNR type SEQNR default 000
      !FIELD_SAP type /SEW/DD_FIELD optional
      !FIELD_ORACLE type /SEW/DD_FIELD optional
      !IMPORT type BOOLE_D
      value(EXPORT) type BOOLE_D
      !FIELDS type /SEW/CL_INT_INFTY_PROC_XML=>T_FIELDS
    exporting
      value(VALUE_OUT) type /SEW/DD_VALUE
      !MESSAGE type BAPIRET1
      !DO_SIMPLE type BOOLE_D .
  class-methods GET_USRTY
    importing
      !PERNR type PERNR_D optional
      !OBJID type HROBJID optional
      !BEGDA type DATS optional
      !ENDDA type DATS optional
      !INFTY type INFTY optional
      !SEQNR type SEQNR default 000
      !FIELD_SAP type /SEW/DD_FIELD optional
      !FIELD_ORACLE type /SEW/DD_FIELD optional
      !IMPORT type BOOLE_D
      value(EXPORT) type BOOLE_D
      !FIELDS type /SEW/CL_INT_INFTY_PROC_XML=>T_FIELDS
    exporting
      value(VALUE_OUT) type /SEW/DD_VALUE
      !MESSAGE type BAPIRET1
      !DO_SIMPLE type BOOLE_D .
  class-methods GET_WERKS
    importing
      !PERNR type PERNR_D optional
      !OBJID type HROBJID optional
      !BEGDA type DATS optional
      !ENDDA type DATS optional
      !INFTY type INFTY optional
      !SEQNR type SEQNR default 000
      !FIELD_SAP type /SEW/DD_FIELD optional
      !FIELD_ORACLE type /SEW/DD_FIELD optional
      !IMPORT type BOOLE_D
      value(EXPORT) type BOOLE_D
      !FIELDS type /SEW/CL_INT_INFTY_PROC_XML=>T_FIELDS
    exporting
      value(VALUE_OUT) type /SEW/DD_VALUE
      !MESSAGE type BAPIRET1
      !DO_SIMPLE type BOOLE_D .
  class-methods GET_GRELG
    importing
      !PERNR type PERNR_D optional
      !OBJID type HROBJID optional
      !BEGDA type DATS optional
      !ENDDA type DATS optional
      !INFTY type INFTY optional
      !SEQNR type SEQNR default 000
      !FIELD_SAP type /SEW/DD_FIELD optional
      !FIELD_ORACLE type /SEW/DD_FIELD optional
      !IMPORT type BOOLE_D
      value(EXPORT) type BOOLE_D
      !FIELDS type /SEW/CL_INT_INFTY_PROC_XML=>T_FIELDS
    exporting
      value(VALUE_OUT) type /SEW/DD_VALUE
      !MESSAGE type BAPIRET1
      !DO_SIMPLE type BOOLE_D .
  class-methods GET_GRAWG
    importing
      !PERNR type PERNR_D optional
      !OBJID type HROBJID optional
      !BEGDA type DATS optional
      !ENDDA type DATS optional
      !INFTY type INFTY optional
      !SEQNR type SEQNR default 000
      !FIELD_SAP type /SEW/DD_FIELD optional
      !FIELD_ORACLE type /SEW/DD_FIELD optional
      !IMPORT type BOOLE_D
      value(EXPORT) type BOOLE_D
      !FIELDS type /SEW/CL_INT_INFTY_PROC_XML=>T_FIELDS
    exporting
      value(VALUE_OUT) type /SEW/DD_VALUE
      !MESSAGE type BAPIRET1
      !DO_SIMPLE type BOOLE_D .
  class-methods GET_ZAUSW
    importing
      !PERNR type PERNR_D optional
      !OBJID type HROBJID optional
      !BEGDA type DATS optional
      !ENDDA type DATS optional
      !INFTY type INFTY optional
      !SEQNR type SEQNR default 000
      !FIELD_SAP type /SEW/DD_FIELD optional
      !FIELD_ORACLE type /SEW/DD_FIELD optional
      !IMPORT type BOOLE_D
      value(EXPORT) type BOOLE_D
      !FIELDS type /SEW/CL_INT_INFTY_PROC_XML=>T_FIELDS
    exporting
      value(VALUE_OUT) type /SEW/DD_VALUE
      !MESSAGE type BAPIRET1
      !DO_SIMPLE type BOOLE_D .
  class-methods GET_ZPINC
    importing
      !PERNR type PERNR_D optional
      !OBJID type HROBJID optional
      !BEGDA type DATS optional
      !ENDDA type DATS optional
      !INFTY type INFTY optional
      !SEQNR type SEQNR default 000
      !FIELD_SAP type /SEW/DD_FIELD optional
      !FIELD_ORACLE type /SEW/DD_FIELD optional
      !IMPORT type BOOLE_D
      value(EXPORT) type BOOLE_D
      !FIELDS type /SEW/CL_INT_INFTY_PROC_XML=>T_FIELDS
    exporting
      value(VALUE_OUT) type /SEW/DD_VALUE
      !MESSAGE type BAPIRET1
      !DO_SIMPLE type BOOLE_D .
  class-methods PROCESS_COMPLEX_MAPPING
    importing
      !PERNR type PERNR_D optional
      !BEGDA type DATS optional
      !ENDDA type DATS optional
      !OBJID type HROBJID optional
      !INFTY type INFTY optional
      !SEQNR type SEQNR default 000
      !FIELD_SAP type /SEW/DD_FIELD optional
      !FIELD_ORACLE type /SEW/DD_FIELD optional
      !IMPORT type BOOLE_D
      value(EXPORT) type BOOLE_D
      !FIELDS type /SEW/CL_INT_INFTY_PROC_XML=>T_FIELDS
    exporting
      !MESSAGE type BAPIRET1
      !DO_SIMPLE type BOOLE_D
    changing
      value(VALUE) type /SEW/DD_VALUE .
  class-methods PROCESS_SIMPLE_MAPPING
    importing
      !PERNR type PERNR_D optional
      !OBJID type HROBJID optional
      !INFTY type INFTY optional
      !SEQNR type SEQNR default 000
      !FIELD_SAP type /SEW/DD_FIELD optional
      !FIELD_ORACLE type /SEW/DD_FIELD optional
      !MAPPING_VALUES type /SEW/CL_INT_CUSTOMIZING_XML=>T_CUSTOMIZING_MAPPING optional
      !IMPORT type BOOLE_D
      !EXPORT type BOOLE_D
    exporting
      !MESSAGE type BAPIRET1
    changing
      !VALUE type /SEW/DD_VALUE .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS /SEW/CL_INT_MAPPING IMPLEMENTATION.


  METHOD get_awart.
    IF export EQ abap_true.

      READ TABLE fields WITH KEY field_sap = 'MOLGA' ASSIGNING FIELD-SYMBOL(<molga>).
      READ TABLE fields WITH KEY field_sap = 'AWART' ASSIGNING FIELD-SYMBOL(<subty>).


      SELECT SINGLE oracle_awart FROM /sew/int_awa_map WHERE molga EQ @<molga>-value
                                                         AND infty EQ '2001'
                                                         AND awart EQ @<subty>-value
                                                         AND begda <= @endda
                                                         AND endda >= @begda
                                                        INTO @DATA(oracle_awart).

      CHECK oracle_awart IS NOT INITIAL AND sy-subrc IS INITIAL.

      value_out = oracle_awart.

    ELSEIF import EQ abap_true.


    ENDIF.


  ENDMETHOD.


  METHOD get_begda.
    CASE infty.
      WHEN /sew/cl_int_constants=>it0000.
        IF seqnr = 000.
          IF begda IS NOT INITIAL AND endda IS NOT INITIAL.
            READ TABLE fields WITH KEY field_oracle = /sew/cl_int_constants=>fields-hire_date begda = begda endda = endda ASSIGNING FIELD-SYMBOL(<hire_date>).
            READ TABLE fields WITH KEY field_oracle = /sew/cl_int_constants=>fields-termination_date begda = begda endda = endda ASSIGNING FIELD-SYMBOL(<termination_date>).
          ELSE.
            READ TABLE fields WITH KEY field_oracle = /sew/cl_int_constants=>fields-hire_date ASSIGNING <hire_date>.
            READ TABLE fields WITH KEY field_oracle = /sew/cl_int_constants=>fields-termination_date ASSIGNING <termination_date>.
          ENDIF.
          IF <hire_date> IS NOT INITIAL.
            SELECT * FROM pa0000 INTO TABLE @DATA(it0000) WHERE pernr = @pernr AND massn = @/sew/cl_int_constants=>hire.
            IF sy-subrc IS INITIAL.
              SORT it0000 BY begda ASCENDING.
              READ TABLE it0000 ASSIGNING FIELD-SYMBOL(<s0000>) INDEX 1.
*              IF <s0000>-begda NE /sew/cl_int_conversion=>convert_date( value_in = <hire_date>-value ).
              value_out = /sew/cl_int_conversion=>convert_date( value_in = <hire_date>-value ).
*              ENDIF.
            ELSE.
              value_out = <hire_date>-value.
            ENDIF.
          ENDIF.
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD get_btrtl.
    IF export = abap_true.
      READ TABLE fields WITH KEY field_sap = 'BTRTL' ASSIGNING FIELD-SYMBOL(<field>).
      IF <field> IS ASSIGNED.
        DATA(btrtl) = CONV btrtl( <field>-value ).
      ENDIF.
      READ TABLE fields WITH KEY field_sap = 'WERKS' ASSIGNING <field>.
      IF <field> IS ASSIGNED.
        DATA(werks) = CONV persa( <field>-value ).
      ENDIF.
      SELECT SINGLE * FROM /sew/int_map_loc WHERE werks = @werks AND btrtl = @btrtl INTO @DATA(mapping).
      IF sy-subrc IS INITIAL.
        value_out = mapping-location_code.
      ELSE.
*       TODO!!! ERROR HANDLING
      ENDIF.
    ELSEIF import = abap_true.
      READ TABLE fields WITH KEY field_oracle = 'LOCATIONID' ASSIGNING <field>.
      IF <field> IS ASSIGNED.
        DATA(location_code) = CONV btrtl( <field>-value ).
        SELECT SINGLE * FROM /sew/int_map_loc WHERE location_code = @location_code INTO @mapping.
      ENDIF.
      IF sy-subrc IS INITIAL.
        btrtl = mapping-location_code.
      ELSE.
*       TODO!!! ERROR HANDLING
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_ename.
    IF export = abap_true.

    ELSEIF import = abap_true.
      READ TABLE fields INTO DATA(field_firstname) WITH KEY field_sap = field_sap.
      READ TABLE fields INTO DATA(field_lastname) WITH KEY field_oracle = 'LastName'.
*      READ TABLE fields INTO DATA(field_ext) WITH KEY field_sap = 'USRID_EXT'.
*      READ TABLE fields INTO DATA(field_acode) WITH KEY field_sap = 'USRID_ACODE'.
*      READ TABLE fields INTO DATA(field_ccode) WITH KEY field_sap = 'USRID_CCODE'.
*      CONCATENATE '+' field_ccode-value '/' field_acode-value '/' field_number-value '/' field_ext-value '/' INTO value_out.

      CONCATENATE field_firstname-value field_lastname-value INTO value_out SEPARATED BY space.
    ENDIF.
  ENDMETHOD.


  METHOD get_endda.
    CASE infty.
      WHEN /sew/cl_int_constants=>it0000.
        IF seqnr = 000.
          IF begda IS NOT INITIAL AND endda IS NOT INITIAL.
            READ TABLE fields WITH KEY field_oracle = /sew/cl_int_constants=>fields-termination_date begda = begda endda = endda ASSIGNING FIELD-SYMBOL(<termination_date>).
          ELSE.
            READ TABLE fields WITH KEY field_oracle = /sew/cl_int_constants=>fields-termination_date ASSIGNING <termination_date>.
          ENDIF.
          IF <termination_date> IS NOT INITIAL.
            value_out = /sew/cl_int_conversion=>convert_date( value_in = <termination_date>-value ).
          ENDIF.
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD GET_GRAWG.
    IF export = abap_true.

    ELSEIF import = abap_true.
      READ TABLE fields INTO DATA(field_number) WITH KEY field_sap = field_sap.
      READ TABLE fields INTO DATA(field_type) WITH KEY field_sap = 'ZAUSW_TYPE'.


      IF field_type-value = 'Time Device Badge ID'."/sew/cl_int_constants=>zausw.
        value_out = field_number-value.
      ELSE. "IF field_type-value = 'ORA_TCLOCK_BADGE_ID'.
        READ TABLE fields INTO DATA(field_type_cofu) WITH KEY field_sap = 'ZAUSW_TYPE' value = 'ORA_TCLOCK_BADGE_ID'.
        READ TABLE fields INTO DATA(field_number_cofu) WITH KEY field_sap = field_sap seqnr = field_type_cofu-seqnr.
        value_out = field_number_cofu-value.
      ENDIF.


    ENDIF.
  ENDMETHOD.


  METHOD get_grelg.
    IF export = abap_true.

    ELSEIF import = abap_true.
      READ TABLE fields INTO DATA(field_number) WITH KEY field_sap = field_sap.
      READ TABLE fields INTO DATA(field_type) WITH KEY field_sap = 'ZAUSW_TYPE'.


      IF field_type-value = 'Time Device Badge ID'."/sew/cl_int_constants=>zausw.
        value_out = field_number-value.
      ELSE. "IF field_type-value = 'ORA_TCLOCK_BADGE_ID'.
        READ TABLE fields INTO DATA(field_type_cofu) WITH KEY field_sap = 'ZAUSW_TYPE' value = 'ORA_TCLOCK_BADGE_ID'.
*        READ TABLE fields INTO DATA(field_number_cofu) WITH KEY field_sap = field_sap seqnr = field_type_cofu-seqnr.
        IF field_type_cofu IS NOT INITIAL.
          value_out = '001'.
        ENDIF.
      ENDIF.


    ENDIF.
  ENDMETHOD.


METHOD get_mapping_values.
  IF molga IS NOT INITIAL.
    "get mapping values
    SELECT * FROM /sew/int_mapping INTO TABLE @mapping_values WHERE molga        IN @molga        AND
                                                                    infty        IN @infty        AND
                                                                    field_sap    IN @sap_field    AND
                                                                    field_oracle IN @oracle_field AND
                                                                    export       IN @export       AND
                                                                    import       IN @import.
  ENDIF.

  IF sy-subrc NE 0 OR
     molga    IS INITIAL.
    SELECT * FROM /sew/int_mapping INTO TABLE @mapping_values WHERE molga        EQ '*'           AND
                                                                    infty        IN @infty        AND
                                                                    field_sap    IN @sap_field    AND
                                                                    field_oracle IN @oracle_field AND
                                                                    export       IN @export       AND
                                                                    import       IN @import.
  ENDIF.
ENDMETHOD.


  METHOD get_massn.
    IF begda IS NOT INITIAL AND endda IS NOT INITIAL.
      READ TABLE fields WITH KEY field_oracle = field_oracle begda = begda endda = endda ASSIGNING FIELD-SYMBOL(<term_action>).
    ELSE.
      READ TABLE fields WITH KEY field_oracle = field_oracle ASSIGNING <term_action>.
    ENDIF.
    CASE infty.
      WHEN /sew/cl_int_constants=>it0000.
        IF seqnr = 000.
          IF begda IS NOT INITIAL AND endda IS NOT INITIAL.
            READ TABLE fields WITH KEY field_oracle = /sew/cl_int_constants=>fields-hire_date begda = begda endda = endda ASSIGNING FIELD-SYMBOL(<hire_date>).
            READ TABLE fields WITH KEY field_oracle = /sew/cl_int_constants=>fields-termination_date begda = begda endda = endda ASSIGNING FIELD-SYMBOL(<termination_date>).
          ELSE.
            READ TABLE fields WITH KEY field_oracle = /sew/cl_int_constants=>fields-hire_date ASSIGNING <hire_date>.
            READ TABLE fields WITH KEY field_oracle = /sew/cl_int_constants=>fields-termination_date ASSIGNING <termination_date>.
          ENDIF.
          IF pernr IS NOT INITIAL.
            IF <term_action>-value = 'DEATH' OR <term_action>-value = 'INVOLUNTARY_TERMINATION' OR <term_action>-value = 'INVOL_TERMINATE_PLACEMENT'
              OR <term_action>-value = 'REDUCTION_FORCE' OR <term_action>-value = 'RESIGNATION' OR <term_action>-value = 'RETIREMENT'
              OR <term_action>-value = 'TERMINATE_PLACEMENT' OR <term_action>-value = 'TERMINATION'.
              value_out = /sew/cl_int_constants=>termination.
            ELSEIF <term_action>-value EQ 'REHIRE'.
            ELSEIF <term_action>-value EQ 'GLB_TRANSFER'.
              IF <term_action>-endda IS NOT INITIAL AND <term_action>-endda NE /sew/cl_int_constants=>highdate.
                value_out = /sew/cl_int_constants=>termination.
              ELSE.
                value_out = /sew/cl_int_constants=>hire.
              ENDIF.
            ELSEIF <term_action>-value EQ 'HIRE_ADD_WORK_RELATION'.
              IF <term_action>-endda IS NOT INITIAL AND <term_action>-endda NE /sew/cl_int_constants=>highdate.
                value_out = /sew/cl_int_constants=>termination.
              ENDIF.
            ELSE.
              IF <hire_date> IS NOT INITIAL.
                SELECT * FROM pa0000 INTO TABLE @DATA(it0000) WHERE pernr = @pernr AND massn = @/sew/cl_int_constants=>hire.
                IF sy-subrc IS INITIAL.
                  SORT it0000 BY begda ASCENDING.
                  READ TABLE it0000 ASSIGNING FIELD-SYMBOL(<s0000>) INDEX 1.
                  DATA(hire_date) = /sew/cl_int_conversion=>convert_date( value_in = <hire_date>-value ).
                  IF <s0000>-begda NE hire_date.
                    SELECT SINGLE * FROM pa0041 INTO @DATA(s0041) WHERE pernr = @pernr.
                    IF s0041-dar01 = /sew/cl_int_constants=>date_type_hire.
                      IF s0041-dat01 EQ <s0000>-begda.
                        value_out = /sew/cl_int_constants=>hire_date_change.
                      ELSEIF s0041-dat01 NE hire_date.
                        value_out = /sew/cl_int_constants=>hire_date_change.
                      ENDIF.
                    ELSEIF s0041-dar02 = /sew/cl_int_constants=>date_type_hire.
                      IF s0041-dat02 EQ <s0000>-begda.
                        value_out = /sew/cl_int_constants=>hire_date_change.
                      ENDIF.
                    ELSEIF s0041-dar03 = /sew/cl_int_constants=>date_type_hire.
                      IF s0041-dat03 EQ <s0000>-begda.
                        value_out = /sew/cl_int_constants=>hire_date_change.
                      ENDIF.
                    ELSEIF s0041-dar04 = /sew/cl_int_constants=>date_type_hire.
                      IF s0041-dat04 EQ <s0000>-begda.
                        value_out = /sew/cl_int_constants=>hire_date_change.
                      ENDIF.
                    ELSEIF s0041-dar05 = /sew/cl_int_constants=>date_type_hire.
                      IF s0041-dat05 EQ <s0000>-begda.
                        value_out = /sew/cl_int_constants=>hire_date_change.
                      ENDIF.
                    ELSEIF s0041-dar06 = /sew/cl_int_constants=>date_type_hire.
                      IF s0041-dat06 EQ <s0000>-begda.
                        value_out = /sew/cl_int_constants=>hire_date_change.
                      ENDIF.
                    ELSEIF s0041-dar07 = /sew/cl_int_constants=>date_type_hire.
                      IF s0041-dat07 EQ <s0000>-begda.
                        value_out = /sew/cl_int_constants=>hire_date_change.
                      ENDIF.
                    ELSEIF s0041-dar08 = /sew/cl_int_constants=>date_type_hire.
                      IF s0041-dat08 EQ <s0000>-begda.
                        value_out = /sew/cl_int_constants=>hire_date_change.
                      ENDIF.
                    ELSEIF s0041-dar09 = /sew/cl_int_constants=>date_type_hire.
                      IF s0041-dat09 EQ <s0000>-begda.
                        value_out = /sew/cl_int_constants=>hire_date_change.
                      ENDIF.
                    ELSEIF s0041-dar10 = /sew/cl_int_constants=>date_type_hire.
                      IF s0041-dat10 EQ <s0000>-begda.
                        value_out = /sew/cl_int_constants=>hire_date_change.
                      ENDIF.
                    ELSEIF s0041-dar11 = /sew/cl_int_constants=>date_type_hire.
                      IF s0041-dat11 EQ <s0000>-begda.
                        value_out = /sew/cl_int_constants=>hire_date_change.
                      ENDIF.
                    ELSEIF s0041-dar12 = /sew/cl_int_constants=>date_type_hire.
                      IF s0041-dat12 EQ <s0000>-begda.
                        value_out = /sew/cl_int_constants=>hire_date_change.
                      ENDIF.
                    ELSEIF s0041-dar13 = /sew/cl_int_constants=>date_type_hire.
                      IF s0041-dat13 EQ <s0000>-begda.
                        value_out = /sew/cl_int_constants=>hire_date_change.
                      ENDIF.
                    ELSEIF s0041-dar14 = /sew/cl_int_constants=>date_type_hire.
                      IF s0041-dat14 EQ <s0000>-begda.
                        value_out = /sew/cl_int_constants=>hire_date_change.
                      ENDIF.
                    ELSEIF s0041-dar15 = /sew/cl_int_constants=>date_type_hire.
                      IF s0041-dat15 EQ <s0000>-begda.
                        value_out = /sew/cl_int_constants=>hire_date_change.
                      ENDIF.
                    ELSEIF s0041-dar16 = /sew/cl_int_constants=>date_type_hire.
                      IF s0041-dat16 EQ <s0000>-begda.
                        value_out = /sew/cl_int_constants=>hire_date_change.
                      ENDIF.
                    ELSEIF s0041-dar17 = /sew/cl_int_constants=>date_type_hire.
                      IF s0041-dat17 EQ <s0000>-begda.
                        value_out = /sew/cl_int_constants=>hire_date_change.
                      ENDIF.
                    ELSEIF s0041-dar18 = /sew/cl_int_constants=>date_type_hire.
                      IF s0041-dat18 EQ <s0000>-begda.
                        value_out = /sew/cl_int_constants=>hire_date_change.
                      ENDIF.
                    ELSEIF s0041-dar19 = /sew/cl_int_constants=>date_type_hire.
                      IF s0041-dat19 EQ <s0000>-begda.
                        value_out = /sew/cl_int_constants=>hire_date_change.
                      ENDIF.
                    ELSEIF s0041-dar20 = /sew/cl_int_constants=>date_type_hire.
                      IF s0041-dat20 EQ <s0000>-begda.
                        value_out = /sew/cl_int_constants=>hire_date_change.
                      ENDIF.
                    ELSEIF s0041-dar21 = /sew/cl_int_constants=>date_type_hire.
                      IF s0041-dat21 EQ <s0000>-begda.
                        value_out = /sew/cl_int_constants=>hire_date_change.
                      ENDIF.
                    ELSEIF s0041-dar22 = /sew/cl_int_constants=>date_type_hire.
                      IF s0041-dat22 EQ <s0000>-begda.
                        value_out = /sew/cl_int_constants=>hire_date_change.
                      ENDIF.
                    ELSEIF s0041-dar23 = /sew/cl_int_constants=>date_type_hire.
                      IF s0041-dat23 EQ <s0000>-begda.
                        value_out = /sew/cl_int_constants=>hire_date_change.
                      ENDIF.
                    ELSEIF s0041-dar24 = /sew/cl_int_constants=>date_type_hire.
                      IF s0041-dat24 EQ <s0000>-begda.
                        value_out = /sew/cl_int_constants=>hire_date_change.
                      ENDIF.
                    ENDIF.
                    IF <termination_date> IS NOT INITIAL AND <termination_date>-value NE /sew/cl_int_constants=>highdate and <termination_date>-value ne 'DELETED'.
                      value_out = /sew/cl_int_constants=>termination.
                    ENDIF.
                  ELSE.
                    IF <termination_date> IS NOT INITIAL AND <termination_date>-value NE /sew/cl_int_constants=>highdate and <termination_date>-value ne 'DELETED'.
                      value_out = /sew/cl_int_constants=>termination.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ELSE.
            IF <term_action>-value = 'HIRE'.
              value_out = /sew/cl_int_constants=>hire.
            ENDIF.
          ENDIF.
        ELSEIF seqnr = 001.
          READ TABLE fields WITH KEY field_oracle = /sew/cl_int_constants=>fields-ass_stat ASSIGNING FIELD-SYMBOL(<ass_stat>).
          READ TABLE fields WITH KEY field_oracle = /sew/cl_int_constants=>fields-action_code ASSIGNING FIELD-SYMBOL(<action_code>).
          IF <action_code>-value NE 'HIRE' AND <action_code>-value NE 'ADD_CWK' AND <action_code>-value NE 'HIRE_ADD_WORK_RELATION'.
            IF <ass_stat>-value = 'SUSPEND_PROCESS'.
              value_out = '08'.
            ELSE.
              do_simple = abap_true.
            ENDIF.
          ELSE.
            IF <action_code>-value EQ 'HIRE' OR <action_code>-value EQ 'ADD_CWK' OR <action_code>-value EQ 'HIRE_ADD_WORK_RELATION'.
              IF pernr IS NOT INITIAL.
                value_out = ''.
              ELSE.
                do_simple = abap_true.
              ENDIF.
            ELSE.
              do_simple = abap_true.
            ENDIF.
          ENDIF.
        ENDIF.
    ENDCASE.
  ENDMETHOD.


METHOD get_num01.
  IF export = abap_true.

  ELSEIF import = abap_true.
*    READ TABLE fields INTO DATA(field_number) WITH KEY field_sap = field_sap value = 'HM'.

*    DATA(field_number) = fields[ field_sap = field_sap value = 'HM' ].
*    READ TABLE fields INTO DATA(field_ext) WITH KEY field_sap = 'NUM01_EXT'.
*    READ TABLE fields INTO DATA(field_acode) WITH KEY field_sap = 'NUM01_ACODE'.
*    READ TABLE fields INTO DATA(field_ccode) WITH KEY field_sap = 'NUM01_CCODE'.
    READ TABLE fields INTO DATA(field_type) WITH KEY field_sap = 'NUM01_TYPE' value = 'HM'.

*    LOOP AT fields ASSIGNING FIELD-SYMBOL(<field_number>) WHERE field_sap = field_sap AND value = 'HM'.
*      EXIT.
*    ENDLOOP.
    IF field_type IS NOT INITIAL.
      READ TABLE fields INTO DATA(field_ext) WITH KEY field_sap = 'NUM01_EXT' seqnr = field_type-seqnr.
      READ TABLE fields INTO DATA(field_acode) WITH KEY field_sap = 'NUM01_ACODE' seqnr = field_type-seqnr.
      READ TABLE fields INTO DATA(field_ccode) WITH KEY field_sap = 'NUM01_CCODE' seqnr = field_type-seqnr.
      READ TABLE fields INTO DATA(field_number) WITH KEY field_sap = field_sap seqnr = field_type-seqnr.
      CONCATENATE '+' field_ccode-value '/' field_acode-value '/' field_number-value '/' field_ext-value '/' INTO value_out.

    ENDIF.
*    IF seqnr = 005.
*      value_out = '9001'.
*    ELSE.
*      IF field_type-value = 'HM'.
*        CONCATENATE '+' field_ccode-value '/' field_acode-value '/' field_number-value '/' field_ext-value '/' INTO value_out.
*      ENDIF.
*
*    ENDIF.
  ENDIF.
ENDMETHOD.


  METHOD get_objid.
    IF export = abap_true.

    ELSEIF import = abap_true.
      READ TABLE fields INTO DATA(field_objid) WITH KEY field_sap = field_sap.
      READ TABLE fields INTO DATA(field_classification) WITH KEY field_oracle = 'CLASSIFICATION_CODE'.

      LOOP AT fields ASSIGNING FIELD-SYMBOL(<field>).
        IF <field>-value = 'MANAGER'.
          /sew/cl_int_utility=>get_pernr_by_cloudid( EXPORTING cloud_id = CONV #( field_objid-value ) begda = field_objid-begda endda = field_objid-endda IMPORTING pernr = DATA(manager_pernr) ).
          /sew/cl_int_utility=>get_plans_it0001( EXPORTING pernr = manager_pernr begda = field_objid-begda endda = field_objid-endda IMPORTING plans = DATA(plans) ).
          value_out = plans.
          EXIT.
        ELSEIF <field>-value = 'COST_CENTER'.
          value_out = <field>-value.
          EXIT.
        ENDIF.
        IF seqnr = 002.
          SELECT SINGLE objid FROM hrp9401 INTO value_out WHERE oracleid = field_objid-value.
        ENDIF.
      ENDLOOP.

*      DATA(infty_operation) = NEW /sew/cl_int_it_operation( pernr = pernr ).
*      infty_operation->read_paxxxx(
*        EXPORTING
*          begda = field_objid-begda
*          endda = field_objid-endda
*          infty = /sew/cl_int_constants=>it0001
*          pernr = pernr
**          subty = subty
*          simu = simu
*        IMPORTING
*          return_tab = return_tab
*          record_tab = <record_old_tab>
*      ).
*      IF field_subty-value = 'W1'.
*        IF field_email-value IS INITIAL.
*          value_out = '9901'.
*        ELSE.
*          value_out = '0010'.
*        ENDIF.
*      ELSEIF field_subty-value = 'W2'.
*        value_out = '9906'.
*      ELSEIF field_subty-value = 'WF'.
*        value_out = '9902'.
*      ELSEIF field_subty-value = 'WM'.
*        value_out = '9905'.
*      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_orgeh.
    IF export = abap_true.

    ELSEIF import = abap_true.
      READ TABLE fields INTO DATA(field) WITH KEY field_sap = field_sap.
      SELECT SINGLE objid FROM hrp9401 INTO value_out WHERE oracleid = field-value AND begda LE field-endda AND endda GE field-begda.
      IF sy-subrc IS NOT INITIAL.
        "TODO
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_otype.
    IF export = abap_true.

    ELSEIF import = abap_true.
      READ TABLE fields INTO DATA(field_otype) WITH KEY field_sap = field_sap.
      READ TABLE fields INTO DATA(field_classification) WITH KEY field_oracle = 'CLASSIFICATION_CODE'.

      LOOP AT fields ASSIGNING FIELD-SYMBOL(<field>).
        IF <field>-value = 'MANAGER'.
          value_out = 'S'.
          EXIT.
        ELSEIF <field>-value = 'COST_CENTER'.
          value_out = 'O'.
          EXIT.
        ENDIF.
        IF seqnr = 002.
          value_out = 'O'.
        ENDIF.
      ENDLOOP.

*      READ TABLE fields INTO DATA(field_email) WITH KEY field_sap = 'USRID_LONG'.
*      IF field_subty-value = 'W1'.
*        IF field_email-value IS INITIAL.
*          value_out = '9901'.
*        ELSE.
*          value_out = '0010'.
*        ENDIF.
*      ELSEIF field_subty-value = 'W2'.
*        value_out = '9906'.
*      ELSEIF field_subty-value = 'WF'.
*        value_out = '9902'.
*      ELSEIF field_subty-value = 'WM'.
*        value_out = '9905'.
*      ENDIF.
    ENDIF.
  ENDMETHOD.


METHOD get_persk.

  IF import = abap_true.
    READ TABLE fields INTO DATA(field_persontype) WITH KEY field_sap = field_sap.
    READ TABLE fields INTO DATA(field_job) WITH KEY field_sap = 'PERSK_JOB'.
    READ TABLE fields INTO DATA(field_manager) WITH KEY field_sap = 'PERSK_MANAGER'.
    READ TABLE fields INTO DATA(field_wcategory) WITH KEY field_sap = 'PERSK_WCAT'.
    READ TABLE fields INTO DATA(field_empltype) WITH KEY field_sap = 'PERSG'.
    "Employee logic
    IF field_empltype-value NE 'C'.
      "Logic for permanent and fixed term employees is the same
      "Check Jobcode first - if Jobcode has values XXXXXXXXXXXXX then persk = 1A
      "Then check if manager flag is set -> persk = 1A
      "Then check worker category -> WC means persk = 1C and BC means persk = 1B
      IF field_persontype-value = 'Employee'.
        IF field_job-value = 'GJ_SEW_0196' OR field_job-value = 'GJ_SEW_0197' OR field_job-value = 'GJ_SEW_0198'
           OR field_job-value = 'GJ_SEW_0199'.
          value_out = '0A'.
        ELSE.
          IF field_manager-value = 'Y'.
            value_out = '1A'.
          ELSE.
            IF field_wcategory-value = 'WC'.
              value_out = '1C'.
            ELSEIF field_wcategory-value = 'BC'.
              value_out = '1B'.
            ENDIF.
          ENDIF.
        ENDIF.
        "Intern / Vacation helper / Working student
        "Worker category -> WC means persk = 1C and BC means persk = 1B
      ELSEIF field_persontype-value = 'Intern/Vacation Helper/Working Student'.
        IF field_wcategory-value = 'WC'.
          value_out = '1C'.
        ELSEIF field_wcategory-value = 'BC'.
          value_out = '1B'.
        ENDIF.
        "Direct mapping for apprentices, temp labour agency and external consultants
        "Apprentices means persk = 1D
      ELSEIF field_persontype-value = 'Apprentice'.
        value_out = '1D'.
      ENDIF.
      "Contingent worker logic
    ELSE.
      "Temp labour agency
      IF field_persontype-value = 'Temp Labour Agency'.
        value_out = '2B'.
        "External consultants
      ELSE.
        value_out = '2A'.
      ENDIF.

    ENDIF.

    "Perform in case of export mapping
  ELSEIF export = abap_true.
    "check for OracleField
    DATA(method_ext) = SWITCH string( field_oracle
                                      WHEN /sew/cl_mig_utils=>workertype                THEN /sew/cl_mig_utils=>persk_worker_type
                                      WHEN /sew/cl_mig_assignment=>assignmenttype       THEN /sew/cl_mig_utils=>persk_assign_type
                                      WHEN /sew/cl_mig_assignment=>assignmentcategory   THEN /sew/cl_mig_utils=>persk_assign_category
                                      WHEN /sew/cl_mig_assignment=>assignmentpersontype THEN /sew/cl_mig_utils=>persk_assign_person_type
                                      WHEN /sew/cl_mig_assignment=>persontypecode       THEN /sew/cl_mig_utils=>persk_person_type
                                      WHEN /sew/cl_mig_assignment=>systempersontype     THEN /sew/cl_mig_utils=>persk_system_person ).

    CHECK method_ext IS NOT INITIAL.

    DATA(method) = 'GET_' && method_ext.
    CALL METHOD /sew/cl_int_mapping=>(method)
      EXPORTING
        pernr        = pernr
        begda        = begda
        endda        = endda
        objid        = objid
        infty        = infty
        seqnr        = seqnr
        field_sap    = field_sap
        field_oracle = field_oracle
        import       = import
        export       = export
        fields       = fields
      IMPORTING
        value_out    = value_out
        do_simple    = do_simple
        message      = message.
  ENDIF.
ENDMETHOD.


METHOD get_persk_assign_category.
  IF export EQ abap_true.

    READ TABLE fields WITH KEY field_sap = /sew/cl_int_constants=>persk ASSIGNING FIELD-SYMBOL(<persk>).
    READ TABLE fields WITH KEY field_sap = /sew/cl_int_constants=>persg ASSIGNING FIELD-SYMBOL(<persg>).
    READ TABLE fields WITH KEY field_sap = 'MOLGA'                      ASSIGNING FIELD-SYMBOL(<molga>).

    DATA(country) = SWITCH string( sy-mandt
                                   WHEN /sew/cl_int_constants=>cofu_mandant-germany     THEN 'DE_'
                                   WHEN /sew/cl_int_constants=>cofu_mandant-france      THEN 'FR_'
                                   WHEN /sew/cl_int_constants=>cofu_mandant-netherlands THEN 'NL_'
                                   WHEN /sew/cl_int_constants=>cofu_mandant-australia   THEN 'AU_'
                                   WHEN /sew/cl_int_constants=>cofu_mandant-newzealand  THEN 'NZ_' ).

    "IT and AT are on the same mandant
    CHECK <molga> IS ASSIGNED.
    country        = SWITCH string( <molga>-value
                                    WHEN '03' THEN 'AT_'
                                    WHEN '15' THEN 'IT_'
                                    ELSE country ).

    CHECK country IS NOT INITIAL.

    CONCATENATE country <persg>-value '_' <persk>-value INTO value_out.
  ENDIF.
ENDMETHOD.


METHOD get_persk_assign_person_type.
  IF export EQ abap_true.

    READ TABLE fields WITH KEY field_sap = /sew/cl_int_constants=>persk ASSIGNING FIELD-SYMBOL(<persk>).
    READ TABLE fields WITH KEY field_sap = /sew/cl_int_constants=>persg ASSIGNING FIELD-SYMBOL(<persg>).
**IFT20211021 - Start Insert
*
    CHECK fields IS NOT INITIAL.
* End Insert
    "check SAP mandant
    CASE sy-mandt.
      WHEN /sew/cl_int_constants=>cofu_mandant-germany.
        "employees
        DATA(e_persk) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '05'  )
                                            ( sign = 'I' option = 'EQ' low = '10' )
                                            ( sign = 'I' option = 'BT' low = '15' high = '18' )
                                            ( sign = 'I' option = 'EQ' low = '20' )
                                            ( sign = 'I' option = 'BT' low = '30' high = '35')
                                            ( sign = 'I' option = 'BT' low = '60' high = '65')
                                            ( sign = 'I' option = 'BT' low = '85' high = '88')
                                            ( sign = 'I' option = 'EQ' low = '90' ) ) .
        "external consultants
        DATA(ec_persk) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '09'  )
                                             ( sign = 'I' option = 'BT' low = '95' high = '96' )
                                             ( sign = 'I' option = 'BT' low = '98' high = '99' ) ) .
        "Intern/vacation helper/working student
        DATA(vh_persk) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '40'  )
                                             ( sign = 'I' option = 'BT' low = '80' high = '84' )
                                             ( sign = 'I' option = 'EQ' low = '89'  ) ).
        "apprentice
        DATA(a_persk) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '70'  )
                                            ( sign = 'I' option = 'BT' low = '50' high = '93' ) ).

        "temp labour Agency
        DATA(la_persk) = VALUE rsdsselopt_t( ( sign = 'I' option = 'BT' low = '92' high = '94' ) ).

        "retiree
        DATA(r_persk) = VALUE rsdsselopt_t( ( sign = 'I' option = 'BT' low = '01' high = '04' )
                                            ( sign = 'I' option = 'BT' low = '06' high = '08' ) ) .

        CASE <persg>-value.
          WHEN '1'.
            value_out = /sew/cl_int_constants=>employee.

            IF <persk>-value IN ec_persk.
              value_out = /sew/cl_int_constants=>external_consultants.
            ENDIF.

            IF <persk>-value IN vh_persk.
              value_out = /sew/cl_int_constants=>vacation_helper.
            ENDIF.

            IF <persk>-value IN a_persk.
              value_out = /sew/cl_int_constants=>apprentice.
            ENDIF.

            IF <persk>-value IN la_persk.
              value_out = /sew/cl_int_constants=>labour_agency.
            ENDIF.

          WHEN '3'.
            CHECK <persk>-value IN r_persk.
            value_out = /sew/cl_int_constants=>retiree.
          WHEN '8'.
            value_out = /sew/cl_int_constants=>external_consultants.
        ENDCASE.

      WHEN /sew/cl_int_constants=>cofu_mandant-france.

        READ TABLE fields WITH KEY field_sap = 'ANSVH' ASSIGNING FIELD-SYMBOL(<ansvh>).

        "apprentice
        DATA(ansvh_ap) = VALUE rsdsselopt_t( ( sign = 'I' option = 'BT' low = '41' high = '42' )
                                             ( sign = 'I' option = 'BT' low = '44' high = '49' )
                                             ( sign = 'I' option = 'BT' low = '55' high = '57' )
                                             ( sign = 'I' option = 'EQ' low = '59' ) ).
        "employee
        DATA(ansvh_e) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '00'  )
                                            ( sign = 'I' option = 'EQ' low = '05'  )
                                            ( sign = 'I' option = 'EQ' low = '10'  )
                                            ( sign = 'I' option = 'EQ' low = '13'  )
                                            ( sign = 'I' option = 'EQ' low = '15'  )
                                            ( sign = 'I' option = 'BT' low = '20' high = '28' )
                                            ( sign = 'I' option = 'BT' low = '30' high = '31' )
                                            ( sign = 'I' option = 'EQ' low = '35' )
                                            ( sign = 'I' option = 'EQ' low = '43' )
                                            ( sign = 'I' option = 'BT' low = '50' high = '51' ) ).
        "vacation helper
        DATA(ansvh_vh) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '40' )
                                             ( sign = 'I' option = 'EQ' low = '70' ) ).
        "labour agency
        DATA(ansvh_la) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '90' )
                                             ( sign = 'I' option = 'BT' low = '94' high = '95' )  ).

        "default for PERSG 1, 2, 6, 7, 8
        IF <ansvh>-value IN ansvh_vh.
          value_out = /sew/cl_int_constants=>vacation_helper.
        ENDIF.

        IF <ansvh>-value IN ansvh_ap.
          value_out = /sew/cl_int_constants=>apprentice.
        ENDIF.

        IF <ansvh>-value IN ansvh_e.
          value_out = /sew/cl_int_constants=>employee.
        ENDIF.

        IF <ansvh>-value IN ansvh_la.
          value_out = /sew/cl_int_constants=>labour_agency.
        ENDIF.


        CASE <persg>-value.
          WHEN '1'.

            "vacation helper
            DATA(persk_vh) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '14' )
                                                 ( sign = 'I' option = 'EQ' low = '24' )
                                                 ( sign = 'I' option = 'EQ' low = '60' )
                                                 ( sign = 'I' option = 'BT' low = '70' high = '72' )
                                                 ( sign = 'I' option = 'EQ' low = '75' )
                                                 ( sign = 'I' option = 'EQ' low = 'F3' ) ).

            "apprentice
            DATA(persk_ap) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = 'F2' ) ).

            "employee
            DATA(persk_e) = VALUE rsdsselopt_t( ( sign = 'I' option = 'BT' low = '11' high = '12' )
                                                ( sign = 'I' option = 'BT' low = '15' high = '17' )
                                                ( sign = 'I' option = 'BT' low = '25' high = '26' )
                                                ( sign = 'I' option = 'EQ' low = '31' )
                                                ( sign = 'I' option = 'BT' low = 'F4' high = 'F5' ) ).

            IF <persk>-value IN persk_vh.
              value_out = /sew/cl_int_constants=>vacation_helper.
            ENDIF.

            IF <persk>-value IN persk_ap.
              value_out = /sew/cl_int_constants=>apprentice.
            ENDIF.

            IF <persk>-value IN persk_e.
              value_out = /sew/cl_int_constants=>employee.
            ENDIF.

          WHEN '4'.
            value_out = /sew/cl_int_constants=>vacation_helper.
          WHEN '5'.
            value_out = /sew/cl_int_constants=>retiree.
          WHEN '9'.
            value_out = /sew/cl_int_constants=>labour_agency.
        ENDCASE.

      WHEN /sew/cl_int_constants=>cofu_mandant-italy  OR
           /sew/cl_int_constants=>cofu_mandant-austria.
        "employee
        DATA(persl_e_itat) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '0A' )
                                                 ( sign = 'I' option = 'EQ' low = '1A' )
                                                 ( sign = 'I' option = 'EQ' low = '1B' )
                                                 ( sign = 'I' option = 'EQ' low = '1C' )
                                                 ( sign = 'I' option = 'EQ' low = 'A1' )
                                                 ( sign = 'I' option = 'EQ' low = 'I2' )
                                                 ( sign = 'I' option = 'EQ' low = 'I1' ) ).
        "apprentice
        DATA(persl_ap_itat) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '1D' ) ).

        "External consultants
        DATA(persl_ec_itat) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '2A' ) ).

        "Labour Agency
        DATA(persl_la_itat) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '2B' ) ).

        "vacation helper
        DATA(persl_vh_itat) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = 'I3' ) ).

        CASE <persg>-value.
          WHEN '1' OR '6'.
            value_out = /sew/cl_int_constants=>employee.

            CHECK <persk>-value IN persl_ap_itat.
            value_out = /sew/cl_int_constants=>apprentice.

          WHEN '2'.
            value_out = /sew/cl_int_constants=>retiree.
          WHEN '9'.
            value_out = /sew/cl_int_constants=>external_consultants.

            IF <persk>-value IN persl_la_itat.
              value_out = /sew/cl_int_constants=>labour_agency.
            ENDIF.

            IF <persk>-value IN persl_e_itat.
              value_out = /sew/cl_int_constants=>employee.
            ENDIF.

            IF <persk>-value IN persl_vh_itat.
              value_out = /sew/cl_int_constants=>vacation_helper.
            ENDIF.

        ENDCASE.


    ENDCASE.
  ENDIF.
ENDMETHOD.


  METHOD get_persk_assign_type.
    IF export EQ abap_true.

      READ TABLE fields WITH KEY field_sap = /sew/cl_int_constants=>persk ASSIGNING FIELD-SYMBOL(<persk>).
      READ TABLE fields WITH KEY field_sap = /sew/cl_int_constants=>persg ASSIGNING FIELD-SYMBOL(<persg>).

      "check SAP mandant
      CASE sy-mandt.
        WHEN /sew/cl_int_constants=>cofu_mandant-germany.
          DATA(e_persk) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '05'  )
                                              ( sign = 'I' option = 'EQ' low = '10' )
                                              ( sign = 'I' option = 'BT' low = '15' high = '18' )
                                              ( sign = 'I' option = 'EQ' low = '20' )
                                              ( sign = 'I' option = 'BT' low = '30' high = '35')
                                              ( sign = 'I' option = 'EQ' low = '40' )
                                              ( sign = 'I' option = 'BT' low = '50' high = '53')
                                              ( sign = 'I' option = 'BT' low = '60' high = '65')
                                              ( sign = 'I' option = 'EQ' low = '70' )
                                              ( sign = 'I' option = 'BT' low = '80' high = '90') ) .
          DATA(c_persk) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '09'  )
                                              ( sign = 'I' option = 'BT' low = '92' high = '99' ) ) .
          DATA(p_persk) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '01' high = '04' )
                                              ( sign = 'I' option = 'BT' low = '06' high = '08' ) ) .
          CASE <persg>-value.
            WHEN '1'.
              value_out = 'E'.
              CHECK <persk>-value IN c_persk.
              value_out = 'C'.
            WHEN '3'.
              value_out = 'P'.
            WHEN '8'.
              value_out = 'C'.
          ENDCASE.

        WHEN /sew/cl_int_constants=>cofu_mandant-france.

          READ TABLE fields WITH KEY field_sap = 'ANSVH' ASSIGNING FIELD-SYMBOL(<ansvh>).

          DATA(ansvh_e) = VALUE rsdsselopt_t( ( sign = 'I' option = 'BT' low = '41' high = '42' )
                                              ( sign = 'I' option = 'BT' low = '44' high = '49' )
                                              ( sign = 'I' option = 'BT' low = '55' high = '57' )
                                              ( sign = 'I' option = 'EQ' low = '59' )
                                              ( sign = 'I' option = 'EQ' low = '00'  )
                                              ( sign = 'I' option = 'EQ' low = '05'  )
                                              ( sign = 'I' option = 'EQ' low = '10'  )
                                              ( sign = 'I' option = 'EQ' low = '13'  )
                                              ( sign = 'I' option = 'EQ' low = '15'  )
                                              ( sign = 'I' option = 'BT' low = '20' high = '28' )
                                              ( sign = 'I' option = 'BT' low = '30' high = '31' )
                                              ( sign = 'I' option = 'EQ' low = '35' )
                                              ( sign = 'I' option = 'EQ' low = '40' )
                                              ( sign = 'I' option = 'EQ' low = '43' )
                                              ( sign = 'I' option = 'BT' low = '50' high = '51' )
                                              ( sign = 'I' option = 'EQ' low = '70' ) ).

          DATA(ansvh_c) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '90' )
                                              ( sign = 'I' option = 'BT' low = '94' high = '95' ) ).

          "default for PERSG 1, 2, 6, 7, 8
          IF <ansvh>-value IN ansvh_e.
            value_out = 'E'.
          ENDIF.
          IF <ansvh>-value IN ansvh_c.
            value_out = 'C'.
          ENDIF.

          CASE <persg>-value.
            WHEN '1'.
              value_out = 'E'.
              IF <ansvh>-value IN ansvh_c.
                value_out = 'C'.
              ENDIF.
            WHEN '4'.
              value_out = 'E'.
            WHEN '5'.
              value_out = 'P'.
            WHEN '9'.
              value_out = 'C'.
          ENDCASE.

      ENDCASE.
    ENDIF.
  ENDMETHOD.


  METHOD get_persk_person_type.
    IF export EQ abap_true.

      READ TABLE fields WITH KEY field_sap = /sew/cl_int_constants=>persk ASSIGNING FIELD-SYMBOL(<persk>).
      READ TABLE fields WITH KEY field_sap = /sew/cl_int_constants=>persg ASSIGNING FIELD-SYMBOL(<persg>).

      "check SAP mandant
      CASE sy-mandt.
        WHEN /sew/cl_int_constants=>cofu_mandant-germany.
          DATA(e_persk) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '05'  )
                                              ( sign = 'I' option = 'EQ' low = '10' )
                                              ( sign = 'I' option = 'BT' low = '15' high = '18' )
                                              ( sign = 'I' option = 'EQ' low = '20' )
                                              ( sign = 'I' option = 'BT' low = '30' high = '35')
                                              ( sign = 'I' option = 'EQ' low = '40' )
                                              ( sign = 'I' option = 'BT' low = '50' high = '53')
                                              ( sign = 'I' option = 'BT' low = '60' high = '65')
                                              ( sign = 'I' option = 'EQ' low = '70' )
                                              ( sign = 'I' option = 'BT' low = '80' high = '90') ) .
          DATA(c_persk) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '09'  )
                                              ( sign = 'I' option = 'BT' low = '92' high = '99' ) ) .
          DATA(p_persk) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '01' high = '04' )
                                              ( sign = 'I' option = 'BT' low = '06' high = '08' ) ) .
          CASE <persg>-value.
            WHEN '1'.
              value_out = /sew/cl_int_constants=>employee.
              CHECK <persk>-value IN c_persk.
              value_out = /sew/cl_int_constants=>contingent_worker.
            WHEN '3'.
              CHECK <persk>-value IN p_persk.
              value_out = /sew/cl_int_constants=>retiree.
            WHEN '8'.
              CHECK <persk>-value IN c_persk.
              value_out = /sew/cl_int_constants=>contingent_worker.
          ENDCASE.

        WHEN /sew/cl_int_constants=>cofu_mandant-france.
          READ TABLE fields WITH KEY field_sap = 'ANSVH' ASSIGNING FIELD-SYMBOL(<ansvh>).

          "apprentice
          DATA(ansvh_ap) = VALUE rsdsselopt_t( ( sign = 'I' option = 'BT' low = '41' high = '42' )
                                               ( sign = 'I' option = 'BT' low = '44' high = '49' )
                                               ( sign = 'I' option = 'BT' low = '55' high = '57' )
                                               ( sign = 'I' option = 'EQ' low = '59' ) ).
          "employee
          DATA(ansvh_e) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '00'  )
                                              ( sign = 'I' option = 'EQ' low = '05'  )
                                              ( sign = 'I' option = 'EQ' low = '10'  )
                                              ( sign = 'I' option = 'EQ' low = '13'  )
                                              ( sign = 'I' option = 'EQ' low = '15'  )
                                              ( sign = 'I' option = 'BT' low = '20' high = '28' )
                                              ( sign = 'I' option = 'BT' low = '30' high = '31' )
                                              ( sign = 'I' option = 'EQ' low = '35' )
                                              ( sign = 'I' option = 'EQ' low = '43' )
                                              ( sign = 'I' option = 'BT' low = '50' high = '51' ) ).
          "vacation helper
          DATA(ansvh_vh) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '40' )
                                               ( sign = 'I' option = 'EQ' low = '70' ) ).
          "labour agency
          DATA(ansvh_la) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '90' )
                                               ( sign = 'I' option = 'BT' low = '94' high = '95' )  ).

          "default for PERSG 1, 2, 6, 7, 8
          IF <ansvh>-value IN ansvh_vh.
            value_out = /sew/cl_int_constants=>vacation_helper.
          ENDIF.

          IF <ansvh>-value IN ansvh_ap.
            value_out = /sew/cl_int_constants=>apprentice.
          ENDIF.

          IF <ansvh>-value IN ansvh_e.
            value_out = /sew/cl_int_constants=>employee.
          ENDIF.

          IF <ansvh>-value IN ansvh_la.
            value_out = /sew/cl_int_constants=>labour_agency.
          ENDIF.


          CASE <persg>-value.
            WHEN '1'.
              "vacation helper
              DATA(persk_vh) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '14' )
                                                   ( sign = 'I' option = 'EQ' low = '24' )
                                                   ( sign = 'I' option = 'EQ' low = '60' )
                                                   ( sign = 'I' option = 'BT' low = '70' high = '72' )
                                                   ( sign = 'I' option = 'EQ' low = '75' )
                                                   ( sign = 'I' option = 'EQ' low = 'F3' ) ).

              "apprentice
              DATA(persk_ap) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = 'F2' ) ).

              "employee
              DATA(persk_e) = VALUE rsdsselopt_t( ( sign = 'I' option = 'BT' low = '11' high = '12' )
                                                  ( sign = 'I' option = 'BT' low = '15' high = '17' )
                                                  ( sign = 'I' option = 'BT' low = '25' high = '26' )
                                                  ( sign = 'I' option = 'EQ' low = '31' )
                                                  ( sign = 'I' option = 'BT' low = 'F4' high = 'F5' ) ).

              IF <persk>-value IN persk_vh.
                value_out = /sew/cl_int_constants=>vacation_helper.
              ENDIF.

              IF <persk>-value IN persk_ap.
                value_out = /sew/cl_int_constants=>apprentice.
              ENDIF.

              IF <persk>-value IN persk_e.
                value_out = /sew/cl_int_constants=>employee.
              ENDIF.

            WHEN '4'.
              value_out = /sew/cl_int_constants=>vacation_helper.
            WHEN '5'.
              value_out = /sew/cl_int_constants=>retiree.
            WHEN '9'.
              value_out = /sew/cl_int_constants=>labour_agency.
          ENDCASE.

      ENDCASE.
    ENDIF.
  ENDMETHOD.


  METHOD get_persk_system_person.
    IF export EQ abap_true.

      READ TABLE fields WITH KEY field_sap = /sew/cl_int_constants=>persk ASSIGNING FIELD-SYMBOL(<persk>).
      READ TABLE fields WITH KEY field_sap = /sew/cl_int_constants=>persg ASSIGNING FIELD-SYMBOL(<persg>).

      "check SAP mandant
      CASE sy-mandt.
        WHEN /sew/cl_int_constants=>cofu_mandant-germany.
          DATA(e_persk) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '05'  )
                                              ( sign = 'I' option = 'EQ' low = '10' )
                                              ( sign = 'I' option = 'BT' low = '15' high = '18' )
                                              ( sign = 'I' option = 'EQ' low = '20' )
                                              ( sign = 'I' option = 'BT' low = '30' high = '35')
                                              ( sign = 'I' option = 'EQ' low = '40' )
                                              ( sign = 'I' option = 'BT' low = '50' high = '53')
                                              ( sign = 'I' option = 'BT' low = '60' high = '65')
                                              ( sign = 'I' option = 'EQ' low = '70' )
                                              ( sign = 'I' option = 'BT' low = '80' high = '90') ) .
          DATA(c_persk) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '09'  )
                                              ( sign = 'I' option = 'BT' low = '92' high = '99' ) ) .
          DATA(p_persk) = VALUE rsdsselopt_t( ( sign = 'I' option = 'BT' low = '01' high = '04' )
                                              ( sign = 'I' option = 'BT' low = '06' high = '08' ) ) .
          CASE <persg>-value.
            WHEN '1'.
              value_out = /sew/cl_int_constants=>emp.
              CHECK <persk>-value IN c_persk.
              value_out = /sew/cl_int_constants=>cwk.
            WHEN '3'.
              CHECK <persk>-value IN p_persk.
              value_out = /sew/cl_int_constants=>pen.
            WHEN '8'.
              CHECK <persk>-value IN c_persk.
              value_out = /sew/cl_int_constants=>cwk.
          ENDCASE.

        WHEN /sew/cl_int_constants=>cofu_mandant-france.

          READ TABLE fields WITH KEY field_sap = 'ANSVH' ASSIGNING FIELD-SYMBOL(<ansvh>).

          DATA(ansvh_e) = VALUE rsdsselopt_t( ( sign = 'I' option = 'BT' low = '41' high = '42' )
                                              ( sign = 'I' option = 'BT' low = '44' high = '49' )
                                              ( sign = 'I' option = 'BT' low = '55' high = '57' )
                                              ( sign = 'I' option = 'EQ' low = '59' )
                                              ( sign = 'I' option = 'EQ' low = '00'  )
                                              ( sign = 'I' option = 'EQ' low = '05'  )
                                              ( sign = 'I' option = 'EQ' low = '10'  )
                                              ( sign = 'I' option = 'EQ' low = '13'  )
                                              ( sign = 'I' option = 'EQ' low = '15'  )
                                              ( sign = 'I' option = 'BT' low = '20' high = '28' )
                                              ( sign = 'I' option = 'BT' low = '30' high = '31' )
                                              ( sign = 'I' option = 'EQ' low = '35' )
                                              ( sign = 'I' option = 'EQ' low = '40' )
                                              ( sign = 'I' option = 'EQ' low = '43' )
                                              ( sign = 'I' option = 'BT' low = '50' high = '51' )
                                              ( sign = 'I' option = 'EQ' low = '70' ) ).

          DATA(ansvh_c) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '90' )
                                              ( sign = 'I' option = 'BT' low = '94' high = '95' ) ).

          "default for PERSG 1, 2, 6, 7, 8
          IF <ansvh>-value IN ansvh_e.
            value_out = /sew/cl_int_constants=>emp.
          ENDIF.
          IF <ansvh>-value IN ansvh_c.
            value_out = /sew/cl_int_constants=>cwk.
          ENDIF.

          CASE <persg>-value.
            WHEN '1'.
              value_out = /sew/cl_int_constants=>emp.
              IF <ansvh>-value IN ansvh_c.
                value_out = /sew/cl_int_constants=>cwk.
              ENDIF.
            WHEN '4'.
              value_out = /sew/cl_int_constants=>emp.
            WHEN '5'.
              value_out = /sew/cl_int_constants=>pen.
            WHEN '9'.
              value_out = /sew/cl_int_constants=>cwk.
          ENDCASE.
      ENDCASE.
    ENDIF.
  ENDMETHOD.


METHOD get_persk_worker_type.
  IF export EQ abap_true.

    READ TABLE fields WITH KEY field_sap = /sew/cl_int_constants=>persk ASSIGNING FIELD-SYMBOL(<persk>).
    READ TABLE fields WITH KEY field_sap = /sew/cl_int_constants=>persg ASSIGNING FIELD-SYMBOL(<persg>).

    "check SAP mandant
    CASE sy-mandt.
      WHEN /sew/cl_int_constants=>cofu_mandant-germany.
        DATA(e_persk) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '05'  )
                                            ( sign = 'I' option = 'EQ' low = '10' )
                                            ( sign = 'I' option = 'BT' low = '15' high = '18' )
                                            ( sign = 'I' option = 'EQ' low = '20' )
                                            ( sign = 'I' option = 'BT' low = '30' high = '35')
                                            ( sign = 'I' option = 'EQ' low = '40' )
                                            ( sign = 'I' option = 'BT' low = '50' high = '53')
                                            ( sign = 'I' option = 'BT' low = '60' high = '65')
                                            ( sign = 'I' option = 'EQ' low = '70' )
                                            ( sign = 'I' option = 'BT' low = '80' high = '90') ) .
        DATA(c_persk) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '09'  )
                                            ( sign = 'I' option = 'BT' low = '92' high = '99' ) ) .
        DATA(p_persk) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '01' high = '04' )
                                            ( sign = 'I' option = 'BT' low = '06' high = '08' ) ) .
        CASE <persg>-value.
          WHEN '1'.
            value_out = 'E'.
            CHECK <persk>-value IN c_persk.
            value_out = 'C'.
          WHEN '3'.
            value_out = 'P'.
          WHEN '8'.
            value_out = 'C'.
        ENDCASE.

      WHEN /sew/cl_int_constants=>cofu_mandant-france.

        READ TABLE fields WITH KEY field_sap = 'ANSVH' ASSIGNING FIELD-SYMBOL(<ansvh>).

        DATA(ansvh_e) = VALUE rsdsselopt_t( ( sign = 'I' option = 'BT' low = '41' high = '42' )
                                            ( sign = 'I' option = 'BT' low = '44' high = '49' )
                                            ( sign = 'I' option = 'BT' low = '55' high = '57' )
                                            ( sign = 'I' option = 'EQ' low = '59' )
                                            ( sign = 'I' option = 'EQ' low = '00'  )
                                            ( sign = 'I' option = 'EQ' low = '05'  )
                                            ( sign = 'I' option = 'EQ' low = '10'  )
                                            ( sign = 'I' option = 'EQ' low = '13'  )
                                            ( sign = 'I' option = 'EQ' low = '15'  )
                                            ( sign = 'I' option = 'BT' low = '20' high = '28' )
                                            ( sign = 'I' option = 'BT' low = '30' high = '31' )
                                            ( sign = 'I' option = 'EQ' low = '35' )
                                            ( sign = 'I' option = 'EQ' low = '40' )
                                            ( sign = 'I' option = 'EQ' low = '43' )
                                            ( sign = 'I' option = 'BT' low = '50' high = '51' )
                                            ( sign = 'I' option = 'EQ' low = '70' ) ).

        DATA(ansvh_c) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '90' )
                                            ( sign = 'I' option = 'BT' low = '94' high = '95' ) ).

        "default for PERSG 1, 2, 6, 7, 8
        IF <ansvh>-value IN ansvh_e.
          value_out = 'E'.
        ENDIF.
        IF <ansvh>-value IN ansvh_c.
          value_out = 'C'.
        ENDIF.

        CASE <persg>-value.
          WHEN '1'.
            value_out = 'E'.
            IF <ansvh>-value IN ansvh_c.
              value_out = 'C'.
            ENDIF.
          WHEN '4'.
            value_out = 'E'.
          WHEN '5'.
            value_out = 'P'.
          WHEN '9'.
            value_out = 'C'.
        ENDCASE.

    ENDCASE.
  ENDIF.
ENDMETHOD.


  METHOD get_relat.
    IF export = abap_true.

    ELSEIF import = abap_true.
      READ TABLE fields INTO DATA(field_relat) WITH KEY field_sap = field_sap.
      READ TABLE fields INTO DATA(field_classification) WITH KEY field_oracle = 'CLASSIFICATION_CODE'.

      LOOP AT fields ASSIGNING FIELD-SYMBOL(<field>).
        IF <field>-value = 'MANAGER'.
          value_out = '012'.
          EXIT.
        ELSEIF <field>-value = 'COST_CENTER'.
          value_out = '011'.
          EXIT.
        ENDIF.
        IF seqnr = 002.
          value_out = '002'.
        ENDIF.
      ENDLOOP.


*      IF field_subty-value = 'W1'.
*        IF field_email-value IS INITIAL.
*          value_out = '9901'.
*        ELSE.
*          value_out = '0010'.
*        ENDIF.
*      ELSEIF field_subty-value = 'W2'.
*        value_out = '9906'.
*      ELSEIF field_subty-value = 'WF'.
*        value_out = '9902'.
*      ELSEIF field_subty-value = 'WM'.
*        value_out = '9905'.
*      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_rsign.
    IF export = abap_true.

    ELSEIF import = abap_true.
      READ TABLE fields INTO DATA(field_rsign) WITH KEY field_sap = field_sap.
      READ TABLE fields INTO DATA(field_classification) WITH KEY field_oracle = 'CLASSIFICATION_CODE'.

      LOOP AT fields ASSIGNING FIELD-SYMBOL(<field>).
        IF <field>-value = 'MANAGER'.
          value_out = 'A'.
          EXIT.
        ELSEIF <field>-value = 'COST_CENTER'.
          value_out = 'A'.
          EXIT.
        ENDIF.
        IF seqnr = 002.
          value_out = 'A'.
        ENDIF.
      ENDLOOP.

*      IF field_subty-value = 'W1'.
*        IF field_email-value IS INITIAL.
*          value_out = '9901'.
*        ELSE.
*          value_out = '0010'.
*        ENDIF.
*      ELSEIF field_subty-value = 'W2'.
*        value_out = '9906'.
*      ELSEIF field_subty-value = 'WF'.
*        value_out = '9902'.
*      ELSEIF field_subty-value = 'WM'.
*        value_out = '9905'.
*      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_sobid.
    IF export = abap_true.

    ELSEIF import = abap_true.
      READ TABLE fields INTO DATA(field_sobid) WITH KEY field_sap = field_sap.
      READ TABLE fields INTO DATA(field_sclas) WITH KEY field_sap = 'SCLAS'.
      IF field_sclas-value NE 'COST_CENTER'.
        IF field_sclas-value = 'MANAGER'.
          /sew/cl_int_utility=>get_pernr_by_cloudid( EXPORTING cloud_id = CONV #( field_sobid-value ) begda = field_sobid-begda endda = field_sobid-endda IMPORTING pernr = DATA(manager_pernr) ).
          /sew/cl_int_utility=>get_plans_it0001( EXPORTING pernr = manager_pernr begda = field_sobid-begda endda = field_sobid-endda IMPORTING plans = DATA(plans) ).
          value_out = plans.
        ELSE.
          SELECT SINGLE objid FROM hrp9401 INTO value_out WHERE oracleid = field_sobid-value AND begda LE field_sobid-endda AND endda GE field_sobid-begda.
        ENDIF.
      ELSE.
        value_out = field_sobid-value.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_stat2.

**JMB20210712 start insert - differentiate between import and export logic
*
    IF import EQ abap_true.
      CASE infty.
        WHEN /sew/cl_int_constants=>it0000.
          IF seqnr = 001.

            READ TABLE fields WITH KEY field_oracle = /sew/cl_int_constants=>fields-ass_stat ASSIGNING FIELD-SYMBOL(<ass_stat>).
            READ TABLE fields WITH KEY field_oracle = /sew/cl_int_constants=>fields-action_code ASSIGNING FIELD-SYMBOL(<action_code>).
            IF <action_code>-value NE 'HIRE'.
              IF <ass_stat>-value = 'SUSPEND_PROCESS'.
                value_out = '1'.
              ELSE.
                do_simple = abap_true.
              ENDIF.
            ELSE.
              do_simple = abap_true.
            ENDIF.
          ENDIF.
      ENDCASE.
    ELSEIF export EQ abap_true.

      "differentiate between SAP mandants
      CASE sy-mandt.

        WHEN /sew/cl_int_constants=>cofu_mandant-germany. "DE
          READ TABLE fields WITH KEY field_sap = /sew/cl_int_constants=>kostl  ASSIGNING FIELD-SYMBOL(<kostl>).
          READ TABLE fields WITH KEY field_sap = /sew/cl_mig_assignment=>stat2 ASSIGNING FIELD-SYMBOL(<stat2>).
          READ TABLE fields WITH KEY field_sap = 'STAT1'                       ASSIGNING FIELD-SYMBOL(<stat1>).
          DATA(kostl_inactive) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '0019990000' )
                                                     ( sign = 'I' option = 'EQ' low = '0029990000' )
                                                     ( sign = 'I' option = 'EQ' low = '0039990000' )
                                                     ( sign = 'I' option = 'EQ' low = '0049990000' )
                                                     ( sign = 'I' option = 'EQ' low = '0059990000' )
                                                     ( sign = 'I' option = 'EQ' low = '0069990000' )
                                                     ( sign = 'I' option = 'EQ' low = '0079990000' )
                                                     ( sign = 'I' option = 'EQ' low = '0089990000' )
                                                     ( sign = 'I' option = 'EQ' low = '0099990000' ) ).

          "check employee status
          CASE <stat2>-value.
            WHEN '0' OR '2'.
              value_out = /sew/cl_int_constants=>oracle_assign_status_code-inactive_process.
            WHEN '1'.
              value_out = /sew/cl_int_constants=>oracle_assign_status_code-suspend_process.
            WHEN '3'.
              value_out = /sew/cl_int_constants=>oracle_assign_status_code-active_process.
          ENDCASE.

**JMB20210901 start insert - in case of ATZ II pass suspend process C400129651-5611
*
          CASE <stat1>-value.
            WHEN '2'.
              value_out = /sew/cl_int_constants=>oracle_assign_status_code-suspend_process.
          ENDCASE.
*JMB20210901 end insert

          "JMB20210914 I - check inactive cost center (C400129651-5610)
          CHECK <kostl>-value IN kostl_inactive.
          value_out = /sew/cl_int_constants=>oracle_assign_status_code-suspend_process.

      ENDCASE.

    ENDIF.
*JMB20210712 insert end
  ENDMETHOD.


METHOD get_subty.
  IF export = abap_true.

**JMB20210907 start insert - check for infty
*
    DATA(method) = 'GET_SUBTY_' && infty.
    CALL METHOD /sew/cl_int_mapping=>(method)
      EXPORTING
        pernr        = pernr
        begda        = begda
        endda        = endda
        objid        = objid
        infty        = infty
        seqnr        = seqnr
        field_sap    = field_sap
        field_oracle = field_oracle
        import       = import
        export       = export
        fields       = fields
      IMPORTING
        value_out    = value_out
        do_simple    = do_simple
        message      = message.
*JMB20210907 insert end

  ELSEIF import = abap_true.
    READ TABLE fields INTO DATA(field_subty) WITH KEY field_sap = field_sap.
    READ TABLE fields INTO DATA(field_email) WITH KEY field_sap = 'USRID_LONG'.
    IF seqnr = 005.
      value_out = '9001'.
    ELSE.
      IF field_subty-value = 'W1'.
        IF field_email-value IS INITIAL.
          value_out = '9901'.
        ELSE.
          value_out = '0010'.
        ENDIF.
      ELSEIF field_subty-value = 'W2'.
        value_out = '9906'.
      ELSEIF field_subty-value = 'WF'.
        value_out = '9902'.
      ELSEIF field_subty-value = 'WM'.
        value_out = '9905'.
      ELSEIF field_subty-value = 'H1'.
        IF field_email-value IS INITIAL.
        ELSE.
          value_out = '0010'.
        ENDIF.
      ENDIF.
      "User ID
      IF seqnr = 002.
        value_out = '0001'.
        "Netpass
      ELSEIF seqnr = 003.
        value_out = '0003'.
        "CRM and Third party payroll system ID
      ELSEIF seqnr = 004.
        READ TABLE fields INTO DATA(field_type) WITH KEY field_sap = field_sap.
        IF field_type-value = 'Third-Party Payroll ID'.
          value_out = '9900'.
        ELSEIF field_type-value = 'CRM Business Partner'.
          value_out = '9998'.
        ELSEIF field_type-value = 'SEW_REFERENCE_CODE'.
          value_out = '9017'.
*        ELSEIF field_type-value = 'SEW_REFERENCE_CODE'.
*          value_out = '9998'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDMETHOD.


  METHOD get_subty_2001.

    IF export EQ abap_true.

      READ TABLE fields WITH KEY field_sap = 'MOLGA' ASSIGNING FIELD-SYMBOL(<molga>).
      READ TABLE fields WITH KEY field_sap = 'AWART' ASSIGNING FIELD-SYMBOL(<subty>).


      SELECT SINGLE oracle_awart FROM /sew/int_awa_map WHERE molga EQ @<molga>-value
                                                         AND infty EQ '2001'
                                                         AND awart EQ @<subty>-value
                                                         AND begda <= @endda
                                                         AND endda >= @begda
                                                        INTO @DATA(oracle_awart).

      CHECK oracle_awart IS NOT INITIAL AND sy-subrc IS INITIAL.

      value_out = oracle_awart.

    ELSEIF import EQ abap_true.


    ENDIF.

  ENDMETHOD.


METHOD get_subty_2006.
  IF export = abap_true.
    READ TABLE fields WITH KEY field_sap = 'MOLGA' ASSIGNING FIELD-SYMBOL(<molga>).
    READ TABLE fields WITH KEY field_sap = 'SUBTY' ASSIGNING FIELD-SYMBOL(<subty>).
    READ TABLE fields WITH KEY field_sap = 'DESTA' ASSIGNING FIELD-SYMBOL(<desta>).
    READ TABLE fields WITH KEY field_sap = 'DEEND' ASSIGNING FIELD-SYMBOL(<deend>).

    CHECK <molga> IS ASSIGNED AND
          <subty> IS ASSIGNED.

    "Check MANDT to set Molga
    DATA(molga) = SWITCH molga( sy-mandt
                                WHEN /sew/cl_int_constants=>cofu_mandant-germany     THEN '01'
                                WHEN /sew/cl_int_constants=>cofu_mandant-france      THEN '06'
                                WHEN /sew/cl_int_constants=>cofu_mandant-netherlands THEN '05'
                                ELSE <molga>-value ).

    SELECT SINGLE planname, year_flag FROM /sew/int_abk_pln INTO @DATA(abk_pln) WHERE ktart EQ @<subty>-value AND
                                                                                      molga EQ @molga         AND
                                                                                      begda LE @endda         AND
                                                                                      endda GE @begda.

    CHECK abk_pln IS NOT INITIAL.
    value_out = abk_pln-planname.

    CHECK abk_pln-year_flag EQ abap_true.

    CONCATENATE value_out <desta>-value+0(4) INTO value_out SEPARATED BY space.

    "in case of france, start and end year are relevant for planname
    CHECK <molga>-value EQ '06'.

    CONCATENATE value_out '/' <desta>-value+0(4) INTO value_out.

  ELSEIF import = abap_true.

  ENDIF.
ENDMETHOD.


METHOD get_telnr.
  IF export = abap_true.

  ELSEIF import = abap_true.
    READ TABLE fields INTO DATA(field_type) WITH KEY field_sap = 'NUM01_TYPE' value = 'H1'.

*    LOOP AT fields ASSIGNING FIELD-SYMBOL(<field_number>) WHERE field_sap = field_sap AND value = 'HM'.
*      EXIT.
*    ENDLOOP.
    IF field_type IS NOT INITIAL.
      READ TABLE fields INTO DATA(field_ext) WITH KEY field_sap = 'NUM01_EXT' seqnr = field_type-seqnr.
      READ TABLE fields INTO DATA(field_acode) WITH KEY field_sap = 'NUM01_ACODE' seqnr = field_type-seqnr.
      READ TABLE fields INTO DATA(field_ccode) WITH KEY field_sap = 'NUM01_CCODE' seqnr = field_type-seqnr.
      READ TABLE fields INTO DATA(field_number) WITH KEY field_sap = field_sap seqnr = field_type-seqnr.
      CONCATENATE '+' field_ccode-value '/' field_acode-value '/' field_number-value '/' field_ext-value '/' INTO value_out.

    ENDIF.
  ENDIF.
ENDMETHOD.


  METHOD get_usrid.
    IF export = abap_true.

    ELSEIF import = abap_true.
      READ TABLE fields INTO DATA(field_number) WITH KEY field_sap = field_sap.
      READ TABLE fields INTO DATA(field_ext) WITH KEY field_sap = 'USRID_EXT'.
      READ TABLE fields INTO DATA(field_acode) WITH KEY field_sap = 'USRID_ACODE'.
      READ TABLE fields INTO DATA(field_ccode) WITH KEY field_sap = 'USRID_CCODE'.
      IF seqnr = 005.
        value_out = field_number.
      ELSE.
        CONCATENATE '+' field_ccode-value '/' field_acode-value '/' field_number-value '/' field_ext-value '/' INTO value_out.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_usrty.
    IF export = abap_true.

    ELSEIF import = abap_true.
      READ TABLE fields INTO DATA(field_subty) WITH KEY field_sap = field_sap.
      READ TABLE fields INTO DATA(field_email) WITH KEY field_sap = 'USRID_LONG'.
      IF seqnr = 005.
        value_out = '9001'.
      ELSE.
        IF field_subty-value = 'W1'.
          IF field_email-value IS INITIAL.
            value_out = '9901'.
          ELSE.
            value_out = '0010'.
          ENDIF.
        ELSEIF field_subty-value = 'W2'.
          value_out = '9906'.
        ELSEIF field_subty-value = 'WF'.
          value_out = '9902'.
        ELSEIF field_subty-value = 'WM'.
          value_out = '9905'.
        ENDIF.
        IF seqnr = 002.
          value_out = '0001'.
        ELSEIF seqnr = 003.
          value_out = '0003'.
          "CRM and Third party payroll system ID
        ELSEIF seqnr = 004.
          READ TABLE fields INTO DATA(field_type) WITH KEY field_sap = field_sap.
          IF field_type-value = 'Third-Party Payroll ID'.
            value_out = '9900'.
          ELSEIF field_type-value = 'CRM Business Partner'.
            value_out = '9998'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_werks.
    IF export = abap_true.
      READ TABLE fields WITH KEY field_sap = 'BTRTL' ASSIGNING FIELD-SYMBOL(<field>).
      IF <field> IS ASSIGNED.
        DATA(btrtl) = CONV btrtl( <field>-value ).
      ENDIF.
      READ TABLE fields WITH KEY field_sap = 'WERKS' ASSIGNING <field>.
      IF <field> IS ASSIGNED.
        DATA(werks) = CONV persa( <field>-value ).
      ENDIF.
      SELECT SINGLE * FROM /sew/int_map_loc WHERE werks = @werks AND btrtl = @btrtl INTO @DATA(mapping).
      IF sy-subrc IS INITIAL.
        value_out = mapping-location_code.
      ELSE.
*       TODO!!! ERROR HANDLING
      ENDIF.
    ELSEIF import = abap_true.
      READ TABLE fields WITH KEY field_oracle = 'LOCATIONID' ASSIGNING <field>.
      IF <field> IS ASSIGNED.
        DATA(location_code) = CONV werks( <field>-value ).
*        SELECT SINGLE * FROM /sew/int_map_loc WHERE location_code = @location_code INTO @mapping.
      ENDIF.
      IF sy-subrc IS INITIAL.
        werks = mapping-werks.
      ELSE.
*       TODO!!! ERROR HANDLING
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_zausw.
    IF export = abap_true.

    ELSEIF import = abap_true.
      READ TABLE fields INTO DATA(field_number) WITH KEY field_sap = field_sap.
      READ TABLE fields INTO DATA(field_type) WITH KEY field_sap = 'ZAUSW_TYPE'.


      IF field_type-value = 'Time Device Badge ID'."/sew/cl_int_constants=>zausw.
        value_out = field_number-value.
      ELSE. "IF field_type-value = 'ORA_TCLOCK_BADGE_ID'.
        READ TABLE fields INTO DATA(field_type_cofu) WITH KEY field_sap = 'ZAUSW_TYPE' value = 'ORA_TCLOCK_BADGE_ID'.
        READ TABLE fields INTO DATA(field_number_cofu) WITH KEY field_sap = field_sap seqnr = field_type_cofu-seqnr.
        value_out = field_number_cofu-value.
      ENDIF.


    ENDIF.
  ENDMETHOD.


  METHOD get_zpinc.
    IF export = abap_true.

    ELSEIF import = abap_true.
      READ TABLE fields INTO DATA(field_type_cofu) WITH KEY field_sap = 'ZPINC_TYPE' value = 'SEW_PERS_ACC_CODE'.
      READ TABLE fields INTO DATA(field_number_cofu) WITH KEY field_sap = field_sap seqnr = field_type_cofu-seqnr.
      value_out = field_number_cofu-value.


    ENDIF.
  ENDMETHOD.


  METHOD process_complex_mapping.
    DATA(method) = CONV string( 'GET_' && field_sap ).
    CALL METHOD /sew/cl_int_mapping=>(method)
      EXPORTING
        pernr        = pernr
        begda        = begda
        endda        = endda
        objid        = objid
        infty        = infty
        seqnr        = seqnr
        field_sap    = field_sap
        field_oracle = field_oracle
        import       = import
        export       = export
        fields       = fields
      IMPORTING
        value_out    = value
        do_simple    = do_simple
        message      = message.
  ENDMETHOD.


  METHOD process_mapping.
    DATA(field_oracle_new) = field_oracle.
    REPLACE ALL OCCURRENCES OF '/' IN field_oracle_new WITH ''.
    TRANSLATE field_oracle_new TO UPPER CASE.
    IF import = abap_true AND export = abap_false.
      READ TABLE mapping_fields WITH KEY infty = infty seqnr = seqnr field_sap = field_sap field_oracle = field_oracle_new import = import ASSIGNING FIELD-SYMBOL(<mapping>).
    ENDIF.
    IF export = abap_true AND import = abap_false..
      READ TABLE mapping_fields WITH KEY infty = infty seqnr = seqnr field_sap = field_sap field_oracle = field_oracle_new export = export ASSIGNING <mapping>.
    ENDIF.
    IF <mapping> IS ASSIGNED.
      IF <mapping>-is_complex = abap_true.
        DATA(value_tmp) = value.
        /sew/cl_int_mapping=>process_complex_mapping(
          EXPORTING
            pernr        = pernr
            begda        = begda
            endda        = endda
            objid        = objid
            infty        = infty
            seqnr        = seqnr
            field_sap    = field_sap
            field_oracle = field_oracle
            import       = import
            export       = export
            fields       = fields
          IMPORTING
            message      = message
            do_simple    = DATA(do_simple)
            CHANGING
               value     = value ).
        IF do_simple     = abap_true.
          value = value_tmp.
          /sew/cl_int_mapping=>process_simple_mapping(
           EXPORTING
             infty        = infty
             seqnr        = seqnr
             field_sap    = field_sap
             field_oracle = field_oracle_new
             import       = import
             export       = export
             mapping_values = VALUE #( FOR mapped_value IN mapping_values WHERE (
                         molga = <mapping>-molga
                         AND infty = <mapping>-infty
                         AND field_sap = <mapping>-field_sap
                         AND field_oracle = <mapping>-field_oracle
                         AND import = <mapping>-import
                         AND export = <mapping>-export ) ( mapped_value ) )
           IMPORTING
             message        = message
           CHANGING
             value = value ).
          CLEAR: value_tmp.
        ENDIF.
        IF value IS INITIAL AND infty NE '0006'.
          skip = abap_true.
        ENDIF.
      ELSE.
        /sew/cl_int_mapping=>process_simple_mapping(
          EXPORTING
            infty        = infty
            seqnr = seqnr
            field_sap    = field_sap
            field_oracle = field_oracle_new
            import = import
            export = export
            mapping_values = VALUE #( FOR mapped_value IN mapping_values WHERE (
                        molga = <mapping>-molga
                        AND infty = <mapping>-infty
                        AND field_sap = <mapping>-field_sap
                        AND field_oracle = <mapping>-field_oracle
                        AND import = <mapping>-import
                        AND export = <mapping>-export ) ( mapped_value ) )
          IMPORTING
            message        = message
          CHANGING
            value = value ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD process_simple_mapping.
    IF import = abap_true.
      READ TABLE mapping_values WITH KEY infty = infty field_sap = field_sap field_oracle = field_oracle value_oracle = value seqnr = seqnr ASSIGNING FIELD-SYMBOL(<mapping_value>).
      IF <mapping_value> IS ASSIGNED.
        value = <mapping_value>-value_sap.
      ELSE.
        message = VALUE bapiret1( type = /sew/cl_int_constants=>error
                                  id = /sew/cl_int_constants=>msg_class_int
                                  number = /sew/cl_int_constants=>msg_no-m6
                                  message_v1 = value
                                  message_v2 = field_sap
                                  message_v3 = field_oracle ).
        CLEAR value.
      ENDIF.
    ELSEIF export = abap_true.
      READ TABLE mapping_values WITH KEY infty = infty field_sap = field_sap field_oracle = field_oracle value_sap = value seqnr = seqnr ASSIGNING <mapping_value>.
      IF <mapping_value> IS ASSIGNED.
        value = <mapping_value>-value_oracle.
      ELSE.
        message = VALUE bapiret1( type = /sew/cl_int_constants=>error
                                  id = /sew/cl_int_constants=>msg_class_int
                                  number = /sew/cl_int_constants=>msg_no-m6
                                  message_v1 = value
                                  message_v2 = field_sap
                                  message_v3 = field_oracle ).
        CLEAR value.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
