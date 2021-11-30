class /SEW/CL_INT_DATA_TRANSFORM definition
  public
  final
  create public .

public section.

  data INT_RUN type GUID_32 .
  data MOLGA type MOLGA .
  data SAP_ID type HROBJID .
  data CLOUD_ID type /SEW/DD_OBJECTID .
  data PRELP_TAB type PRELP_TAB .
  data PRELP_TAB_ORIG type PRELP_TAB .
  data PRELP_TAB_NOCHNG type PRELP_TAB .
  data INFOTYPE_CHANGES type ref to /SEW/CL_INT_IT_AEND .

  class-methods GET_INSTANCE
    importing
      !INT_RUN type GUID optional
      !MOLGA type MOLGA optional
    returning
      value(RO_INSTANCE) type ref to /SEW/CL_INT_DATA_TRANSFORM .
  methods CONSTRUCTOR
    importing
      !INT_RUN type GUID
      !MOLGA type MOLGA .
protected section.
private section.

  class-data GO_INSTANCE type ref to /SEW/CL_INT_DATA_TRANSFORM .
ENDCLASS.



CLASS /SEW/CL_INT_DATA_TRANSFORM IMPLEMENTATION.


  METHOD constructor.
    me->int_run = int_run.
    me->molga = molga.
  ENDMETHOD.


  METHOD get_instance.
    IF /sew/cl_int_data_transform=>go_instance IS BOUND.
*      AND /sew/cl_int_data_transform=>go_instance->molga = molga
*      AND /sew/cl_int_data_transform=>go_instance->int_run = int_run.
      ro_instance = /sew/cl_int_data_transform=>go_instance.
    ELSE.
      CREATE OBJECT /sew/cl_int_data_transform=>go_instance
        EXPORTING
          int_run = int_run
          molga   = molga.
      ro_instance = /sew/cl_int_data_transform=>go_instance.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
