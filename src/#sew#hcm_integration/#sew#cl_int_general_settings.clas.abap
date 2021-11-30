class /SEW/CL_INT_GENERAL_SETTINGS definition
  public
  final
  create public .

public section.

  class-methods GET_ACTIONHANDLER
    returning
      value(VALUE) type STRING .
  class-methods GET_AL11_ARCHIVE
    returning
      value(VALUE) type STRING .
  class-methods GET_AL11_DOWN
    returning
      value(VALUE) type STRING .
  class-methods GET_AL11_UP
    returning
      value(VALUE) type STRING .
  class-methods GET_INFOTYPHANDLER
    returning
      value(VALUE) type STRING .
protected section.
private section.
ENDCLASS.



CLASS /SEW/CL_INT_GENERAL_SETTINGS IMPLEMENTATION.


  method GET_ACTIONHANDLER.
    SELECT SINGLE path FROM /sew/int_general WHERE field = '05' INTO @value.
  endmethod.


  method GET_AL11_ARCHIVE.
    SELECT SINGLE path FROM /sew/int_general WHERE field = '03' INTO @value.
  endmethod.


  METHOD GET_AL11_DOWN.
    SELECT SINGLE path FROM /sew/int_general WHERE field = '01' INTO @value.
  ENDMETHOD.


  method GET_AL11_UP.
    SELECT SINGLE path FROM /sew/int_general WHERE field = '02' INTO @value.
  endmethod.


  method GET_INFOTYPHANDLER.
    SELECT SINGLE path FROM /sew/int_general WHERE field = '04' INTO @value.
  endmethod.
ENDCLASS.
