class /SEW/CL_INFOTYPE_UTILS definition
  public
  create public .

public section.

  class-methods CLASS_CONSTRUCTOR .
  class-methods READ_INFOTYPE_RECORDS
    importing
      !PERNR type PERNR_D
      !INFTY type INFTY
      !SUBTY type SUBTY optional
      !OBJPS type OBJPS optional
      !SPRPS type SPRPS optional
      !BEGDA type BEGDA optional
      !ENDDA type ENDDA optional
      !NO_AUTH_CHECK type BOOLE_D optional
    exporting
      !PNNNN type STANDARD TABLE
      !DATA_EXISTS type BOOLE_D .
protected section.
private section.

  class-data READ_INFOTYPE type ref to IF_HRPA_READ_INFOTYPE .
ENDCLASS.



CLASS /SEW/CL_INFOTYPE_UTILS IMPLEMENTATION.


METHOD class_constructor.

* instantiate infotype reader class
  cl_hrpa_read_infotype=>get_instance( IMPORTING infotype_reader = read_infotype ).

ENDMETHOD.


METHOD read_infotype_records.

  DATA: pnnnn_tmp    TYPE REF TO data,
        descr    TYPE REF TO cl_abap_typedescr,
        infotype TYPE hrpad_prelp_tab.

  FIELD-SYMBOLS: <pnnnn> TYPE STANDARD TABLE.

  CLEAR: pnnnn, data_exists.

  descr = cl_abap_typedescr=>describe_by_data( pnnnn ).
  CREATE DATA pnnnn_tmp TYPE (descr->absolute_name).
  ASSIGN pnnnn_tmp->* TO <pnnnn>.

  read_infotype->read(
    EXPORTING
      tclas         = cl_hrpa_tclas=>tclas_employee
      pernr         = pernr
      infty         = infty
      subty         = subty
      objps         = objps
      sprps         = sprps
      begda         = begda
      endda         = endda
      no_auth_check = no_auth_check
    IMPORTING
      infotype_tab  = infotype
      data_exists   = data_exists ).

  cl_hr_pnnnn_type_cast=>prelp_to_pnnnn_tab(
    EXPORTING
      prelp_tab = infotype
    IMPORTING
      pnnnn_tab = <pnnnn> ).

  pnnnn = <pnnnn>.

ENDMETHOD.
ENDCLASS.
