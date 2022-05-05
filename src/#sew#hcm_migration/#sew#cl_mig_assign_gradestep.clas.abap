class /SEW/CL_MIG_ASSIGN_GRADESTEP definition
  public
  create public .

public section.

  data PERNR type RSDSSELOPT_T .
  data BEGDA type BEGDA .
  data ENDDA type ENDDA .
  data COFU type BOOLEAN .
  data COGL type BOOLEAN .
  data MOLGA type RSDSSELOPT_T .
  data VP_GRADE_STEP type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  constants ASSIGN_GRADE_STEP type STRING value 'AssignmentGradeSteps' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !PERNR type RSDSSELOPT_T
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !COFU type BOOLEAN
      !COGL type BOOLEAN
      !MOLGA type RSDSSELOPT_T
      !COGU type BOOLEAN .
  class-methods MAP_COFU_DATA
    importing
      !PERNR type PERNR_D
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !TRFST type TRFST
    returning
      value(ASSIGNSTEP) type STRING .
  methods CREATE_METADATA
    returning
      value(METADATA) type STRING .
protected section.
private section.
ENDCLASS.



CLASS /SEW/CL_MIG_ASSIGN_GRADESTEP IMPLEMENTATION.


METHOD constructor.

  me->pernr = pernr.
  me->begda = begda.
  me->endda = endda.
  me->cofu  = cofu.
  me->cogl  = cogl.
  me->molga = molga.

  IF cofu EQ abap_true.
    vp_grade_step = VALUE #( ( name = 1  value = /sew/cl_mig_utils=>merge )
                             ( name = 2  value = assign_grade_step )
                             ( name = 6  value = 'AssignmentId(SourceSystemId)' )
                             ( name = 9  value = 'EffectiveStartDate' )
                             ( name = 10 value = 'EffectiveEndDate' )
                             ( name = 12 value = 'GradeStepName' ) ).
  ENDIF.
ENDMETHOD.


METHOD create_metadata.

  DESCRIBE TABLE vp_grade_step LINES DATA(length).

  LOOP AT vp_grade_step ASSIGNING FIELD-SYMBOL(<grade_step_data>).

    "set METADATA title
    CASE <grade_step_data>-name.
      WHEN 1.
        CONCATENATE /sew/cl_mig_utils=>metadata /sew/cl_mig_utils=>separator INTO metadata.
        CONTINUE.
    ENDCASE.

    CONCATENATE metadata <grade_step_data>-value INTO metadata.

    "set separator
    CHECK length NE sy-tabix.
    CONCATENATE metadata /sew/cl_mig_utils=>separator INTO metadata.
  ENDLOOP.

ENDMETHOD.


METHOD map_cofu_data.

  CONCATENATE /sew/cl_mig_utils=>sap sy-mandt INTO DATA(sys_id).

  DATA(assignmentid) = 'E' && pernr.

  DATA(begda_tmp) = /sew/cl_mig_utils=>convert_date( begda ).
  DATA(endda_tmp) = /sew/cl_mig_utils=>convert_date( endda ).

  CONCATENATE /sew/cl_mig_utils=>merge
              assign_grade_step
              assignmentid
              begda_tmp
              endda_tmp
              trfst
  INTO assignstep SEPARATED BY /sew/cl_mig_utils=>separator.
endmethod.
ENDCLASS.
