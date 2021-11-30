class /SEW/CL_INT_STATICS definition
  public
  final
  create public .

public section.

  types:
    t_it_aend TYPE TABLE OF /sew/int_it_aend with DEFAULT KEY .
  types:
    t_om_aend TYPE TABLE OF /sew/int_om_aend with DEFAULT KEY .
  types:
    t_messages TYPE TABLE OF /sew/int_msg_l .

  class-data TEST_RUN type BOOLE_D .
  class-data ARCHIVE type BOOLE_D .
  class-data READ_ARCHIVE type BOOLE_D .
  class-data IT_AEND type T_IT_AEND .
  class-data OM_AEND type T_OM_AEND .
  class-data MESSAGES type T_MESSAGES .
  class-data ORA_BAL type /SEW/TT_ORA_BAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /SEW/CL_INT_STATICS IMPLEMENTATION.
ENDCLASS.
