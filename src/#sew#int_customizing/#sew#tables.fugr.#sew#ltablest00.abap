*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /SEW/INT_ABK_PLN................................*
DATA:  BEGIN OF STATUS_/SEW/INT_ABK_PLN              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/SEW/INT_ABK_PLN              .
CONTROLS: TCTRL_/SEW/INT_ABK_PLN
            TYPE TABLEVIEW USING SCREEN '0029'.
*...processing: /SEW/INT_AWA_MAP................................*
DATA:  BEGIN OF STATUS_/SEW/INT_AWA_MAP              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/SEW/INT_AWA_MAP              .
CONTROLS: TCTRL_/SEW/INT_AWA_MAP
            TYPE TABLEVIEW USING SCREEN '0030'.
*...processing: /SEW/INT_A_V....................................*
TABLES: /SEW/INT_A_V, */SEW/INT_A_V. "view work areas
CONTROLS: TCTRL_/SEW/INT_A_V
TYPE TABLEVIEW USING SCREEN '0031'.
DATA: BEGIN OF STATUS_/SEW/INT_A_V. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_/SEW/INT_A_V.
* Table for entries selected to show on screen
DATA: BEGIN OF /SEW/INT_A_V_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE /SEW/INT_A_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /SEW/INT_A_V_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF /SEW/INT_A_V_TOTAL OCCURS 0010.
INCLUDE STRUCTURE /SEW/INT_A_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /SEW/INT_A_V_TOTAL.

*...processing: /SEW/INT_C_V....................................*
TABLES: /SEW/INT_C_V, */SEW/INT_C_V. "view work areas
CONTROLS: TCTRL_/SEW/INT_C_V
TYPE TABLEVIEW USING SCREEN '0007'.
DATA: BEGIN OF STATUS_/SEW/INT_C_V. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_/SEW/INT_C_V.
* Table for entries selected to show on screen
DATA: BEGIN OF /SEW/INT_C_V_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE /SEW/INT_C_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /SEW/INT_C_V_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF /SEW/INT_C_V_TOTAL OCCURS 0010.
INCLUDE STRUCTURE /SEW/INT_C_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /SEW/INT_C_V_TOTAL.

*...processing: /SEW/INT_DCIF_CO................................*
DATA:  BEGIN OF STATUS_/SEW/INT_DCIF_CO              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/SEW/INT_DCIF_CO              .
CONTROLS: TCTRL_/SEW/INT_DCIF_CO
            TYPE TABLEVIEW USING SCREEN '0021'.
*...processing: /SEW/INT_FC_V...................................*
TABLES: /SEW/INT_FC_V, */SEW/INT_FC_V. "view work areas
CONTROLS: TCTRL_/SEW/INT_FC_V
TYPE TABLEVIEW USING SCREEN '0035'.
DATA: BEGIN OF STATUS_/SEW/INT_FC_V. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_/SEW/INT_FC_V.
* Table for entries selected to show on screen
DATA: BEGIN OF /SEW/INT_FC_V_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE /SEW/INT_FC_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /SEW/INT_FC_V_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF /SEW/INT_FC_V_TOTAL OCCURS 0010.
INCLUDE STRUCTURE /SEW/INT_FC_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /SEW/INT_FC_V_TOTAL.

*...processing: /SEW/INT_FO_V...................................*
TABLES: /SEW/INT_FO_V, */SEW/INT_FO_V. "view work areas
CONTROLS: TCTRL_/SEW/INT_FO_V
TYPE TABLEVIEW USING SCREEN '0015'.
DATA: BEGIN OF STATUS_/SEW/INT_FO_V. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_/SEW/INT_FO_V.
* Table for entries selected to show on screen
DATA: BEGIN OF /SEW/INT_FO_V_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE /SEW/INT_FO_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /SEW/INT_FO_V_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF /SEW/INT_FO_V_TOTAL OCCURS 0010.
INCLUDE STRUCTURE /SEW/INT_FO_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /SEW/INT_FO_V_TOTAL.

*...processing: /SEW/INT_FU_V...................................*
TABLES: /SEW/INT_FU_V, */SEW/INT_FU_V. "view work areas
CONTROLS: TCTRL_/SEW/INT_FU_V
TYPE TABLEVIEW USING SCREEN '0017'.
DATA: BEGIN OF STATUS_/SEW/INT_FU_V. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_/SEW/INT_FU_V.
* Table for entries selected to show on screen
DATA: BEGIN OF /SEW/INT_FU_V_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE /SEW/INT_FU_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /SEW/INT_FU_V_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF /SEW/INT_FU_V_TOTAL OCCURS 0010.
INCLUDE STRUCTURE /SEW/INT_FU_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /SEW/INT_FU_V_TOTAL.

*...processing: /SEW/INT_F_V....................................*
TABLES: /SEW/INT_F_V, */SEW/INT_F_V. "view work areas
CONTROLS: TCTRL_/SEW/INT_F_V
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_/SEW/INT_F_V. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_/SEW/INT_F_V.
* Table for entries selected to show on screen
DATA: BEGIN OF /SEW/INT_F_V_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE /SEW/INT_F_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /SEW/INT_F_V_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF /SEW/INT_F_V_TOTAL OCCURS 0010.
INCLUDE STRUCTURE /SEW/INT_F_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /SEW/INT_F_V_TOTAL.

*...processing: /SEW/INT_G_V....................................*
TABLES: /SEW/INT_G_V, */SEW/INT_G_V. "view work areas
CONTROLS: TCTRL_/SEW/INT_G_V
TYPE TABLEVIEW USING SCREEN '0006'.
DATA: BEGIN OF STATUS_/SEW/INT_G_V. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_/SEW/INT_G_V.
* Table for entries selected to show on screen
DATA: BEGIN OF /SEW/INT_G_V_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE /SEW/INT_G_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /SEW/INT_G_V_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF /SEW/INT_G_V_TOTAL OCCURS 0010.
INCLUDE STRUCTURE /SEW/INT_G_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /SEW/INT_G_V_TOTAL.

*...processing: /SEW/INT_IG_V...................................*
TABLES: /SEW/INT_IG_V, */SEW/INT_IG_V. "view work areas
CONTROLS: TCTRL_/SEW/INT_IG_V
TYPE TABLEVIEW USING SCREEN '0033'.
DATA: BEGIN OF STATUS_/SEW/INT_IG_V. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_/SEW/INT_IG_V.
* Table for entries selected to show on screen
DATA: BEGIN OF /SEW/INT_IG_V_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE /SEW/INT_IG_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /SEW/INT_IG_V_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF /SEW/INT_IG_V_TOTAL OCCURS 0010.
INCLUDE STRUCTURE /SEW/INT_IG_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /SEW/INT_IG_V_TOTAL.

*...processing: /SEW/INT_IT_AEND................................*
DATA:  BEGIN OF STATUS_/SEW/INT_IT_AEND              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/SEW/INT_IT_AEND              .
CONTROLS: TCTRL_/SEW/INT_IT_AEND
            TYPE TABLEVIEW USING SCREEN '0004'.
*...processing: /SEW/INT_I_V....................................*
TABLES: /SEW/INT_I_V, */SEW/INT_I_V. "view work areas
CONTROLS: TCTRL_/SEW/INT_I_V
TYPE TABLEVIEW USING SCREEN '0008'.
DATA: BEGIN OF STATUS_/SEW/INT_I_V. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_/SEW/INT_I_V.
* Table for entries selected to show on screen
DATA: BEGIN OF /SEW/INT_I_V_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE /SEW/INT_I_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /SEW/INT_I_V_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF /SEW/INT_I_V_TOTAL OCCURS 0010.
INCLUDE STRUCTURE /SEW/INT_I_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /SEW/INT_I_V_TOTAL.

*...processing: /SEW/INT_MAP_ACT................................*
DATA:  BEGIN OF STATUS_/SEW/INT_MAP_ACT              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/SEW/INT_MAP_ACT              .
CONTROLS: TCTRL_/SEW/INT_MAP_ACT
            TYPE TABLEVIEW USING SCREEN '0034'.
*...processing: /SEW/INT_MAP_LOC................................*
DATA:  BEGIN OF STATUS_/SEW/INT_MAP_LOC              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/SEW/INT_MAP_LOC              .
CONTROLS: TCTRL_/SEW/INT_MAP_LOC
            TYPE TABLEVIEW USING SCREEN '0016'.
*...processing: /SEW/INT_MF_V...................................*
TABLES: /SEW/INT_MF_V, */SEW/INT_MF_V. "view work areas
CONTROLS: TCTRL_/SEW/INT_MF_V
TYPE TABLEVIEW USING SCREEN '0013'.
DATA: BEGIN OF STATUS_/SEW/INT_MF_V. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_/SEW/INT_MF_V.
* Table for entries selected to show on screen
DATA: BEGIN OF /SEW/INT_MF_V_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE /SEW/INT_MF_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /SEW/INT_MF_V_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF /SEW/INT_MF_V_TOTAL OCCURS 0010.
INCLUDE STRUCTURE /SEW/INT_MF_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /SEW/INT_MF_V_TOTAL.

*...processing: /SEW/INT_M_V....................................*
TABLES: /SEW/INT_M_V, */SEW/INT_M_V. "view work areas
CONTROLS: TCTRL_/SEW/INT_M_V
TYPE TABLEVIEW USING SCREEN '0003'.
DATA: BEGIN OF STATUS_/SEW/INT_M_V. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_/SEW/INT_M_V.
* Table for entries selected to show on screen
DATA: BEGIN OF /SEW/INT_M_V_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE /SEW/INT_M_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /SEW/INT_M_V_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF /SEW/INT_M_V_TOTAL OCCURS 0010.
INCLUDE STRUCTURE /SEW/INT_M_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /SEW/INT_M_V_TOTAL.

*...processing: /SEW/INT_N_V....................................*
TABLES: /SEW/INT_N_V, */SEW/INT_N_V. "view work areas
CONTROLS: TCTRL_/SEW/INT_N_V
TYPE TABLEVIEW USING SCREEN '0018'.
DATA: BEGIN OF STATUS_/SEW/INT_N_V. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_/SEW/INT_N_V.
* Table for entries selected to show on screen
DATA: BEGIN OF /SEW/INT_N_V_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE /SEW/INT_N_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /SEW/INT_N_V_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF /SEW/INT_N_V_TOTAL OCCURS 0010.
INCLUDE STRUCTURE /SEW/INT_N_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /SEW/INT_N_V_TOTAL.

*...processing: /SEW/INT_O_V....................................*
TABLES: /SEW/INT_O_V, */SEW/INT_O_V. "view work areas
CONTROLS: TCTRL_/SEW/INT_O_V
TYPE TABLEVIEW USING SCREEN '0009'.
DATA: BEGIN OF STATUS_/SEW/INT_O_V. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_/SEW/INT_O_V.
* Table for entries selected to show on screen
DATA: BEGIN OF /SEW/INT_O_V_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE /SEW/INT_O_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /SEW/INT_O_V_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF /SEW/INT_O_V_TOTAL OCCURS 0010.
INCLUDE STRUCTURE /SEW/INT_O_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /SEW/INT_O_V_TOTAL.

*...processing: /SEW/INT_REL_FLD................................*
DATA:  BEGIN OF STATUS_/SEW/INT_REL_FLD              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/SEW/INT_REL_FLD              .
CONTROLS: TCTRL_/SEW/INT_REL_FLD
            TYPE TABLEVIEW USING SCREEN '0011'.
*...processing: /SEW/INT_REM_MAP................................*
DATA:  BEGIN OF STATUS_/SEW/INT_REM_MAP              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/SEW/INT_REM_MAP              .
CONTROLS: TCTRL_/SEW/INT_REM_MAP
            TYPE TABLEVIEW USING SCREEN '0028'.
*...processing: /SEW/INT_RES_MAP................................*
DATA:  BEGIN OF STATUS_/SEW/INT_RES_MAP              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/SEW/INT_RES_MAP              .
CONTROLS: TCTRL_/SEW/INT_RES_MAP
            TYPE TABLEVIEW USING SCREEN '0024'.
*...processing: /SEW/INT_S......................................*
TABLES: /SEW/INT_S, */SEW/INT_S. "view work areas
CONTROLS: TCTRL_/SEW/INT_S
TYPE TABLEVIEW USING SCREEN '0020'.
DATA: BEGIN OF STATUS_/SEW/INT_S. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_/SEW/INT_S.
* Table for entries selected to show on screen
DATA: BEGIN OF /SEW/INT_S_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE /SEW/INT_S.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /SEW/INT_S_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF /SEW/INT_S_TOTAL OCCURS 0010.
INCLUDE STRUCTURE /SEW/INT_S.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /SEW/INT_S_TOTAL.

*...processing: /SEW/INT_SUPERUS................................*
DATA:  BEGIN OF STATUS_/SEW/INT_SUPERUS              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/SEW/INT_SUPERUS              .
CONTROLS: TCTRL_/SEW/INT_SUPERUS
            TYPE TABLEVIEW USING SCREEN '0022'.
*...processing: /SEW/INT_V_V....................................*
TABLES: /SEW/INT_V_V, */SEW/INT_V_V. "view work areas
CONTROLS: TCTRL_/SEW/INT_V_V
TYPE TABLEVIEW USING SCREEN '0019'.
DATA: BEGIN OF STATUS_/SEW/INT_V_V. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_/SEW/INT_V_V.
* Table for entries selected to show on screen
DATA: BEGIN OF /SEW/INT_V_V_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE /SEW/INT_V_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /SEW/INT_V_V_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF /SEW/INT_V_V_TOTAL OCCURS 0010.
INCLUDE STRUCTURE /SEW/INT_V_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF /SEW/INT_V_V_TOTAL.

*...processing: /SEW/INT_WS_MAP.................................*
DATA:  BEGIN OF STATUS_/SEW/INT_WS_MAP               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/SEW/INT_WS_MAP               .
CONTROLS: TCTRL_/SEW/INT_WS_MAP
            TYPE TABLEVIEW USING SCREEN '0026'.
*.........table declarations:.................................*
TABLES: */SEW/INT_ABK_PLN              .
TABLES: */SEW/INT_AWA_MAP              .
TABLES: */SEW/INT_DCIF_CO              .
TABLES: */SEW/INT_IT_AEND              .
TABLES: */SEW/INT_MAP_ACT              .
TABLES: */SEW/INT_MAP_LOC              .
TABLES: */SEW/INT_REL_FLD              .
TABLES: */SEW/INT_REM_MAP              .
TABLES: */SEW/INT_RES_MAP              .
TABLES: */SEW/INT_SUPERUS              .
TABLES: */SEW/INT_WS_MAP               .
TABLES: /SEW/INT_ABK_PLN               .
TABLES: /SEW/INT_ACTIONS               .
TABLES: /SEW/INT_AWA_MAP               .
TABLES: /SEW/INT_CONVERS               .
TABLES: /SEW/INT_DCIF_CO               .
TABLES: /SEW/INT_FIELDS                .
TABLES: /SEW/INT_FIE_GEN               .
TABLES: /SEW/INT_FOLDERS               .
TABLES: /SEW/INT_FOUP                  .
TABLES: /SEW/INT_GENERAL               .
TABLES: /SEW/INT_INFOTYP               .
TABLES: /SEW/INT_INF_GEN               .
TABLES: /SEW/INT_IT_AEND               .
TABLES: /SEW/INT_MAPPING               .
TABLES: /SEW/INT_MAPP_FI               .
TABLES: /SEW/INT_MAP_ACT               .
TABLES: /SEW/INT_MAP_LOC               .
TABLES: /SEW/INT_NOTES                 .
TABLES: /SEW/INT_OBJECTS               .
TABLES: /SEW/INT_REL_FLD               .
TABLES: /SEW/INT_REM_MAP               .
TABLES: /SEW/INT_RES_MAP               .
TABLES: /SEW/INT_SUPERUS               .
TABLES: /SEW/INT_VALUES                .
TABLES: /SEW/INT_WS_MAP                .
TABLES: T000                           .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
