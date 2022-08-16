*&---------------------------------------------------------------------*
*& Include          /MIBA/ZOPEN_ARCHIVE_SCC
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*      SELECTION DECLARATIONS
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE TEXT-002.
PARAMETERS : r_tcode RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND abc,
             r_prog  RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK block1.

SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE TEXT-003.
PARAMETERS: p_tcode  TYPE tcode MODIF ID a1,
            p_prog   TYPE vari_reprt MODIF ID a2,
            p_var    TYPE variant,
            p_layout TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK block2.

SELECTION-SCREEN BEGIN OF BLOCK block3 WITH FRAME TITLE TEXT-004.
PARAMETERS : r_disp RADIOBUTTON GROUP g2  DEFAULT 'X' USER-COMMAND def,
             r_al11 RADIOBUTTON GROUP g2.
SELECTION-SCREEN END OF BLOCK block3.

SELECTION-SCREEN BEGIN OF BLOCK block4 WITH FRAME TITLE TEXT-005.
PARAMETERS : p_path TYPE string MODIF ID a3,
             p_file TYPE string MODIF ID a3,
             r_xls  RADIOBUTTON GROUP g3 DEFAULT 'X' MODIF ID a3,
             r_csv  RADIOBUTTON GROUP g3 MODIF ID a3.
SELECTION-SCREEN END OF BLOCK block4.

SELECTION-SCREEN BEGIN OF BLOCK block6 WITH FRAME TITLE TEXT-006.
PARAMETERS : r_new RADIOBUTTON GROUP g5 DEFAULT 'X' MODIF ID a3,
             r_rep RADIOBUTTON GROUP g5 MODIF ID a3.
SELECTION-SCREEN END OF BLOCK block6.

SELECTION-SCREEN BEGIN OF BLOCK block7 WITH FRAME TITLE TEXT-016.
PARAMETERS: p_def   RADIOBUTTON GROUP g6 DEFAULT 'X' MODIF ID a3,
            p_space RADIOBUTTON GROUP g6 MODIF ID a3,
            p_x     RADIOBUTTON GROUP g6 MODIF ID a3,
            p_y     RADIOBUTTON GROUP g6 MODIF ID a3.
SELECTION-SCREEN END OF BLOCK block7.
*---------------------------------------------------------------------*
*      AT SELECTION-SCREEN OUTPUT
*---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.

    IF r_tcode = 'X'.
      IF screen-group1 = 'A2'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ELSEIF r_prog = 'X'.
      IF screen-group1 = 'A1'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF r_disp = 'X'.
      IF screen-group1 = 'A3'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ELSE.
      IF screen-group1 = 'A3'.
        screen-active = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

  ENDLOOP.