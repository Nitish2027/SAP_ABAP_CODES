*&---------------------------------------------------------------------*
*& Report ZTEST_USEREXIT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest_userexit.

DATA: width  TYPE int1,
      length TYPE int1.

DATA: area TYPE int1.

START-OF-SELECTION.

*width = 5.
*length = 10.
*
*CALL FUNCTION 'Z_RECT_CALC_AREA'
*  EXPORTING
*    i_width        = width
*    i_length       = length
* IMPORTING
*   E_AREA         = area.
*
*write:/ 'Area = ' , area.

  CALL FUNCTION 'Z_RECT_POPUP_CALC_AREA'
    IMPORTING
      e_area = area.

  WRITE:/ 'Area = ' , area.