*&---------------------------------------------------------------------*
*& Report ztest_segment
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest_segment.

"To work with data containing delimiters such as , or / or ;

DATA: lv_data TYPE string VALUE 'Nitish,Prachi,Vidyasgar,Sharmila'.
                                " 1         2       3           4
                                "-4        -3      -2          -1

DATA(lv_result1) = SEGMENT( VAL =  lv_data INDEX = 2 SEP = ',' ).

WRITE:/ lv_result1.

DATA(lv_result2) = SEGMENT( VAL =  lv_data INDEX = -1 SEP = ',' ).

WRITE:/ lv_result2.
WRITE:/.

"By Default the separator is SPACE.

DATA: lv_result  TYPE string.
DATA: lv_data1 TYPE string VALUE 'Nitish Prachi Vidyasgar Sharmila'.
DO.

TRY.

lv_result = SEGMENT( VAL =  lv_data1 INDEX = SY-INDEX ).
WRITE:/ lv_result.

CATCH CX_SY_STRG_PAR_VAL.
EXIT.
ENDTRY.

ENDDO.