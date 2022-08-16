*&---------------------------------------------------------------------*
*& Report ztest_corresponding
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest_corresponding.

TYPES: BEGIN OF ty_data1,
       c1 TYPE i,
       c2 TYPE i,
       c3 TYPE i,
       c4 TYPE i,
       END OF ty_data1,
       BEGIN OF ty_data2,
       c5 TYPE i,
       c6 TYPE i,
       c7 TYPE i,
       c1 TYPE i,
       c2 TYPE i,
       END OF ty_data2.

DATA: itab1 TYPE TABLE OF ty_data1,
      itab2 TYPE TABLE OF ty_data2,
      itab3 TYPE TABLE OF ty_data2,
      itab4 TYPE TABLE OF ty_data2.

itab1 = VALUE #( ( c1 = 1 c2 = 2 c3 = 3 c4 = 4 )
                 ( c1 = 10 c2 = 20 c3 = 30 c4 = 40 ) ).

cl_demo_output=>write( itab1 ).

itab2 = CORRESPONDING #( itab1 ).

cl_demo_output=>write( itab2 ).

itab3 = CORRESPONDING #( itab1 MAPPING c5 = c1 c6 = c2 ).

cl_demo_output=>write( itab3 ).

itab4 = CORRESPONDING #( itab1 MAPPING c5 = c1 c6 = c2 EXCEPT c1 ).

cl_demo_output=>display( itab4 ).