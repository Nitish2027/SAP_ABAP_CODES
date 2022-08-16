*&---------------------------------------------------------------------*
*& Report ztest_read_modify
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest_read_modify.

TYPES: BEGIN OF ty_data,
       name TYPE char20,
       role TYPE char20,
       END OF ty_data.

DATA: it_data TYPE STANDARD TABLE OF ty_data.

it_data = VALUE #( ( name = 'nitish' role = 'abap')
                   ( name = 'prachi' role = 'abap')
                   ( name = 'abhinav' role = 'c++') ).

cl_demo_output=>write( it_data ).

DATA(result1) = it_data[ name = 'prachi' ].

cl_demo_output=>write( result1 ).

DATA(result2) = it_data[ name = 'prachi' ]-role.

cl_demo_output=>write( result2 ).


*DATA(result4) = it_data[ role = 'abap' ]-name.
*
*cl_demo_output=>write( result4 ).

TRY.

DATA(result3) = it_data[ name = 'sumit' ].

CATCH CX_SY_ITAB_LINE_NOT_FOUND.
cl_demo_output=>write( 'Entry Not Found' ).

ENDTRY.

cl_demo_output=>write( result3 ).

IF line_exists( it_data[ name = 'sumit' ] ).
cl_demo_output=>write( 'Entry Found' ).
ELSE.
cl_demo_output=>write( 'Entry Not Found' ).
ENDIF.

DATA(lv_index) = line_index( it_data[ name = 'prachi' ] ).

cl_demo_output=>write( lv_index ).

it_data[ name = 'prachi' ] = 'New_Prachi'.
cl_demo_output=>write( it_data ).

it_data[ name = 'nitish' ]-role = 'python'.
cl_demo_output=>display( it_data ).