*&---------------------------------------------------------------------*
*& Report ztest_filter
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest_filter.

TYPES: BEGIN OF ty_data,
       name TYpE char20,
       attend type char01,  " 'P-Present, A-Absent'
       END OF ty_data.

*DATA: it_data TYPE SORTED TABLE OF ty_data WITH NON-UNIQUE KEY attend.
*
*it_data = VALUE #( ( name = 'harry' attend = 'P' )
*                   ( name = 'hermione' attend = 'P' )
*                   ( name = 'ron' attend = 'P' )
*                   ( name = 'draco' attend = 'A' )   ).
*
*cl_demo_output=>write( it_data ).
*
*DATA(it_present) = FILTER #( it_data WHERE attend = 'P' ).
*
*cl_demo_output=>write( it_present ).
*
*DATA(it_absent) = FILTER #( it_data EXCEPT WHERE attend = 'P' ).
*
*cl_demo_output=>display( it_absent ).

*DATA: it_data TYPE STANDARD TABLE OF ty_data WITH NON-UNIQUE SORTED KEY att COMPONENTS attend.
*
*it_data = VALUE #( ( name = 'harry' attend = 'P' )
*                   ( name = 'hermione' attend = 'P' )
*                   ( name = 'ron' attend = 'P' )
*                   ( name = 'draco' attend = 'A' )
*                   ( name = 'hagrid' attend = '' )  ).

*cl_demo_output=>write( it_data ).
*
*DATA(it_present) = FILTER #( it_data USING KEY att WHERE attend = 'P' ).
*
*cl_demo_output=>write( it_present ).
*
*DATA(it_absent) = FILTER #( it_data EXCEPT USING KEY att WHERE attend = 'P' ).
*
*cl_demo_output=>display( it_absent ).

DATA: it_data TYPE STANDARD TABLE OF ty_data WITH NON-UNIQUE SORTED KEY att COMPONENTS attend.

it_data = VALUE #( ( name = 'harry' attend = 'P' )
                   ( name = 'hermione' attend = 'P' )
                   ( name = 'ron' attend = 'P' )
                   ( name = 'draco' attend = 'A' )
                   ( name = 'hagrid' attend = '' )  ).

DATA: it_filter TYPE SORTED TABLE OF CHAR01 WITH NON-UNIQUE KEY TABLE_LINE.

it_filter = VALUE #( ( 'A') ( 'P' ) ).

DATA(it_filtered) = FILTER #( it_data IN it_filter WHERE attend = table_line  ).

cl_demo_output=>display( it_filtered ).