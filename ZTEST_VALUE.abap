*&---------------------------------------------------------------------*
*& Report ztest_value
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest_value.

TYPES: BEGIN OF ty_1,
       name TYPE string,
       role TYPE string,
       END OF ty_1.

*DATA(wa1) = VALUE ty_1( name = 'nitish' role = 'abap' ).

*DATA: wa1 TYPE ty_1.
*
*      wa1 = VALUE #( name = 'nitish' role = 'abap' ).
*
*DATA(wa2) = VALUE #( BASE wa1 name = 'prachi' role = 'abap' ).
*
*cl_demo_output=>display( wa2 ).


TYPES: tt_st1 TYPE TABLE OF ty_1 WITH EMPTY KEY.

DATA(itab1) = VALUE tt_st1( ( name = 'nitish' role = 'abap')
                            ( name = 'prachi' role = 'abap')
                            ( name = 'abhinav' role = 'c++') ).

cl_demo_output=>display( itab1 ).

*DATA: itab1 TYPE TABLE OF ty_1.
*
*itab1 = VALUE #( ( name = 'nitish' role = 'abap')
*                 ( name = 'prachi' role = 'abap')
*                 ( name = 'abhinav' role = 'c++') ).
*
*APPEND VALUE #( name = 'kalli' role = 'millioner') TO itab1.
*
*DATA(itab2) = VALUE #( BASE itab1 ( name = 'vidyasagar' role = 'python')
*                            ( name = 'sharmila' role = 'portal') ).
*
*cl_demo_output=>display( itab1 ).
*cl_demo_output=>display( itab2 ).

*DATA(wa1) = itab1[ name = 'nitish' ].

*DATA(wa1) = VALUE #( itab1[ name = 'nitisha' ] OPTIONAL ).

*DATA: wa_default TYPE ty_1.
*
*wa_default = VALUE #( name = 'default' role = 'default' ).
*
*DATA(wa1) = VALUE #( itab1[ name = 'nitisha' ] DEFAULT wa_default ).
*
*cl_demo_output=>display( wa1 ).