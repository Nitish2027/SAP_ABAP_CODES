*&---------------------------------------------------------------------*
*& Report ztest_insert
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest_insert.

DATA: wa_employee TYPE zemployee,
      it_emp TYPE TABLE OF zemployee.

*wa_employee = VALUE #( id = 1 name = 'Nitish' role = 'abap' ).

*INSERT INTO zemployee VALUES wa_employee.
*WRITE:/ SY-SUBRC.

*wa_employee = VALUE #( id = 2 name = 'Nitish' role = 'abap' ).
*
*INSERT zemployee FROM wa_employee.
*WRITE:/ SY-SUBRC.
*
*DELETE FROM zemployee.

*it_emp = VALUE #( ( id = 1 name = 'nitish' role = 'abap')
*                  ( id = 2 name = 'prachi' role = 'abap')
*                  ( id = 3 name = 'abhinav' role = 'c++') ).
*
*INSERT zemployee FROM TABLE it_emp.
*WRITE:/ SY-SUBRC.

*it_emp = VALUE #( ( id = 1 name = 'nitish' role = 'abap')
*                  ( id = 2 name = 'swarup' role = 'powerapps')
*                  ( id = 3 name = 'rohit' role = 'abap') ).

*it_emp = VALUE #( ( id = 7 name = 'test_user' role = 'abap') ).

INSERT zemployee FROM TABLE it_emp ACCEPTING DUPLICATE KEYS.
WRITE:/ SY-SUBRC.


"If need to populate another z table with similar structure.
*INSERT zemoployee2 FROM ( SELECT * FROM zemployee).