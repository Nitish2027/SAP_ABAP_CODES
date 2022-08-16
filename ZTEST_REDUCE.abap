*&---------------------------------------------------------------------*
*& Report ztest_reduce
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest_reduce.

*"To find the sum of first 10 numbers.
DATA(v1) = REDUCE i( INIT sum = 0 FOR i = 1 THEN i + 1 UNTIL i > 10 NEXT sum = sum + i ).
*
"If we do not provide THEN, by default it will take it as increment of 1 only.
DATA(v2) = REDUCE i( INIT sum = 0 FOR i = 1 UNTIL i > 10 NEXT sum = sum + i ).

WRITE:/ v1, v2.
*
*"Reducing to a string
DATA(v3) = REDUCE string( INIT text = `Count up:` FOR i = 1 THEN i + 1 UNTIL i > 10 NEXT text = text && | { i } | ).

WRITE:/ v3.

DATA(v4) = REDUCE string( INIT text = `Count down:` FOR i = 10 THEN i - 1 UNTIL i < 1 NEXT text = text && | { i } | ).

WRITE:/ v4.

TYPES: BEGIN OF ty_student,
       name TYPE char20,
       subject TYPE char20,
       marks TYPE i,
       END OF ty_student.

DATA: it_student TYPE TABLE OF ty_student.

it_student = VALUE #( FOR i = 1 UNTIL i > 5 ( name = 'Student' &&  i subject = 'Physics' marks = i ) ).

cl_demo_output=>write( it_student ).

DATA(total_marks) = REDUCE i( INIT total = 0 FOR wa IN it_student NEXT total = total + wa-marks ).

cl_demo_output=>write( total_marks ).

APPEND VALUE #( name = 'Student5' subject = 'Maths' marks = 5 ) TO it_student.

cl_demo_output=>write( it_student ).

DATA(total_marks2) = REDUCE i( INIT total = 0 FOR wa IN it_student WHERE ( name = 'Student5' ) NEXT total = total + wa-marks ).

cl_demo_output=>display( total_marks2 ).