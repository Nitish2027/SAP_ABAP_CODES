*&---------------------------------------------------------------------*
*& Report ztest_reduce_example
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest_reduce_example.

CLASS LCL_CLASS DEFINITION CREATE PRIVATE FINAL.

  PUBLIC SECTION.

    CLASS-METHODS: CREATE
      RETURNING
        VALUE(RO_OBJ) TYPE REF TO LCL_CLASS.

    METHODS: RUN.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.

CLASS LCL_CLASS IMPLEMENTATION.

  METHOD CREATE.
    ro_obj = NEW LCL_CLASS( ).
  ENDMETHOD.

  METHOD RUN.

    DATA: lt_test TYPE TABLE OF i WITH EMPTY KEY.

          lt_test = VALUE #( for j = 1 WHILE j <= 10 ( j ) ).

*    cl_demo_output=>DISPLAY( |Hello World!| ).
     cl_demo_output=>new( )->next_section( |Simple Sum using REDUCE| )->write(

        REDUCE i( INIT sum = 0
                  FOR i = 1 UNTIL i > 10
                  NEXT sum = sum + i )

     )->next_section( |String Concatenation using REDUCE| )->write(

        REDUCE string( INIT text = |Count Down:|
                       FOR i = 10 THEN i - 1 UNTIL i = 0
                       NEXT text = text && | { i } | )

     )->next_section( |Table Example| )->write(
        lt_test

     )->next_section( |Table Reduction using Reduce| )->write(

        REDUCE i( INIT x = 0
                  FOR ls_test IN lt_test
                  NEXT x = x + ls_test )

     )->display( ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

lcl_class=>create( )->run( ).