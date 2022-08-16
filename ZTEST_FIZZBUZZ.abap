*&---------------------------------------------------------------------*
*& Report ztest_fizzbuzz_program
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest_fizzbuzz_program.

CLASS LCL_FIZZBUZZ DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS: CREATE
      RETURNING VALUE(RO_FIZZBUZZ) TYPE REF TO LCL_FIZZBUZZ.

    METHODS: RUN.
  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.

CLASS LCL_FIZZBUZZ IMPLEMENTATION.

  METHOD CREATE.
    RO_FIZZBUZZ = NEW LCL_FIZZBUZZ(  ).
  ENDMETHOD.

  METHOD RUN.

    DATA(LT_FIZZBUZZ) = VALUE STRINGTAB(
        FOR I = 1 WHILE I <= 100 (
        COND STRING(
            LET LV_3 = I MOD 3
                LV_5 = I MOD 5 IN
            WHEN LV_3 = 0 AND LV_5 = 0 THEN |FIZZBUZZ|
            WHEN LV_3 = 0 THEN |FIZZ|
            WHEN LV_5 = 0 THEN |BUZZ|
            ELSE I ) ) ).


    CL_DEMO_OUTPUT=>DISPLAY( LT_FIZZBUZZ ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  LCL_FIZZBUZZ=>CREATE( )->RUN( ).