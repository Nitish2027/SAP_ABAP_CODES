*&---------------------------------------------------------------------*
*& Report ztest_events
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest_events.

CLASS LCL_CLASS DEFINITION.

    PUBLIC SECTION.

        EVENTS: EVENT1.

        METHODS: METHOD1 FOR EVENT EVENT1 OF LCL_CLASS.

        METHODS: TRIGGER1.

ENDCLASS.

CLASS LCL_CLASS IMPLEMENTATION.

METHOD: METHOD1.

    WRITE:/ 'Event Handeler Method'.

ENDMETHOD.

METHOD: TRIGGER1.

    WRITE:/ 'Trigger Method'.
    RAISE EVENT EVENT1.

ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
DATA: OBJ TYPE REF TO LCL_CLASS.

      CREATE OBJECT: OBJ.

      SET HANDLER OBJ->METHOD1 FOR OBJ.
*      SET HANDLER OBJ->METHOD1 FOR ALL INSTANCES.

      CALL METHOD OBJ->TRIGGER1.