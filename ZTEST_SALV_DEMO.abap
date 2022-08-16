*&---------------------------------------------------------------------*
*& Report ztest_salv_demo
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest_salv_demo.

TABLES: zemployee.

SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: so_id  FOR zemployee-id.

PARAMETERS: p_name(30) TYPE c,
                          p_role(30) TYPE c.

PARAMETERS : r_disp RADIOBUTTON GROUP g1 DEFAULT 'X',
             r_ins  RADIOBUTTON GROUP g1.

SELECTION-SCREEN END OF BLOCK block1.

CLASS lcl_class DEFINITION.

  PUBLIC SECTION.

    METHODS:
      get_data,
      display,
      insert.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: lt_employee TYPE TABLE OF zemployee,
          ls_employee TYPE zemployee,
          lo_alv      TYPE REF TO cl_salv_table.

ENDCLASS.

CLASS lcl_class IMPLEMENTATION.

  METHOD insert.

    ls_employee-mandt = sy-mandt.
    ls_employee-id = so_id-low.
    ls_employee-name = p_name.
    ls_employee-role = p_role.

*    INSERT INTO target VALUES wa.
*    INSERT target FROM wa.
*    INSERT target FROM TABLE itab.

    INSERT INTO zemployee VALUES ls_employee.
    IF sy-subrc = 0.
      MESSAGE 'Record Inserted Successfully!' TYPE 'I' DISPLAY LIKE 'I'.
    ELSE.
      MESSAGE 'Error! Entry Already Exists' TYPE 'I' DISPLAY LIKE 'I'.
    ENDIF.

  ENDMETHOD.

  METHOD get_data.

    SELECT mandt, id, name, role
    FROM zemployee
    INTO TABLE @lt_employee
    WHERE id IN @so_id.
    IF sy-subrc = 0.
      SORT lt_employee BY id.
    ENDIF.

  ENDMETHOD.

  METHOD display.

    TRY.
        cl_salv_table=>factory(
        IMPORTING
        r_salv_table = lo_alv
        CHANGING
        t_table = lt_employee
        ).

      CATCH cx_salv_msg INTO DATA(lx_msg).
        cl_demo_output=>display( lx_msg ).

    ENDTRY.

    lo_alv->display( ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  DATA(lo_class) = NEW lcl_class( ).
  lo_class->get_data( ).

  IF r_disp = 'X'.
    lo_class->display( ).
  ENDIF.
  IF r_ins = 'X'.
    lo_class->insert( ).
  ENDIF.