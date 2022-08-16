*&---------------------------------------------------------------------*
*& Report ZTEST_EMP_POC
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest_emp_poc.

TABLES: zemployee, zemployee_data.

SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE TEXT-001.

PARAMETERS : ch_disp AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK block1.

CLASS lcl_class DEFINITION.

  PUBLIC SECTION.

    METHODS: display,
      insert.

  PRIVATE SECTION.

    DATA:
      ls_employee      TYPE zemployee,
      ls_employee_data TYPE zemployee_data,
      lo_alv           TYPE REF TO cl_salv_table,
      lo_func          TYPE REF TO cl_salv_functions.

    DATA: lv_count TYPE int2 VALUE 0.

    DATA: gr_layout TYPE REF TO cl_salv_layout.
    DATA: key TYPE salv_s_layout_key.

ENDCLASS.

CLASS lcl_class IMPLEMENTATION.

  METHOD insert.

      SELECT
      FROM zemployee
      FIELDS MAX( id )
      INTO @DATA(lv_id).
      IF sy-subrc = 0.

        lv_id = lv_id + 1.

        ls_employee-id = lv_id.
        ls_employee-name = CONV #( |Employee_| && lv_id ).
        INSERT INTO zemployee VALUES ls_employee.

        lv_count = 1.
        ls_employee_data-id = lv_id.
        ls_employee_data-role = CONV #( |Role_| && lv_count ).
        ls_employee_data-age = 30.
        ls_employee_data-address = CONV #( |MIBA| ).
        ls_employee_data-zdate = sy-datum.
        ls_employee_data-ztime = sy-uzeit.
        INSERT INTO zemployee_data VALUES ls_employee_data.

        lv_count = 2.
        ls_employee_data-id = lv_id.
        ls_employee_data-role = CONV #( |Role_| && lv_count ).
        ls_employee_data-age = 35.
        ls_employee_data-address = CONV #( |MIBA| ).
        ls_employee_data-zdate = sy-datum.
        ls_employee_data-ztime = sy-uzeit.
        INSERT INTO zemployee_data VALUES ls_employee_data.

      ENDIF.

  ENDMETHOD.

  METHOD display.

    SELECT
    FROM zemployee AS head
    INNER JOIN zemployee_data AS item
    ON head~id = item~id
    FIELDS head~id,
                  name,
                  role,
                  age,
                  address,
                  zdate,
                  ztime
    INTO TABLE @DATA(lt_final).
    IF sy-subrc = 0.
      SORT lt_final BY id.
    ENDIF.

    TRY.

        cl_salv_table=>factory(
      IMPORTING
          r_salv_table = lo_alv
      CHANGING
        t_table = lt_final
      ).

        lo_func = lo_alv->get_functions( ).
        lo_func->set_all( abap_true ).

        gr_layout = lo_alv->get_layout( ).
        key-report = sy-repid.
        gr_layout->set_key( key ).
        gr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

        lo_alv->display( ).

      CATCH cx_salv_msg INTO DATA(lx_msg).

    ENDTRY.

  ENDMETHOD.

ENDCLASS.


START-OF-SELECTION.

  DATA(lo_class) = NEW lcl_class( ).

  IF ch_disp = 'X'.
    lo_class->insert( ).
    lo_class->display( ).
  ENDIF.