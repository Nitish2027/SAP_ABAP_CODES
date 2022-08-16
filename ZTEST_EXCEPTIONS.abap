*&---------------------------------------------------------------------*
*& Report ZTEST_EXCEPTIONS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest_exceptions.

CLASS lcl_employee DEFINITION FINAL CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES: BEGIN OF empl_data,
             empid  TYPE int4,          "Employee ID
             emptyp TYPE string,        "Org Assignment data
             salary TYPE decfloat16,    "Pay data
             phone  TYPE numc10,        "Communication data
           END OF empl_data,
           empl_data_t TYPE SORTED TABLE OF empl_data WITH UNIQUE KEY empid.

    METHODS constructor IMPORTING VALUE(i_empid)   TYPE int4.
    METHODS get_data    RETURNING VALUE(rs_result) TYPE empl_data
                        RAISING   RESUMABLE(cx_no_data_found).

  PRIVATE SECTION.

    DATA  emp_id       TYPE int4.

    METHODS get_emptyp RETURNING VALUE(r_result) TYPE string
                       RAISING   cx_no_data_found.
    METHODS get_salary RETURNING VALUE(r_result) TYPE decfloat16
                       RAISING   RESUMABLE(cx_no_data_found).
    METHODS get_phone  RETURNING VALUE(r_result) TYPE numc10.
    METHODS get_emp_id RETURNING VALUE(r_result) TYPE int4.

ENDCLASS.

CLASS lcl_employee IMPLEMENTATION.

  METHOD constructor.
    me->emp_id = i_empid.
  ENDMETHOD.

  METHOD get_data.
    rs_result = VALUE #( empid  = me->get_emp_id( )
                         emptyp = me->get_emptyp( )
                         salary = me->get_salary( )
                         phone  = me->get_phone( )
                       ).
  ENDMETHOD.

  METHOD get_emptyp.
    r_result = SWITCH #( me->get_emp_id( )
                WHEN 1 THEN |Full-Time|
                WHEN 2 THEN |Part-Time|
                WHEN 3 THEN |Contractor|
                WHEN 4 THEN |Casual|
                ELSE THROW cx_no_data_found(
                            rel_proc_id = CONV #( me->get_emp_id( ) ) )
              ).
  ENDMETHOD.

  METHOD get_phone.
    r_result = SWITCH #( me->get_emptyp( )
                WHEN `Full-Time` THEN |1234567890|
                WHEN `Part-Time` THEN |5678901234|
                WHEN `Casual`    THEN |7890123456|
                ELSE |0399999999|
              ).
  ENDMETHOD.

  METHOD get_salary.
    r_result = SWITCH #( me->get_emptyp( )
                WHEN `Full-Time` THEN 50000
                WHEN `Part-Time` THEN 25000
                WHEN `Casual`    THEN 5000
                ELSE THROW RESUMABLE cx_no_data_found(
                            rel_proc_id = CONV #( me->get_emp_id( ) ) )
               ).
  ENDMETHOD.

  METHOD get_emp_id.
    r_result = me->emp_id.
  ENDMETHOD.

ENDCLASS.

DATA extract_t TYPE lcl_employee=>empl_data_t.
DATA error_t   TYPE string_table.

START-OF-SELECTION.

  DATA(all_employees_t) = VALUE int4_table(  ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) ).

  LOOP AT all_employees_t REFERENCE INTO DATA(dref).
    TRY.
        INSERT NEW lcl_employee( dref->* )->get_data( ) INTO TABLE extract_t.

      CATCH BEFORE UNWIND cx_no_data_found INTO DATA(lx_no_data).
        IF lx_no_data->is_resumable = abap_true.
          "Resumable Exception was raised
          RESUME.
        ELSE.
          "Non-Resumable Exception was raised
          error_t = VALUE #( BASE error_t ( lx_no_data->get_text( ) ) ).
        ENDIF.
    ENDTRY.
  ENDLOOP.

  cl_demo_output=>new( )->write( extract_t )->write( error_t )->display( ).