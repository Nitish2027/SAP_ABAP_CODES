*&---------------------------------------------------------------------*
*& Report ZTEST_MVC_DEMO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest_mvc_demo.

TABLES: vbak.

SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: so_date FOR vbak-erdat.
SELECTION-SCREEN END OF BLOCK block1.

CLASS lcl_model DEFINITION.

  PUBLIC SECTION.
    TYPES: ty_erdat TYPE RANGE OF vbak-erdat.
    METHODS: get_data IMPORTING im_date TYPE ty_erdat.
    DATA: it_vbak TYPE STANDARD TABLE OF vbak.

ENDCLASS.

CLASS lcl_model IMPLEMENTATION.

  METHOD get_data.

    SELECT *
      FROM vbak
      INTO TABLE it_vbak
      WHERE erdat IN im_date.
    IF sy-subrc NE 0.
      CLEAR it_vbak.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_view DEFINITION.

  PUBLIC SECTION.
    TYPES: ty_vbak TYPE STANDARD TABLE OF vbak.
    METHODS: display_alv IMPORTING it_vbak    TYPE ty_vbak
                         CHANGING  it_vbak_ch TYPE ty_vbak.

    INTERFACES if_alv_rm_grid_friend .

    METHODS: evh_after_refresh FOR EVENT after_refresh OF cl_gui_alv_grid IMPORTING sender,
      evh_change_selection FOR EVENT delayed_changed_sel_callback OF cl_gui_alv_grid.

  PRIVATE SECTION.
    DATA: handler_added TYPE abap_bool.

ENDCLASS.

CLASS lcl_view IMPLEMENTATION.

  METHOD display_alv.

    DATA: lr_alv TYPE REF TO cl_salv_table.
    it_vbak_ch = it_vbak.

    TRY.

        IF it_vbak IS NOT INITIAL.

          cl_salv_table=>factory(
          IMPORTING
          r_salv_table = lr_alv
          CHANGING
          t_table = it_vbak_ch
          ).

        ENDIF.

      CATCH cx_salv_msg INTO DATA(lx_msg).
        cl_demo_output=>display( lx_msg ).

    ENDTRY.

    "Setting handler for event after_refresh for all grids
    SET HANDLER evh_after_refresh FOR ALL INSTANCES.

    "just to triger handler
    lr_alv->refresh( ).

    DATA(selections) = lr_alv->get_selections( ).
    selections->set_selection_mode(   if_salv_c_selection_mode=>cell  ). "Single row selection

    lr_alv->display( ).

  ENDMETHOD.

  METHOD evh_after_refresh.

    CHECK handler_added EQ abap_false.
    SET HANDLER me->evh_change_selection FOR sender.

    sender->set_delay_change_selection(
      EXPORTING
        time   =  100    " Time in Milliseconds
      EXCEPTIONS
        error  = 1
        OTHERS = 2
    ).
    IF sy-subrc <> 0.
*     message id sy-msgid type sy-msgty number sy-msgno
*                with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    sender->register_delayed_event(
      EXPORTING
        i_event_id =  sender->mc_evt_delayed_change_select
      EXCEPTIONS
        error      = 1
        OTHERS     = 2
    ).
    IF sy-subrc <> 0.
    ENDIF.

    sender->get_frontend_fieldcatalog(
      IMPORTING
        et_fieldcatalog = DATA(fcat)    " Field Catalog
    ).

    "setting editable fields
    ASSIGN fcat[ fieldname = 'ERDAT' ] TO FIELD-SYMBOL(<fcat>).
    IF sy-subrc EQ 0.
      <fcat>-edit = abap_true.
    ENDIF.

    sender->set_frontend_fieldcatalog( it_fieldcatalog = fcat ).
    sender->register_edit_event(
      EXPORTING
        i_event_id = sender->mc_evt_modified    " Event ID
      EXCEPTIONS
        error      = 1
        OTHERS     = 2
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    sender->set_ready_for_input(
        i_ready_for_input = 1
    ).

    handler_added = abap_true.

    sender->refresh_table_display(
*      exporting
*        is_stable      =     " With Stable Rows/Columns
*        i_soft_refresh =     " Without Sort, Filter, etc.
*      exceptions
*        finished       = 1
*        others         = 2
    ).
    IF sy-subrc <> 0.
*     message id sy-msgid type sy-msgty number sy-msgno
*                with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


  ENDMETHOD.

  METHOD evh_change_selection.
    MESSAGE i001(00) WITH 'You have selected one column'.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_controller DEFINITION.

  PUBLIC SECTION.
    METHODS: instantiate_model IMPORTING lv_model TYPE char20,
      instantiate_view IMPORTING lv_view TYPE char20.

    DATA: gt_model TYPE REF TO lcl_model,
          gt_view  TYPE REF TO lcl_view.

ENDCLASS.

CLASS lcl_controller IMPLEMENTATION.

  METHOD instantiate_model.

    DATA: lo_model TYPE REF TO object.
    CREATE OBJECT lo_model TYPE (lv_model).

    IF lo_model IS BOUND.
      gt_model ?= lo_model.
    ENDIF.

  ENDMETHOD.

  METHOD instantiate_view.

    DATA: lo_view TYPE REF TO object.
    CREATE OBJECT lo_view TYPE (lv_view).

    IF lo_view IS BOUND.
      gt_view ?= lo_view.

    ENDIF.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  DATA: lo_controller TYPE REF TO lcl_controller.
  CREATE OBJECT lo_controller.

  CALL METHOD lo_controller->instantiate_model( 'LCL_MODEL' ).

  CALL METHOD lo_controller->gt_model->get_data( so_date[] ).

  CALL METHOD lo_controller->instantiate_view( 'LCL_VIEW' ).

  CALL METHOD lo_controller->gt_view->display_alv(
    EXPORTING
      it_vbak    = lo_controller->gt_model->it_vbak
    CHANGING
      it_vbak_ch = lo_controller->gt_model->it_vbak ).