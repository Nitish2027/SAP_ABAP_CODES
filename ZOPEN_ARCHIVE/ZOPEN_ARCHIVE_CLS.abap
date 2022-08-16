*&---------------------------------------------------------------------*
*& Include          /MIBA/ZOPEN_ARCHIVE_CLS
*&---------------------------------------------------------------------*

CLASS lcl_class DEFINITION.

    PUBLIC SECTION.
      CLASS-DATA: gr_obj TYPE REF TO lcl_class.
      CLASS-METHODS: class_constructor.
      METHODS:  get_program,
        execute_program,
        place_file,
        display,
        validation.
  
  ENDCLASS.                 "LCL_CLASS DEFINITION
  
  CLASS lcl_class IMPLEMENTATION.
  
    METHOD class_constructor.
      TRY.
          IF gr_obj IS NOT BOUND.
            CREATE OBJECT gr_obj.
          ENDIF.
        CATCH cx_root.
      ENDTRY.
    ENDMETHOD.
  
  
    METHOD get_program.
  
      IF r_tcode = abap_true.
        SELECT SINGLE pgmna
          FROM tstc
          INTO gv_program
          WHERE tcode EQ p_tcode.
        IF sy-subrc = 0.
          IF gv_program IS INITIAL.
            MESSAGE TEXT-008 TYPE 'E' DISPLAY LIKE 'I'.
          ENDIF.
        ENDIF.
      ENDIF.
  
      IF r_prog = abap_true.
        gv_program = p_prog.
      ENDIF.
  
      gv_var = p_var.
      gv_layout = p_layout.
  
      IF r_csv = abap_true.
        gv_extension = '.CSV'.
        gt_tab = ';'.
      ELSEIF r_xls = abap_true.
        gv_extension = '.XLS'.
        gt_tab = cl_abap_char_utilities=>horizontal_tab.
      ENDIF.
  
      IF p_file IS NOT INITIAL.
        gv_filename = p_file.
      ELSE.
        IF r_tcode = abap_true.
          gv_filename = p_tcode.
        ELSEIF r_prog = abap_true.
          gv_filename = p_prog.
        ENDIF.
      ENDIF.
  
      REPLACE FIRST OCCURRENCE OF '/' IN gv_filename WITH ''.
      REPLACE ALL OCCURRENCES OF '/' IN gv_filename WITH '_'.
  
      IF r_new = abap_true.
        CONCATENATE gv_filename '_' sy-datum '_' sy-uzeit gv_extension INTO gv_filename.
      ELSEIF r_rep = abap_true.
        CONCATENATE gv_filename gv_extension INTO gv_filename.
      ENDIF.
  
      CONCATENATE p_path '/' gv_filename INTO gv_directory.
      TRANSLATE gv_directory TO LOWER CASE.
  
      IF gv_program IS NOT INITIAL AND p_var IS NOT INITIAL.
        lcl_class=>gr_obj->execute_program( ).
      ENDIF.
  
    ENDMETHOD.
  
    METHOD execute_program.
  
  *   Getting the run-time alv information containing both structure and data.
      cl_salv_bs_runtime_info=>set(
    display = abap_false
    metadata = abap_true
    data = abap_true ).
  
      SUBMIT (gv_program) USING SELECTION-SET gv_var AND RETURN. "#EC CI_SUBMIT
  
      TRY.
  
          CLEAR gv_flag.
          cl_salv_bs_runtime_info=>get_data_ref(
          IMPORTING r_data = gr_data ).
          ASSIGN gr_data->* TO <gt_data>.
          IF <gt_data> IS ASSIGNED.
            gv_flag = abap_true.
          ENDIF.
  
        CATCH cx_salv_bs_sc_runtime_info.
          MESSAGE TEXT-007 TYPE 'S' DISPLAY LIKE 'E'.
          LEAVE LIST-PROCESSING.
      ENDTRY.
  
      cl_salv_bs_runtime_info=>clear_all( ).
  
    ENDMETHOD.
  
    METHOD place_file.
  
  *   Locally declared variables for faster execution time.
      DATA: lr_str          TYPE REF TO cl_abap_structdescr,
            lv_header       TYPE string,
            lv_msg          TYPE string,
            lv_string       TYPE string,
            lv_str          TYPE string,
            lv_success_msg  TYPE string,
            lt_ddfield      TYPE ddfields,
            ls_ddfield      TYPE dfies,
            lt_ddfield_temp TYPE ddfields,
            ls_ddfield_temp TYPE dfies,
            lv_dcpfm        TYPE xudcpfm,
            lwa_varkey      TYPE ltdxkey,
            lt_fcat         TYPE TABLE OF ltdxdata.
  
      TYPES: BEGIN OF ty_field,
               field TYPE fieldname,
             END OF ty_field.
  
      DATA: lwa_field TYPE ty_field,
            lt_field  TYPE TABLE OF ty_field.
  
      IF <gt_data> IS ASSIGNED.
  
        READ TABLE <gt_data> ASSIGNING <gs_data> INDEX 1.
        IF sy-subrc = 0.
  
  *       Get RTTI(Run-time Type Information) object for the local structure.
          lr_str ?= cl_abap_typedescr=>describe_by_data( <gs_data> ).
  
  *       Get field details of the structure from the run-time object.
          lt_ddfield = cl_salv_data_descr=>read_structdescr( lr_str ).
  
        ENDIF.
  
        IF gv_layout IS NOT INITIAL.
  
          lwa_varkey-report = gv_program.
          lwa_varkey-handle = 'FLAT'.
          lwa_varkey-log_group = ''.
          lwa_varkey-username = ''.
          lwa_varkey-variant = gv_layout.
          lwa_varkey-type = 'F'.
  
          CALL FUNCTION 'LT_DBDATA_READ_FROM_LTDX'
            EXPORTING
              i_tool       = 'LT'
              is_varkey    = lwa_varkey
            TABLES
              t_dbfieldcat = lt_fcat
            EXCEPTIONS
              not_found    = 1
              wrong_relid  = 2
              OTHERS       = 3.
          IF sy-subrc = 0.
  
            IF lt_fcat IS INITIAL.
  
              lwa_varkey-handle = ''.
  
              CALL FUNCTION 'LT_DBDATA_READ_FROM_LTDX'
                EXPORTING
                  i_tool       = 'LT'
                  is_varkey    = lwa_varkey
                TABLES
                  t_dbfieldcat = lt_fcat
                EXCEPTIONS
                  not_found    = 1
                  wrong_relid  = 2
                  OTHERS       = 3.
  
            ENDIF.
  
            LOOP AT lt_fcat INTO DATA(lwa_fcat) WHERE param = 'ROW_POS' AND value = 1.
              lwa_field-field = lwa_fcat-key1.
              APPEND lwa_field TO lt_field.
            ENDLOOP.
  
            LOOP AT lt_fcat INTO lwa_fcat WHERE param = 'NO_OUT' AND value = 'X'.
              READ TABLE lt_field INTO lwa_field WITH KEY field =  lwa_fcat-key1.
              IF sy-subrc = 0.
                DELETE lt_field WHERE field =  lwa_fcat-key1.
              ENDIF.
            ENDLOOP.
  
            LOOP AT lt_ddfield INTO ls_ddfield.
              READ TABLE lt_field INTO lwa_field WITH KEY field = ls_ddfield-fieldname.
              IF sy-subrc NE 0.
                DELETE lt_ddfield.
              ENDIF.
            ENDLOOP.
            LOOP AT lt_field INTO lwa_field.
              READ TABLE lt_ddfield INTO ls_ddfield WITH KEY fieldname = lwa_field-field.
              IF sy-subrc = 0.
                APPEND ls_ddfield TO lt_ddfield_temp.
              ENDIF.
            ENDLOOP.
            CLEAR lt_ddfield.
            lt_ddfield = lt_ddfield_temp.
          ELSE.
            MESSAGE TEXT-015 TYPE 'S' DISPLAY LIKE 'E'.
            LEAVE LIST-PROCESSING.
          ENDIF.
        ENDIF.
  
        LOOP AT lt_ddfield INTO ls_ddfield.
  *       Creating the final header information for the file.
          IF ls_ddfield-scrtext_l IS NOT INITIAL.
            IF lv_header IS INITIAL.
              lv_header = ls_ddfield-scrtext_l.
            ELSE.
              CONCATENATE lv_header ls_ddfield-scrtext_l INTO lv_header SEPARATED BY gt_tab.
            ENDIF.
          ELSE.
            IF lv_header IS INITIAL.
              lv_header = ls_ddfield-fieldname.
            ELSE.
              CONCATENATE lv_header  ls_ddfield-fieldname INTO lv_header SEPARATED BY gt_tab.
            ENDIF.
          ENDIF.
  
        ENDLOOP.
  
        OPEN DATASET gv_directory FOR OUTPUT IN TEXT MODE ENCODING DEFAULT MESSAGE lv_msg.
  
        IF sy-subrc NE 0.
          MESSAGE TEXT-009 TYPE 'E' DISPLAY LIKE 'I'.
          EXIT.
        ENDIF.
  
        TRANSFER lv_header TO gv_directory.
  
        LOOP AT <gt_data> INTO <gs_data>.
  *       Getting fields dynamically one by one and concatenating to a string.
          CLEAR lv_string.
  
          LOOP AT lt_ddfield  INTO ls_ddfield.
            CLEAR lv_str.
            ASSIGN COMPONENT ls_ddfield-fieldname OF STRUCTURE <gs_data> TO <gs_field>.
            IF sy-subrc = 0.
              lv_str = <gs_field>.
  
              IF p_space = abap_true.
                lv_dcpfm = ''.
              ELSEIF p_x = abap_true.
                lv_dcpfm = 'X'.
              ELSEIF p_y = abap_true.
                lv_dcpfm = 'Y'.
              ENDIF.
  
              IF ls_ddfield-datatype = 'CURR' OR ls_ddfield-datatype = 'QUAN' .
                CALL FUNCTION '/MIBA/ZDECIMAL_CONVERSAION'
                  EXPORTING
                    im_value = lv_str
                    im_def   = p_def
                    im_dcpfm = lv_dcpfm
                  IMPORTING
                    ex_value = lv_str.
              ENDIF.
  
              IF ls_ddfield-datatype = 'DATS'.
                CONCATENATE lv_str+6(2) lv_str+4(2) lv_str+0(4) INTO lv_str SEPARATED BY '.'.
              ENDIF.
              CONCATENATE lv_string lv_str INTO lv_string SEPARATED BY gt_tab.
            ENDIF.
          ENDLOOP.
  
          REPLACE FIRST OCCURRENCE OF gt_tab IN lv_string  WITH ''.
  
          TRANSFER lv_string TO gv_directory.
  
        ENDLOOP.
  
        CONCATENATE 'File' ##NO_TEXT gv_filename 'placed successfully at location' ##NO_TEXT p_path
        INTO lv_success_msg SEPARATED BY space.
        MESSAGE  '' && lv_success_msg TYPE 'I' DISPLAY LIKE 'I'.
  
        CLOSE DATASET gv_directory.
  
      ENDIF.
  
    ENDMETHOD.
  
    METHOD display.
  
      DATA: lr_alv TYPE REF TO cl_salv_table.
  
  *   Using Factory method to diretly get a ALV Report for Data Preview.
      TRY.
  
          IF <gt_data> IS ASSIGNED.
  
            cl_salv_table=>factory(
            IMPORTING
            r_salv_table = lr_alv
            CHANGING
            t_table = <gt_data>
            ).
  
          ENDIF.
  
        CATCH cx_salv_msg INTO DATA(lx_msg).
          cl_demo_output=>display( lx_msg ).
  
      ENDTRY.
  
      lr_alv->display( ).
  
    ENDMETHOD.
  
    METHOD validation.
  
      IF p_tcode IS INITIAL AND p_prog IS INITIAL.
        MESSAGE TEXT-010 TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
  
      IF p_var IS INITIAL.
        MESSAGE TEXT-011 TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
  
      IF r_al11 = abap_true AND p_path IS INITIAL.
        MESSAGE TEXT-012 TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
  
    ENDMETHOD.
  
  ENDCLASS.