*&---------------------------------------------------------------------*
*& Include          /MIBA/ZOPEN_ARCHIVE_CLS
*&---------------------------------------------------------------------*

CLASS lcl_class DEFINITION.

    PUBLIC SECTION.
      CLASS-DATA: gr_obj TYPE REF TO lcl_class.
      CLASS-METHODS: class_constructor.
      CLASS-METHODS:
        output         IMPORTING cl_excel            TYPE REF TO zcl_excel
                                 iv_writerclass_name TYPE clike OPTIONAL
                                 iv_info_message     TYPE abap_bool DEFAULT abap_true
                       RAISING   zcx_excel,
        f4_path        RETURNING VALUE(selected_folder) TYPE string.
      METHODS:  get_data,
        execute_program,
        download_frontend RAISING zcx_excel,
        download_backend,
        display,
        send_email,
        validation,
        create_excel,
        get_layout.
  
    PRIVATE SECTION.
  
      CLASS-DATA: xdata     TYPE xstring,             " Will be used for sending as email
                  t_rawdata TYPE solix_tab,           " Will be used for downloading or open directly
                  bytecount TYPE i.                   " Will be used for downloading or open directly
  
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
  
  
    METHOD get_data.
  
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
  
      IF r_tcode IS NOT INITIAL AND gv_program IS INITIAL.
        MESSAGE TEXT-008 TYPE 'E' DISPLAY LIKE 'I'.
      ENDIF.
  
      IF r_prog = abap_true.
        gv_program = p_prog.
      ENDIF.
  
      gv_var = p_var.
      gv_layout = p_layout.
  
      gv_extension = '.XLSX'.
  
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
  
      lcl_class=>gr_obj->get_layout( ).
  
    ENDMETHOD.
  
    METHOD get_layout.
  
  *   Locally declared variables for faster execution time.
      DATA: lwa_varkey TYPE ltdxkey,
            lt_fcat    TYPE TABLE OF ltdxdata.
      DATA: lv_pos     TYPE i VALUE 1,
            lt_ddfield TYPE ddfields,
            ls_ddfield TYPE dfies,
            lr_str     TYPE REF TO cl_abap_structdescr.
  
      IF gv_layout IS NOT INITIAL.
  
        SELECT report, handle, log_group, username, variant, type
        FROM ltdx
        INTO TABLE @DATA(lt_ltdx)
        WHERE report = @gv_program
        AND variant = @gv_layout.
        IF sy-subrc = 0.
          READ TABLE lt_ltdx INTO DATA(lwa_ltdx) INDEX 1.
          IF sy-subrc = 0.
            lwa_varkey-report = lwa_ltdx-report.
            lwa_varkey-handle = lwa_ltdx-handle.
            lwa_varkey-log_group = lwa_ltdx-log_group.
            lwa_varkey-username = lwa_ltdx-username.
            lwa_varkey-variant = lwa_ltdx-variant.
            lwa_varkey-type = lwa_ltdx-type.
  
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
            IF sy-subrc = 0 AND lt_fcat IS NOT INITIAL.
  
              LOOP AT lt_fcat INTO DATA(lwa_fcat) WHERE param = 'ROW_POS' AND value = 1.
                gwa_layout-field = lwa_fcat-key1.
                APPEND gwa_layout TO gt_layout.
              ENDLOOP.
  
              LOOP AT lt_fcat INTO lwa_fcat WHERE param = 'NO_OUT' AND value = 'X'.
                READ TABLE gt_layout INTO gwa_layout WITH KEY field =  lwa_fcat-key1.
                IF sy-subrc = 0.
                  DELETE gt_layout WHERE field =  lwa_fcat-key1.
                ENDIF.
              ENDLOOP.
  
              LOOP AT gt_layout ASSIGNING FIELD-SYMBOL(<fs_layout>).
                <fs_layout>-pos = lv_pos.
                lv_pos = lv_pos + 1.
              ENDLOOP.
  
              IF <gt_data> IS ASSIGNED.
  
                READ TABLE <gt_data> ASSIGNING <gs_data> INDEX 1.
                IF sy-subrc = 0.
  
  *       Get RTTI(Run-time Type Information) object for the local structure.
                  lr_str ?= cl_abap_typedescr=>describe_by_data( <gs_data> ).
  
  *       Get field details of the structure from the run-time object.
                  lt_ddfield = cl_salv_data_descr=>read_structdescr( lr_str ).
  
                ENDIF.
  
              ENDIF.
  
              LOOP AT gt_layout ASSIGNING <fs_layout>.
                READ TABLE lt_ddfield INTO ls_ddfield WITH KEY fieldname = <fs_layout>-field.
                IF sy-subrc = 0.
                  <fs_layout>-len = ls_ddfield-outputlen.
                ENDIF.
              ENDLOOP.
  
            ENDIF.
  
          ENDIF.
        ENDIF.
  
      ENDIF.
  
      IF gt_layout IS INITIAL.
        MESSAGE TEXT-018 TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
  
    ENDMETHOD.
  
    METHOD create_excel.
  
      DATA: lo_excel           TYPE REF TO zcl_excel,
            lo_worksheet       TYPE REF TO zcl_excel_worksheet,
            lo_column          TYPE REF TO zcl_excel_column,
            lo_style           TYPE REF TO zcl_excel_style,
            lo_style_date      TYPE REF TO zcl_excel_style,
            lo_style_editable  TYPE REF TO zcl_excel_style,
            lo_data_validation TYPE REF TO zcl_excel_data_validation.
  
      DATA: lt_field_catalog      TYPE zexcel_t_fieldcatalog,
            lwa_field_catalog     TYPE zexcel_s_fieldcatalog,
            lt_field_catalog_temp TYPE zexcel_t_fieldcatalog,
            ls_table_settings     TYPE zexcel_s_table_settings,
            ls_table_settings_out TYPE zexcel_s_table_settings,
            lv_column_alpha       TYPE zexcel_cell_column_alpha,
            lv_column_int         TYPE zexcel_cell_column.
  
      DATA: lv_style_guid           TYPE zexcel_cell_style.
  
      DATA: lv_row   TYPE char10,
            lv_count TYPE i VALUE 1,
            lv_str   TYPE string,
            lv_dcpfm TYPE xudcpfm,
            cl_error TYPE REF TO zcx_excel.
  
      FIELD-SYMBOLS: <fs_field_catalog>      TYPE zexcel_s_fieldcatalog,
                     <fs_field_catalog_temp> TYPE zexcel_s_fieldcatalog.
  
      " Creates active sheet
      CREATE OBJECT lo_excel.
      TRY.
          " Get active sheet
          lo_worksheet = lo_excel->get_active_worksheet( ).
          lo_worksheet->set_title( ip_title = 'Sheet 1' ).
  
          " sheet style (white background)
          lo_style = lo_excel->add_new_style( ).
          lo_style->fill->filltype = zcl_excel_style_fill=>c_fill_none.
          lo_style->fill->fgcolor-rgb  = zcl_excel_style_color=>c_white.
          lv_style_guid = lo_style->get_guid( ).
  
          " Get active sheet
          lo_worksheet = lo_excel->get_active_worksheet( ).
  
          lo_worksheet->set_default_excel_date_format( zcl_excel_style_number_format=>c_format_date_ddmmyyyydot ).
  
          lt_field_catalog_temp = zcl_excel_common=>get_fieldcatalog( ip_table = <gt_data> ).
  
          ASSIGN lwa_field_catalog TO <fs_field_catalog>.
          LOOP AT lt_field_catalog_temp ASSIGNING <fs_field_catalog_temp>.
  
            READ TABLE gt_layout INTO gwa_layout WITH KEY field = <fs_field_catalog_temp>-fieldname.
            IF sy-subrc = 0.
              <fs_field_catalog_temp>-dynpfld    = abap_true.
              <fs_field_catalog_temp>-style      = lo_style->get_guid( ).
              <fs_field_catalog_temp>-position = gwa_layout-pos.
              MOVE-CORRESPONDING <fs_field_catalog_temp> TO <fs_field_catalog>.
              APPEND <fs_field_catalog> TO lt_field_catalog.
            ELSE.
              <fs_field_catalog_temp>-dynpfld    = abap_false.
            ENDIF.
  
          ENDLOOP.
  
          "loop over all the columns.
          LOOP AT lt_field_catalog ASSIGNING <fs_field_catalog>.
  
            READ TABLE gt_layout INTO gwa_layout WITH KEY field = <fs_field_catalog>-fieldname.
            IF sy-subrc = 0.
  
              IF gwa_layout-len <= 10.
                DATA(lv_len) = 12.
              ELSE.
                lv_len = gwa_layout-len + 2.
              ENDIF.
  
              lv_column_int = gwa_layout-pos.
              lv_column_alpha = zcl_excel_common=>convert_column2alpha( lv_column_int ).
  
              lo_column = lo_worksheet->get_column( ip_column = lv_column_alpha  ).
              lo_column->set_width( lv_len ).
  
            ENDIF.
  
          ENDLOOP.
  
          ls_table_settings-table_style  = zcl_excel_table=>builtinstyle_medium2.
          ls_table_settings-show_row_stripes = abap_true.
          ls_table_settings-show_column_stripes = abap_true.
  
          IF <gt_data> IS ASSIGNED AND lt_field_catalog IS NOT INITIAL.
  
            lo_worksheet->bind_table( EXPORTING
                                        ip_table          = <gt_data>
                                        it_field_catalog  = lt_field_catalog
                                        is_table_settings = ls_table_settings
                                      IMPORTING
                                        es_table_settings = ls_table_settings_out ).
  
            lcl_class=>output( lo_excel ).
  
          ELSE.
  
            MESSAGE TEXT-019 TYPE 'S' DISPLAY LIKE 'E'.
            LEAVE LIST-PROCESSING.
  
          ENDIF.
  
        CATCH zcx_excel INTO cl_error.
          MESSAGE cl_error TYPE 'I' DISPLAY LIKE 'E'.
      ENDTRY.
  
    ENDMETHOD.
  
    METHOD: download_frontend.
  
      DATA: filename TYPE string,
            message  TYPE string.
  
      filename = p_down.
  
      IF p_fname IS INITIAL.
        p_fname = p_tcode.
      ENDIF.
  
      REPLACE FIRST OCCURRENCE OF '/' IN p_fname WITH ''.
      REPLACE ALL OCCURRENCES OF '/' IN p_fname WITH '_'.
  
      CONCATENATE filename '\' p_fname '.xlsx' INTO filename.
  
      cl_gui_frontend_services=>gui_download( EXPORTING bin_filesize = bytecount
                                                        filename     = filename
                                                        filetype     = 'BIN'
                                               CHANGING data_tab     = t_rawdata
                                             EXCEPTIONS OTHERS       = 1 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.
        RAISE EXCEPTION TYPE zcx_excel EXPORTING error = message.
      ENDIF.
  
    ENDMETHOD.                    "download_frontend
  
    METHOD download_backend.
  
      DATA: bytes_remain   TYPE i,
            lv_success_msg TYPE string.
      FIELD-SYMBOLS: <rawdata> LIKE LINE OF t_rawdata.
  
      OPEN DATASET gv_directory FOR OUTPUT IN BINARY MODE.
      CHECK sy-subrc = 0.
  
      bytes_remain = bytecount.
  
      LOOP AT t_rawdata ASSIGNING <rawdata>.
  
        AT LAST.
          CHECK bytes_remain >= 0.
          TRANSFER <rawdata> TO gv_directory LENGTH bytes_remain.
          EXIT.
        ENDAT.
  
        TRANSFER <rawdata> TO gv_directory.
        SUBTRACT 255 FROM bytes_remain.  " Solix has length 255
  
      ENDLOOP.
  
      CLOSE DATASET gv_directory.
  
      IF sy-repid <> sy-cprog AND sy-cprog IS NOT INITIAL.
        LEAVE PROGRAM.
      ELSE.
        CONCATENATE 'File' ##NO_TEXT gv_filename 'placed successfully at location' ##NO_TEXT p_path
        INTO lv_success_msg SEPARATED BY space.
        MESSAGE  '' && lv_success_msg TYPE 'I' DISPLAY LIKE 'I'.
      ENDIF.
  
    ENDMETHOD.                          "download_backend
  
    METHOD output.
  
      DATA: cl_output TYPE REF TO lcl_class,
            cl_writer TYPE REF TO zif_excel_writer,
            cl_error  TYPE REF TO zcx_excel.
  
      TRY.
  
          IF iv_writerclass_name IS INITIAL.
            CREATE OBJECT cl_output.
            CREATE OBJECT cl_writer TYPE zcl_excel_writer_2007.
          ELSE.
            CREATE OBJECT cl_output.
            CREATE OBJECT cl_writer TYPE (iv_writerclass_name).
          ENDIF.
          cl_output->xdata = cl_writer->write_file( cl_excel ).
  
          cl_output->t_rawdata = cl_bcs_convert=>xstring_to_solix( iv_xstring  = cl_output->xdata ).
          cl_output->bytecount = xstrlen( cl_output->xdata ).
  
          CASE 'X'.
  
            WHEN r_down.
              IF sy-batch IS INITIAL.
                cl_output->download_frontend( ).
              ELSE.
                MESSAGE e802(zabap2xlsx).
              ENDIF.
  
            WHEN r_al11.
              cl_output->download_backend( ).
  
            WHEN r_email.
              cl_output->send_email( ).
  
          ENDCASE.
  
        CATCH zcx_excel INTO cl_error.
          IF iv_info_message = abap_true.
            MESSAGE cl_error TYPE 'I' DISPLAY LIKE 'E'.
          ELSE.
            RAISE EXCEPTION cl_error.
          ENDIF.
      ENDTRY.
  
    ENDMETHOD.                    "output
  
    METHOD display.
  
      DATA: lr_alv     TYPE REF TO cl_salv_table,
            lr_columns TYPE REF TO cl_salv_columns_table,
            lr_column  TYPE REF TO cl_salv_column.
      DATA: gr_strucdesc TYPE REF TO cl_abap_structdescr.
      DATA: lt_comp TYPE abap_compdescr_tab.
      DATA: ls_comp TYPE abap_compdescr.
  
      TRY.
  
          IF <gt_data> IS ASSIGNED.
  
            cl_salv_table=>factory(
            IMPORTING
            r_salv_table = lr_alv
            CHANGING
            t_table = <gt_data>
            ).
  
          ENDIF.
  
          TRY.
              lr_columns = lr_alv->get_columns( ).
  
              READ TABLE <gt_data> ASSIGNING <gs_data> INDEX 1.
              IF sy-subrc = 0.
  
                gr_strucdesc ?= cl_abap_typedescr=>describe_by_data( <gs_data> ).
                lt_comp[] = gr_strucdesc->components[].
                LOOP AT lt_comp INTO ls_comp.
                  READ TABLE gt_layout INTO gwa_layout WITH KEY field = ls_comp-name.
                  IF sy-subrc = 0.
                    lr_column ?= lr_columns->get_column( ls_comp-name  ).
                    lr_column->set_visible(  abap_true ).
                    lr_columns->set_column_position( columnname = ls_comp-name
                                     position   = gwa_layout-pos ).
                  ELSE.
                    lr_column ?= lr_columns->get_column( ls_comp-name  ).
                    lr_column->set_visible(  abap_false ).
                  ENDIF.
                ENDLOOP.
  
              ENDIF.
  
            CATCH cx_salv_not_found.                      "#EC NO_HANDLER
          ENDTRY.
  
        CATCH cx_salv_msg INTO DATA(lx_msg).
          cl_demo_output=>display( lx_msg ).
  
      ENDTRY.
  
      lr_alv->display( ).
  
    ENDMETHOD.
  
    METHOD f4_path.
      DATA: new_path      TYPE string,
            repid         TYPE syrepid,
            dynnr         TYPE sydynnr,
            lt_dynpfields TYPE TABLE OF dynpread,
            ls_dynpfields LIKE LINE OF lt_dynpfields.
  
  * Get current value
      dynnr = sy-dynnr.
      repid = sy-repid.
      ls_dynpfields-fieldname = 'P_DOWN'.
      APPEND ls_dynpfields TO lt_dynpfields.
  
      CALL FUNCTION 'DYNP_VALUES_READ'
        EXPORTING
          dyname               = repid
          dynumb               = dynnr
        TABLES
          dynpfields           = lt_dynpfields
        EXCEPTIONS
          invalid_abapworkarea = 1
          invalid_dynprofield  = 2
          invalid_dynproname   = 3
          invalid_dynpronummer = 4
          invalid_request      = 5
          no_fielddescription  = 6
          invalid_parameter    = 7
          undefind_error       = 8
          double_conversion    = 9
          stepl_not_found      = 10
          OTHERS               = 11.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        RETURN.
      ENDIF.
  
      READ TABLE lt_dynpfields INTO ls_dynpfields INDEX 1.
  
      new_path = ls_dynpfields-fieldvalue.
      selected_folder = new_path.
  
      cl_gui_frontend_services=>directory_browse(
        EXPORTING
          window_title         = 'Select path to download EXCEL-file'
          initial_folder       = new_path
        CHANGING
          selected_folder      = new_path
        EXCEPTIONS
          cntl_error           = 1
          error_no_gui         = 2
          not_supported_by_gui = 3
          OTHERS               = 4
             ).
      cl_gui_cfw=>flush( ).
      CHECK new_path IS NOT INITIAL.
      selected_folder = new_path.
  
    ENDMETHOD.                                                "f4_path
  
    METHOD send_email.
  
      DATA: bcs_exception        TYPE REF TO cx_bcs,
            errortext            TYPE string,
            cl_send_request      TYPE REF TO cl_bcs,
            cl_document          TYPE REF TO cl_document_bcs,
            cl_recipient         TYPE REF TO if_recipient_bcs,
            cl_sender            TYPE REF TO cl_cam_address_bcs,
            t_attachment_header  TYPE soli_tab,
            wa_attachment_header LIKE LINE OF t_attachment_header,
            attachment_subject   TYPE sood-objdes,
  
            sood_bytecount       TYPE sood-objlen,
            mail_title           TYPE so_obj_des,
            t_mailtext           TYPE soli_tab,
            wa_mailtext          LIKE LINE OF t_mailtext,
            send_to              TYPE adr6-smtp_addr,
            sent                 TYPE abap_bool,
            filename             TYPE string.
  
      IF p_filen IS INITIAL.
        filename = p_tcode.
      ELSE.
        filename = p_filen.
      ENDIF.
  
      REPLACE FIRST OCCURRENCE OF '/' IN filename WITH ''.
      REPLACE ALL OCCURRENCES OF '/' IN filename WITH '_'.
  
      filename = filename && '.xlsx'.
  
  
      IF p_title IS NOT INITIAL.
        mail_title     = p_title.
      ELSE.
        mail_title     = 'Email Title'.
      ENDIF.
  
      IF p_text IS NOT INITIAL.
        wa_mailtext    = p_text.
      ELSE.
        wa_mailtext    = 'Email Body'.
      ENDIF.
  
  
      APPEND wa_mailtext TO t_mailtext.
  
      TRY.
  * Create send request
          cl_send_request = cl_bcs=>create_persistent( ).
  * Create new document with mailtitle and mailtextg
          cl_document = cl_document_bcs=>create_document( i_type    = 'RAW' "#EC NOTEXT
                                                          i_text    = t_mailtext
                                                          i_subject = mail_title ).
  * Add attachment to document
  * since the new excelfiles have an 4-character extension .xlsx but the attachment-type only holds 3 charactes .xls,
  * we have to specify the real filename via attachment header
  * Use attachment_type xls to have SAP display attachment with the excel-icon
          attachment_subject  = filename.
          CONCATENATE '&SO_FILENAME=' attachment_subject INTO wa_attachment_header.
          APPEND wa_attachment_header TO t_attachment_header.
  * Attachment
          sood_bytecount = bytecount.  " next method expects sood_bytecount instead of any positive integer *sigh*
          cl_document->add_attachment(  i_attachment_type    = 'XLS' "#EC NOTEXT
                                        i_attachment_subject = attachment_subject
                                        i_attachment_size    = sood_bytecount
                                        i_att_content_hex    = t_rawdata
                                        i_attachment_header  = t_attachment_header ).
  
  * add document to send request
          cl_send_request->set_document( cl_document ).
  
  * add recipient(s) - here only 1 will be needed
          send_to = p_email.
          IF send_to IS INITIAL.
            send_to = 'no_email@no_email.no_email'.  " Place into SOST in any case for demonstration purposes
          ENDIF.
          cl_recipient = cl_cam_address_bcs=>create_internet_address( send_to ).
          cl_send_request->add_recipient( cl_recipient ).
  
  * Und abschicken
          sent = cl_send_request->send( i_with_error_screen = 'X' ).
  
          COMMIT WORK.
  
          IF sent = abap_true.
            MESSAGE s805(zabap2xlsx).
            MESSAGE 'Document ready to be sent - Check SOST or SCOT' TYPE 'I'.
          ELSE.
            MESSAGE i804(zabap2xlsx) WITH p_email.
          ENDIF.
  
        CATCH cx_bcs INTO bcs_exception.
          errortext = bcs_exception->if_message~get_text( ).
          MESSAGE errortext TYPE 'I'.
  
      ENDTRY.
  
    ENDMETHOD.                                                  "send_email
  
    METHOD validation.
  
      IF p_tcode IS INITIAL AND p_prog IS INITIAL.
        MESSAGE TEXT-010 TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
  
      IF p_var IS INITIAL.
        MESSAGE TEXT-011 TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
  
      IF p_layout IS INITIAL.
        MESSAGE TEXT-017 TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
  
      IF r_al11 = abap_true AND p_path IS INITIAL.
        MESSAGE TEXT-012 TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
  
      IF r_down = abap_true AND p_down IS INITIAL.
        MESSAGE TEXT-012 TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
  
    ENDMETHOD.
  
  ENDCLASS.