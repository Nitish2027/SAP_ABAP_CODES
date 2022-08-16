***************************************************************************
* Program ID       : /MIBA/ZDYNAMIC_REPORT                                     *
* Program Title    : Dynamic Report to extract data from ALV transactions.     *
*                                                                                              *
* Author                : Nitish Singh                                                    *
* Date                  : 26 April'2022                                                   *
* Transport Number  : MIEK9A0E2T                                                    *
* DESCRIPTION      : Report will help us extract data from any ALV            *
*                           transaction dynamically based on variant and layout.      *
*==================================================================*
* Modification History                                                                     *
*------------------------------------------------------------------*
* Mod.no. |  Date     | Name            |   Transport#                               *
***************************************************************************
*  1      |           |                 |                                                      *
***************************************************************************

*&-----------------------------------------------------------------*
*& Report /MIBA/ZDYNAMIC_REPORT
*&-----------------------------------------------------------------*
*&
*&-----------------------------------------------------------------*
REPORT /miba/zdynamic_report.

INCLUDE /miba/zdynamic_report_top.
INCLUDE /miba/zdynamic_report_scc.
INCLUDE /miba/zdynamic_report_cls.

INITIALIZATION.
  IF lcl_class=>gr_obj IS NOT BOUND.
*Check if single object instance is available.
    MESSAGE TEXT-001 TYPE 'I' DISPLAY LIKE 'I'.
    LEAVE LIST-PROCESSING.
  ENDIF.
  IF sy-batch IS INITIAL.
    cl_gui_frontend_services=>get_sapgui_workdir( CHANGING sapworkdir = p_down ).
    cl_gui_cfw=>flush( ).
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_down.
  p_down = lcl_class=>f4_path( ).

START-OF-SELECTION.
  lcl_class=>gr_obj->validation( ).
  lcl_class=>gr_obj->get_data( ).

END-OF-SELECTION.

  IF ( r_al11 = 'X' OR r_down = 'X'  OR r_email = 'X') AND gv_flag = 'X'.
    TRY.
        lcl_class=>gr_obj->create_excel( ).
      CATCH zcx_excel INTO exref.
        gv_msg = exref->get_text( ).
        MESSAGE gv_msg TYPE 'S' DISPLAY LIKE 'E'.
        MESSAGE TEXT-007 TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ELSEIF r_disp = 'X' AND gv_flag = 'X'.
    lcl_class=>gr_obj->display( ).
  ELSE.
    MESSAGE TEXT-007 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  cl_abap_memory_utilities=>do_garbage_collection( ).