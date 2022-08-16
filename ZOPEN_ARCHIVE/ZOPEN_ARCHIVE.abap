***************************************************************************
* Program ID       : /MIBA/ZOPEN_ARCHIVE                                        *
* Program Title    : Dynamic Report to extract data from ALV transactions.     *
*                                                                                              *
* Author                : Nitish Singh                                                    *
* Date                  : 22nd November'2021                                          *
* Transport Number  : MIEK9A09YF                                                    *
* DESCRIPTION      : Report will help us extract data from any ALV            *
*                           transaction dynamically. The coulmns needed in the      *
*                           extraction can be maintained in STAVRV Tcode, if not  *
*                           maintained all columns will be selected automatically     *
*==================================================================*
* Modification History                                                                     *
*------------------------------------------------------------------*
* Mod.no. |  Date     | Name            |   Transport#                               *
***************************************************************************
*  1      |           |                 |                                                      *
***************************************************************************

*&-----------------------------------------------------------------*
*& Report /MIBA/ZOPEN_ARCHIVE
*&-----------------------------------------------------------------*
*&
*&-----------------------------------------------------------------*
REPORT /miba/zopen_archive.

INCLUDE /miba/zopen_archive_top.
INCLUDE /miba/zopen_archive_scc.
INCLUDE /miba/zopen_archive_cls.

INITIALIZATION.
  IF lcl_class=>gr_obj IS NOT BOUND.
*Check if single object instance is available.
    MESSAGE TEXT-001 TYPE 'I' DISPLAY LIKE 'I'.
    LEAVE LIST-PROCESSING.
  ENDIF.

START-OF-SELECTION.
  lcl_class=>gr_obj->validation( ).
  lcl_class=>gr_obj->get_program( ).

END-OF-SELECTION.

  IF r_al11 = 'X' AND gv_flag = 'X'.
    lcl_class=>gr_obj->place_file( ).
  ELSEIF r_disp = 'X' AND gv_flag = 'X'.
    lcl_class=>gr_obj->display( ).
  ELSE.
    MESSAGE TEXT-007 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  cl_abap_memory_utilities=>do_garbage_collection( ).