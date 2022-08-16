***************************************************************************
* Program ID       : /MIBA/ZREF_VS_DEL                                            *
* Program Title    : Reference vs Delivery Note Report                             *
*                                                                                              *
* Author                : Nitish Singh                                                    *
* Date                  : 19 July'2022                                                    *
* Transport Number  : MIEK9A0FAX                                                    *
*==================================================================*
* Modification History                                                                     *
*------------------------------------------------------------------*
* Mod.no. |  Date     | Name            |   Transport#                               *
***************************************************************************
*  1      |           |                 |                                                      *
***************************************************************************

*&-----------------------------------------------------------------*
*& Report /MIBA/ZREF_VS_DEL
*&-----------------------------------------------------------------*
*&
*&-----------------------------------------------------------------*
REPORT /miba/zref_vs_del.

INCLUDE /miba/zref_vs_del_top.
INCLUDE /miba/zref_vs_del_scc.
INCLUDE /miba/zref_vs_del_cls.

START-OF-SELECTION.

  DATA(lo_class) = NEW lcl_class( ).
  lo_class->get_data( ).
  lo_class->display( ).