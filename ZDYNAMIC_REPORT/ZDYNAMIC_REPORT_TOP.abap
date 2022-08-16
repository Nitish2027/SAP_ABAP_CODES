*&---------------------------------------------------------------------*
*& Include          /MIBA/ZOPEN_ARCHIVE_TOP
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*         DATA DECLARATIONS
*----------------------------------------------------------------------*

TYPES: BEGIN OF ty_tvarvc,
         name TYPE rvari_vnam,
         sign TYPE tvarv_sign,
         opti TYPE tvarv_opti,
         low  TYPE tvarv_val,
         high TYPE tvarv_val,
       END OF ty_tvarvc.

DATA: gv_program   TYPE tstc-pgmna,
      gv_var       TYPE  variant,
      gv_layout    TYPE slis_vari,
      gv_flag(1)   TYPE c,
      gr_data      TYPE REF TO data,
      gv_extension TYPE string,
      gv_filename  TYPE string,
      gv_directory TYPE string,
      gt_tvarvc    TYPE TABLE OF ty_tvarvc,
      gwa_tvarvc   TYPE ty_tvarvc.

DATA: exref  TYPE REF TO zcx_excel,
      gv_msg TYPE string.

TYPES: BEGIN OF ty_field,
         field TYPE fieldname,
         pos   TYPE I,
         len    TYPE I,
       END OF ty_field.

DATA: gwa_layout TYPE ty_field,
      gt_layout  TYPE TABLE OF ty_field.

FIELD-SYMBOLS: <gt_data>  TYPE table,
               <gs_data>  TYPE any,
               <gs_field> TYPE any.

TYPE-POOLS: slis.
TABLES: spfli.

*--------------------------------------------------------------*
*Data Types
*--------------------------------------------------------------*
TYPES: BEGIN OF ty_final,
         data TYPE string,
       END OF ty_final.

*--------------------------------------------------------------*
*Data Declaration
*--------------------------------------------------------------*
DATA: gwa_selfield TYPE slis_selfield.
DATA: ob_salv_table TYPE REF TO cl_salv_table.
DATA: ob_salv_events    TYPE REF TO cl_salv_events_table.
DATA: lr_columns TYPE REF TO   cl_salv_columns_table,
      lr_column  TYPE REF TO   cl_salv_column.

DATA: gt_tab(1) TYPE c.

DATA: gt_final  TYPE TABLE OF ty_final,
      gwa_final TYPE ty_final.

*----------------------------------------------------------------------*
*         CONSTANTS DECLARATIONS
*----------------------------------------------------------------------*