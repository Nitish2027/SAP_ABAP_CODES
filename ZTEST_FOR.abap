*&---------------------------------------------------------------------*
*& Report ztest_for
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest_for.

*TYPES: BEGIN OF ty_data,
*       c1 TYPE i,
*       c2 TYPE i,
*       c3 TYPE i,
*       c4 TYPE i,
*       END OF ty_data.
*
*DATA: it_data TYPE TABLE OF ty_data,
*      it_data2 TYPE TABLE OF ty_data,
*      it_data3 TYPE TABLE OF ty_data.
*
*it_data = VALUE #( FOR i = 1 THEN i + 1 UNTIL i > 5 ( c1 = i c2 = i + 1 c3 = i + 2 c4 = i + 3 ) ).
*
*cl_demo_output=>write( it_data ).
*
*it_data2 = VALUE #( FOR wa in it_data WHERE ( c1 > 3 ) ( wa ) ).
*
*cl_demo_output=>write( it_data2 ).
*
*it_data3 = VALUE #( FOR wa IN it_data INDEX INTO lv_index WHERE ( c1 = 3 ) ( LINES OF it_data FROM lv_index ) ).
*
*cl_demo_output=>write( it_data3 ).
*
*TYPES: BEGIN OF ty_data1,
*       c1 TYPE i,
*       c2 TYPE i,
*       END OF ty_data1.
*
*TYPES: tt_data TYPE TABLE OF ty_data1 WITH EMPTY KEY.
*
*DATA(it_data4) = VALUE tt_data( FOR wa in it_data FROM 4 to 5 ( c1 = wa-c1 c2 = wa-c2 ) ).
*
*cl_demo_output=>write( it_data4 ).
*
*TYPES: tt_i TYPE TABLE OF i WITH EMPTY KEY.
*
*DATA(it_data5) = VALUE tt_i( FOR wa in it_data ( wa-c1 ) ).
*
*cl_demo_output=>write( it_data5 ).
*
*DATA(it_data6) = VALUE tt_i( FOR wa in it_data ( wa-c1 ) ( wa-c2 ) ). "Magic
*
*cl_demo_output=>display( it_data6 ).

*Situation 1: To move values from a source internal table to a target internal table which has all the fields of source
*internal table + some additional fields ( say ).

*Define structure
TYPES:
  BEGIN OF ty_struct1,
    field1 TYPE i,
    field2 TYPE string,
  END OF ty_struct1,
  BEGIN OF ty_struct2,
    field1 TYPE i,
    field2 TYPE string,
    field3 TYPE i,
  END OF ty_struct2.

*Define table types
TYPES:  gtt_struct1 TYPE STANDARD TABLE OF ty_struct1 WITH DEFAULT KEY,
        gtt_struct2 TYPE STANDARD TABLE OF ty_struct2 WITH DEFAULT KEY.

* Initialize source table with some random values
DATA(lt_source) = VALUE gtt_struct1(
    ( field1 = 1 field2 = 'A' )
    ( field1 = 2 field2 = 'B' ) ).

*Use like simple MOVE CORRESPONDING
DATA(lt_target1) = VALUE gtt_struct2( FOR lwa_source IN lt_source ( CORRESPONDING #( lwa_source ) ) ).
cl_demo_output=>display( lt_target1 ).

*Populate sy-tabix in the additional fields within the for loop
DATA(lt_target2) = VALUE gtt_struct2( FOR lwa_source IN lt_source
                            INDEX INTO index
                            LET base = VALUE ty_struct2( field3 = index )
                            IN ( CORRESPONDING #( BASE ( base ) lwa_source ) ) ).
cl_demo_output=>display( lt_target2 ).

*Populate any value or call custom method  in the additional fields within the for loop
DATA(lt_target3) = VALUE gtt_struct2( FOR lwa_source IN lt_source
             LET base = VALUE ty_struct2( field3 = 10 ) "<<< Custom method/any value
             IN ( CORRESPONDING #( BASE ( base ) lwa_source ) ) ).
cl_demo_output=>display( lt_target3 ).



"We can further enhance this by using line index option to specify from which row the FOR loop will start iterating.
"The below example illustrates how to populate a final table from the header, item and some other table data.

"Sample code is as follows:

TYPES: BEGIN OF ty_final,
         ebeln TYPE ebeln,
         ebelp TYPE ebelp,
         lifnr TYPE lifnr,
         matnr TYPE matnr,
         maktx TYPE maktx,
         werks TYPE werks_d,
         name1 TYPE name1,
       END OF ty_final,
       ty_t_final TYPE STANDARD TABLE OF ty_final WITH DEFAULT KEY.

START-OF-SELECTION.
  SELECT * FROM ekko
   UP TO 10 ROWS
    INTO TABLE @DATA(lt_ekko).
  IF sy-subrc = 0.
    SELECT * FROM ekpo
     INTO TABLE @DATA(lt_ekpo)
      FOR ALL ENTRIES IN @lt_ekko
       WHERE ebeln = @lt_ekko-ebeln.
    IF sy-subrc = 0.
      SELECT * FROM makt INTO TABLE @DATA(lt_makt)
       FOR ALL ENTRIES IN @lt_ekpo
        WHERE matnr = @lt_ekpo-matnr
        AND spras = @sy-langu.

      SELECT * FROM t001w INTO TABLE @DATA(lt_t001w)
         FOR ALL ENTRIES IN @lt_ekpo
          WHERE werks = @lt_ekpo-werks.

*Sort the tables with proper key
      SORT: lt_ekko  BY ebeln,
            lt_ekpo  BY ebeln ebelp,
            lt_makt  BY matnr,
            lt_t001w BY werks.

*Populate the final table based on the data from EKKO,EKPO,MAKT and T001W using "Parallel Cursor"
      DATA(lt_final) = VALUE ty_t_final( FOR ls_ekpo IN lt_ekpo
                       FOR ls_ekko IN lt_ekko FROM line_index( lt_ekko[ ebeln = ls_ekpo-ebeln ] )
                       WHERE ( ebeln = ls_ekpo-ebeln )
                       FOR ls_makt IN lt_makt FROM line_index( lt_makt[ matnr = ls_ekpo-matnr ] )
                       WHERE ( matnr = ls_ekpo-matnr )
                       FOR ls_t001w IN lt_t001w FROM line_index( lt_t001w[ werks = ls_ekpo-werks ] )
                       WHERE ( werks = ls_ekpo-werks )
                       LET ls_final = VALUE ty_final(
                            lifnr = ls_ekko-lifnr
                            maktx = ls_makt-maktx
                            name1 = ls_t001w-name1 )
                       IN ( CORRESPONDING #( BASE ( ls_final ) ls_ekpo ) ) ).

      cl_demo_output=>display( lt_final ).

    ENDIF.
  ENDIF.



"Situation 2: Suppose we have an internal table and we want to create another range table with the values of one particular
"field from that internal table


*Get details from DB table
SELECT * FROM
  sflight
  INTO TABLE @DATA(lt_sflight)
  WHERE connid IN (17,555).

IF sy-subrc = 0.
*Prepare a range table
  DATA: lr_carrid  TYPE RANGE OF s_carr_id.
  lr_carrid = VALUE #( FOR ls_value IN lt_sflight ( sign = 'I'
                                                   option = 'EQ'
                                                   low = ls_value-carrid ) ).
  SORT lr_carrid BY low.
  DELETE ADJACENT DUPLICATES FROM lr_carrid
  COMPARING low.
  cl_demo_output=>display( lr_carrid ).
ENDIF.

"Situation 3: There are 2 tables and based on which a third table need to be constructed.
"Target table has fields common from the first 2 source table.

*Define the structures of header & item table
TYPES:
  BEGIN OF comp,
    ebeln TYPE ebeln,
    bukrs TYPE bukrs,
    netwr TYPE netwr,
  END OF comp.

*Declare table type
TYPES: gtt_comp   TYPE STANDARD TABLE OF comp WITH DEFAULT KEY,
       gtt_header TYPE STANDARD TABLE OF ekko WITH DEFAULT KEY,
       gtt_item   TYPE SORTED TABLE OF ekpo WITH NON-UNIQUE KEY ebeln.

*Populate dummy values
DATA(lt_header) = VALUE gtt_header(
                      ( ebeln = '4500000027' )
                      ( ebeln = '4500000028' )
                      ( ebeln = '4500000029' ) ).

DATA(lt_item) = VALUE gtt_item(
                    ( ebeln = '4500000027' ebelp = '000010' netwr = '100' bukrs = 'IND' )
                    ( ebeln = '4500000027' ebelp = '000020' netwr = '200' bukrs = 'IND' )
                    ( ebeln = '4500000027' ebelp = '000030' netwr = '300' bukrs = 'IND' )
                    ( ebeln = '4500000028' ebelp = '000010' netwr = '999' bukrs = 'USA' )
                    ( ebeln = '4500000029' ebelp = '000010' netwr = '25' bukrs = 'GB' )
                    ( ebeln = '4500000029' ebelp = '000020' netwr = '50' bukrs = 'GB' )
                    ( ebeln = '4500000029' ebelp = '000030' netwr = '100' bukrs = 'GB' )
                    ( ebeln = '4500000029' ebelp = '000040' netwr = '150' bukrs = 'GB' ) ).


*Now populate the values of Compor from item table to a new table
DATA(lt_comp) = VALUE gtt_comp(
                    FOR ls_header IN lt_header
                    FOR ls_item IN lt_item WHERE ( ebeln = ls_header-ebeln )
                    (  ebeln = ls_header-ebeln
                       bukrs = ls_item-bukrs ) ).

SORT lt_comp BY ebeln.
DELETE ADJACENT DUPLICATES FROM lt_comp
COMPARING ebeln.

cl_demo_output=>display( lt_comp ).

"Situation 4: Summation of item values and populate the total in header table

"Use the code from situation 3 for the declaration part
*Now sum up all the netwr for a particular purchase order

LOOP AT lt_comp ASSIGNING FIELD-SYMBOL(<lfs_comp>).
  <lfs_comp>-netwr = REDUCE netwr( INIT lv_netwr TYPE netwr
                       FOR ls_item IN
                       FILTER #( lt_item
                       WHERE ebeln = <lfs_comp>-ebeln )
                       NEXT lv_netwr = lv_netwr + ls_item-netwr ).
ENDLOOP.

cl_demo_output=>display( lt_comp ).


"Situation 5: Append new values into an internal table which already have some values

"Use the code from situation 3 for the declaration part
*Append new records in item table
lt_item = VALUE #( BASE lt_item
          ( ebeln = '4500000030' ebelp = '000010' netwr = '1099' bukrs = 'CAN' ) ).

cl_demo_output=>display( lt_item ).
