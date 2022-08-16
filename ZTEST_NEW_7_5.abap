*&---------------------------------------------------------------------*
*& Report ztest_new_7_5
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest_new_7_5.

*DATA arg TYPE int8 VALUE 2.
*
*cl_demo_output=>new(
*)->write( |**  : { arg ** 62 }|
*)->write( |ipow: { ipow( base = arg exp = 62 ) }|
*)->display( ).
*
*cl_demo_output=>new(
*)->write( | Old Syntax : { arg ** 62 } |
*)->write( | New Syntax : { ipow( base = arg exp = 62 ) }|
*)->display( ).

"While ** calculates with floating point type f and produces a wrong result,
"ipow calculates with int8 and produces the correct result.



"Tip # 9. Whenever we have situations to apply control breaks etc. we should go for
"‘Group By‘ clause instead. It’s more readable , easy to maintain and does not come with
"hassle of fields getting asteriked. Also, it does not cause quadrtic loops.



* LOOP AT ct_all_mat ASSIGNING FIELD-SYMBOL(<fs_all_mat>)
*           GROUP BY ( field1 = <fs_all_mat>-field1 )
*           ASCENDING ASSIGNING FIELD-SYMBOL(<fs_all_mat_grp_tab>).
*
*      " Prepare the sequence number as a running number
*      " irrespective of field1
*      LOOP AT GROUP <fs_all_mat_grp_tab>
*              ASSIGNING FIELD-SYMBOL(<fs_all_mat_grp_rec>).
*       " Processing for all item in a Group
*      ENDLOOP.
*" Processing once for each matching group.
*
* ENDLOOP.

**********************************************************************
*CLASS class1 DEFINITION.
*
*ENDCLASS.
*
*CLASS subclass1 DEFINITION INHERITING FROM class1.
*
*ENDCLASS.
*
*CLASS subclass2 DEFINITION INHERITING FROM class1.
*
*ENDCLASS.
*
***Way to find if a reference variable is containing the instance of a particular class or not
*DATA: ref1 TYPE REF TO class1,
*      ref2 TYPE REF TO subclass1,
*      ref3 TYPE REF TO subclass2.
*
*ref1 = NEW subclass1( ).  "Down Cast
*
****With Old Syntax, try catch.
*TRY.
*    ref2 ?= ref1.
*  CATCH cx_sy_move_cast_error.
*    TRY.
*        ref3 ?= ref1.
*      CATCH cx_sy_move_cast_error.
*    ENDTRY.
*ENDTRY.
*
****With  new ABAP syntax
*
*IF ref1 IS INSTANCE OF subclass1.
*  ref2 ?= ref1.
*ELSEIF ref1 IS INSTANCE OF subclass2.
*  ref3 ?= ref1.
*ENDIF.
*
*
**********************************************************************


CLASS lcl_class DEFINITION.

  PUBLIC SECTION.

    METHODS: insert,
      process.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: it_head TYPE TABLE OF zemployee,
          it_item TYPE TABLE OF zemployee_data.

    TYPES: lr_role_type TYPE RANGE OF zrole.
    DATA: lr_role TYPE lr_role_type.


ENDCLASS.


CLASS lcl_class IMPLEMENTATION.

  METHOD insert.

*it_head = VALUE #(
*            ( id = '1' name = 'Nitish' )
*            ( id = '2' name = 'Channa')
*            ( id = '3' name = 'Shakti' )
*            ( id = '4' name = 'Swarup' )
*            ( id = '5' name = 'Sayali' )
*            ( id = '6' name = 'Ashish' )
*            ).
*
*it_item = VALUE #(
*                ( id = '1' role = 'Abap' salary = '10' age = '26' address = 'Varanasi' )
*                ( id = '1' role = 'Fiori' salary = '11' age = '26' address = 'Varanasi' )
*                ( id = '1' role = 'FullStack' salary = '15' age = '26' address = 'Varanasi' )
*                ( id = '2' role = 'UiPath' salary = '10' age = '26' address = 'Sangali' )
*                ( id = '2' role = 'Kibana' salary = '15' age = '26' address = 'Sangali' )
*                ( id = '3' role = 'UiPath' salary = '10' age = '25' address = 'Indore' )
*                ( id = '3' role = 'Timepaas' salary = '20' age = '25' address = 'Indore' )
*                ( id = '4' role = 'PowerApps' salary = '10' age = '26' address = 'Sholapur' )
*                ( id = '4' role = 'Power' salary = '18' age = '26' address = 'Sholapur' )
*                ( id = '4' role = 'MorePower' salary = '10' age = '26' address = 'Sholapur' )
*                ( id = '5' role = 'UiPath' salary = '10' age = '26' address = 'Lonavala' )
*                ( id = '5' role = 'Traveller' salary = '19' age = '26' address = 'Lonavala' )
*                ( id = '6' role = 'UiPath' salary = '10' age = '37' address = 'Pune' )
*                ( id = '6' role = 'LifeGuru' salary = '18' age = '37' address = 'Pune' )
*                ( id = '6' role = 'Architect' salary = '28' age = '37' address = 'Pune' )
*                ( id = '6' role = 'KyunAshishBhai' salary = '10' age = '37' address = 'Pune' )
*             ).
*
*INSERT zemployee FROM TABLE @it_head.
*INSERT zemployee_data FROM TABLE @it_item.

  ENDMETHOD.

  METHOD process.

    DATA(out) = cl_demo_output=>new( ).


*    SELECT
*    FROM zemployee
*    FIELDS  id,
*            name
*    INTO TABLE @it_head.
*
*    SELECT
*    FROM zemployee_data
*    FIELDS  id,
*            role,
*            age,
*            address,
*            zdate,
*            ztime
*    INTO TABLE @it_item.
*
*        lr_role = VALUE lr_role_type(
*            FOR wa IN it_item (
*            sign = 'I'
*            option = 'EQ'
*            low = wa-role
*            )
*         ).
*
*         SORT lr_role BY low.
*         DELETE ADJACENT DUPLICATES FROM lr_role.
*
*         cl_demo_output=>display( lr_role ).
*
*         it_item = CORRESPONDING #( lr_role MAPPING role = low ).
*
*         cl_demo_output=>display( it_item ).

*          SELECT SINGLE 'I' as sign,
*                 'EQ' as option,
*                 matnr as low,
*                 matnr as high
*          FROM MARA
*          INTO @DATA(mat_range).
*
*        out->write( : data = sy-uname ),
*                      data = sy-datum ),
*                      data = sy-uzeit ),
*                      data = mat_range )->display( ).

**********************************************************************
*line_exists and line_index example

*    IF line_exists( it_item[ age = 29 ] ).
*      out->write( : 'Found age' )->display( ).
*    ELSE.
*      out->write( : 'Age not found' )->display( ).
*    ENDIF.
*
*    DATA(lv_index) = line_index( it_head[ id = 3 ] ).
*    out->write( : |Index found as | && lv_index )->display( ).
**********************************************************************

*    DATA(lv_var1) = '0000123456'.
*
*    WRITE:/ lv_var1.
*
*    lv_var1 = | { lv_var1 ALPHA = OUT } |.
*
*    WRITE:/ lv_var1.
*
*    DATA: lv_var2 TYPE vbeln VALUE '1234'.
*
*    WRITE:/ lv_var2.
*
*    DATA(lv_var3) = | { lv_var2 ALPHA = IN } |.
*
*    WRITE:/ lv_var3.

**********************************************************************

*CAST operator in ABAP.

*The casting operator CAST is a CONSTRUCTOR operator that performs a down cast or an up cast for the argument object and creates
*reference variable as a result.

*    DATA: structdescr TYPE REF TO cl_abap_structdescr.
*
*    structdescr ?= cl_abap_typedescr=>describe_by_name( 'MARA' ).
*
*    DATA: components TYPE abap_compdescr_tab.
*
*    components  = structdescr->components.
*
*    out->write( components )->display( ).
*
*    DATA(components1) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( 'MARA' ) )->components.
*
*    out->write( components1 )->display( ).

**********************************************************************

*    DATA: lv_discount TYPE p DECIMALS 2 VALUE '0.25'.
*
*    SELECT
*    FROM vbap
*    FIELDS vbeln,
*           'Material' && ' ' && matnr AS material,
*           matkl,
*           netpr,
*           waerk,
*           CASE
*            WHEN netpr > 10
*            THEN 'High'
*            ELSE
*            'Low'
*            END AS sales_type,
*            netpr * @lv_discount AS discount,
*            ( netpr - ( @lv_discount * netpr ) ) AS net_amount,
*            dats_days_between( @sy-datum, erdat ) AS dayssincecreated
*            WHERE erdat > '20220101'
*            INTO TABLE @DATA(lt_final).
*
*    out->write( lt_final )->display( ).
*
*
*    TYPES: BEGIN OF ty_type,
*             id        TYPE i,
*             from_date TYPE datum,
*             to_date   TYPE datum,
*           END OF ty_type.
*
*    DATA: it_table TYPE TABLE OF ty_type,
*          lv_from  TYPE datum,
*          lv_to    TYPE datum.
*
*    TYPES: BEGIN OF ty_type1,
*           date type datum,
*           END OF TY_TYPE1.
*
*    DATA: lt_dates TYPE TABLE OF ty_type1.
*    DATA: lr_range TYPE RANGE OF datum.
*
*    it_table = VALUE #( ( id = '1' from_date = '20220801' to_date = '20220805' )
*                        ( id = '1' from_date = '20220810' to_date = '20220815' )
*                      ).
*
*    lv_from = '20220801'.
*    lv_to = '20220803'.
*
*    DATA(lv_dates) = REDUCE string( INIT text = ``
*                                     FOR ls_table IN it_table
*                                     FOR date = ls_table-from_date THEN date + 1 WHILE date <= ls_table-to_date
*                                     NEXT text = text && |{ date }, | ).
*
**    out->write( lv_dates )->display( ).
*
*
*    SPLIT lv_dates AT ',' INTO TABLE lt_dates.
*
*    lr_range = VALUE #( FOR ls_date in lt_dates
*                        ( sign = 'I'
*                        option = 'EQ'
*                        low = ls_date-date ) ).
*
*    IF lv_from IN lr_range AND lv_to IN lr_range.
*      out->write( 'Yes' )->display( ).
*    ELSE.
*      out->write( 'No' )->display( ).
*    ENDIF.
*
*DATA: lv_num TYPE INT8 VALUE 32433423.
*
*DATA(lv_result) = REDUCE string(
*                    INIT text = ``
*                         rem = 0
*                         rev = 0
*                    FOR i = lv_num THEN i / 10 WHILE i <> 0
*                    NEXT rem = i MOD 10
*                         rev = rev * 10 + rem
*                         text = COND #(
*                                        WHEN rev = lv_num
*                                        THEN lv_num && ' is a palindrome.'
*                                        ELSE lv_num && ' is not a palindrome.' )
*                    ).
*
*out->write( lv_result )->display( ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

*  DATA(lo_class) = NEW lcl_class( ).
*
**lo_class->insert( ).
*  lo_class->process( ).

  NEW lcl_class( )->process( ).